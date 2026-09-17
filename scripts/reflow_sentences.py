#!/usr/bin/env python3
"""Rewrite Quarto/Markdown prose so that each sentence begins on its own line.

LandWeb reports are written one sentence per line: a reworded sentence then shows up as a
one-line diff instead of reflowing a whole paragraph, which makes review and `git blame`
usable on prose.

    scripts/reflow_sentences.py reports/*.qmd      # rewrite in place
    scripts/reflow_sentences.py --check reports/*.qmd   # exit 1 if anything would change

Left untouched: YAML headers, fenced code and knitr chunks, tables, table/figure captions,
div fences (`:::`), headings, raw LaTeX lines, HTML comments, and lines ending in a hard
line break. Paragraphs, block quotes and list items are joined and then split at sentence
boundaries; a list item keeps its marker and its continuation sentences are indented to the
item's text.

Every rewrite is checked before it is written: the file's pandoc AST must be unchanged, with
chunk fences normalised (pandoc pairs ```{r label, opt=TRUE} differently than knitr does),
soft breaks read as spaces, and the native pretty-printer's own wrapping ignored. A file
whose AST would change is reported and left alone -- most often because a line ends in a
backslash-escaped non-breaking space or a hard line break, whose meaning depends on where the
line breaks. Pass --no-verify to skip the check (it needs pandoc on PATH).
"""

import argparse
import re
import subprocess
import sys

## periods here never end a sentence
ABBREV = {
    "e.g", "i.e", "cf", "vs", "etc", "al", "fig", "eq", "no", "dr", "mr", "ms", "prof",
    "approx", "ca", "inc", "ltd", "st", "jr", "sr", "viz", "resp", "pp", "ch", "sec",
    "vol", "ref", "min", "max", "avg", "sd", "yr", "ha", "km", "m", "v",
}
CODE_SPAN = re.compile(r"`+[^`]*`+|\$[^$\n]+\$")
WS = r"[ \t\r\n\f\v]"  # NOT \s: that includes U+00A0, which must survive a rewrite
SENT_END = re.compile(r"[.!?][\"')\]`*_]*(" + WS + r"+)")
## a produced line must not start with something Markdown would read as a new block
BLOCK_START = re.compile(r"^(\s*([-*+]|\d+[.)])\s|#{1,6}\s|>|:|\||```|~~~|:::|\\)")
LIST_ITEM = re.compile(r"^(\s*)([-*+]|\d+[.)])(\s+)")
VERBATIM = re.compile(r"^(\s*\||#{1,6}\s|:::|\\|<!--|-->|\s*$|\$\$|!\[|\s*\{)")
FENCE = re.compile(r"^(\s*)(```+|~~~+)")
CHUNK_FENCE = re.compile(r"^(\s*```+)\{.*\}\s*$")


def _mask(text):
    """Hide code spans and inline math so their periods can't end a sentence."""
    spans = []

    def sub(m):
        spans.append(m.group(0))
        return f"\x00{len(spans) - 1}\x00"

    return CODE_SPAN.sub(sub, text), spans


def _unmask(text, spans):
    return re.sub(r"\x00(\d+)\x00", lambda m: spans[int(m.group(1))], text)


def split_sentences(text):
    """Split one paragraph's text into sentences, conservatively.

    Anything ambiguous is left joined: a missed split costs nothing, a wrong one is visible.
    """
    masked, spans = _mask(text)
    out, start = [], 0
    for m in SENT_END.finditer(masked):
        end, nxt = m.start(1), m.end(1)
        before = masked[:end].rstrip("\"')]`*_")
        word = re.split(WS + r"|[(\[]", before)[-1].rstrip(".")
        tail = word.split(".")[-1] if "." in word else word
        if tail.lower() in ABBREV or word.lower() in ABBREV:
            continue
        if len(word) == 1 and word.isalpha():  # an initial, as in "J. C. White"
            continue
        if masked[end - 1 : end + 1] == ".." or masked[max(0, end - 2) : end] == "..":
            continue
        if end and masked[end - 1].isdigit() and re.match(WS + r"*\d", masked[end:]):
            continue
        piece = masked[start : end + 1].strip()
        if piece.count("**") % 2 or piece.count("_") % 2:
            continue  # the period sits inside an unclosed emphasis run
        if re.fullmatch(r"\*\*[^*]+\*\*", piece) or re.fullmatch(r"_[^_]+_", piece):
            continue  # a bold/italic lead-in label, not a sentence
        rest = masked[nxt:]
        if not re.match(r"^[*_`\"'(\[\\]*[A-Z]", rest):
            continue
        if BLOCK_START.match(_unmask(rest, spans)):
            continue
        out.append(_unmask(masked[start:nxt].rstrip(" \t\r\n\f\v"), spans))
        start = nxt
    out.append(_unmask(masked[start:], spans))
    return [s for s in out if s]


def _flush(buf, kind, prefix, cont_indent, out):
    if not buf:
        return
    text = " ".join(x.strip(" \t\r\n\f\v") for x in buf).strip(" \t\r\n\f\v")
    if kind == "caption":  # joined onto one line, never split
        out.append(prefix + text)
    else:
        parts = split_sentences(text)
        out.append(prefix + parts[0])
        out.extend(cont_indent + p for p in parts[1:])
    buf.clear()


def reflow(lines):
    out, buf = [], []
    kind = prefix = cont = None
    in_yaml = bool(lines) and lines[0].strip() == "---"
    seen_yaml_close = False
    fence = None

    def close():
        nonlocal kind, prefix, cont
        _flush(buf, kind, prefix or "", cont or "", out)
        kind = prefix = cont = None

    for i, ln in enumerate(lines):
        if in_yaml and not seen_yaml_close:
            out.append(ln)
            if i > 0 and ln.strip() in ("---", "..."):
                seen_yaml_close = True
            continue
        if fence is not None:
            out.append(ln)
            if FENCE.match(ln) and ln.strip().startswith(fence):
                fence = None
            continue
        m = FENCE.match(ln)
        if m:
            close()
            out.append(ln)
            fence = m.group(2)
            continue
        if ln.rstrip().endswith("  ") or ln.rstrip().endswith("\\"):  # hard line break
            close()
            out.append(ln)
            continue
        cap = re.match(r"^\s*:\s", ln)
        if cap:
            close()
            kind, prefix, cont = "caption", cap.group(0), ""
            buf.append(ln[len(prefix) :])
            continue
        if VERBATIM.match(ln):
            close()
            out.append(ln)
            continue
        if ln.startswith(">"):
            if kind != "quote":
                close()
                kind, prefix, cont = "quote", "> ", "> "
            buf.append(re.sub(r"^>\s?", "", ln))
            continue
        li = LIST_ITEM.match(ln)
        if li:
            close()
            kind, prefix, cont = "list", li.group(0), " " * len(li.group(0))
            buf.append(ln[len(prefix) :])
            continue
        if kind is None:
            kind, prefix, cont = "para", "", ""
        buf.append(ln)
    close()
    return out


def pandoc_ast(text):
    """The document body's pandoc AST, normalised so that line breaks don't register."""
    lines = text.split("\n")
    if lines and lines[0].strip() == "---":
        for i, l in enumerate(lines[1:], 1):
            if l.strip() in ("---", "..."):
                lines = lines[i + 1 :]
                break
    lines = [CHUNK_FENCE.sub(r"\1r", l) for l in lines]
    res = subprocess.run(
        ["pandoc", "-f", "markdown", "-t", "native"],
        input="\n".join(lines), capture_output=True, text=True,
    )
    if res.returncode != 0:
        raise RuntimeError(res.stderr.strip())
    return re.sub(r"\s+", " ", res.stdout.replace("SoftBreak", "Space"))


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("paths", nargs="+", help="Quarto/Markdown files to reflow")
    ap.add_argument("--check", action="store_true",
                    help="report what would change; write nothing (exit 1 if any would)")
    ap.add_argument("--no-verify", action="store_true",
                    help="skip the pandoc AST check (not recommended)")
    args = ap.parse_args()

    if not (args.check or args.no_verify):
        try:
            subprocess.run(["pandoc", "--version"], capture_output=True, check=True)
        except (OSError, subprocess.CalledProcessError):
            sys.exit("pandoc is needed to verify the rewrite; install it or pass --no-verify")

    changed = failed = 0
    for path in args.paths:
        src = open(path).read()
        lines = src.split("\n")
        trailing_nl = bool(lines) and lines[-1] == ""
        res = "\n".join(reflow(lines[:-1] if trailing_nl else lines)) + ("\n" if trailing_nl else "")
        if res == src:
            print(f"unchanged {path}")
            continue
        changed += 1
        if args.check:
            print(f"would reflow {path}")
            continue
        if not args.no_verify:
            try:
                if pandoc_ast(src) != pandoc_ast(res):
                    print(f"NOT WRITTEN (AST would change) {path}")
                    failed += 1
                    continue
            except RuntimeError as e:
                print(f"NOT WRITTEN (pandoc failed: {e}) {path}")
                failed += 1
                continue
        open(path, "w").write(res)
        print(f"reflowed {path} ({len(lines)} -> {len(res.split(chr(10)))} lines)")

    if failed:
        return 2
    return 1 if (args.check and changed) else 0


if __name__ == "__main__":
    sys.exit(main())
