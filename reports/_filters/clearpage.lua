-- The `{{< pagebreak >}}` shortcode emits `\newpage{}` in LaTeX. Replace it with `\clearpage`, which
-- first places every pending float, so a figure cannot drift past the next section (e.g. into the
-- references or an appendix). Other formats keep Quarto's native page break.
-- Runs `at: post-quarto` (see _quarto.yml), after Quarto has resolved the shortcode.
function RawBlock(el)
  if (el.format == "tex" or el.format == "latex") and el.text == "\\newpage{}" then
    return pandoc.RawBlock("tex", "\\clearpage")
  end
end
