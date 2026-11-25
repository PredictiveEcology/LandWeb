# syntax=docker/dockerfile:1

## LandWeb in RStudio Server. Built and pushed by .github/workflows/docker-image.yaml;
## see docker/LandWeb-standalone_README.md.

## R must be the version in renv.lock: the workflow passes it in, and the restore step checks it.
ARG R_VERSION=4.6.1
FROM rocker/geospatial:${R_VERSION}

LABEL org.opencontainers.image.source="https://github.com/PredictiveEcology/LandWeb"

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ARG DEBIAN_FRONTEND=noninteractive

## TeX Live for the PDF reports: rocker/geospatial:4.6.1 has none. This is rocker's own script
## with the dated CTAN snapshot rocker/verse:4.6.1 uses; when a report needs a LaTeX package not
## installed here, Quarto installs it from the same snapshot. The script writes the whole PATH
## into Renviron.site, which overrides every R process's PATH (so R CMD INSTALL could not find
## the Rust compiler below); append to the inherited PATH instead.
ARG CTAN_REPO=https://www.texlive.info/tlnet-archive/2026/10/28/tlnet
ENV PATH=${PATH}:/usr/local/texlive/bin/linux
# hadolint ignore=SC2016
RUN /rocker_scripts/install_texlive.sh "${CTAN_REPO}" \
    && sed -i 's|^PATH=.*|PATH=${PATH}:/usr/local/texlive/bin/linux|' "${R_HOME}/etc/Renviron.site"

## Quarto at the version the report tests use (.github/workflows/tests.yaml).
ARG QUARTO_VERSION=1.9.38
RUN /rocker_scripts/install_quarto.sh "${QUARTO_VERSION}"

## System libraries for the packages in renv.lock, derived from the lockfile by the script the
## manual's CI uses. It needs renv, installed into a throwaway library. Debian's texlive is left
## out: TeX Live is installed above, and Debian's older pdflatex would come first on PATH.
WORKDIR /tmp/sysreqs
COPY renv.lock .github/scripts/sysreqs.R ./
RUN mkdir lib \
    && R_LIBS=/tmp/sysreqs/lib Rscript --vanilla \
      -e 'install.packages("renv", lib = "/tmp/sysreqs/lib", repos = Sys.getenv("CRAN"))' \
    && R_LIBS=/tmp/sysreqs/lib Rscript --vanilla sysreqs.R renv.lock apt.txt \
    && sed -i -E '/^(texlive|tex-common)/d' apt.txt \
    && apt-get update \
    && xargs -a apt.txt apt-get install -y --no-install-recommends curl \
    && rm -rf /var/lib/apt/lists/* /tmp/sysreqs

## renv: CRAN packages as Posit Package Manager binaries for this image's Ubuntu release (renv
## turns the URL into the binary one), and the library copied out of the cache rather than
## symlinked into it, so the image does not depend on the cache.
ENV RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/latest \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE \
    RENV_PATHS_PREFIX_AUTO=TRUE \
    RENV_WATCHDOG_ENABLED=FALSE

ARG PROJECT_DIR=/home/rstudio/GitHub/LandWeb
WORKDIR ${PROJECT_DIR}

## Restore the R packages before copying the code, so a code change alone does not repeat it.
## The renv cache lives in a BuildKit cache mount, which the workflow carries between runs; the
## GitHub token (for the GitHub-hosted packages) is a BuildKit secret, so it is in no layer.
## pizzarr is built from source with Rust (rustc >= 1.91); the toolchain stays in its own cache
## mount, out of the image. The downloads renv and biocmake keep under /root/.cache (~0.8 GB) are
## removed in the same step, so they never reach a layer.
COPY renv.lock .Rprofile ./
COPY renv/activate.R renv/settings.json renv/
## SC2016: the single-quoted R code is meant for R, not the shell.
# hadolint ignore=SC2016
RUN --mount=type=cache,id=renv-cache,target=/renv/cache,sharing=locked \
    --mount=type=cache,id=rust,target=/opt/rust,sharing=locked \
    --mount=type=secret,id=github_pat \
    if [ -s /run/secrets/github_pat ]; then \
      GITHUB_PAT="$(cat /run/secrets/github_pat)"; export GITHUB_PAT; \
    fi \
    && export RUSTUP_HOME=/opt/rust/rustup CARGO_HOME=/opt/rust/cargo \
    && export PATH="${CARGO_HOME}/bin:${PATH}" \
    && if ! command -v cargo >/dev/null; then \
      curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
        | sh -s -- -y --profile minimal --no-modify-path; \
    fi \
    && RENV_PATHS_CACHE=/renv/cache MAKEFLAGS="-j$(nproc)" Rscript -e ' \
      lock <- renv::lockfile_read("renv.lock"); \
      if (format(getRversion()) != lock$R$Version) \
        stop("R ", getRversion(), " is not the R ", lock$R$Version, " in renv.lock: set R_VERSION"); \
      renv::restore(prompt = FALSE)' \
    && rm -rf /root/.cache/R \
    && mkdir -p cache inputs outputs logs \
    && chown -R rstudio:rstudio /home/rstudio/GitHub

## Every package in renv.lock must load: a binary package can install and then fail to load on a
## missing system library.
# hadolint ignore=SC2016
RUN R_MAX_NUM_DLLS=500 Rscript -e ' \
      pkgs <- names(renv::lockfile_read("renv.lock")$Packages); \
      ok <- vapply(pkgs, function(p) tryCatch({ loadNamespace(p); TRUE }, error = function(e) { \
        message(p, ": ", conditionMessage(e)); FALSE }), logical(1)); \
      if (!all(ok)) stop(sum(!ok), " packages in renv.lock do not load")'

## The project code, submodules included.
COPY --chown=rstudio:rstudio . .

## RStudio opens the project; git accepts the checkouts whatever user the container remaps
## rstudio to (USERID at run time).
RUN mkdir -p /home/rstudio/.local/share/rstudio/projects_settings \
    && echo "${PROJECT_DIR}/LandWeb.Rproj" \
      > /home/rstudio/.local/share/rstudio/projects_settings/switch-to-project \
    && chown -R rstudio:rstudio /home/rstudio/.local \
    && git config --system --add safe.directory '*'
