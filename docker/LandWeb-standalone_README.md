# LandWeb Docker image

The image runs LandWeb in RStudio Server.
It holds R at the version in `renv.lock` (on `rocker/geospatial`), the project's R packages restored from `renv.lock`, TeX Live and Quarto for the reports, and the project with its submodules at `/home/rstudio/GitHub/LandWeb`.
It holds no data: mount `inputs/` and `outputs/` from the host.

## Getting the image

GitHub Actions builds the image (`.github/workflows/docker-image.yaml`) and pushes it to the GitHub Container Registry:

| Tag | Built from |
|---|---|
| `latest` | the newest build of `main`, or the newest release |
| `3.0.0` (etc.) | a published release, tagged `v3.0.0` |
| `YYYY-MM-DD` | the monthly rebuild of `main`, or a by-hand build of `main` |
| `<branch>` | a by-hand build of another branch (for testing) |

```bash
docker pull ghcr.io/predictiveecology/landweb:latest
```

To build by hand, run the workflow from the repository's Actions tab ("Run workflow"), or:

```bash
gh workflow run docker-image.yaml --ref main
```

## Running a container

```bash
INPUTS_HOST=/path/to/LandWeb/inputs
OUTPUTS_HOST=/path/to/LandWeb/outputs
PROJ_CONTAINER=/home/rstudio/GitHub/LandWeb

docker run -d -it \
  -e USERID=$(id -u) \
  -e GROUPID=$(id -g) \
  -e PASSWORD='<mySecretPassword>' \
  -e GITHUB_PAT=$(grep GITHUB_PAT ~/.Renviron | cut -d '=' -f 2) \
  --memory=128g \
  --cpus=32 \
  -p 127.0.0.1:8080:8787 \
  --mount type=bind,source=$INPUTS_HOST,target=$PROJ_CONTAINER/inputs \
  --mount type=bind,source=$OUTPUTS_HOST,target=$PROJ_CONTAINER/outputs \
  --name landweb01 \
  ghcr.io/predictiveecology/landweb:latest
```

- `USERID` and `GROUPID` make the container's `rstudio` user your user, so files written to the mounted directories are yours.
- `GITHUB_PAT` is needed only to install further packages from GitHub inside the container.
- To share a host renv cache with the container, add `-e RENV_PATHS_CACHE=/renv/cache -v /path/to/renv/cache:/renv/cache`.
  The image does not need one: its packages are in the project library.

Open RStudio in a browser at `localhost:8080`, and log in as `rstudio` with the password above.
If the container runs on a remote machine, forward the port first:

```bash
ssh <host> -L8080:localhost:8080
```

When finished, stop and remove the container:

```bash
docker stop landweb01
docker rm landweb01
```

## Building locally

The workflow is the usual way to build.
To test a change to the Dockerfile, build from the repository on GitHub, which gives a context of tracked files only, submodules included:

```bash
docker buildx build \
  --file docker/LandWeb-standalone.Dockerfile \
  --build-arg R_VERSION=$(jq -r .R.Version renv.lock) \
  --secret id=github_pat,env=GITHUB_PAT \
  --tag landweb:test \
  "https://github.com/PredictiveEcology/LandWeb.git#development"
```

Building from a working tree (context `.`) also works; `docker/LandWeb-standalone.Dockerfile.dockerignore` keeps the host's renv library, secrets and data links out of the image, but anything else untracked goes in.

`R_VERSION` must be the R version in `renv.lock`; the build stops if it is not.
The `github_pat` secret raises GitHub's rate limit for the packages renv installs from GitHub; it never enters an image layer.
BuildKit keeps the renv package cache in a cache mount between local builds, so a rebuild after a change to `renv.lock` installs only the packages that changed.
A first build restores about 450 packages; expect it to take an hour or more.
