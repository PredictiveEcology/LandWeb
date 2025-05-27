# Docker images for LandWeb

1. build the container (once; don't need to rebuild unless there's a major change):

```bash
cd ~/GitHub/LandWeb

eval `ssh-agent -s`
ssh-add ~/.ssh/id_ed25519_GitHub
```

## main

```bash
docker build . \
  --ssh default \
  --build-arg GH_ORG=PredictiveEcology \
  --build-arg GH_REPO=LandWeb \
  --build-arg GH_TAG=main \
  --build-arg USERID=$(id -u) \
  --build-arg GROUPID=$(id -g) \
  -f docker/LandWeb-standalone.Dockerfile \
  -t achubaty/landweb-standalone:latest

## optionally, push to docker hub
docker push achubaty/landweb-standalone:latest
```

## development

```bash
docker build . \
  --ssh default \
  --build-arg GH_ORG=PredictiveEcology \
  --build-arg GH_REPO=LandWeb \
  --build-arg GH_TAG=development \
  -f docker/LandWeb-standalone.Dockerfile \
  -t achubaty/landweb-standalone:development

## optionally, push to docker hub
docker push achubaty/landweb-standalone:development
```

2. launch an instance of the container:

**NOTE:** the first time launching the container will take a while, as packages need to be restored, but this will be much faster if providing a pre-populated `renv` package cache.

```bash
PROJ_PATHS_INPUTS_CONTAINER=/home/rstudio/GitHub/LandWeb/inputs
PROJ_PATHS_INPUTS_HOST=/mnt/projects/HRV/LandWeb/inputs

PROJ_PATHS_OUTPUTS_CONTAINER=/home/rstudio/GitHub/LandWeb/outputs
PROJ_PATHS_OUTPUTS_HOST=/mnt/projects/HRV/LandWeb/outputs

RENV_PATHS_CACHE_CONTAINER=/renv/cache
RENV_PATHS_CACHE_HOST=/mnt/shared_cache/renv/cache

docker run -d -it \
  -e USERID=$(id -u) \
  -e GROUPID=$(id -g) \
  -e GITHUB_PAT=$(cat ${HOME}/.Renviron | grep GITHUB_PAT | cut -d '=' -f 2) \
  -e PASSWORD='myPassword' \
  -e RENV_PATHS_CACHE=$RENV_PATHS_CACHE_CONTAINER \
  -v $RENV_PATHS_CACHE_HOST:$RENV_PATHS_CACHE_CONTAINER \
  --memory=128g \
  --cpus=32 \
  -p 127.0.0.1:8080:8787 \
  --mount type=bind,source=$PROJ_PATHS_INPUTS_HOST,target=$PROJ_PATHS_INPUTS_CONTAINER \
  --mount type=bind,source=$PROJ_PATHS_OUTPUTS_HOST,target=$PROJ_PATHS_OUTPUTS_CONTAINER \
  --name landweb01 \
  achubaty/landweb-standalone:development
```

3. to connect to the container, open ssh tunnel, then open browser at `localhost:8080`:

```bash
ssh picea.for-cast.ca -L8080:localhost:8080
```

4. once finished, you can stop and destroy the container:

```bash
docker stop landweb01
docker rm landweb01
```

