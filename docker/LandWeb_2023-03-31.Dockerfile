FROM achubaty/spades-project:4.2.3

LABEL org.opencontainers.image.authors="achubaty@for-cast.ca"

## create targets for shared volume symlinks (used below)
ARG MNT_INPUTS
ARG MNT_OUTPUTS
RUN mkdir -p $MNT_INPUTS \
  && mkdir -p $MNT_OUTPUTS

## safely allow ssh access to private GitHub repos via the build machine's ssh
RUN mkdir -p -m 0700 ~/.ssh && ssh-keyscan github.com >> ~/.ssh/known_hosts

## update uid:gid for default user to mach some user
## pass USERNAME as environment variable during build if don't want `rstudio`
ARG USERNAME=rstudio
ARG USER_UID=1000
ARG USER_GID=1000

ENV DEFAULT_USER=$USERNAME

## clone project repo and set up additional project directories
WORKDIR /home/$DEFAULT_USER/GitHub

ARG GH_ORG=PredictiveEcology
ARG GH_REPO=LandWeb
ARG GH_TAG=development
RUN --mount=type=ssh git clone --single-branch -b $GH_TAG \
      --recurse-submodules="." \
      --recurse-submodules=":(exclude)app" \
      --recurse-submodules=":(exclude)deploy" \
      -j8 https://github.com/$GH_ORG/$GH_REPO

WORKDIR /home/$DEFAULT_USER/GitHub/$GH_REPO

RUN mkdir cache

## set default project (https://stackoverflow.com/a/53547334/1380598)
RUN mkdir -p /home/$DEFAULT_USER/.rstudio/projects_settings
RUN echo /home/$DEFAULT_USER/GitHub/$GH_REPO/$GH_REPO.Rproj > /home/$DEFAULT_USER/.rstudio/projects_settings/switch-to-project

## create symlinks to subdirs in shared volume
RUN ln -s $MNT_INPUTS ./inputs \
  && ln -s $MNT_OUTPUTS ./outputs

RUN groupmod --gid $USER_GID $DEFAULT_USER \
    && usermod --uid $USER_UID --gid $USER_GID $DEFAULT_USER \
    && chown -R $USER_UID:$USER_GID /home/$DEFAULT_USER
