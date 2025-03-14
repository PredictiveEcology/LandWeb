#!/bin/bash

cd ~/GitHub/LandWeb
eval `ssh-agent -s`
ssh-add ~/.ssh/id_ed25519_GitHub

# docker buildx prune

docker build . \
   --ssh default \
   --build-arg GH_ORG=PredictiveEcology \
   --build-arg GH_REPO=LandWeb \
   --build-arg GH_TAG=development \
   -f docker/LandWeb-standalone.Dockerfile \
   -t achubaty/landweb-standalone:development

# docker push achubaty/landweb-standalone:development

docker build . \
   --ssh default \
   --build-arg GH_ORG=PredictiveEcology \
   --build-arg GH_REPO=LandWeb \
   --build-arg GH_TAG=development \
   -f docker/LandWeb-standalone.Dockerfile \
   -t achubaty/landweb-standalone:latest

# docker push achubaty/landweb-standalone:latest
