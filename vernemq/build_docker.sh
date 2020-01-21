#!/bin/bash

TAG=${1:-"latest"}

echo "$TAG"

cd plugins || exit
./plugin_builder.sh

cd .. 
docker build -t dojot/vernemq:"$TAG" .
docker push dojot/vernemq:"$TAG"