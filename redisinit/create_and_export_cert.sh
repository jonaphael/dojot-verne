#!/bin/bash
# Generates the certificates, export their files and the Redis dump.

REDIS_INSTANCES=`docker ps -aqf "name=redis"`
N_REDIS=`echo REDIS_INSTANCES | wc -l`

if [ $N_REDIS -gt 1 ]
then
  echo "More than one Redis instance encountered. Please type the ID to the correct one: "
  read REDIS_ID
else
  REDIS_ID=$REDIS_INSTANCES
fi

echo "Redis ID: $REDIS_ID"

python3 insert_things.py

rm -rf cert
mkdir cert

python3 create_pem_files_from_redis.py

./retrieveCACertificate.sh

docker exec -it $REDIS_ID redis-cli save

echo "Retrieving dump"
docker cp $REDIS_ID:/data/dump.rdb .
