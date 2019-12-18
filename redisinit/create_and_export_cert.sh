#!/bin/bash
# Generates the certificates, export their files and the Redis dump.

REDIS_INSTANCES=`docker ps -aqf "name=redis"`
N_REDIS=`docker ps -aqf "name=redis" | wc -l`

# If there is no Redis instances
if [ $N_REDIS -eq 0 ]
then
  echo "No Redis instance encountered!"
  exit 1
fi

if [ $N_REDIS -gt 1 ]
then
  echo "More than one Redis instance encountered. Please type the ID to the correct one: "
  read REDIS_ID
else
  REDIS_ID=$REDIS_INSTANCES
fi

echo "Redis ID: $REDIS_ID"

python3 insert_things.py

# The return from insert_things.py should not be 1 to have succeeded
if [ $? -eq 1 ]
then
  echo "Error while running insert_things.py"
  exit 1
fi

rm -rf cert
mkdir cert

python3 create_pem_files_from_redis.py

./retrieveCACertificate.sh

docker exec -it $REDIS_ID redis-cli save

echo "Retrieving dump"
docker cp $REDIS_ID:/data/dump.rdb .
