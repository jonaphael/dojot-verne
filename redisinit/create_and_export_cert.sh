#!/bin/bash
# Generates the certificates, export their files and the Redis dump.

REDIS_INSTANCES=docker ps -aqf "name=redis"
N_REDIS=`echo REDIS_INSTANCES | wc -l`

if [ $N_REDIS -gt 1 ]
then
  echo "More than one Redis instance encountered. Please type the ID to the correct one: "
  read REDIS_ID
else
  REDIS_ID=$REDIS_INSTANCES
fi

python3 insert_things.py
python3 create_pem_files_from_redis.py
./retrieveCACertificate.sh

SAVED_DUMP=`docker exec -it $REDIS_ID redis-cli`

if [ "$SAVED_DUMP" != "OK" ]
then
  echo "Error while saving dump. Goodbye!"
  exit 1
else
  echo "Successfully created database dump!"
fi

echo "Retrieving dump"
docker cp $REDIS_ID:/data/dump.rdb .