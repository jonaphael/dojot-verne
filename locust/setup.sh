#!/bin/bash
# Creates n devices into dojot using a pre-defined template and
# adds the  device indentifiers into redis
set -ex

# Dojot parameters
DOJOT_URL=${DOJOT_URL:-"http://127.0.0.1:8000"}
DOJOT_USER=${DOJOT_USER:-"admin"}
DOJOT_PASSWD=${DOJOT_PASSWD:-"admin"}

# Redis parameters
REDIS_HOST=${REDIS_HOST:-"127.0.0.1"}
REDIS_PORT=${REDIS_PORT:-"6379"}
REDIS_PASSWD=${REDIS_PASSWD:-""}

# Devices
NUMBER_OF_DEVICES=${NUMBER_OF_DEVICES:-"10000"}

# Get JWT Token
echo 'Getting jwt token ...'
JWT=$(curl --silent -X POST ${DOJOT_URL}/auth \
-H "Content-Type:application/json" \
-d "{\"username\": \"${DOJOT_USER}\", \"passwd\" : \"${DOJOT_PASSWD}\"}" | jq '.jwt' | tr -d '"')
echo "... Got jwt token ${JWT}."

# Create Template
echo 'Creating template ...'
TEMPLATE_ID=$(curl --silent -X POST ${DOJOT_URL}/template \
-H 'Content-Type:application/json' \
-H "Authorization: Bearer ${JWT}" \
-d  '{
       "label": "CargoContainer",
       "attrs": [
                  {
                    "label": "temperature",
                    "type": "dynamic",
                    "value_type": "float"},
                  {
                    "label": "humidity",
                    "type": "dynamic",
                    "value_type": "float"},
                  {
                    "label": "lightness",
                    "type": "dynamic",
                    "value_type": "float"},
                  {
                    "label" : "gps",
                    "type" : "dynamic",
                    "value_type" : "geo:point"}
               ]
    }' | jq '.template.id')
echo "... Created template ${TEMPLATE_ID}."

# Create Devices
N=0 # Number of created devices
I=0 # Iteration
INCREMENT=500 # Number of devices created in batch
KEY=1 # Incremental key for the device
while [ ${N} -lt ${NUMBER_OF_DEVICES} ]
do
  let REMAINING=NUMBER_OF_DEVICES-N
  if [ ${INCREMENT} -gt ${NUMBER_OF_DEVICES} ]
  then
    INCREMENT=${NUMBER_OF_DEVICES}
  elif [ ${INCREMENT} -gt ${REMAINING} ]
  then
    let INCREMENT=REMAINING
  fi
  let N=N+INCREMENT

  echo "Creating ${INCREMENT} new devices (Iteration = ${I}) ..."
  DEVICE_IDS=$(curl --silent -X POST ${DOJOT_URL}/device?count=${INCREMENT}\&verbose=false \
  -H 'Content-Type:application/json' \
  -H "Authorization: Bearer ${JWT}" \
  -d  "{
        \"templates\": [\"${TEMPLATE_ID}\"],
        \"attrs\": {},
        \"label\": \"CargoContainer_${I}\"
      }" | jq '.devices[].id' | tr -d '"')
  for DEVICE_ID in ${DEVICE_IDS}
  do
    echo "SET ${KEY} ${DEVICE_ID}" | redis-cli -h ${REDIS_HOST} -p ${REDIS_PORT} -a "${REDIS_PASSWD}" &> /dev/null
    let KEY=KEY+1
  done
  echo "... Created ${N} devices from ${NUMBER_OF_DEVICES}"
  let I=I+1
done

