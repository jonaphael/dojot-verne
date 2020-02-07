#!/bin/sh
while true; do echo hello; sleep 10;done

echo "100k-loopback start"

DOJOT_USERNAME=${DOJOT_USERNAME:-"admin"}
DOJOT_PASSWORD=${DOJOT_PASSWORD:-"admin"}
AUTH_HOST=${AUTH_HOST:-"http://10.202.46.164:30001"}
DATA_BROKER_HOST=${DATA_BROKER_HOST:-"http://10.202.46.164:30004"}

JSON_CONTENT_TYPE="Content-Type:application/json"

# auth
AUTH_URL="${AUTH_HOST}/auth"
AUTH_DATA="{\"username\": \"${DOJOT_USERNAME}\", \"passwd\":\"${DOJOT_PASSWORD}\"}"
DEVICE_DATA="device-data"
DEVICE_CONFIGURE="iotagent.device"
KAFKACAT_CONSUMER_GROUP=${KAFKACAT_CONSUMER_GROUP:-"100k-loopback-group"}

# device data
TOKEN=$(curl --silent -X POST ${AUTH_URL} -H "${JSON_CONTENT_TYPE}" -d "${AUTH_DATA}" | jq '.jwt' -r)

if [ ! -z "$TOKEN" ]
then
    echo "Retrived token sucessfully ..."
    DEVICE_DATA_TOPIC=$(curl --silent -X GET ${DATA_BROKER_HOST}/topic/${DEVICE_DATA} -H "Authorization: Bearer ${TOKEN}" | jq '.topic' -r)
    DEVICE_CONFIGURE_TOPIC=$(curl --silent -X GET ${DATA_BROKER_HOST}/topic/${DEVICE_CONFIGURE} -H "Authorization: Bearer ${TOKEN}" | jq '.topic' -r)

    echo ${DEVICE_DATA_TOPIC}
    echo ${DEVICE_CONFIGURE_TOPIC}

    if [ ! -z "${DEVICE_DATA_TOPIC}" -a ! -z "${DEVICE_CONFIGURE_TOPIC}" ]
    then
    
        kafkacat -C -b kafka-server -G ${KAFKACAT_CONSUMER_GROUP} ${DEVICE_DATA_TOPIC} -f '%k:%s\n' 
        # | kafkacat -P -b kafka-server -t abcde -K:  
    else
        "Restarting - unable to retrieve '${DEVICE_DATA}' and '${DEVICE_CONFIGURE}' topics"
    fi
else
   echo "Restarting token not available ..."
    exit 1
fi

