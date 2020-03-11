#!/bin/sh
echo "100k-loopback start"

DOJOT_USERNAME=${DOJOT_USERNAME:-"admin"}
DOJOT_PASSWORD=${DOJOT_PASSWORD:-"admin"}

AUTH_HOST=${AUTH_HOST:-"http://auth:5000"}
DATA_BROKER_HOST=${DATA_BROKER_HOST:-"http://data-broker:80"}
KAFKA_HOSTS=${KAFKA_HOSTS:-"kafka-server:9092"}

LOOPBACK_CONSUMER_GROUP=${LOOPBACK_CONSUMER_GROUP:-"100k-loopback-group"}
DEVICE_DATA=${DEVICE_DATA:-"device-data"}
DEVICE_CONFIGURE=${DEVICE_CONFIGURE:-"iotagent.device"}

# auth
readonly AUTH_DATA="{\"username\": \"${DOJOT_USERNAME}\", \"passwd\":\"${DOJOT_PASSWORD}\"}"
readonly JSON_CONTENT_TYPE="Content-Type:application/json"
readonly TOKEN=$(curl --silent -X POST ${AUTH_HOST} -H "${JSON_CONTENT_TYPE}" -d "${AUTH_DATA}" | jq '.jwt' -r)

if [ ! -z "$TOKEN" ]
then
    echo "Retrived token sucessfully ..."
    DEVICE_DATA_TOPIC=$(curl --silent -X GET ${DATA_BROKER_HOST}/topic/${DEVICE_DATA} -H "Authorization: Bearer ${TOKEN}" | jq '.topic' -r)
    DEVICE_CONFIGURE_TOPIC=$(curl --silent -X GET ${DATA_BROKER_HOST}/topic/${DEVICE_CONFIGURE} -H "Authorization: Bearer ${TOKEN}" | jq '.topic' -r)

    echo ${DEVICE_DATA_TOPIC}
    echo ${DEVICE_CONFIGURE_TOPIC}

    if [ ! -z "${DEVICE_DATA_TOPIC}" -a ! -z "${DEVICE_CONFIGURE_TOPIC}" ]
    then

        echo "Starting loopback ...."
        kafkacat -C -b ${KAFKA_HOSTS} -q -f '{ "key": "%k" , "msg": %s }\n' -u -G ${LOOPBACK_CONSUMER_GROUP} ${DEVICE_DATA_TOPIC} | \
        unbuffer -p jq -r '"\(.key)@{\"event\": \"configure\",\"meta\": {\"service\": \"\(.msg.metadata.tenant)\",\"timestamp\": \(.msg.metadata.timestamp)},\"data\" : {\"id\" : \"\(.msg.metadata.deviceid)\",\"attrs\": \(.msg.attrs)}}"' \
        |  kafkacat -P -b ${KAFKA_HOSTS} -t ${DEVICE_CONFIGURE_TOPIC} -K @ -l
        
        echo "Application Stopped restarting ..."
    else
        "Restarting - unable to retrieve '${DEVICE_DATA}' and '${DEVICE_CONFIGURE}' topics"
    fi
else
   echo "Restarting token not available ..."
    exit 1
fi