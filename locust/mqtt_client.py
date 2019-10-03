"""
    Handles Paho MQTT-Client operations like publish/subscription, connection,
    loop function.
"""
import paho.mqtt.client as mqtt
import os
import logging
from time import gmtime, strftime, time
import json
import random
import threading

from utils import Utils
from locust import TaskSet, task, seq_task

MQTT_HOST = os.environ.get("DOJOT_MQTT_HOST", "127.0.0.1")
MQTT_PORT = int(os.environ.get("DOJOT_MQTT_PORT", "1883"))
MQTT_TIMEOUT = int(os.environ.get("DOJOT_MQTT_TIMEOUT", "60"))
MQTT_QOS = 1

TENANT = "admin"
REQUEST_TYPE = 'mqtt'
MESSAGE_TYPE_PUB = 'publish'
PUBLISH_TIMEOUT = 5000

# Dir of Certificates
# Just a test! These cert will be stay on Redis
CA_CRT = "/l/disk0/kevin/Downloads/ca.crt"
DEVICE_CRT = "/l/disk0/kevin/Downloads/admin_46b6c7.crt"
PRIVATE_KEY = "/l/disk0/kevin/Downloads/admin_46b6c7.key"

class MQTT_Client:

    def __init__(self, device_id, filename_dir):
        username = '{0}:{1}'.format(TENANT, device_id)

        self.mqttc = mqtt.Client()
        self.mqttc.username_pw_set(username, '')
        self.pubmmap = {}

        self.filename_dir = filename_dir

    def get_client(self):
        return self.mqttc

    def connect(self):
        #self.mqttc.tls_set(CA_CRT, DEVICE_CRT, PRIVATE_KEY)
        #self.mqttc.tls_insecure_set(True)
        self.mqttc.connect(host=MQTT_HOST, port=MQTT_PORT, keepalive=MQTT_TIMEOUT)

    def loop(self, time_loop):
        self.mqttc.loop(timeout=time_loop)

    def publishing(self, device_id):
        topic = "/{0}/{1}/attrs".format(TENANT, device_id)
        payload = {'int': 1}

        start_time = time()

        try:
            res = self.mqttc.publish(
                    topic=topic,
                    payload=json.dumps(payload),
                    qos=MQTT_QOS,
                    retain=False
                )

            [ err, mid ] = res

            if err:
                logging.error(str(err))
                Utils.fire_locust_failure(
                    request_type=REQUEST_TYPE,
                    name=MESSAGE_TYPE_PUB,
                    response_time=Utils.time_delta(start_time, time()),
                    exception=ValueError(err)
                )
                log_file = open(self.filename_dir, "a")
                log_file.write("publish ERROR: err,mid:"+str(err)+","+str(mid)+"\n")
                log_file.close()

            self.pubmmap[mid] = {
                'name': MESSAGE_TYPE_PUB,
                'qos': MQTT_QOS,
                'topic': topic,
                'payload': payload,
                'start_time': start_time,
                'timed_out':PUBLISH_TIMEOUT,
                'messages': 'messages'
            }

            message = self.pubmmap.pop(mid, None)
            end_time = time()
            total_time = Utils.time_delta(message['start_time'], end_time)
            
            Utils.fire_locust_success(
                request_type=REQUEST_TYPE,
                name=message['name'],
                response_time=total_time,
                response_length=len(message['payload']),
            )

        except Exception as e:
            log_file = open(self.filename_dir, "a")
            log_file.write(str(e)+"\n")
            log_file.close()
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_PUB,
                response_time=Utils.time_delta(start_time, time()),
                exception=e,
            )

    def locust_on_publish(self, client, userdata, mid):
        logging.info("--locust_on_publish--")

        end_time = time()
        message = self.pubmmap.pop(mid, None)

        if message is None:
            log_file = open(self.filename_dir, "a")
            log_file.write("message is none\n")
            log_file.close()
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_PUB,
                response_time=0,
                exception=ValueError("Published message could not be found"),
            )
            return

        total_time = Utils.time_delta(message['start_time'], end_time)

        Utils.fire_locust_success(
            request_type=REQUEST_TYPE,
            name=message['name'],
            response_time=total_time,
            response_length=len(message['payload']),
        )