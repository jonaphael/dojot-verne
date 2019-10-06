"""
    Handles Paho MQTT-Client operations like publish/subscription, connection,
    loop function.
"""
import paho.mqtt.client as mqtt
from datetime import datetime
import os
import logging
import time
import json
import random
import threading

from utils import Utils
from locust import TaskSet, task, seq_task

MQTT_HOST = os.environ.get("DOJOT_MQTT_HOST", "127.0.0.1")
MQTT_PORT = int(os.environ.get("DOJOT_MQTT_PORT", "1883"))
MQTT_QOS = 1

TENANT = "admin"
REQUEST_TYPE = 'mqtt'
MESSAGE_TYPE_PUB = 'publish'
PUBLISH_TIMEOUT = 40000


lst_log_error = list()


class LocustError(Exception):
    pass


class TimeoutError(ValueError):
    pass


class ConnectError(Exception):
    pass


class DisconnectError(Exception):
    pass


class MQTT_Client:

    def __init__(self, device_id, filename_dir):
        username = '{0}:{1}'.format(TENANT, device_id)

        self.mqttc = mqtt.Client(client_id=device_id)
        self.device_id = device_id
        self.mqttc.username_pw_set(username, '')

        self.mqttc.on_connect = self.locust_on_connect
        self.mqttc.on_disconnect = self.locust_on_disconnect
        self.mqttc.on_publish = self.locust_on_publish

        self.pubmmap = {}
        self.filename_dir = filename_dir

    def save_log_list(self):
        log_file = open(self.filename_dir, "w")
        log_file.writelines(lst_log_error)
        log_file.close()

    def connect(self):
        try:
            self.mqttc.connect_async(host=MQTT_HOST, port=MQTT_PORT,
                                     keepalive=120)
            self.mqttc.loop_start()
        except Exception as e:
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name='connect',
                response_time=0,
                response_length=0,
                exception=ConnectError("disconnected")
            )

    def publishing(self):
        topic = "/{0}/{1}/attrs".format(TENANT, self.device_id)
        payload = {'int': 1}

        start_time = time.time()

        try:
            res = self.mqttc.publish(
                topic=topic,
                payload=json.dumps(payload),
                qos=MQTT_QOS
            )

            [err, mid] = res

            if err:
                Utils.fire_locust_failure(
                    request_type=REQUEST_TYPE,
                    name=MESSAGE_TYPE_PUB,
                    response_time=Utils.time_delta(start_time, time.time()),
                    exception=ValueError(err)
                )

                timestamp = int(datetime.timestamp(datetime.now()))
                msg_error = "Time: {0} - {1}\n".format(timestamp, str(err))
                lst_log_error.append(msg_error)

            self.pubmmap[mid] = {
                'name': MESSAGE_TYPE_PUB,
                'qos': MQTT_QOS,
                'topic': topic,
                'payload': payload,
                'start_time': start_time,
                'timed_out': PUBLISH_TIMEOUT,
                'messages': 'messages'
            }

        except Exception as e:
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_PUB,
                response_time=Utils.time_delta(start_time, time.time()),
                exception=e,
            )

    def locust_on_publish(self, client, userdata, mid):

        end_time = time.time()
        message = self.pubmmap.pop(mid, None)

        if message is None:
            lst_log_error.append("message is none\n")
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

    def locust_on_connect(self, client, flags_dict, userdata, rc):
        if rc == 0:
            Utils.fire_locust_success(
                request_type=REQUEST_TYPE,
                name='connect',
                response_time=0,
                response_length=0
            )

    def locust_on_disconnect(self, client, userdata, rc):
        print("--locust_on_disconnect--, RC: " + str(rc))

        if rc != 0:
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name='disconnect',
                response_time=0,
                exception=DisconnectError("disconnected"),
            )

        self.mqttc.reconnect()
