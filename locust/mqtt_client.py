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
from queue import Queue

from utils import Utils
from config import config

from locust import TaskSet, task, seq_task


TENANT = "admin"
REQUEST_TYPE = 'mqtt'
MESSAGE_TYPE_CONNECT = 'connect'
MESSAGE_TYPE_DISCONNECT = 'disconnect'
MESSAGE_TYPE_PUB = 'publish'
MESSAGE_TYPE_SUB = 'subscribe'
MESSAGE_TYPE_RECV_MESSAGE = 'recv_message'


class LocustError(Exception):
    pass


class TimeoutError(ValueError):
    pass


class ConnectError(Exception):
    pass


class DisconnectError(Exception):
    pass


class MQTT_Client:

    def __init__(self, device_id: str, client_dir: str, run_id: str):
        """MQTT client constructor.

        Args:
            device_id (string): device identifier
            client_dir (string): directory to export data from the client
            run_id (string): client run identifier
        """

        self.device_id = device_id
        self.client_dir = client_dir
        self.run_id = run_id

        self.username = '{0}:{1}'.format(TENANT, device_id)
        self.topic = "/{0}/{1}/attrs".format(TENANT, self.device_id)

        self.lst_log_error = list()

        # Configuring MQTT client
        self.mqttc = mqtt.Client(client_id=device_id)
        self.mqttc.username_pw_set(self.username, '')

        # Registering MQTT client callbacks
        self.mqttc.on_connect = self.locust_on_connect
        self.mqttc.on_disconnect = self.locust_on_disconnect
        self.mqttc.on_publish = self.locust_on_publish
        self.mqttc.on_subscribe = self.locust_on_subscribe
        self.mqttc.on_message = self.locust_on_message

        self.pubmmap = {}
        self.submmap = {}
        self.recvmqueue = Queue()

    def save_log_list(self) -> None:
        """Saves the list of log messages in the log file."""

        log_file = open(self.client_dir + config['locust']['log_file'], "w")
        log_file.writelines(self.lst_log_error)
        log_file.close()

    def connect(self) -> None:
        """Connects to MQTT host."""

        try:
            self.mqttc.connect_async(host=config['mqtt']['host'], port=config['mqtt']['port'],
                                     keepalive=config['mqtt']['con_timeout'])
            self.mqttc.loop_start()
        except Exception as e:
            self.lst_log_error.append(e)
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
            err, mid = self.mqttc.publish(
                topic=self.topic,
                payload=json.dumps(payload),
                qos=config['mqtt']['qos']
            )

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
                'qos': config['mqtt']['qos'],
                'topic': self.topic,
                'payload': payload,
                'start_time': start_time,
                'timed_out': config['mqtt']['pub_timeout'],
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
                name=MESSAGE_TYPE_CONNECT,
                response_time=0,
                response_length=0
            )

    def locust_on_disconnect(self, client, userdata, rc):
        print("--locust_on_disconnect--, RC: " + str(rc))

        if rc != 0:
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_DISCONNECT,
                response_time=0,
                exception=DisconnectError("disconnected"),
            )

        self.mqttc.reconnect()
