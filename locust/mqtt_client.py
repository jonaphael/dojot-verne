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
from log_controller import LogController
import ssl

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

    def __init__(self, device_id: str, run_id: str):
        """MQTT client constructor.

        Args:
            device_id (string): device identifier
            run_id (string): client run identifier
        """

        self.device_id = device_id
        self.run_id = run_id

        # Certification files
        cert_dir = config['locust']['redis']['cert_dir']
        ca_cert_file = cert_dir + config['locust']['redis']['ca_cert_file']
        cert_file = cert_dir + Utils.get_certificate_file(device_id)
        key_file = cert_dir + Utils.get_private_key_file(device_id)

        self.username = '{0}:{1}'.format(TENANT, device_id)
        self.topic = "{0}/attrs".format(self.username)

        self.log = LogController(self.run_id)

        self.is_connected = False

        # Configuring MQTT client
        self.mqttc = mqtt.Client(client_id=device_id)

        # Setting up TLS
        self.mqttc.tls_set(ca_cert_file, cert_file, key_file)
        # TODO: investigate the problem when the insecure TLS mode is False
        self.mqttc.tls_insecure_set(True)

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

    def connect(self) -> None:
        """Connects to MQTT host."""

        try:
            self.mqttc.connect_async(host=config['mqtt']['host'], port=config['mqtt']['port'],
                                     keepalive=config['mqtt']['con_timeout'])
            self.mqttc.loop_start()
        except Exception as e:
            self.log.append_log(e)
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name='connect',
                response_time=0,
                response_length=0,
                exception=ConnectError("disconnected")
            )

    def publishing(self) -> None:
        """Handles the publishing of messages to MQTT host."""

        payload = { 'int': 1 }
        start_time = time.time()

        try:
            err, mid = self.mqttc.publish(
                topic=self.topic,
                payload=json.dumps(payload),
                qos=config['mqtt']['qos']
            )

            if err:
                raise ValueError(err)

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
            timestamp = int(datetime.timestamp(datetime.now()))
            err_msg = Utils.error_message(int(str(e)))
            self.log.append_log("{0}\nTime: {1} - {2}\n".format(err_msg, timestamp, str(e)))
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_PUB,
                response_time=Utils.time_delta(start_time, time.time()),
                exception=err_msg,
            )

    def subscribing(self, topic: str=None) -> None:
        """Handles the subscription in MQTT topics.

        Args:
            topic (string): topic to subscribe
        """

        if topic is None:
            topic = self.topic

        start_time = time.time()

        try:
            err, mid = self.mqttc.subscribe((topic, config['mqtt']['qos']))

            if err:
                raise ValueError(err)

            self.submmap[mid] = {
                'name': MESSAGE_TYPE_SUB,
                'qos': config['mqtt']['qos'],
                'topic': topic,
                'payload': "",
                'start_time': start_time,
                'timed_out': config['mqtt']['sub_timeout'],
                'messages': 'messages'
            }

            if config['app']['debug']:
                logging.info("Successfully subscribed")

        except Exception as e:
            err_msg = Utils.error_message(int(str(e)))
            self.log.append_log(err_msg)

            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_SUB,
                response_time=Utils.time_delta(start_time, time.time()),
                exception=err_msg,
            )


    ###############
    ## Callbacks ##
    ###############

    def locust_on_subscribe(self, client: mqtt.Client, userdata, mid, granted_qos) -> None:
        """Subscription callback function."""

        end_time = time.time()
        message = self.submmap.pop(mid, None)

        if message is None:
            if config['app']['debug']:
                logging.error("subscribe message is None")
            self.log.append_log("subscribe message is None\n")
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_SUB,
                response_time=0,
                exception=ValueError("Subscribed message could not be found"),
            )
            return

        total_time = float(Utils.time_delta(message['start_time'], end_time))

        if total_time > float(message['timed_out']):
            if config['app']['debug']:
                logging.error("subscribe timed out, response time: {0}".format(total_time))
            self.log.append_log("subscribe timed out, response time: {0}".format(total_time))
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_SUB,
                response=total_time,
                exception=TimeoutError("subscribe timed out")
            )

        else:
            if config['app']['debug']:
                logging.info("Subscription received")
            Utils.fire_locust_success(
                request_type=REQUEST_TYPE,
                name=message['name'],
                response_time=total_time,
                response_length=0
            )

    def locust_on_publish(self, client: mqtt.Client, userdata, mid) -> None:
        """Publishing callback function. """

        end_time = time.time()
        message = self.pubmmap.pop(mid, None)

        if message is None:
            self.log.append_log("publish message is none\n")
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

        self.recvmqueue.put({
            # The end_time for publish is the start_time for the subscribed clients
            'start_time': end_time,
        })

    def locust_on_connect(self, client: mqtt.Client, flags_dict, userdata, rc) -> None:
        """Connection callback function."""

        if rc == 0:
            self.subscribing()
            self.is_connected = True
            Utils.fire_locust_success(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_CONNECT,
                response_time=0,
                response_length=0
            )

    def locust_on_disconnect(self, client: mqtt.Client, userdata, rc) -> None:
        """Disconnection callback function."""

        if rc != 0:
            self.is_connected = False
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_DISCONNECT,
                response_time=0,
                exception=DisconnectError("disconnected"),
            )

        self.mqttc.reconnect()

    def locust_on_message(self, client: mqtt.Client, userdata, message: mqtt.MQTTMessage):
        """Message reception callback function."""

        if message is not None:
            saved_message = self.recvmqueue.get()

            if saved_message is not None:
                Utils.fire_locust_success(
                    request_type=REQUEST_TYPE,
                    name=MESSAGE_TYPE_RECV_MESSAGE,
                    response_time=Utils.time_delta(saved_message['start_time'], time.time()),
                    response_length=len(message.payload)
                )
