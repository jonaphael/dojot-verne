import logging
import os
import time
import uuid

from locust import Locust, task, TaskSet, events

from utils import Utils
from config import config
from mqtt_client import MQTT_Client
from redis_client import RedisClient


if Utils.is_master() and config['locust']['redis']['map_device_ids']:
    db = RedisClient()
    db.connect()
    db.map_device_ids()


class MqttLocust(Locust):
    """Locust client using MQTT."""

    def __init__(self, *args, **kwargs):
        super(Locust, self).__init__(*args, **kwargs)

        # Connects to Redis database that stores the device_id for each client
        cache = RedisClient()
        cache.connect()
        device_id = cache.next_device_id()

        # UUID to identify the client run
        run_id = uuid.uuid4()

        self.client = MQTT_Client(device_id, run_id)
        self.client.connect()


class ThingBehavior(TaskSet):
    """MQTT pub/sub load test class. It specifies the transmission and reception behaviours
    of messages from the MQTT broker."""

    @task
    def publish(self):
        """Publishes a message to MQTT broker."""
        if self.client.is_connected:
            self.client.publishing()

    def on_stop(self):
        # Saving the log messages in a file
        self.client.log.save_log_list()


class Client(MqttLocust):
    """The client that will run the tasks when hatched."""

    task_set = ThingBehavior
    min_wait = config['locust']['task_min_time']
    max_wait = config['locust']['task_max_time']
