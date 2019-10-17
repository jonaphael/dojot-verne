import logging
import os
import time
import uuid

from locust import Locust, task, TaskSet, events

from config import config
from mqtt_client import MQTT_Client
from redis_client import RedisClient


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
        # The directory named 'run_id' will be used to store all files related to the client's run
        client_dir = "{0}/{1}/".format(config['locust']['log_dir'], run_id)

        # Since the UUID is unique, we do not check whether the directory exists or not
        os.makedirs(client_dir)

        self.client = MQTT_Client(device_id, client_dir, run_id)
        self.client.connect()


class ThingBehavior(TaskSet):
    """MQTT pub/sub load test class. It specifies the transmission and reception behaviours
    of messages from the MQTT broker."""

    @task
    def publish(self):
        """Publishes a message to MQTT broker."""
        self.client.publishing()

    def on_start(self):
        time.sleep(5)

    def on_stop(self):
        # Saving the log messages in a file
        self.client.save_log_list()


class Client(MqttLocust):
    """The client that will run the tasks when hatched."""

    task_set = ThingBehavior
    min_wait = config['locust']['task_min_time']
    max_wait = config['locust']['task_max_time']
