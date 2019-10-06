import logging
import os
import time
import uuid

from locust import Locust, task, TaskSet, events
from mqtt_client import MQTT_Client

from redis_client import RedisClient
cache = RedisClient()
cache.connect()

TASK_MIN_TIME = os.environ.get("TASK_MIN_TIME", "1000")
TASK_MAX_TIME = os.environ.get("TASK_MAX_TIME", "1000")


LOG_DIR = os.environ.get("LOCUST_LOG_DIR", '/log')

if not os.path.exists(LOG_DIR):
    os.makedirs(LOG_DIR)

filename_dir = "{0}/{1}.log".format(LOG_DIR, uuid.uuid4())


class MqttLocust(Locust):
    def __init__(self, *args, **kwargs):
        super(Locust, self).__init__(*args, **kwargs)
        device_id = cache.next_device_id()
        self.client = MQTT_Client(device_id, filename_dir)
        self.client.connect()


class ThingBehavior(TaskSet):
    @task
    def publish(self):
        self.client.publishing()

    def on_start(self):
        time.sleep(5)

    def on_stop(self):
        self.client.save_log_list()


class Client(MqttLocust):
    task_set = ThingBehavior
    min_wait = int(TASK_MIN_TIME)
    max_wait = int(TASK_MAX_TIME)
