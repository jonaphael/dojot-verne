import os
import random
import time
import uuid
from datetime import datetime

from locust import TaskSet, task, seq_task
from mqtt_client import MQTT_Client
from redis_client import RedisClient

cache = RedisClient()
cache.connect()

timestamp = int(datetime.timestamp(datetime.now()))

LOG_DIR = os.environ.get("LOCUST_LOG_DIR", None)

if not os.path.exists(LOG_DIR):
    os.makedirs(LOG_DIR)

filename_dir = "{0}/{1}.log".format(LOG_DIR, uuid.uuid4())

class IoT_Device(TaskSet):

    def on_start(self):
        self.device_id = cache.next_device_id()
        self.client_mqtt = MQTT_Client(self.device_id, filename_dir)
        self.client_mqtt.connect()
        time.sleep(2)

    def on_stop(self):
        self.client_mqtt.save_log_list()

    @task
    def publish(self):
        self.client_mqtt.publishing(self.device_id)