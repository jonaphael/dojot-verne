import os
import random
import time
from datetime import datetime

from locust import TaskSet, task, seq_task
from mqtt_client import MQTT_Client
from redis_client import RedisClient

cache = RedisClient()
cache.connect()

timestamp = int(datetime.timestamp(datetime.now()))

if not os.path.exists('logs/'):
    os.makedirs("logs/")
    
filename_dir = "logs/{0}.log".format(timestamp)

class IoT_Device(TaskSet):

    def on_start(self):
        self.device_id = cache.next_device_id()
        self.client_mqtt = MQTT_Client(self.device_id, filename_dir)
        self.client_mqtt.connect()

        self.init_time = 0.0

    @task
    def publish(self):
        if time.time() - self.init_time >= 30.0:
            self.init_time = time.time()
            self.client_mqtt.publishing(self.device_id)
            
        self.client_mqtt.loop(0.05)
