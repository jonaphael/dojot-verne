import os
import random

from locust import TaskSet, task, seq_task
from mqtt_client import MQTT_Client
from redis_client import RedisClient

cache = RedisClient()
cache.connect()

class IoT_Device(TaskSet):

    def on_start(self):
        self.device_id = cache.next_device_id()
        self.client_mqtt = MQTT_Client()  

        self.client_mqtt.connect()     

    @task(1)
    def loop(self):
        self.client_mqtt.loop()

    @task(1)
    def publish(self):
       self.client_mqtt.publishing(self.device_id)
       self.client_mqtt.loop()
    