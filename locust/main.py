import logging
from locust import Locust
from iot_device import IoT_Device

class Client(Locust):
    logging.info("Initializing client...")
    task_set = IoT_Device

    min_wait = 1000 
    max_wait = 1000
