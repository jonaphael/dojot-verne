import logging
import os
from locust import Locust
from iot_device import IoT_Device

TASK_MIN_TIME = os.environ.get("TASK_MIN_TIME", "1000")
TASK_MAX_TIME = os.environ.get("TASK_MAX_TIME", "1000")

class Client(Locust):
    task_set = IoT_Device

    min_wait = int(TASK_MIN_TIME)
    max_wait = int(TASK_MAX_TIME)
