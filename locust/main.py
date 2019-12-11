"""
Locust Load Test.
"""
import uuid

from locust import Locust, task, TaskSet

from src.utils import Utils
from src.config import CONFIG
from src.mqtt_locust.mqtt_client import MQTTClient
from src.mqtt_locust.redis_client import RedisClient


if Utils.is_master() and CONFIG['locust']['redis']['map_device_ids']:
    DB = RedisClient()
    DB.map_device_ids()


class MqttLocust(Locust):
    """Locust client using MQTT."""

    def __init__(self, *args, **kwargs):
        super(Locust, self).__init__(*args, **kwargs)

        # Connects to Redis database that stores the device_id for each client
        cache = RedisClient()

        revoke = cache.has_to_revoke()
        should_revoke = False
        should_renew = False

        device_id = None
        # We need to differentiate the device IDs to be revogated/renewed from the other ones
        # The revogated/renewed ones will not be stored in Redis; instead, they will be created
        # at runtime
        if revoke:
            should_revoke = revoke["should_revoke"]
            device_id = revoke["device_id"]
        else:
            renew = cache.has_to_renew()

            if renew:
                should_renew = renew["should_renew"]
                device_id = renew["device_id"]
            else:
                device_id = cache.next_device_id()

        # UUID to identify the client run
        run_id = uuid.uuid4()

        self.client = MQTTClient(device_id, run_id, should_revoke, should_renew)
        self.client.connect()


class ThingBehavior(TaskSet):
    """MQTT pub/sub load test class. It specifies the transmission and reception behaviours
    of messages from the MQTT broker."""

    @task
    def publish(self):
        """Publishes a message to MQTT broker."""
        if self.client.is_connected:
            self.client.revoke_cert()
            self.client.renew_cert()
            self.client.publishing()

    def on_stop(self):
        """
        Treats the client when Locust test has stopped.
        """
        # Saving the log messages in a file
        self.client.log.save_log_list()


class Client(MqttLocust):
    """The client that will run the tasks when hatched."""

    task_set = ThingBehavior
    min_wait = CONFIG['locust']['task_min_time']
    max_wait = CONFIG['locust']['task_max_time']
