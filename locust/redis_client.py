import logging
import redis
import os

from config import config


class RedisClient():
    """Redis handler class."""

    def connect(self) -> None:
        """Attempts to stablish a connection."""

        try:
            self.cache = redis.Redis(host=config['locust']['redis']['host'], port=config['locust']['redis']['port'], db=0)
        except Exception:
            logging.error("error on connect to redis")


    def next_device_id(self) -> int:
        """Retrieves the next device_id in the list.

        It is important that each client has a unique ID to simulate our scenario,
        since there is no repetition in IDs in any scenario."""

        try:
            device_count = self.cache.incr('device_count')
            device_id = self.cache.get(device_count)
            return device_id
        except Exception:
            logging.error("error to get device identifier")
