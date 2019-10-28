import logging
import redis
import os

from utils import Utils
from config import config


class RedisClient():
    """Redis handler class."""

    def connect(self) -> None:
        """Attempts to stablish a connection."""

        try:
            self.certificates = redis.Redis(
                host=config['locust']['redis']['host'],
                port=config['locust']['redis']['port'],
                db=config['locust']['redis']['certificates_db'])
            self.mapped = redis.Redis(
                host=config['locust']['redis']['host'],
                port=config['locust']['redis']['port'],
                db=config['locust']['redis']['mapped_db'])

        except Exception as e:
            logging.error(str(e.with_traceback()))

    def next_device_id(self) -> str:
        """Retrieves the next device_id in the list.

        It is important that each client has a unique ID to simulate our scenario,
        since there is no repetition in IDs in any scenario."""

        try:
            device_count = self.mapped.incr('device_count')
            device_id = self.mapped.get(device_count)
            return device_id.decode('utf-8')
        except Exception as e:
            logging.error(str(e.with_traceback()))

    def map_device_ids(self) -> None:
        """Maps device IDs from certificate database in Redis to sequential keys in mapping database.

        The certificates database stores the certificates and private keys  by using a Hash Map.
        When testing with Locust, we will read the already created certificates from Redis database,
        but since we are using Hash Maps, the retrieval is not trivial because we need to know the
        device ID.

        The solution is to create another database sequentially mapping the device IDs (i.e., the key
        will be an integer from 0 to the number of devices and the value is the device ID), so we can
        retrieve them safely."""

        try:
            logging.info("Beginning database mapping...")

            keys = self.certificates.keys()
            for i in range(len(keys)):
                self.mapped.set(i+1, keys[i])

        except Exception as e:
            logging.error(str(e.with_traceback()))

        else:
            logging.info("Finished database mapping.")
