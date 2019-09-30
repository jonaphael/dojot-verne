import logging
import redis
import os

REDIS_HOST = os.environ.get("REDIS_HOST", "127.0.0.1")
REDIS_PORT = int(os.environ.get("REDIS_PORT", "6379"))

class RedisClient():

    def connect(self):
        try:
            self.cache = redis.Redis(host=REDIS_HOST, port=REDIS_PORT, db=0)
            logging.info("connected to redis")
        except Exception:
            logging.error("error on connect to redis")


    def next_device_id(self):
        try:
            device_count = self.cache.incr('device_count')
            device_id = self.cache.get(device_count)
            return device_id
        except Exception:
            logging.error("error to get device identifier")