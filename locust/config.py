"""Environment configuration related module. """

import os
from utils import Utils

config = {
    'app': {
        'debug': Utils.str_to_bool(os.environ.get("DEBUG_MODE", "False")),
    },

    'locust': {
        'task_min_time':        int(os.environ.get("TASK_MIN_TIME", 29500)),
        'task_max_time':        int(os.environ.get("TASK_MAX_TIME", 30000)),
        'log_dir':              os.environ.get("LOCUST_LOG_DIR", "/log"),
        'log_message_limit':    int(os.environ.get("LOCUST_LOG_MESSAGE_LIMIT", 1000)),
        'redis': {
            'port':       int(os.environ.get("REDIS_PORT", 6379)),
            'host':       os.environ.get("REDIS_HOST", "127.0.0.1"),
        }
    },

    'mqtt': {
        'host':         os.environ.get("DOJOT_MQTT_HOST", "127.0.0.1"),
        'port':         int(os.environ.get("DOJOT_MQTT_PORT", 1883)),
        'con_timeout':  int(os.environ.get("DOJOT_MQTT_TIMEOUT", 120)),
        'qos':          int(os.environ.get("DOJOT_MQTT_QOS", 1)),
        'pub_timeout':  int(os.environ.get("MQTT_PUBLISH_TIMEOUT", 40000)),
        'sub_timeout':  int(os.environ.get("MQTT_SUBSCRIBE_TIMEOUT", 40000)),
    },
}
