"""Environment configuration related module. """

import os
from utils import Utils

config = {
    'app': {
        'debug':        Utils.str_to_bool(os.environ.get("DEBUG_MODE", "False")),
        'log_in_file':  Utils.str_to_bool(os.environ.get("LOG_IN_FILE", "False")),
    },

    'locust': {
        'task_min_time':        int(os.environ.get("TASK_MIN_TIME", 29500)),
        'task_max_time':        int(os.environ.get("TASK_MAX_TIME", 30000)),
        'log_dir':              os.environ.get("LOCUST_LOG_DIR", "/log"),
        'log_message_limit':    int(os.environ.get("LOCUST_LOG_MESSAGE_LIMIT", 1000)),
        'redis': {
            'certificates_db':      int(os.environ.get("REDIS_CERTIFICATES_DB", 0)),
            'mapped_db':            int(os.environ.get("REDIS_MAPPED_DB", 1)),
            'host':                 os.environ.get("REDIS_HOST", "127.0.0.1"),
            'port':                 int(os.environ.get("REDIS_PORT", 6379)),
            'map_device_ids':       Utils.str_to_bool(os.environ.get("MAP_DEVICE_IDS", "False")),
            'cert_dir':             os.environ.get("CERT_DIR", "cert/"),
            'ca_cert_file':         os.environ.get("CA_CERT_FILE", "ca.crt"),
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
