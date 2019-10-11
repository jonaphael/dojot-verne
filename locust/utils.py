from locust import events

class Utils():

    @staticmethod
    def time_delta(t1, t2):
        return int((t2 - t1) * 1000)

    @staticmethod
    def fire_locust_failure(**kwargs):
        events.request_failure.fire(**kwargs)

    @staticmethod
    def fire_locust_success(**kwargs):
        events.request_success.fire(**kwargs)        @staticmethod
    def error_message(error: int) -> str:
        """Converts the error code from Locust in an understandable string."""

        if error == mqtt.MQTT_ERR_AGAIN:
            return "MQTT_ERR_AGAIN"
        if error == mqtt.MQTT_ERR_SUCCESS:
            return "MQTT_ERR_SUCCESS"
        if error == mqtt.MQTT_ERR_NOMEM:
            return "MQTT_ERR_NOMEM"
        if error == mqtt.MQTT_ERR_PROTOCOL:
            return "MQTT_ERR_PROTOCOL"
        if error == mqtt.MQTT_ERR_INVAL:
            return "MQTT_ERR_INVAL"
        if error == mqtt.MQTT_ERR_NO_CONN:
            return "MQTT_ERR_NO_CONN"
        if error == mqtt.MQTT_ERR_CONN_REFUSED:
            return "MQTT_ERR_CONN_REFUSED"
        if error == mqtt.MQTT_ERR_NOT_FOUND:
            return "MQTT_ERR_NOT_FOUND"
        if error == mqtt.MQTT_ERR_CONN_LOST:
            return "MQTT_ERR_CONN_LOST"
        if error == mqtt.MQTT_ERR_TLS:
            return "MQTT_ERR_TLS"
        if error == mqtt.MQTT_ERR_PAYLOAD_SIZE:
            return "MQTT_ERR_PAYLOAD_SIZE"
        if error == mqtt.MQTT_ERR_NOT_SUPPORTED:
            return "MQTT_ERR_NOT_SUPPORTED"
        if error == mqtt.MQTT_ERR_AUTH:
            return "MQTT_ERR_AUTH"
        if error == mqtt.MQTT_ERR_ACL_DENIED:
            return "MQTT_ERR_ACL_DENIED"
        if error == mqtt.MQTT_ERR_UNKNOWN:
            return "MQTT_ERR_UNKNOWN"
        if error == mqtt.MQTT_ERR_ERRNO:
            return "MQTT_ERR_ERRNO"
        if error == mqtt.MQTT_ERR_QUEUE_SIZE:
            return "MQTT_ERR_QUEUE_SIZE"
        return "{0}\n".format(error)
