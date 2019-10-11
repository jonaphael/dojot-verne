from locust import events
import paho.mqtt.client as mqtt

class Utils():
    """Project Utils class."""

    @staticmethod
    def time_delta(start: float, end: float) -> int:
        """Calculates the difference between two time values.

        Args:
            start (float): initial time
            end (float): final time

        Returns:
            int: difference between the two times (in milliseconds).
        """

        return int((end - start) * 1000)

    @staticmethod
    def fire_locust_failure(**kwargs):
        """Fires the request_failure event in Locust.

        Args:
            kwargs: Locust event keyword arguments.
        """

        events.request_failure.fire(**kwargs)

    @staticmethod
    def fire_locust_success(**kwargs):
        """Fires the request_success event in Locust.

        Args:
            kwargs: Locust event keyword arguments.
        """

        events.request_success.fire(**kwargs)

    @staticmethod
    def str_to_bool(string: str) -> bool:
        """Converts a string to bool.

        Args:
            str (string): string to be converted. Accepted values: 'True', 'False'. Any other
            value will be considered False.

        Returns:
            bool
        """

        return True if string == "True" else False

    @staticmethod
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
