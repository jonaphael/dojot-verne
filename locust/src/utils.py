"""
Utils functions.
"""
from logging import NOTSET, DEBUG, INFO, WARNING, ERROR, CRITICAL
import sys

from decimal import Decimal, getcontext
import paho.mqtt.client as mqtt
from locust import events

# Fix Decimal type precision
getcontext().prec = 5

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

        return int((Decimal(end) - Decimal(start)) * 1000)

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

        return string.lower() == "true"

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

    @staticmethod
    def is_master() -> bool:
        """
        Checks if the code is being run by the Locust master.
        """
        return "--master" in sys.argv

    @staticmethod
    def is_slave() -> bool:
        """
        Checks if the code is being run by a Locust slave.
        """
        return "--slave" in sys.argv

    @staticmethod
    def log_level(level_name: str) -> int:
        """
        Parses the log level and returns the correct values for the logging package.

        Args:
            level_name: wanted log level.

        Returns the value that the logging package understands.
        """
        level = -1
        level_name = level_name.lower()

        if level_name == "notset":
            level = NOTSET
        elif level_name == "debug":
            level = DEBUG
        elif level_name == "info":
            level = INFO
        elif level_name == "warning":
            level = WARNING
        elif level_name == "error":
            level = ERROR
        elif level_name == "critical":
            level = CRITICAL

        return level

    @staticmethod
    def validate_device_id(device_id: str) -> None:
        """
        Validates the device ID.

        Raises a ValueError when the ID is invalid.
        """
        if len(device_id) < 1:
            raise ValueError("the device ID must have at least one character")

    @staticmethod
    def validate_tenant(tenant: str) -> None:
        """
        Validates the tenant.

        Raises a ValueError when the tenant is invalid.
        """
        if len(tenant) < 1:
            raise ValueError("the tenant name must have at least one character")

    @staticmethod
    def validate_thing_id(thing_id: str) -> None:
        """
        Validates a thing ID. Its format must be tenant:deviceid.

        Raises a ValueError when the thing ID is invalid.
        """
        split = thing_id.split(":")

        if len(split) != 2:
            raise ValueError("the thing ID must be in the format tenant:deviceid")

        Utils.validate_tenant(split[0])
        Utils.validate_device_id(split[1])

    @staticmethod
    def create_thing_id(tenant: str, device_id: str) -> str:
        """
        Create the thing ID.
        """
        Utils.validate_tenant(tenant)
        Utils.validate_device_id(device_id)

        return f"{tenant}:{device_id}"
