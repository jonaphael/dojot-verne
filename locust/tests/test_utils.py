"""
Tests for the Utils class.
"""
from logging import NOTSET, DEBUG, INFO, WARNING, ERROR, CRITICAL
from unittest.mock import MagicMock
import unittest
import sys
import paho.mqtt.client as mqtt
from locust import events

from src.utils import Utils


class TestUtils(unittest.TestCase):
    """
    Utils class tests.
    """
    # time_delta() #
    def test_time_delta(self):
        """
        Should return correct time.
        """
        start = 2000.000
        end = 2000.300
        result = Utils.time_delta(start, end)
        self.assertEqual(result, 300)


    # fire_locust_failure() #
    @classmethod
    def test_fire_locust_failure(cls):
        """
        Should fire a failure event.
        """
        events.request_failure.fire = MagicMock()
        Utils.fire_locust_failure()
        assert events.request_failure.fire.called


    # fire_locust_success() #
    @classmethod
    def test_fire_locust_success(cls):
        """
        Should fire a success event.
        """
        events.request_success.fire = MagicMock()
        Utils.fire_locust_success()
        assert events.request_success.fire.called


    # str_to_bool() #
    @classmethod
    def test_str_to_bool_true(cls):
        """
        Should return boolean True.
        """
        assert Utils.str_to_bool("True")
        assert Utils.str_to_bool("true")
        assert Utils.str_to_bool("tRuE")

    @classmethod
    def test_str_to_bool_false(cls):
        """
        Should return boolean False.
        """
        assert not Utils.str_to_bool("False")
        assert not Utils.str_to_bool("false")
        assert not Utils.str_to_bool("fAlSe")

    @classmethod
    def test_str_to_bool_other_value(cls):
        """
        Should return boolean False.
        """
        assert not Utils.str_to_bool("tru")
        assert not Utils.str_to_bool("fals")
        assert not Utils.str_to_bool("montypython")


    # error_message() #
    def test_error_message(self):
        """
        Should return the correct names for errors.
        """
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_AGAIN), "MQTT_ERR_AGAIN")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_SUCCESS), "MQTT_ERR_SUCCESS")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_NOMEM), "MQTT_ERR_NOMEM")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_PROTOCOL), "MQTT_ERR_PROTOCOL")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_INVAL), "MQTT_ERR_INVAL")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_NO_CONN), "MQTT_ERR_NO_CONN")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_CONN_REFUSED), "MQTT_ERR_CONN_REFUSED")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_NOT_FOUND), "MQTT_ERR_NOT_FOUND")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_CONN_LOST), "MQTT_ERR_CONN_LOST")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_TLS), "MQTT_ERR_TLS")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_PAYLOAD_SIZE), "MQTT_ERR_PAYLOAD_SIZE")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_NOT_SUPPORTED), "MQTT_ERR_NOT_SUPPORTED")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_AUTH), "MQTT_ERR_AUTH")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_ACL_DENIED), "MQTT_ERR_ACL_DENIED")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_UNKNOWN), "MQTT_ERR_UNKNOWN")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_ERRNO), "MQTT_ERR_ERRNO")
        self.assertEqual(Utils.error_message(mqtt.MQTT_ERR_QUEUE_SIZE), "MQTT_ERR_QUEUE_SIZE")
        self.assertEqual(Utils.error_message(1000000), "1000000\n")


    # is_master()
    def test_is_master(self):
        """
        Should return True when the program is run with --master flag
        """
        unittest.mock.patch.object(sys, "argv")
        sys.argv = ["--master"]
        self.assertTrue(Utils.is_master())


    # is_slave()
    def test_is_slave(self):
        """
        Should return True when the program is run with --slave flag
        """
        unittest.mock.patch.object(sys, "argv")
        sys.argv = ["--slave"]
        self.assertTrue(Utils.is_slave())


    # log_level()
    def test_log_level(self):
        """
        Should return the correct value for the log levels
        """
        self.assertEqual(Utils.log_level("notset"), NOTSET)
        self.assertEqual(Utils.log_level("NOTSET"), NOTSET)
        self.assertEqual(Utils.log_level("NoTsEt"), NOTSET)

        self.assertEqual(Utils.log_level("debug"), DEBUG)
        self.assertEqual(Utils.log_level("DEBUG"), DEBUG)
        self.assertEqual(Utils.log_level("dEbUg"), DEBUG)

        self.assertEqual(Utils.log_level("info"), INFO)
        self.assertEqual(Utils.log_level("INFO"), INFO)
        self.assertEqual(Utils.log_level("iNfO"), INFO)

        self.assertEqual(Utils.log_level("warning"), WARNING)
        self.assertEqual(Utils.log_level("WARNING"), WARNING)
        self.assertEqual(Utils.log_level("wArNiNg"), WARNING)

        self.assertEqual(Utils.log_level("error"), ERROR)
        self.assertEqual(Utils.log_level("ERROR"), ERROR)
        self.assertEqual(Utils.log_level("eRrOr"), ERROR)

        self.assertEqual(Utils.log_level("critical"), CRITICAL)
        self.assertEqual(Utils.log_level("CRITICAL"), CRITICAL)
        self.assertEqual(Utils.log_level("cRiTiCaL"), CRITICAL)

        self.assertEqual(Utils.log_level("debu"), -1)


    # validate_id() #
    @classmethod
    def test_validate_id(cls):
        """
        Should not raise when given a valid ID.
        """
        Utils.validate_device_id("testID")

    def test_validate_id_empty_id(self):
        """
        Should raise when given an empty ID.
        """
        with self.assertRaises(ValueError):
            Utils.validate_device_id("")


    # validate_tenant() #
    @classmethod
    def test_validate_tenant(cls):
        """
        Should not raise when given a valid device ID.
        """
        Utils.validate_tenant("testTenant")

    def test_validate_tenant_empty_id(self):
        """
        Should raise when given an empty ID.
        """
        with self.assertRaises(ValueError):
            Utils.validate_tenant("")


    # validate_thing_id() #
    @classmethod
    def test_validate_thing_id(cls):
        """
        Should not raise when given a valid thing ID.
        """
        Utils.validate_thing_id("admin:123")

    def test_validate_thing_id_invalid(self):
        """
        Should raise when given an invalid thing ID.
        """
        with self.assertRaises(ValueError):
            Utils.validate_thing_id("admin")

        with self.assertRaises(ValueError):
            Utils.validate_thing_id("admin:")

        with self.assertRaises(ValueError):
            Utils.validate_thing_id(":123")

        with self.assertRaises(ValueError):
            Utils.validate_thing_id(":")

        with self.assertRaises(ValueError):
            Utils.validate_thing_id("")


    # create_thing_id() #
    @classmethod
    def test_create_thing_id(cls):
        """
        Should not raise when given valid tenant and device ID.
        """
        Utils.create_thing_id("admin", "123")

    def test_create_thing_id_invalid_tenant(self):
        """
        Should raise when given invalid tenant.
        """
        with self.assertRaises(ValueError):
            Utils.create_thing_id("", "123")

    def test_create_thing_id_invalid_id(self):
        """
        Should raise when given invalid device ID.
        """
        with self.assertRaises(ValueError):
            Utils.create_thing_id("admin", "")

if __name__ == "__main__":
    unittest.main()
