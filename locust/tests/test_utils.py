"""
  Contains test for Utils module
"""

import sys
import unittest
from unittest.mock import patch
from logging import DEBUG, NOTSET, WARNING, ERROR, CRITICAL, INFO
import paho.mqtt.client as mqtt
from src.utils import Utils


class TestUtils(unittest.TestCase):
    """
      Test Utils Testcase
    """

    def test_time_delta(self):
        """
        time_delta() should return correct time.
        """
        start = 2000.0
        end = 2000.3
        result = Utils.time_delta(start, end)
        self.assertEqual(result, 300)

    def test_str_to_bool_true(self):
        """
            str_to_bool("True") should return boolean True.
        """
        self.assertTrue(Utils.str_to_bool("True"))

    def test_str_to_bool_false(self):
        """
        str_to_bool("False") should return boolean False.
        """
        self.assertFalse(Utils.str_to_bool("False"))

    def test_str_to_bool_other_value(self):
        """
        str_to_bool("other") should return boolean False.
        """
        self.assertFalse(Utils.str_to_bool("other"))

    def test_error_message(self):
        """
        error_message() should return the correct names for errors.
        """
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_AGAIN), "MQTT_ERR_AGAIN")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_SUCCESS), "MQTT_ERR_SUCCESS")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_NOMEM), "MQTT_ERR_NOMEM")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_PROTOCOL), "MQTT_ERR_PROTOCOL")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_INVAL), "MQTT_ERR_INVAL")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_NO_CONN), "MQTT_ERR_NO_CONN")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_CONN_REFUSED), "MQTT_ERR_CONN_REFUSED")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_NOT_FOUND), "MQTT_ERR_NOT_FOUND")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_CONN_LOST), "MQTT_ERR_CONN_LOST")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_TLS), "MQTT_ERR_TLS")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_PAYLOAD_SIZE), "MQTT_ERR_PAYLOAD_SIZE")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_NOT_SUPPORTED), "MQTT_ERR_NOT_SUPPORTED")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_AUTH), "MQTT_ERR_AUTH")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_ACL_DENIED), "MQTT_ERR_ACL_DENIED")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_UNKNOWN), "MQTT_ERR_UNKNOWN")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_ERRNO), "MQTT_ERR_ERRNO")
        self.assertEqual(Utils.error_message(
            mqtt.MQTT_ERR_QUEUE_SIZE), "MQTT_ERR_QUEUE_SIZE")
        self.assertEqual(Utils.error_message(
            101010), "101010\n")

    def test_log_level(self):
        """"
            should return correct log
        """
        self.assertEqual(Utils.log_level('notset'), NOTSET)
        self.assertEqual(Utils.log_level('debug'), DEBUG)
        self.assertEqual(Utils.log_level('info'), INFO)
        self.assertEqual(Utils.log_level('warning'), WARNING)
        self.assertEqual(Utils.log_level('error'), ERROR)
        self.assertEqual(Utils.log_level('critical'), CRITICAL)
        self.assertEqual(Utils.log_level('not-exist'), -1)

    def test_validate_device_id(self):
        """"
        test_validate_device_id(dev-id) raised an exception when (dev-id) < 1
        """
        # not raise
        device_id = "123abc"
        Utils.validate_device_id(device_id)

        with self.assertRaises(ValueError):
            Utils.validate_device_id('')

    def test_validate_thing_id(self):
        """"
        test_validate_thing_id(thing-id) raised an exception when thing id is not correct
        """
        # not raise
        thing_id = "tenant:devid"
        Utils.validate_thing_id(thing_id)

        with self.assertRaises(ValueError):
            Utils.validate_thing_id('')

    def test_create_thing_id(self):
        """"
        test_create_thing_id() should create a thing id
        """
        self.assertEqual(Utils.create_thing_id(
            'tenant', 'dev-id'), 'tenant:dev-id')
        with self.assertRaises(ValueError):
            self.assertEqual(Utils.create_thing_id('tenant', ''), None)
            self.assertEqual(Utils.create_thing_id('', 'dev-id'), None)
            self.assertEqual(Utils.create_thing_id('', ''), None)

    def test_validate_tenant(self):
        """"
        test_validate_tenant(dev-id) raised an exception when (tenant) < 1
        """
        # not raise
        tenant = "123abc"
        Utils.validate_tenant(tenant)

        with self.assertRaises(ValueError):
            Utils.validate_tenant('')

    def test_is_master(self):
        """
        is_master() should return True when the program is run with --master flag
        """
        master_flag = ["--master"]
        with patch.object(sys, "argv", master_flag):
            self.assertTrue(Utils.is_master())

    def test_is_slave(self):
        """
        is_slave() should return True when the program is run with --slave flag
        """
        slave_flag = ["--slave"]
        with patch.object(sys, "argv", slave_flag):
            self.assertTrue(Utils.is_slave())

    @staticmethod
    @patch('src.utils.events')
    def test_fire_locust_failure(mock_events):
        """
            Test for locust fire event on failure
        """
        Utils.fire_locust_failure()
        mock_events.request_failure.fire.assert_called_once()

    @staticmethod
    @patch('src.utils.events')
    def test_fire_locust_success(mock_events):
        """
            Test for locust fire event on success
        """
        Utils.fire_locust_success()
        mock_events.request_success.fire.assert_called_once()


if __name__ == "__main__":
    unittest.main()
