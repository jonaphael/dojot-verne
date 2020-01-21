"""
Tests for the MQTT client class.
"""
import unittest
import logging
from unittest.mock import MagicMock
import paho.mqtt.client as paho

import src.mqtt_locust.mqtt_client as mqtt_client
from src.ejbca.cert_client import CertClient
from src.utils import Utils

class TestMQTTClientInitialization(unittest.TestCase):
    """
    Tests the constructor and initialization functions from MQTTClient.
    """
    def setUp(self):
        logging.basicConfig = MagicMock()

        paho.Client.tls_set = MagicMock()
        paho.Client.tls_insecure_set = MagicMock()
        paho.Client.reconnect_delay_set = MagicMock()

        CertClient.new_cert = MagicMock()
        CertClient.create_cert_files = MagicMock()


    # __init__() #
    @classmethod
    def test_constructor_success(cls):
        """
        Should create a MQTTClient instance.
        """
        client = mqtt_client.MQTTClient("123", "987", False, False)

        assert client.__class__ == mqtt_client.MQTTClient
        assert client.device_id == "123"
        assert client.run_id == "987"
        assert not client.should_revoke
        assert not client.should_renew

        assert not client.is_connected
        assert client.mqttc is None

        assert client.username == ""
        assert client.topic == ""
        assert client.sub_topic == ""

        assert client.device_cert_dir == ""
        assert client.new_cert is None

        assert client.pubmmap == {}
        assert client.submmap == {}

        assert client.start_time == 0

    def test_constructor_invalid_device_id(self):
        """
        Should raise a ValueError when passing an invalid device_id.
        """
        with self.assertRaises(ValueError):
            mqtt_client.MQTTClient("", "987", False, False)

    def test_constructor_invalid_run_id(self):
        """
        Should raise a ValueError when passing an invalid run_id.
        """
        with self.assertRaises(ValueError):
            mqtt_client.MQTTClient("123", "", False, False)

    def test_constructor_both_true(self):
        """
        Should raise a ValueError when passing True to both should_revoke
        and should_renew.
        """
        with self.assertRaises(ValueError):
            mqtt_client.MQTTClient("", "987", True, True)


    # setup() #
    @classmethod
    def test_setup_success(cls):
        """
        Should setup the parameters successfully.
        """
        client = mqtt_client.MQTTClient("123", "987", False, False)
        client.setup()

        assert logging.basicConfig.called
        assert client.new_cert is None
        assert not CertClient.new_cert.called
        assert not CertClient.create_cert_files.called

    @classmethod
    def test_setup_success_revoke(cls):
        """
        Should setup the parameters successfully with certificate revocation.
        """
        client = mqtt_client.MQTTClient("123", "987", True, False)
        client.setup()

        assert logging.basicConfig.called
        assert client.new_cert is not None
        assert CertClient.new_cert.called
        assert CertClient.create_cert_files.called

    @classmethod
    def test_setup_success_renew(cls):
        """
        Should setup the parameters successfully with certificate renovation.
        """
        client = mqtt_client.MQTTClient("123", "987", False, True)
        client.setup()

        assert logging.basicConfig.called
        assert client.new_cert is not None
        assert CertClient.new_cert.called
        assert CertClient.create_cert_files.called


    # configure_mqtt() #
    def test_configure_mqtt(self):
        """
        Should configure the MQTT connection.
        """
        client = mqtt_client.MQTTClient("123", "987", False, True)
        client.configure_mqtt()

        assert client.mqttc is not None

        assert client.mqttc.on_connect is not None
        assert client.mqttc.on_disconnect is not None
        assert client.mqttc.on_publish is not None
        assert client.mqttc.on_subscribe is not None
        assert client.mqttc.on_message is not None

        assert paho.Client.reconnect_delay_set.called
        assert paho.Client.tls_set.called
        assert paho.Client.tls_insecure_set.called


class TestMQTTClient(unittest.TestCase):
    """
    Tests the other methods from MQTTClient class.
    """
    def setUp(self):
        logging.basicConfig = MagicMock()

        paho.Client.tls_set = MagicMock()
        paho.Client.tls_insecure_set = MagicMock()
        paho.Client.reconnect_delay_set = MagicMock()

        CertClient.new_cert = MagicMock()
        CertClient.create_cert_files = MagicMock()


    # connect() #
    def test_connect(self):
        """
        Should connect to the broker successfully.
        """
        paho.Client.connect_async = MagicMock()
        paho.Client.loop_start = MagicMock()

        client = mqtt_client.MQTTClient("123", "987", False, False)
        client.setup()

        client.connect()

        assert paho.Client.connect_async.called
        assert paho.Client.loop_start.called

    def test_connect_async_error(self):
        """
        Should not connect to the broker successfully - connect_async error.
        """
        paho.Client.connect_async = MagicMock()
        paho.Client.connect_async.side_effect = Exception("fake error")
        paho.Client.loop_start = MagicMock()

        client = mqtt_client.MQTTClient("123", "987", False, False)
        client.setup()

        Utils.fire_locust_failure = MagicMock()

        client.connect()

        assert Utils.fire_locust_failure.called

    def test_connect_loop_start_error(self):
        """
        Should not connect to the broker successfully - loop_start error.
        """
        paho.Client.connect_async = MagicMock()
        paho.Client.loop_start = MagicMock()
        paho.Client.loop_start.side_effect = Exception("fake error")

        client = mqtt_client.MQTTClient("123", "987", False, False)
        client.setup()

        Utils.fire_locust_failure = MagicMock()

        client.connect()

        assert Utils.fire_locust_failure.called


    # publishing() #
    # subscribing() #
    # locust_on_subscribe() #
    # locust_on_publish() #
    # locust_on_connect() #
    # locust_on_disconnect() #
    # locust_on_message() #
    # renew_cert() #
    # revoke_cert() #
    # should_renew_now() #
    def test_should_renew_now(self):
        """
        Should renew the certificate now.
        """
        client = mqtt_client.MQTTClient("123", "987", False, True)
        client.setup()

        should_renew = client.should_renew_now()
        assert should_renew

    def test_should_not_renew_now(self):
        """
        Should not renew the certificate now.
        """
        client = mqtt_client.MQTTClient("123", "987", False, False)
        client.setup()
        client.start_time = 0

        should_renew = client.should_renew_now()
        assert not should_renew


    # should_revoke_now() #
    def test_should_revoke_now(self):
        """
        Should revoke the certificate now.
        """
        client = mqtt_client.MQTTClient("123", "987", True, False)
        client.setup()

        should_revoke = client.should_revoke_now()
        assert should_revoke

    def test_should_not_revoke_now(self):
        """
        Should not revoke the certificate now.
        """
        client = mqtt_client.MQTTClient("123", "987", False, False)
        client.setup()
        client.start_time = 0

        should_revoke = client.should_revoke_now()
        assert not should_revoke







class TestMQTTClientConnect(unittest.TestCase):
    """
    Tests the connection in MQTTClient class.
    """
    def setUp(self):
        logging.basicConfig = MagicMock()

        paho.Client.tls_set = MagicMock()
        paho.Client.tls_insecure_set = MagicMock()
        paho.Client.reconnect_delay_set = MagicMock()

        CertClient.new_cert = MagicMock()
        CertClient.create_cert_files = MagicMock()

        self.client = mqtt_client.MQTTClient("123", "987", False, False)
        self.client.setup()








if __name__ == "__main__":
    unittest.main()