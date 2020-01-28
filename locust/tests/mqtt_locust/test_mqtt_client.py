"""
Tests for the MQTT client class.
"""

import unittest
from mock import patch, MagicMock
import paho.mqtt.client as mqtt
from src.mqtt_locust.mqtt_client import MQTTClient

MOCK_CONFIG = {
    'app': {
        'tenant': 'tenant',
        'log_config': {
            'format':   "format",
            'level': 'level',
            'datefmt':  "dateformat",
        }
    },
    'security': {
        'cert_dir': 'cert-dir',
        'revoke_cert_dir': 'revoke-cert-dir',
        'renew_cert_dir': 'renew-cert-dir',
        'ca_cert_file': 'ca-cert-file',
        'min_time_reconn': 0,
        'max_time_reconn': 1,
        'time_to_renew': 100,
        'time_to_revoke': 100
    },
    'mqtt': {
        'host': 'host',
        'port': 0,
        'con_timeout': 0,
        'qos': 0
    }
}


@patch('src.mqtt_locust.mqtt_client.json')
@patch('src.mqtt_locust.mqtt_client.mqtt')
@patch('src.mqtt_locust.mqtt_client.CertClient')
@patch('src.mqtt_locust.mqtt_client.logging')
@patch('src.mqtt_locust.mqtt_client.Utils')
@patch.dict('src.mqtt_locust.mqtt_client.CONFIG', MOCK_CONFIG)
class TestMQTTClientInitialization(unittest.TestCase):
    """
        Mqtt Client tests
    """

    def test_constructor_success(self, mock_utils,
                                 mock_logging,
                                 mock_cert_client,
                                 paho_mqtt_mock,
                                 json_mock):
        """
            Should create a MQTTClient instance.
        """
        client = MQTTClient('123', '987', False, False)
        mock_utils.validate_tenant.assert_called_once_with(
            MOCK_CONFIG['app']['tenant'])
        mock_utils.validate_device_id.assert_called_once_with('123')

        self.assertEqual(client.device_id, '123')
        self.assertEqual(client.run_id, '987')
        self.assertEqual(client.should_revoke, False)
        self.assertEqual(client.should_renew, False)
        self.assertEqual(client.tenant, MOCK_CONFIG['app']['tenant'])

        self.assertFalse(client.is_connected)
        self.assertIsNone(client.mqttc)

        self.assertEqual(client.username, "")
        self.assertEqual(client.topic, "")
        self.assertEqual(client.sub_topic, "")

        self.assertEqual(client.device_cert_dir, "")
        self.assertIsNone(client.new_cert)

        self.assertEqual(client.pubmmap, {})
        self.assertEqual(client.submmap, {})

        self.assertEqual(client.start_time, 0)

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_constructor_invalid_run_id(self, mock_utils,
                                        mock_logging,
                                        mock_cert_client,
                                        paho_mqtt_mock,
                                        json_mock):
        """
        Should raise a ValueError when passing an invalid run_id.
        """
        with self.assertRaises(ValueError):
            MQTTClient("123", "", False, False)

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_constructor_both_true(self, mock_utils,
                                   mock_logging,
                                   mock_cert_client,
                                   paho_mqtt_mock,
                                   json_mock):
        """
        Should raise a ValueError when passing True to both should_revoke
        and should_renew.
        """
        with self.assertRaises(ValueError):
            MQTTClient("123", "987", True, True)

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_setup_success(self, mock_utils,
                           mock_logging,
                           mock_cert_client,
                           paho_mqtt_mock,
                           json_mock):
        """
        Should setup the parameters successfully.
        """

        client = MQTTClient("123", "987", False, False)
        client.configure_mqtt = MagicMock()
        client.setup()
        mock_logging.basicConfig.assert_called_once()
        client.configure_mqtt.assert_called_once()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_setup_success_revoke(self, mock_utils,
                                  mock_logging,
                                  mock_cert_client,
                                  paho_mqtt_mock,
                                  json_mock):
        """
        Should setup the parameters successfully with certificate revocation.
        """
        client = MQTTClient("123", "987", True, False)
        client.configure_mqtt = MagicMock()
        client.setup()

        mock_cert_client.new_cert.assert_called_once_with(
            client.tenant, client.device_id)
        mock_cert_client.create_cert_files.assert_called_once_with(
            client.new_cert, client.device_cert_dir)

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_setup_success_renew(self, mock_utils,
                                 mock_logging,
                                 mock_cert_client,
                                 paho_mqtt_mock,
                                 json_mock):
        """
        Should setup the parameters successfully with certificate renovation.
        """
        client = MQTTClient("123", "987", False, True)
        client.configure_mqtt = MagicMock()
        client.setup()

        mock_cert_client.new_cert.assert_called_once_with(
            client.tenant, client.device_id)
        mock_cert_client.create_cert_files.assert_called_once_with(
            client.new_cert, client.device_cert_dir)

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_configure_mqtt(self, mock_utils,
                            mock_logging,
                            mock_cert_client,
                            paho_mqtt_mock,
                            json_mock):
        """
        Should configure the MQTT connection.
        """
        client = MQTTClient("123", "987", False, True)
        client.configure_mqtt()

        mock_cert_client.get_certificate_file.assert_called_once_with(
            client.device_id)
        mock_cert_client.get_private_key_file.assert_called_once_with(
            client.device_id)

        self.assertIsNotNone(client.mqttc)
        paho_mqtt_mock.Client.assert_called_once_with(
            client_id=client.device_id)
        paho_mqtt_mock.Client().reconnect_delay_set.assert_called_once_with(
            min_delay=MOCK_CONFIG["security"]["min_time_reconn"],
            max_delay=MOCK_CONFIG["security"]["max_time_reconn"])

        paho_mqtt_mock.Client().tls_set.assert_called_once()
        paho_mqtt_mock.Client().tls_insecure_set.assert_called_once_with(True)

        self.assertIsNotNone(client.mqttc.on_connect)
        self.assertIsNotNone(client.mqttc.on_disconnect)
        self.assertIsNotNone(client.mqttc.on_publish)
        self.assertIsNotNone(client.mqttc.on_subscribe)
        self.assertIsNotNone(client.mqttc.on_message)

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    # connect() #
    def test_connect(self, mock_utils,
                     mock_logging,
                     mock_cert_client,
                     paho_mqtt_mock,
                     json_mock):
        """
        Should connect to the broker successfully.
        """
        client = MQTTClient("123", "987", False, False)
        client.setup()

        client.connect()
        paho_mqtt_mock.Client().connect_async.assert_called_once_with(
            host=MOCK_CONFIG['mqtt']['host'],
            port=MOCK_CONFIG['mqtt']['port'],
            keepalive=MOCK_CONFIG['mqtt']['con_timeout'])

        paho_mqtt_mock.Client().loop_start.assert_called_once()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_connect_error(self, mock_utils,
                           mock_logging,
                           mock_cert_client,
                           paho_mqtt_mock,
                           json_mock):
        """
        Should not connect to the broker successfully - connect_async error.
        """
        paho_mqtt_mock.Client().connect_async = Exception("fake error")

        client = MQTTClient("123", "987", False, False)
        client.setup()

        # when exception raises
        client.connect()
        mock_utils.fire_locust_failure.assert_called_once()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_publishing_succes(self, mock_utils,
                               mock_logging,
                               mock_cert_client,
                               paho_mqtt_mock,
                               json_mock):
        """
        Should publish a message successfully
        """
        paho_mqtt_mock.Client().publish.return_value = (0, MagicMock())
        client = MQTTClient("123", "987", False, False)
        client.setup()

        client.publishing()
        paho_mqtt_mock.Client().publish.assert_called_once()
        keys_len = len(client.pubmmap.keys())
        self.assertGreater(keys_len, 0)

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_publishing_error(self, mock_utils,
                              mock_logging,
                              mock_cert_client,
                              paho_mqtt_mock,
                              json_mock):
        """
        Should not publish a message successfully
        """
        paho_mqtt_mock.Client().publish.return_value = (10, MagicMock())
        client = MQTTClient("123", "987", False, False)
        client.setup()

        client.publishing()
        mock_utils.error_message.assert_called_once_with(10)
        mock_utils.fire_locust_failure.assert_called_once()

        keys_len = len(client.pubmmap.keys())
        self.assertEqual(keys_len, 0)

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_subcribe_succes(self, mock_utils,
                             mock_logging,
                             mock_cert_client,
                             paho_mqtt_mock,
                             json_mock):
        """
        Should subscribe a message successfully
        """
        paho_mqtt_mock.Client().subscribe.return_value = (0, MagicMock())
        client = MQTTClient("123", "987", False, False)
        client.setup()

        client.subscribing()
        paho_mqtt_mock.Client().subscribe.assert_called_once()
        keys_len = len(client.submmap.keys())
        self.assertGreater(keys_len, 0)

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_subscribe_error(self, mock_utils,
                             mock_logging,
                             mock_cert_client,
                             paho_mqtt_mock,
                             json_mock):
        """
        Should not subscribe a message successfully
        """
        paho_mqtt_mock.Client().subscribe.return_value = (10, MagicMock())
        client = MQTTClient("123", "987", False, False)
        client.setup()

        client.subscribing()
        mock_utils.error_message.assert_called_once_with(10)
        mock_utils.fire_locust_failure.assert_called_once()

        keys_len = len(client.pubmmap.keys())
        self.assertEqual(keys_len, 0)

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    # callbacks
    def test_locust_on_subcribing(self, mock_utils,
                                  mock_logging,
                                  mock_cert_client,
                                  paho_mqtt_mock,
                                  json_mock):
        """
        Should fire locust success
        """
        mid = MagicMock()
        client = MQTTClient("123", "987", False, False)
        client.submmap[mid] = {'name': 'name', 'start_time': 'time'}
        client.locust_on_subscribe(client.mqttc, {}, mid, 0)
        mock_utils.time_delta.assert_called_once()
        mock_utils.fire_locust_success.assert_called_once()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_locust_on_subcribing_failure(self, mock_utils,
                                          mock_logging,
                                          mock_cert_client,
                                          paho_mqtt_mock,
                                          json_mock):
        """
        Should fire locust failure
        """
        client = MQTTClient("123", "987", False, False)

        client.locust_on_subscribe(client.mqttc, {}, 0, 0)
        mock_utils.fire_locust_failure.assert_called_once()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_locust_on_publish(self, mock_utils,
                               mock_logging,
                               mock_cert_client,
                               paho_mqtt_mock,
                               json_mock):
        """
        Should fire locust success on publish callback
        """
        mid = MagicMock()
        client = MQTTClient("123", "987", False, False)
        client.pubmmap[mid] = {'name': 'name',
                               'start_time': 'time', 'payload': 'payload'}
        client.locust_on_publish(client.mqttc, {}, mid)
        mock_utils.time_delta.assert_called_once()
        mock_utils.fire_locust_success.assert_called_once()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_locust_on_publish_failure(self, mock_utils,
                                       mock_logging,
                                       mock_cert_client,
                                       paho_mqtt_mock,
                                       json_mock):
        """
        Should fire locust failure on publish
        """
        client = MQTTClient("123", "987", False, False)
        client.locust_on_publish(client.mqttc, {}, 0)
        mock_utils.fire_locust_failure.assert_called_once()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_locust_on_connect(self, mock_utils,
                               mock_logging,
                               mock_cert_client,
                               paho_mqtt_mock,
                               json_mock):
        """
        Should fire locust success on connection callback
        """
        paho_mqtt_mock.MQTT_ERR_SUCCESS = 1
        client = MQTTClient("123", "987", False, False)
        client.subscribing = MagicMock()
        client.locust_on_connect(client.mqttc, {}, {}, 1)
        client.subscribing.assert_called_once()
        mock_utils.fire_locust_success.assert_called_once()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_locust_on_connect_failure(self, mock_utils,
                                       mock_logging,
                                       mock_cert_client,
                                       paho_mqtt_mock,
                                       json_mock):
        """
        Should fire locust failure on connection callback
        """
        paho_mqtt_mock.MQTT_ERR_SUCCESS = 1
        client = MQTTClient("123", "987", False, False)
        client.locust_on_connect(client.mqttc, {}, {}, 101010)
        mock_utils.error_message.assert_called_once()
        mock_utils.fire_locust_failure.assert_called_once()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_locust_on_disconnect(self, mock_utils,
                                  mock_logging,
                                  mock_cert_client,
                                  paho_mqtt_mock,
                                  json_mock):
        """
        Should fire locust failure on disconnect callback
        """
        paho_mqtt_mock.MQTT_ERR_SUCCESS = 1
        client = MQTTClient("123", "987", False, False)
        client.setup()
        client.locust_on_disconnect(client.mqttc, {}, 1010)

        self.assertFalse(client.is_connected)
        mock_utils.fire_locust_failure.assert_called_once()
        paho_mqtt_mock.Client().reconnect.assert_called_once()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_locust_on_disconnect_fail(self, mock_utils,
                                       mock_logging,
                                       mock_cert_client,
                                       paho_mqtt_mock,
                                       json_mock):
        """
        Should fire locust failure on disconnect callback
        """
        paho_mqtt_mock.MQTT_ERR_SUCCESS = 1
        client = MQTTClient("123", "987", False, False)
        client.setup()
        client.locust_on_disconnect(client.mqttc, {}, 1)

        self.assertFalse(client.is_connected)
        paho_mqtt_mock.Client().reconnect.assert_called_once()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_locust_on_message(self, mock_utils,
                               mock_logging,
                               mock_cert_client,
                               paho_mqtt_mock,
                               json_mock):
        """
        Should fire locust sucess on message callback
        """
        message: mqtt.MQTTMessage = mqtt.MQTTMessage()
        message.payload = str.encode(str({"timestamp": 0}))
        json_mock.loads.return_value = {"timestamp": 0}
        client = MQTTClient("123", "987", False, False)
        MQTTClient.locust_on_message(client.mqttc, {}, message)

        mock_utils.fire_locust_success.assert_called_once()
        mock_utils.time_delta.assert_called_once()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_locust_on_message_exception(self, mock_utils,
                                         mock_logging,
                                         mock_cert_client,
                                         paho_mqtt_mock,
                                         json_mock):
        """
        Should raize an exception on message callback
        """
        client = MQTTClient("123", "987", False, False)

        with self.assertRaises(Exception):
            MQTTClient.locust_on_message(client.mqttc, {}, {})

        MQTTClient.locust_on_message(client.mqttc, {}, None)

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_should_renew_now(self, mock_utils,
                              mock_logging,
                              mock_cert_client,
                              paho_mqtt_mock,
                              json_mock):
        """
        Should renew the certificate now.
        """
        mock_utils.time_delta.return_value = 100
        client = MQTTClient("123", "987", False, True)
        client.setup()
        self.assertTrue(client.should_renew_now())

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_should_not_renew_now(self, mock_utils,
                                  mock_logging,
                                  mock_cert_client,
                                  paho_mqtt_mock,
                                  json_mock):
        """
        Should not renew the certificate now.
        """
        mock_utils.time_delta.return_value = 1202010
        client = MQTTClient("123", "987", False, False)
        client.setup()

        self.assertFalse(client.should_renew_now())

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_should_revoke_now(self, mock_utils,
                               mock_logging,
                               mock_cert_client,
                               paho_mqtt_mock,
                               json_mock):
        """
        Should revoke the certificate now.
        """
        mock_utils.time_delta.return_value = 1202010
        client = MQTTClient("123", "987", True, False)
        client.setup()

        self.assertTrue(client.should_revoke_now())
        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_should_not_revoke_now(self, mock_utils,
                                   mock_logging,
                                   mock_cert_client,
                                   paho_mqtt_mock,
                                   json_mock):
        """
        Should not revoke the certificate now.
        """
        mock_utils.time_delta.return_value = 0
        client = MQTTClient("123", "987", False, False)
        client.setup()

        self.assertFalse(client.should_revoke_now())

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_renew_cert(self, mock_utils,
                        mock_logging,
                        mock_cert_client,
                        paho_mqtt_mock,
                        json_mock):
        "Sould renew cert"
        client = MQTTClient("123", "987", False, True)

        client.should_renew_now = MagicMock()
        client.should_renew_now.return_value = True
        mock_cert_client.new_cert().renew_cert.return_value = True

        client.setup()
        client.renew_cert()

        client.new_cert.renew_cert.assert_called_once()
        mock_utils.fire_locust_success.assert_called_once()
        self.assertFalse(client.should_renew)

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_not_renew_cert(self, mock_utils,
                            mock_logging,
                            mock_cert_client,
                            paho_mqtt_mock,
                            json_mock):
        "Sould not renew cert"

        client = MQTTClient("123", "987", False, False)
        client.should_renew_now = MagicMock()
        client.should_renew_now.return_value = True

        mock_cert_client.new_cert().renew_cert.return_value = Exception('exception')

        with self.assertRaises(Exception):
            client.renew_cert()

        mock_utils.fire_locust_failure.assert_called_once()

        client.should_renew_now.return_value = False
        client.renew_cert()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_revoke_cert(self, mock_utils,
                         mock_logging,
                         mock_cert_client,
                         paho_mqtt_mock,
                         json_mock):
        "Sould not revoke cert"
        client = MQTTClient("123", "987", True, False)

        client.should_revoke_now = MagicMock()
        client.should_revoke_now.return_value = True
        mock_cert_client.revoke_cert.return_value = MagicMock()
        mock_cert_client.has_been_revoked.return_value = True

        client.setup()
        client.revoke_cert()

        mock_cert_client.revoke_cert.assert_called_once()
        mock_cert_client.has_been_revoked.assert_called_once()
        mock_utils.fire_locust_success.assert_called_once()

        mock_cert_client.has_been_revoked.return_value = False
        client.revoke_cert()
        mock_utils.fire_locust_success.assert_called_once()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()

    def test_not_revoke_cert(self, mock_utils,
                             mock_logging,
                             mock_cert_client,
                             paho_mqtt_mock,
                             json_mock):
        "Sould not revoke cert"
        client = MQTTClient("123", "987", True, False)

        client.should_revoke_now = MagicMock()
        client.should_revoke_now.return_value = True
        mock_cert_client.revoke_cert = None

        client.setup()

        with self.assertRaises(Exception):
            client.revoke_cert()

        mock_utils.fire_locust_failure.assert_called_once()

        client.should_revoke_now.return_value = False
        client.revoke_cert()

        mock_utils.reset_mock()
        mock_logging.reset_mock()
        mock_cert_client.reset_mock()
        paho_mqtt_mock.reset_mock()
        json_mock.reset_mock()


if __name__ == "__main__":
    unittest.main()
