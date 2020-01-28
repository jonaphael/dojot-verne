"""
    Handles Paho MQTT-Client operations like publish/subscription, connection,
    loop function.
"""
import logging
import time
import json
import paho.mqtt.client as mqtt

from src.utils import Utils
from src.config import CONFIG
from src.ejbca.cert_client import CertClient


REQUEST_TYPE = 'mqtt'
MESSAGE_TYPE_CONNECT = 'connect'
MESSAGE_TYPE_DISCONNECT = 'disconnect'
MESSAGE_TYPE_PUB = 'publish'
MESSAGE_TYPE_SUB = 'subscribe'
MESSAGE_TYPE_RECV_MESSAGE = 'recv_message'
MESSAGE_TYPE_RENEW = 'renew'
MESSAGE_TYPE_REVOKE = 'revoke'


class LocustError(Exception):
    """
    Locust error exception.
    """

class ConnectError(Exception):
    """
    Connection error exception.
    """

class DisconnectError(Exception):
    """
    Disconnection error exception.
    """

class CertRevogationError(Exception):
    """
    Certificate revogation error exception.
    """


class MQTTClient:
    """
    MQTT client to load test Dojot MQTT IoTAgent.
    """
    def __init__(self,
                 device_id: str,
                 run_id: str,
                 should_revoke: bool,
                 should_renew: bool):
        """
        MQTT client constructor. To get this to work, you should call setup() after instantiating
        the class.

        Args:
            device_id: device identifier
            run_id: client run identifier
            should_revoke: whether this client should have its certificate revoked
            should_renew: whether this client should have its certificate renewed
        """
        Utils.validate_tenant(CONFIG["app"]["tenant"])
        Utils.validate_device_id(device_id)

        if len(run_id) < 1:
            raise ValueError("the run ID must have at least one character")

        if should_renew and should_revoke:
            raise ValueError("only one of should_renew and should_revoke can be True")

        self.device_id = device_id
        self.run_id = run_id
        self.should_revoke = should_revoke
        self.should_renew = should_renew

        self.is_connected = False
        self.mqttc = None

        self.tenant = CONFIG["app"]["tenant"]
        self.username = ""
        self.topic = ""
        self.sub_topic = ""

        self.device_cert_dir = ""
        self.new_cert = None

        self.pubmmap = {}
        self.submmap = {}

        # Used to count the time between connection and revocation/renovation
        self.start_time = 0

    def setup(self) -> None:
        """
        Initializes the required parameters.
        """
        logging.basicConfig(**CONFIG["app"]["log_config"])

        self.username = '{0}:{1}'.format(self.tenant, self.device_id)
        self.topic = "{0}/attrs".format(self.username)
        self.sub_topic = "{0}/config".format(self.username)

        self.device_cert_dir = CONFIG["security"]["cert_dir"]

        # Creating a new certificate if the client was chosen to be revoked
        if self.should_revoke:
            self.device_cert_dir = self.device_cert_dir + CONFIG["security"]["revoke_cert_dir"]
            self.new_cert = CertClient.new_cert(self.tenant, self.device_id)
            CertClient.create_cert_files(self.new_cert, self.device_cert_dir)

        elif self.should_renew:
            self.device_cert_dir = self.device_cert_dir + CONFIG["security"]["renew_cert_dir"]
            self.new_cert = CertClient.new_cert(self.tenant, self.device_id)
            CertClient.create_cert_files(self.new_cert, self.device_cert_dir)

        self.configure_mqtt()

    def configure_mqtt(self) -> None:
        """
        Configures the MQTT connection.
        """
        # Certification files
        cert_dir = CONFIG["security"]["cert_dir"]
        ca_cert_file = cert_dir + CONFIG["security"]["ca_cert_file"]
        cert_file = self.device_cert_dir + CertClient.get_certificate_file(self.device_id)
        key_file = self.device_cert_dir + CertClient.get_private_key_file(self.device_id)

        # Configuring MQTT client
        self.mqttc = mqtt.Client(client_id=self.device_id)

        # Sets exponential reconnect delay
        self.mqttc.reconnect_delay_set(
            min_delay=CONFIG["security"]["min_time_reconn"],
            max_delay=CONFIG["security"]["max_time_reconn"]
        )

        # Setting up TLS
        self.mqttc.tls_set(ca_cert_file, cert_file, key_file)
        # TODO: investigate the problem when the insecure TLS mode is False
        # This problem seems to happen because the TLS implementation does not
        # expects an IP, but a hostname
        self.mqttc.tls_insecure_set(True)

        # Registering MQTT client callbacks
        self.mqttc.on_connect = self.locust_on_connect
        self.mqttc.on_disconnect = self.locust_on_disconnect
        self.mqttc.on_publish = self.locust_on_publish
        self.mqttc.on_subscribe = self.locust_on_subscribe
        self.mqttc.on_message = self.locust_on_message


    def connect(self) -> None:
        """
        Connects to MQTT host.
        """

        try:
            self.mqttc.connect_async(host=CONFIG['mqtt']['host'], port=CONFIG['mqtt']['port'],
                                     keepalive=CONFIG['mqtt']['con_timeout'])
            self.mqttc.loop_start()
            self.start_time = time.time()
        except Exception as exception:
            logging.error("Error while connecting to the broker: %s", str(exception))
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name='connect',
                response_time=0,
                response_length=0,
                exception=ConnectError("disconnected")
            )

    def publishing(self) -> None:
        """
        Handles the publishing of messages to MQTT host.
        """

        payload = {"timestamp": time.time()}
        start_time = time.time()

        try:
            err, mid = self.mqttc.publish(
                topic=self.topic,
                payload=json.dumps(payload),
                qos=CONFIG['mqtt']['qos']
            )

            if err:
                raise ValueError(err)

            self.pubmmap[mid] = {
                'name': MESSAGE_TYPE_PUB,
                'qos': CONFIG['mqtt']['qos'],
                'topic': self.topic,
                'payload': payload,
                'start_time': start_time,
                'messages': 'messages'
            }

        except Exception as exception:
            error = Utils.error_message(int(str(exception)))

            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_PUB,
                response_time=Utils.time_delta(start_time, time.time()),
                exception=error
            )

    def subscribing(self) -> None:
        """
        Handles the subscription in MQTT topics.
        """

        start_time = time.time()

        try:
            err, mid = self.mqttc.subscribe((self.sub_topic, CONFIG['mqtt']['qos']))

            if err:
                raise ValueError(err)

            self.submmap[mid] = {
                'name': MESSAGE_TYPE_SUB,
                'qos': CONFIG['mqtt']['qos'],
                'topic': self.sub_topic,
                'payload': "",
                'start_time': start_time,
                'messages': 'messages'
            }

        except Exception as exception:
            error = Utils.error_message(int(str(exception)))
            logging.error("Error while subscribing: %s", error)

            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_SUB,
                response_time=Utils.time_delta(start_time, time.time()),
                exception=error
            )


    ###############
    ## Callbacks ##
    ###############

    def locust_on_subscribe(
            self,
            _client: mqtt.Client,
            _userdata,
            mid,
            _granted_qos) -> None:
        """
        Subscription callback function.
        """
        end_time = time.time()
        message = self.submmap.pop(mid, None)


        if message is None:
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_SUB,
                response_time=0,
                exception=ValueError("Subscription not found"),
            )

        else:
            Utils.fire_locust_success(
                request_type=REQUEST_TYPE,
                name=message['name'],
                response_time=Utils.time_delta(message['start_time'], end_time),
                response_length=0
            )

    def locust_on_publish(self, _client: mqtt.Client, _userdata, mid) -> None:
        """
        Publishing callback function.
        """
        end_time = time.time()
        message = self.pubmmap.pop(mid, None)

        if message is None:
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_PUB,
                response_time=0,
                exception=ValueError("Published message could not be found"),
            )

        else:
            Utils.fire_locust_success(
                request_type=REQUEST_TYPE,
                name=message['name'],
                response_time=Utils.time_delta(message['start_time'], end_time),
                response_length=len(message['payload']),
            )

    def locust_on_connect(
            self,
            _client: mqtt.Client,
            _flags_dict,
            _userdata,
            result_code: int) -> None:
        """
        Connection callback function.
        """
        if result_code == mqtt.MQTT_ERR_SUCCESS:
            self.subscribing()
            self.is_connected = True
            Utils.fire_locust_success(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_CONNECT,
                response_time=0,
                response_length=0
            )
        else:
            error = Utils.error_message(result_code)
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_CONNECT,
                response_time=0,
                exception=DisconnectError(error)
            )

    def locust_on_disconnect(self, _client: mqtt.Client, _userdata, result_code: int) -> None:
        """
        Disconnection callback function.
        """
        if result_code != mqtt.MQTT_ERR_SUCCESS:
            self.is_connected = False
            Utils.fire_locust_failure(
                request_type=REQUEST_TYPE,
                name=MESSAGE_TYPE_DISCONNECT,
                response_time=0,
                exception=DisconnectError(Utils.error_message(result_code))
            )

        self.mqttc.reconnect()

    @staticmethod
    def locust_on_message(_client: mqtt.Client, _userdata, message: mqtt.MQTTMessage):
        """
        Message reception callback function.
        """
        if message is not None:
            publish_time = 0.0
            try:
                publish_time = float(json.loads(message.payload.decode())["timestamp"])
            except Exception as exception:
                logging.error("Error while parsing the message payload: %s", str(exception))
                raise Exception(str(exception))
            else:
                Utils.fire_locust_success(
                    request_type=REQUEST_TYPE,
                    name=MESSAGE_TYPE_RECV_MESSAGE,
                    response_time=Utils.time_delta(publish_time, time.time()),
                    response_length=len(message.payload)
                )


    #################
    ## Certificate ##
    #################
    def renew_cert(self) -> None:
        """
        Renew a certificate and emit an event whether it succeeded or not.
        """
        if self.should_renew_now():
            try:
                self.new_cert.renew_cert()

            except Exception as exception:
                Utils.fire_locust_failure(
                    request_type=REQUEST_TYPE,
                    name=MESSAGE_TYPE_RENEW,
                    response_time=0,
                    response_length=0,
                    exception=exception
                )
                raise Exception(str(exception))

            else:
                Utils.fire_locust_success(
                    request_type=REQUEST_TYPE,
                    name=MESSAGE_TYPE_RENEW,
                    response_time=0,
                    response_length=0
                )
                self.should_renew = False

    def revoke_cert(self) -> None:
        """
        Revoke a certificate and emit an event whether it succeeded or not.
        """
        if self.should_revoke_now():
            try:
                CertClient.revoke_cert(self.new_cert)

            except Exception as exception:
                Utils.fire_locust_failure(
                    request_type=REQUEST_TYPE,
                    name=MESSAGE_TYPE_REVOKE,
                    response_time=0,
                    response_length=0,
                    exception=exception
                )
                raise Exception(str(exception))

            else:
                if CertClient.has_been_revoked(self.new_cert):
                    Utils.fire_locust_success(
                        request_type=REQUEST_TYPE,
                        name=MESSAGE_TYPE_REVOKE,
                        response_time=0,
                        response_length=0
                    )
                    self.should_revoke = False
                else:
                    Utils.fire_locust_failure(
                        request_type=REQUEST_TYPE,
                        name=MESSAGE_TYPE_REVOKE,
                        response_time=0,
                        response_length=0,
                        exception=CertRevogationError("failed to revoke")
                    )

    def should_renew_now(self) -> bool:
        """
        Verifies if the conditions to renew the certificate were satisfied.
        """
        return self.should_renew and \
            Utils.time_delta(self.start_time, time.time()) >= CONFIG["security"]["time_to_renew"]

    def should_revoke_now(self) -> bool:
        """
        Verifies if the conditions to revoke the certificate were satisfied.
        """
        return self.should_revoke and \
            Utils.time_delta(self.start_time, time.time()) >= CONFIG["security"]["time_to_revoke"]
