import dojot from "@jonaphael/dojot-module";
import { logger } from "@dojot/dojot-module-logger";
import mqtt from "mqtt";
import config from "./config";
import fs from "fs";

const TAG = { filename: "MqttClientApp" };
const hostname = process.env.HOSTNAME;
const util = require("util");

class App {
  private messenger: dojot.Messenger | null;
  private mqttc: mqtt.Client | null;
  private isConnected: boolean;
  private key: BufferSource;
  private clientCrt: BufferSource;
  private caCrt: BufferSource;
  private options: object;

  constructor() {
    this.key = fs.readFileSync(`/opt/mqtt_client/cert/${hostname}.key`);
    this.clientCrt = fs.readFileSync(`/opt/mqtt_client/cert/${hostname}.crt`);
    this.caCrt = fs.readFileSync('/opt/mqtt_client/cert/ca.crt');
    this.messenger = null;
    this.mqttc = null;
    this.isConnected = false;

    this.options = {
      username: hostname,
      host: config.mqtt.host,
      port: config.mqtt.port,
      protocol: 'mqtts',
      ca: this.caCrt,
      key: this.key,
      cert: this.clientCrt,
      keepAlive: 120,
      rejectUnauthorized: false
    };
  }

  public initApp() {

    /* set log level */
    logger.setLevel(config.app.mqtt_log_level);

    this.messenger = new dojot.Messenger("mqtt-client", config.messenger);

    this.messenger!.init().then(() => {
      this.initKafka();
      this.initMqtt();
    }).catch((error: any) => {
      logger.debug(`... failed to initialize the IoT agent messenger. Error: ${error.toString()}`, TAG);
    });
  }

  private initKafka() {
    this.messenger!.createChannel(config.messenger.kafka.dojot.subjects.verne, "rw");

    const kafkaOnMessageBind = this.kafkaOnMessage.bind(this);
    this.messenger!.on(config.messenger.kafka.dojot.subjects.verne, "message", kafkaOnMessageBind);
  }

  private initMqtt() {
    this.mqttc = mqtt.connect(this.options);

    const mqttOnConnectBind = this.mqttOnConnect.bind(this);
    const mqttOnDisconnectBind = this.mqttOnDisconnect.bind(this);
    this.mqttc.on("connect", mqttOnConnectBind);
    this.mqttc.on("disconnect", mqttOnDisconnectBind);
  }

  /* Kafka Events */

  /**
   * Message receiving callback for Kafka.
   * @param tenant message tenant
   * @param message payload
   */
  private kafkaOnMessage(_tenant: string, message: any, extraInfo: any) {
    logger.debug(`The extra info is ${util.inspect(extraInfo, {depth: null})}`, TAG);
    try {
      const data = JSON.parse(message);
      this.publishMessage(`${extraInfo.key}/config`, JSON.stringify(data.attrs));
    } catch (error) {
      logger.error(`Error parsing Kafka message: ${error}`, TAG);
    }
  }

  /* MQTT Events */

  private mqttOnConnect() {
    this.isConnected = true;
  }

  private mqttOnDisconnect() {
    this.isConnected = false;
    this.mqttc!.reconnect();
    // TODO: close Kafka connection
  }

  /* MQTT Functions */

  private publishMessage(topic: string, message: any) {
    if (this.isConnected) {
      logger.debug(`Publishing on topic ${topic}`, TAG);
      this.mqttc!.publish(topic, message);
    } else {
      logger.error(`Client not connected`, TAG);
    }
  }
}

export default App;
