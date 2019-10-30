import dojot from "@jonaphael/dojot-module";
import { logger } from "@dojot/dojot-module-logger";
import mqtt from "mqtt";
import config from "./config";

const TAG = { filename: "MqttClientApp" };

class App {
  private messenger: dojot.Messenger | null;
  private mqttc: mqtt.Client | null;
  private isConnected: boolean;

  constructor() {
    this.messenger = null;
    this.mqttc = null;
    this.isConnected = false;
  }

  public initApp() {
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
    this.mqttc = mqtt.connect(`mqtt://${config.mqtt.host}:${config.mqtt.port}`);

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
    const topic = `${extraInfo.key.toString()}/config`;
    this.publishMessage(topic, message);
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
      this.mqttc!.publish(topic, message);
    }
  }
}

export default App;
