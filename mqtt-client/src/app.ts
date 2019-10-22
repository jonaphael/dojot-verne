import dojot from "@dojot/dojot-module";
import { logger } from "@dojot/dojot-module-logger";
import paho from "paho-mqtt";
import config from "./config";

const TAG = { filename: "MqttClientApp" };

class App {
  private messenger: dojot.Messenger | null;
  private mqttc: paho.Client | null;
  private isConnected: boolean;

  constructor() {
    this.messenger = null;
    this.mqttc = null;
    this.isConnected = false;
  }

  public initApp() {
    this.messenger = new dojot.Messenger("mqtt-client", config.messenger);

    this.messenger!.init().then(() => {
      // Initializing Kafka
      this.messenger!.createChannel(config.messenger.kafka.dojot.subjects.verne, "rw");

      // Initializing MQTT client
      this.mqttc = new Paho.MQTT.Client(config.paho.host, config.paho.port, "admin:mqtt_client");
      this.mqttc.connect({ onSuccess: this.onConnect });
      this.mqttc.onConnectionLost = this.onConnectionLost;

      const kafkaMessagesBind = this.onKafkaMessages.bind(this);
      this.messenger!.on(config.messenger.kafka.dojot.subjects.verne, "message", kafkaMessagesBind);

    }).catch((error: any) => {
      logger.debug(`... failed to initialize the IoT agent messenger. Error: ${error.toString()}`, TAG);
    });
  }

  private onKafkaMessages(tenant: string, message: any) {
    const mqttMessage: Paho.MQTT.Message = new Paho.MQTT.Message(message);
    mqttMessage.destinationName = `${tenant}:${message.metadata.thingId}/config`;
    this.sendMessage(mqttMessage);
  }

  private onConnect() {
    this.isConnected = true;
  }

  private onConnectionLost(res: Paho.MQTT.MQTTError) {
    if (res.errorCode !== 0) {
      this.isConnected = false;
      this.mqttc!.connect({ onSuccess: this.onConnect });
    }
  }

  private sendMessage(message: Paho.MQTT.Message) {
    if (this.isConnected) {
      this.mqttc!.send(message);
    }
  }
}

export default App;
