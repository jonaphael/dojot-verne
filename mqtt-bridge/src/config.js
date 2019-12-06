const app = {
  mqtt_log_level: process.env.MQTT_CLIENT_LOG_LEVEL || "info"
}

const mqtt = {
  mqttHost: process.env.HOSTNAME || "mqtt-bridge",
  host: process.env.DOJOT_MQTT_HOST || "vernemq-k8s",
  port: parseInt(process.env.DOJOT_MQTT_PORT) || 1883,
  keepalive: 60,
  subscribeTopic: `\$share/group/+/attrs`,
  subscribeQos: 0
};

module.exports = { app, mqtt };
