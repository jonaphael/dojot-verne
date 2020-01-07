const toBoolean = (mode) => ((mode || false) && (mode.toString().toLowerCase().trim() === 'true' || Number(mode) > 0));

const app = {
  mqtt_log_level: process.env.LOG_LEVEL || 'info',
  enable_dojot: toBoolean(process.env.ENABLE_DOJOT),
};

const mqtt = {
  host: process.env.DOJOT_MQTT_HOST || '10.50.11.227',
  port: Number(process.env.DOJOT_MQTT_PORT) || 30010,
};

module.exports = { app, mqtt, toBoolean };
