const app = {
    mqtt_log_level: process.env.MQTT_CLIENT_LOG_LEVEL || "info"
  }
  
  const messenger = {
    auth: {
      connectionRetries: 5,
      timeoutSleep: 5,
      url: process.env.AUTH_URL || "http://auth:5000",
    },
    databroker: {
      connectionRetries: process.env.DATA_BROKER_CONN_RETRIES || 10,
      timeoutSleep : 2,
      url: process.env.DATA_BROKER_URL || "http://data-broker",
    },
    dojot: {
      events: {
        tenantActionType: {
          CREATE: "create",
          DELETE: "delete",
        },
        tenantEvent: {
          DELETE_TENANT: "delete-tenant",
          NEW_TENANT: "new-tenant",
        },
      },
      management: {
        tenant: process.env.DOJOT_MANAGEMENT_USER || "dojot-management",
        user: process.env.DOJOT_MANAGEMENT_USER || "dojot-management",
      },
      subjects: {
        deviceData: process.env.DOJOT_SUBJECT_DEVICE_DATA || "device-data",
        devices: process.env.DOJOT_SUBJECT_DEVICES || "dojot.device-manager.device",
        tenancy: process.env.DOJOT_SUBJECT_TENANCY || "dojot.tenancy",
      },
    },
    kafka: {
      consumer: {
        "group.id": "vernemq-group",
        "metadata.broker.list": process.env.KAFKA_HOSTS || "kafka-server:9092",
      },
      dojot: {
        connectionRetries: 10,
        subjects: {
          verne: "vernemq-epic-channel",
        },
        timeoutSleep: 2,
      },
      producer: {
        "dr_cb": true,
        "metadata.broker.list": process.env.KAFKA_HOSTS || "kafka-server:9092",
        "socket.keepalive.enable":  true,
      },
    },
  };
  
  const mqtt = {
    host: process.env.DOJOT_MQTT_HOST || "vernemq-k8s",
    port: parseInt(process.env.DOJOT_MQTT_PORT) || 1883,
    keepalive: 60,
    subscribeTopic: `\$share/group/+/attrs`,
    subscribeQos: 0
  };
  
  module.exports = { app, messenger, mqtt};
  