const vernemq = {
  port: process.env.VERNE_WEBHOOKS_PORT || 4000,
};

const app = {
  log_level: process.env.VERNE_WEBHOOKS_LOG_LEVEL || "info",
};


const messenger = {
  kafka: {
    producer: {
      "metadata.broker.list": process.env.KAFKA_HOSTS || "kafka-server:9092",
      "socket.keepalive.enable":  true,
      "dr_cb": true
    },
    consumer: {
      "metadata.broker.list": process.env.KAFKA_HOSTS || "kafka-server:9092",
      "group.id": "vernemq-group"
    },
    dojot: {
      timeoutSleep: 2,
      connectionRetries: 10,
      subjects: {
        verne: "vernemq-epic-channel"
      }
    }
  },
  databroker: {
    url: process.env.DATA_BROKER_URL || "http://data-broker",
    timeoutSleep : 2,
    connectionRetries: process.env.DATA_BROKER_CONN_RETRIES || 10
  }, 
  auth: {
    url: process.env.AUTH_URL || "http://auth:5000",
    timeoutSleep: 5,
    connectionRetries: 5,
  },
  dojot: {
    subjects: {
      tenancy: process.env.DOJOT_SUBJECT_TENANCY || "dojot.tenancy",
      devices: process.env.DOJOT_SUBJECT_DEVICES || "dojot.device-manager.device",
      deviceData: process.env.DOJOT_SUBJECT_DEVICE_DATA || "device-data",
    },
    management: {
      user: process.env.DOJOT_MANAGEMENT_USER || "dojot-management",
      tenant: process.env.DOJOT_MANAGEMENT_USER || "dojot-management",
    },
    events:{
      tenantEvent: {
        NEW_TENANT: "new-tenant",
        DELETE_TENANT: "delete-tenant"
      },
      tenantActionType: {
        CREATE: "create",
        DELETE: "delete"
      }
    }
  }
};

export default { app, messenger, vernemq };
