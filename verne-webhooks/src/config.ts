const vernemq = {
  port: process.env.VERNE_WEBHOOKS_PORT || 4000,
};

const app = {
  log_level: process.env.VERNE_WEBHOOKS_LOG_LEVEL || "info",
};

const kafka = {
  producer : {
    'metadata.broker.list': process.env.MYKAFKA_ALL || 'kafka-ip-service:9093'
  }
}

export default { app, vernemq, kafka };
