const vernemq = {
  port: process.env.VERNE_WEBHOOKS_PORT || 4000,
};

const app = {
  log_level: process.env.VERNE_WEBHOOKS_LOG_LEVEL || "info",
};

const kafka = {
  producer : {
    'metadata.broker.list': process.env.KAFKA_PRODUCER_BROKER_LIST || 'kafka-ip-service:9093'
  },
  topic: process.env.KAFKA_PRODUCER_TOPIC || "vernemq-epic-topic",
  partition: process.env.KAFKA_PRODUCER_PARTITION || -1,
  key: process.env.KAFKA_PRODUCER_KEY || 'vernemq-epic-key'
}

export default { app, vernemq, kafka };
