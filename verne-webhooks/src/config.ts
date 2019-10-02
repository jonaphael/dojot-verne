const vernemq = {
  port: process.env.VERNE_WEBHOOKS_PORT || 4000,
};

const app = {
  log_level: process.env.VERNE_WEBHOOKS_LOG_LEVEL || "info",
};

export default { app, vernemq };
