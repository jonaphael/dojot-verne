const { IoTAgent } = require("@jonaphael/iotagent-nodejs")
const { logger } = require("@dojot/dojot-module-logger");
const defaultConfig = require('./config')
const TAG = { filename: "agent-messenger" };
const Utils = require('./utils/utils')

class AgentMessenger {

    constructor(config) {
        this.initialized = false;
        this.config = config || defaultConfig;

        /* set log level */
        logger.setLevel(this.config.app.mqtt_log_level);

        this.iotagent = new IoTAgent();
    }

    init(mqttClient) {
        this.iotagent.init().then(() => {
            logger.debug("... IoT agent was initialized", TAG);

            logger.debug("Registering callbacks for device events...", TAG);
            this.iotagent.on("iotagent.device", "device.create", (tenant, event) => {
                logger.debug(`Got device creation message. Tenant is ${tenant}.`, TAG);
                logger.debug("Got configure event from Device Manager", TAG);
            });

            logger.debug(`Subscribing to vernemq with topic: ${this.config.mqtt.subscribeTopic}`, TAG);

            //subscribe to verne
            mqttClient.subscribe(this.config.mqtt.subscribeTopic);

            logger.debug("... callbacks for device events were registered.", TAG);

            // If there is any configured device, the callback associated to "device.create"
            // event will be called.
            logger.debug("Requesting library to generate event for each device...", TAG);
            this.iotagent.generateDeviceCreateEventForActiveDevices();
            logger.debug("... event generation was requested.", TAG);
        }).catch(() => {
            logger.error("An error occurred while initializing the IoTAgent. Bailing out!", TAG);
            process.exit();
        });

    }

    sendMessage(topic, message) {
        try {
            logger.info(`Message published`, TAG)
            const jsonPayload = JSON.parse(message)
            const generatedData = Utils.generatePayload(topic, jsonPayload)
            const username = `${generatedData.metadata.tenant}:${generatedData.metadata.deviceid}`
            this.iotagent.updateAttrs(generatedData.metadata.deviceid, generatedData.metadata.tenant, generatedData.attrs, generatedData.metadata, username);
        } catch (error) {
            logger.error(`Error : ${error}`, TAG)
        }
    }
}

module.exports = AgentMessenger;