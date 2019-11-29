const { Messenger } = require('@jonaphael/dojot-module')
const { logger } = require("@dojot/dojot-module-logger");
const defaultConfig = require('./config')
const TAG = { filename: "kafka-messenger" };

class KafkaMessenger {

    constructor(config) {
        this.initialized = false;
        this.config = config || defaultConfig;

        /* set log level */
        logger.setLevel(this.config.app.mqtt_log_level);

        this.messenger = new Messenger('client-messenger', this.config.messenger)
    }

    init(mqttClient) {
        this.messenger.init()
            .then(() => {
                this.messenger.createChannel(this.config.messenger.kafka.dojot.subjects.verne, "w");
                logger.info(`Kafka Messenger initialized successfully`, TAG);
                mqttClient.subscribe(this.config.mqtt.subscribeTopic);
            })
            .catch(error => {
                logger.error(`Error initializing messenger ${error}`, TAG);
            })
    }

    sendMessage(message, key = null, partition = null) {


        this.messenger.publish(this.config.messenger.kafka.dojot.subjects.verne, "admin", JSON.stringify(message), key, partition);
    }

}

module.exports = KafkaMessenger;