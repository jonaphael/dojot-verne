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

        this.messeger = new Messenger('client-messenger', this.config.messenger)
    }

    init() {
        this.messeger.init()
            .then( () => {
                this.messeger.createChannel(this.config.messenger.kafka.dojot.subjects.verne, "w");
                logger.info(`Kafka Messenger initialized successfully`, TAG);
                this.initialized = true;
            })
            .catch( error => {
                logger.error(`Error initializing messenger ${error}`, TAG);
                return false;
            })
    }

    sendMessage(mesage, key = null, partition = null) {
        if (this.initialized)
            this.messeger.publish(this.config.messenger.kafka.dojot.subjects.verne, "admin", JSON.stringify(mesage), key, partition);
        else {
            logger.error(`Messenger not initialized descarting message ...`);
        }
    }

}

module.exports = KafkaMessenger;