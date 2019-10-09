import Kafka from "node-rdkafka"
import { logger } from "@dojot/dojot-module-logger";

const TAG = { filename: "kafka" };

export default class KafkaMesssenger {

    private producerConfig: any;
    private producer: any;
    private producerReady: boolean = false;

    constructor(producerConfig: any = null) {
        this.producerConfig = producerConfig;
        this._createProducer();
    }

    private _createProducer() {
        if (!this.producerConfig) {
            logger.warn("No producer created", TAG);
            return;
        }

        this.producer = new Kafka.Producer(this.producerConfig);
        logger.info("Producer created!", TAG);

        // connect producer
        this.producer.connect()
        const boundProducerReady = this._producerOnReady.bind(this);
        const boundProducerError = this._producerOnError.bind(this);
        this.producer.on("ready", boundProducerReady);
        this.producer.on("error", boundProducerError)
    }

    private _producerOnReady() {
        this.producerReady = true;
        logger.info("Producer is ready", TAG);
    }

    private _producerOnError(error: Error) {
        logger.warn(`Error from producer ${error}`, TAG);
    }
    
    produceMessage(payload: any) {
        if (!this.producerReady) {
            logger.info(`Producer is not ready ignoring payload`, TAG);
            return;
        }
        
        logger.info(`Sending payload ${payload} ....`, TAG);

        try {
            this.producer.produce()    
        } catch (error) {
            
        }
    }
}