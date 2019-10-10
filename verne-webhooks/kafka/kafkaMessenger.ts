import Kafka from "node-rdkafka"
import config from "../src/config";
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
        const message = { "status" : "ready" }
        this.produceMessage(message);
    }

    private _producerOnError(error: Error) {
        logger.warn(`Error from producer ${error}`, TAG);
    }
    
    produceMessage(payload: any, topicParam?: string, keyParam ?: string) {
        if (!this.producerReady) {
            logger.warn(`Producer is not ready ignoring payload`, TAG);
            return;
        }
        
        let topic = undefined;
        if (topicParam === undefined) {
            topic = config.kafka.topic;
        }
        else {
            topic = topicParam;
        }

        let key = undefined;

        if (keyParam === undefined) {
            key = config.kafka.key;
        } 
        else { 
            key = keyParam;
        }
        
        logger.info(`Sending payload '${JSON.stringify(payload)}' to topic ${topic}`, TAG);
        
        try {
            const payloadStr = JSON.stringify(payload);
            const message = Buffer.from(payloadStr);
            console.log("Message is" + message + "Payload is" + payloadStr)
 
            this.producer.produce(topic, config.kafka.partition, message, key, Date.now())    
        } catch (error) {
            logger.warn(`A problem occurred when sending the payload ${error}`, TAG);
        }
    }
}