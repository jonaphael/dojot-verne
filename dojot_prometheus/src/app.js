const { getHTTPRouter, logger } = require("@dojot/dojot-module-logger");
const bodyParser = require("body-parser");
const express = require("express");
const { Messenger } = require("@jonaphael/dojot-module");
const config = require("./config");
const util = require("util");
const Prometheus = require('prom-client');
const ss = require('simple-statistics');

const TAG = { filename: "dojot_prom" };

const collectDefaultMetrics = Prometheus.collectDefaultMetrics;
collectDefaultMetrics({ prefix: 'dojot_prom_' });

const dojotLatency = new Prometheus.Gauge({
    name: 'Dojot_Statistics',
    help: 'Dojot statistics',
    labelNames: ['kind', 'unit'],
});


class App {

    constructor() {
        this.isInitialized = false;
        this.appExpress = null;
        this.httpServer = null;
        this.messenger = null;
        this.allTimes = [];
    }

    getAllTimes() {

        return this.allTimes;
    }

    cleanAllTimes() {

        this.allTimes = [];
    }

    isNotEmptyAllTimes() {

        if (this.allTimes && this.allTimes.length > 0)
            return true;

        return false;
    }



    init() {
        this.isInitialized = false;

        this.appExpress = express();
        this.appExpress.use(bodyParser.json({ type: "*/*" }));
        this.appExpress.use(bodyParser.urlencoded({ extended: true }));
        this.appExpress.use(getHTTPRouter());

        logger.setLevel(config.app.log_level);

        this._initDojotMessenger();

        prometheusRoute(this.appExpress);

        this.httpServer = this.appExpress.listen(config.prom.port, () => {

            logger.info(`Listening on port ${config.prom.port}.`, TAG);
            this.isInitialized = true;
        });
    }

    _initDojotMessenger() {
        this.messenger = new Messenger("dojotprommetrics", config.messenger);
        this.messenger.init().then(() => {
            this._initKafka();
        }).catch((error) => {
            logger.debug(`... failed to initialize the dojot-prom-metrics messenger. Error: ${error.toString()}`, TAG);
        });
    }

    _initKafka() {
        this.messenger.createChannel(config.messenger.kafka.dojot.subjects.verne, "rw");
        const kafkaOnMessageBind = this._kafkaOnMessage.bind(this);
        this.messenger.on(config.messenger.kafka.dojot.subjects.verne, "message", kafkaOnMessageBind);
    }

    _kafkaOnMessage(_tenant, message, extraInfo) {
        logger.debug(`The extra info is ${util.inspect(extraInfo, { depth: null })}`, TAG);
        try {
            const { timestamp } = extraInfo;
            const messageContent = JSON.parse(message);
            const { data } = messageContent;

            //To modify
            let buff = Buffer.from(data, 'base64');
            let text = buff.toString('ascii');
            const dataJSON = JSON.parse(text);
            const { timestamp:start_time } = dataJSON;
            const start_time_in_ms = parseInt(Math.floor((start_time * 1000)));
            let totalTime = Number(timestamp) - start_time_in_ms;
            this.allTimes.push(totalTime);
            //# To modify

        } catch (error) {
            logger.error(`Error parsing Kafka message: ${error}`, TAG);
        }
    }

    stopApp() {
        if (this.isInitialized) {
            logger.info("Stopping the server...", TAG);
            this.isInitialized = false;
            this.httpServer.close();
        }
    }
}

promApp = new App();

let prometheusRoute = (app) => {

    app.get('/metrics', (req, res) => {

        res.set('Content-Type', Prometheus.register.contentType);

        logger.debug(`Expose metrics to prometheus from ${util.inspect(promApp.getAllTimes(), { depth: null })} are:`, TAG);

        const max = promApp.isNotEmptyAllTimes() ? ss.max(promApp.getAllTimes()) : 0;
        const min = promApp.isNotEmptyAllTimes() ? ss.min(promApp.getAllTimes()) : 0;
        const avg = promApp.isNotEmptyAllTimes() ? ss.mean(promApp.getAllTimes()) : 0;
        const median = promApp.isNotEmptyAllTimes() ? ss.median(promApp.getAllTimes()) : 0;
        const standardDeviation = promApp.isNotEmptyAllTimes() ? ss.standardDeviation(promApp.getAllTimes()) : 0;

        
        logger.debug(`Max: ${max} ms`, TAG);
        logger.debug(`Min: ${min} ms`, TAG);
        logger.debug(`AVG: ${avg} ms`, TAG);
        logger.debug(`Median: ${median} ms`, TAG);
        logger.debug(`Standard Deviation: ${standardDeviation} ms`, TAG);


        promApp.cleanAllTimes();

        dojotLatency.set({ kind: 'max', unit: 'ms' }, max);
        dojotLatency.set({ kind: 'min', unit: 'ms' }, min);
        dojotLatency.set({ kind: 'avg', unit: 'ms' }, avg);
        dojotLatency.set({ kind: 'median', unit: 'ms' }, median);
        dojotLatency.set({ kind: 'standardDeviation', unit: 'ms' }, standardDeviation);

        res.end(Prometheus.register.metrics());

    });
};

module.exports = promApp;
