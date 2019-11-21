const { getHTTPRouter, logger } = require("@dojot/dojot-module-logger");
const bodyParser = require("body-parser");
const express = require("express");
const { Messenger } = require("@jonaphael/dojot-module");
const config = require("./config");

const prometheusRoute = require("../routes/prometheus_routes");
const { setLatencyData } = require("../utils/prom_structs");

const TAG = { filename: "app" };

/**
 * Manages the Express app.
 */
class App {

    constructor() {
        this.isInitialized = false;
        this.app = null;
        this.httpServer = null;
        this.messenger = null;
    }

    /**
     * Initializes the Express app.
     */
    initApp() {
        this.isInitialized = false;

        this.app = express();
        this.app.use(bodyParser.json({ type: "*/*" }));
        this.app.use(bodyParser.urlencoded({ extended: true }));
        this.app.use(getHTTPRouter());
        logger.setLevel(config.app.log_level);

        // initialize messenger
        // this.messenger = new Messenger("prometheus", config.messenger);
        // this.messenger.init().then(() => {
        //     this.messenger.createChannel(config.messenger.kafka.dojot.subjects.verne, "r")
        // }).catch((error) => {
        //     logger.debug(`... failed to initialize the IoT agent messenger. Error: ${error.toString()}`, TAG);
        // })

        /* Creating the routes */
        prometheusRoute(this.app);

        /* Starting the server */
        this.httpServer = this.app.listen(config.prom.port, () => {
            setLatencyData();
            logger.info(`Listening on port ${config.prom.port}.`, TAG);
            this.isInitialized = true;
        });
    }

    /**
     * Finishes the Express app.
     */
    stopApp() {
        if (this.isInitialized) {
            logger.info("Stopping the server...", TAG);
            this.isInitialized = false;
            this.httpServer.close();
        }
    }
}

module.exports = { App };