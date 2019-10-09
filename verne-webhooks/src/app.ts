"use strict";
import { getHTTPRouter as getLoggerRouter, logger } from "@dojot/dojot-module-logger";
import bodyParser from "body-parser";
import express = require("express");
import { Server } from "http";
import morgan from "morgan";
import verneRoute from "../routes/verneRoute";
import config from "./config";
import KafkaMesssenger from "../kafka/kafkaMessenger";

const TAG = { filename: "app" };

/**
 * Manages the Express app.
 */
class App {
  public isInitialized: boolean;
  private app: express.Application | null;
  private httpServer: Server | null;

  constructor() {
    this.isInitialized = false;
    this.app = null;
    this.httpServer = null;
  }

  /**
   * Initializes the Express app.
   */
  public initApp() {
    this.isInitialized = false;

    this.app = express();
    this.app.use(bodyParser.json({ type: "*/*" }));
    this.app.use(bodyParser.urlencoded({ extended: true }));
    this.app.use(morgan("short"));
    this.app.use(getLoggerRouter());

    /* Creating the routes */
    verneRoute(this.app);

    /* create the kafka Messenger (messenger) */
    const kafkaMessenger = new KafkaMesssenger(config.kafka.producer);
    kafkaMessenger.produceMessage("{ 'message' : 123 }");

    /* Starting the server */
    this.httpServer = this.app.listen(config.vernemq.port, () => {
      logger.info(`Listening on port ${config.vernemq.port}.`, TAG);
      this.isInitialized = true;
    });
  }

  /**
   * Finishes the Express app.
   */
  public stopApp() {
    if (this.isInitialized) {
      logger.info("Stopping the server...", TAG);
      this.isInitialized = false;
      this.httpServer!.close();
    }
  }
}

export default App;
