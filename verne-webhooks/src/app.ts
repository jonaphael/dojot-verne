"use strict";
import { getHTTPRouter as getLoggerRouter, logger } from "@dojot/dojot-module-logger";
import bodyParser from "body-parser";
import express = require("express");
import { Server } from "http";
import morgan from "morgan";
import verneRoute from "../routes/verneRoute";
import config from "./config";
import { Messenger } from "@jonaphael/dojot-module"

const TAG = { filename: "app" };

/**
 * Manages the Express app.
 */
class App {
  public isInitialized: boolean;
  private app: express.Application | null;
  private httpServer: Server | null;
  private messenger: Messenger | null;

  constructor() {
    this.isInitialized = false;
    this.app = null;
    this.httpServer = null;
    this.messenger = null;
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
    logger.setLevel(config.app.log_level);
      
    // initialize messenger
    this.messenger = new Messenger("vernemq-epic", config.messenger);
    this.messenger.init().then(() => {
      this.messenger!.createChannel(config.messenger.kafka.dojot.subjects.verne, "rw")
    }).catch((error: any) => {
      logger.debug(`... failed to initialize the IoT agent messenger. Error: ${error.toString()}`, TAG);
    })

    /* Creating the routes */
    verneRoute(this.app, this.messenger);


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
