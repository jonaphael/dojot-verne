"use strict";
import { logger } from "@dojot/dojot-module-logger";
import bodyParser from "body-parser";
import express = require("express");
import { Server } from "http";
import verneRoute from "../routes/verneRoute";

const TAG = { filename: "app" };

class App {
  public isInitialized: boolean;
  private app: express.Application | null;
  private httpServer: Server | null;

  constructor() {
    this.isInitialized = false;
    this.app = null;
    this.httpServer = null;
  }

  public initApp() {
    this.isInitialized = false;

    this.app = express();
    this.app.use(bodyParser.json({ type: "*/*" }));
    this.app.use(bodyParser.urlencoded({ extended: true }));

    /* Creating the routes */
    verneRoute(this.app);

    /* Starting the server */
    this.httpServer = this.app.listen(4000, () => {
      logger.info("Listening on port 4000.", TAG);
      this.isInitialized = true;
    });
  }

  /**
   * Finishes the Express app.
   */
  public stopApp() {
    if (this.isInitialized) {
      logger.info("Stopping the server.", TAG);
      this.isInitialized = false;
      this.httpServer!.close();
    }
  }
}

export { App };
