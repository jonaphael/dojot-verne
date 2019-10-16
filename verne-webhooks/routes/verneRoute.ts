import { logger } from "@dojot/dojot-module-logger";
import express from "express";
import ProjectUtils from "../utils/utils";
import { Messenger } from "@dojot/dojot-module";
import config from "../src/config"

const TAG = { filename: "verneRoute" };

/**
 * VerneMQ webhooks routes creation function.
 * @param app Express application to be used
 */
const verneRoute = (app: express.Application, messenger: Messenger) => {
  /**
   * Endpoint for webhook on_publish
   */
  app.post("/pub", (req: express.Request, res: express.Response) => {

    const configTopic = /.+\/config/;

    // Verifying whether the message is a configuration one
    if (configTopic.test(req.body.topic)) {
      logger.debug("Message is from /config topic. Discarding!", TAG);

    }
    else {
      const payload = ProjectUtils.setPayload(req.body.payload, req.body.username);
      
      if (messenger) {
        messenger.publish(config.messenger.kafka.dojot.subjects.verne, "admin", JSON.stringify(payload));
      }
    }

    res.status(200).send({});
  });
};

export default verneRoute;
