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
   * Endpoint for webhook auth_on_publish
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
    logger.debug("auth_on_publish - /pub", TAG);
    logger.debug("auth_on_publish - /pub - topic: "+req.body.topic , TAG);
    logger.debug("auth_on_publish - /pub - payload: "+req.body.payload , TAG);
    logger.debug("auth_on_publish - /pub - username: "+req.body.username , TAG);

    res.status(200).send({ "result": "ok" });
  });

  /**
   * Endpoint for webhook auth_on_register
   */
  app.post("/reg", (_req: express.Request, res: express.Response) => {

    logger.debug("auth_on_register - /reg", TAG);

    res.status(200).send({ "result": "ok" });
  });

  /**
   * Endpoint for webhook auth_on_subscribe
   */
  app.post("/sub", (_req: express.Request, res: express.Response) => {

    logger.debug("auth_on_subscribe - /sub", TAG);

    res.status(200).send({ "result": "ok" });
  });
};

export default verneRoute;
