import { logger } from "@dojot/dojot-module-logger";
import express from "express";
import ProjectUtils from "../utils/utils";

const TAG = { filename: "verneRoute" };

/**
 * VerneMQ webhooks routes creation function.
 * @param app Express application to be used
 */
const verneRoute = (app: express.Application) => {
  /**
   * Endpoint for webhook auth_on_publish
   */
  app.post("/pub", (req: express.Request, res: express.Response) => {

    const configTopic = /.+\/config/;

    // Verifying whether the message is a configuration one
    if (configTopic.test(req.body.topic)) {
      logger.debug("Message is from /config topic. Discarding!", TAG);
    } else {
      ProjectUtils.setPayload(req.body.payload, req.body.username);
    }

    res.status(200).send({ "result": "ok" });
  });
};

export default verneRoute;
