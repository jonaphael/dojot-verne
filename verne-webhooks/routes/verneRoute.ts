import { logger } from "@dojot/dojot-module-logger";
import express from "express";
import ProjectUtils from "../utils/utils";
import KafkaMesssenger from "../kafka/kafkaMessenger";

const TAG = { filename: "verneRoute" };

/**
 * VerneMQ webhooks routes creation function.
 * @param app Express application to be used
 */
const verneRoute = (app: express.Application, kafkaMessenger: KafkaMesssenger) => {
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
      ProjectUtils.setPayload(req.body.payload, req.body.username);

    }

    kafkaMessenger.produceMessage(payload);
    res.status(200).send({});
  });
};

export default verneRoute;
