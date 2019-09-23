"use strict";
import { logger } from "@dojot/dojot-module-logger";
import moment from "moment";
import uuid from "uuid/v4";

/* Interfaces */
interface IMetadata {
  messageId: string;
  receivedTs: number;
  thingId: string;
}
interface IDojotMessage {
  data: object;
  metadata: IMetadata;
}

/* Variables */
const TAG = { filename: "utils" };
const messages: Map<string, object> = new Map();

/**
 * Creates the message to be sent to Kafka.
 * @param mqttPayload MQTT message received from the device
 * @param cname device identifier
 */
function setPayload(mqttPayload: object, cname: string): IDojotMessage {
  const value: IDojotMessage = {
    data: mqttPayload,
    metadata: {
      messageId: uuid(),
      receivedTs: moment().unix(),
      thingId: cname,
    },
  };

  messages.set(value.metadata.messageId, value);

  logger.debug(`Number of stored messages: ${messages.size}`, TAG);

  return value;
}

function getPayload(id: string) {
  return messages.get(id);
}

export { setPayload, getPayload };
