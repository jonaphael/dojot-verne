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

/* Functions */

/**
 * Checks if the topic has info from the username.
 *
 * Usernames follow the format: tenant:deviceid
 * Topics follow this format: /tenant/deviceid/topic
 *
 * The validation occurs by verifying whether the data from username is present
 * in the topic. Example of valid data:
 *
 * username: admin:162ba
 * topic: /admin/162ba/device-data
 * @param username device username
 * @param topic topic to which the device wants to publish
 */
function validateTopic(username: string, topic: string): boolean {
  const splitUsername = username.split(":");

  const tenant = splitUsername[0];
  const deviceid = splitUsername[1];

  const topicRegex = new RegExp(`/${tenant}/${deviceid}/.*`);

  if (topicRegex.test(topic)) {
    return true;
  } else {
    return false;
  }
}

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

/**
 * Retrieves one message from the registry.
 * @param id ID of the message to be retrieved
 */
function getPayload(id: string) {
  return messages.get(id);
}

export default { setPayload, getPayload, validateTopic };
