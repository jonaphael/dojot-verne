"use strict";
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

  return value;
}

export default { setPayload, validateTopic };
