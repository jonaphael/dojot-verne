# **Dojot VerneMQ Webhook**

[![Build Status](https://travis-ci.com//dojot/ejbca.svg?branch=development)](https://travis-ci.com/dojot/ejbca)
[![CodeFactor](https://www.codefactor.io/repository/github/dojot/ejbca/badge)](https://www.codefactor.io/repository/github/dojot/ejbca)
[![DeepScan grade](https://deepscan.io/api/teams/5314/projects/7122/branches/66277/badge/grade.svg)](https://deepscan.io/dashboard#view=project&tid=5314&pid=7122&bid=66277)
[![codecov](https://codecov.io/gh/dojot/ejbca/branch/development/graph/badge.svg)](https://codecov.io/gh/dojot/ejbca)

The Dojot VerneMQ Webhooks service is responsible for receiving the messages sent to VerneMQ by IoT devices.

## **Overview**

When a message is sent from an IoT device to VerneMQ, some webhooks are triggered, so it can be treated.
This service receives these messages routed from VerneMQ, adding dojot metadata to them.

Whenever a message is received, we transform it to the following model:
```js
{
  data: message_payload,
  metadata: {
    messageId: uuid4,
    thingId: cname,
    receivedTs: unix_timestamp
  }
}
```

Where:
- data: received message payload
- messageId: message UUIDv4 unique identifier
- thingId: unique thing identifier
- receivedTs: message arrival timestamp

This way, we can pass the message to Kafka, so other modules can consume it.

## **Environment variables**

Key                      | Purpose                                                       | Default Value  | Accepted values
-----------------------  | --------------------------------------------------------------| -------------- | -------------------------
VERNE_WEBHOOKS_LOG_LEVEL | Level of debug                                                | "info"         | info, warn, error, debug
VERNE_WEBHOOKS_PORT      | Webhooks service port                                         | 4000           | port values

## **Using the service**

If you want to run a standalone version of this service, you can use docker or run locally in your computer.

### **Running locally**

To run with NodeJS:

```shell
npm install
npm run-script build
npm run-script webhook
```

To run with TS Node:

```shell
npm run-script dev-run
```

### **Building Docker image by yourself**

```shell
docker build -t verne_webhooks .
docker run verne_webhooks
```

### **Downloading the official image**

```shell
docker push eduardogmisiuk/verne_webhooks
docker run eduardogmisiuk/verne_webhooks
```
