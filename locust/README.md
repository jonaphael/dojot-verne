# dojot-mqtt-locust
mqtt-locust is a Load testing tool for dojot IoT platform using Locust.io and Paho MQTT client.

The main objective of the load testing is to connect 100K IoT devices with dojot plataform.

# References
[Material to understand paho-mqtt threads](http://www.steves-internet-guide.com/loop-python-mqtt-client/)

[Locust]( https://locust.io/)

[Paho-MQTT Python Library](https://pypi.org/project/paho-mqtt/)

# Commands

# Docker-Compose

**Before running any command here, make sure the environment variables are correctly set to match you preferences.**

In Locust root directory, bring up the master node:

```shell
docker-compose -f Docker/docker-compose-master.yml up
```

Running the slave:

```shell
docker-compose -f Docker/docker-compose-slave.yml up
```

You can also scale them:

```shell
docker-compose -f Docker/docker-compose-slave.yml up --scale locust-slave=10
```

# Dockerfile

If you need to execute the containers individually, run the commands:

```shell
sudo docker build -t locust-mqtt .
sudo docker run -it -d -p 8089:8089 locust-mqtt
sudo docker exec -it <CONTAINER_ID> /bin/bash -c "locust -f main.py Client"
```

## Configuration

These are the environment variables used by the tests tool.

Key                      | Purpose                                                         | Default Value | Valid Values    |
-----------------------  | --------------------------------------------------------------- | ------------- | --------------- |
DEBUG_MODE               | show debug messages                                             | False         | True, False     |
LOG_IN_FILE              | write log messages in a file                                    | False         | True, False     |
TASK_MIN_TIME            | min time of each Locust's tasks (ms)                            | 1000          | integer         |
TASK_MAX_TIME            | max time of each Locust's tasks (ms)                            | 1000          | integer         |
LOCUST_LOG_DIR           | client log directory                                            | /logs         | directory name  |
LOCUST_LOG_MESSAGE_LIMIT | limit messagesto write in file                                  | 1000          | integer         |
NUMBER_OF_DEVICES        | number of devices used for tests                                | 100000        | integer         |
GENERATE_IDS             | activate the automatic ID generation                            | 1             | 0, 1            |
REDIS_HOST               | redis host                                                      | redis         | host name/IP    |
REDIS_PORT               | redis port                                                      | 6379          | 1024-65535      |
REDIS_PASSWD             | redis password                                                  | none          | passwords       |
REDIS_BACKUP             | use a Redis dump with IDs instead of generating new ones        | y             | y, n            |
REDIS_CERTIFICATES_DB    | database with the certificates                                  | 0             | integer         |
REDIS_MAPPED_DB          | database with the mapped device IDs from certificates database  | 1             | integer         |
MAP_DEVICE_IDS           | activates device IDs mapping in REDIS_MAPPED_DB database        | False         | True, False     |
CERT_DIR                 | certificates and private keys directory                         | cert/         | directory name  |
CA_CERT_FILE             | CA certificate file                                             | ca.crt        | file name       |
DOJOT_MQTT_HOST          | MQTT broker host                                                | 127.0.0.1     | host name/IP    |
DOJOT_MQTT_PORT          | MQTT broker port                                                | 1883          | 1024-65535      |
DOJOT_MQTT_TIMEOUT       | MQTT broker timeout                                             | 60            | integer         |
DOJOT_MQTT_QOS           | MQTT broker QoS level                                           | 1             | 0, 1, 2         |
MQTT_PUBLISH_TIMEOUT     | publish timeout in milliseconds                                 | 40000         | integer         |
MQTT_SUBSCRIBE_TIMEOUT   | subscribe timeout in milliseconds                               | 40000         | integer         |
DOJOT_ENV                | use a dojot instance                                            | n             | y, n            |
DOJOT_URL                | dojot instance address                                          | 127.0.0.1     | host name/IP    |
DOJOT_USER               | dojot user                                                      | admin         | user names      |
DOJOT_PASSWD             | dojot user's password                                           | admin         | passwords       |
