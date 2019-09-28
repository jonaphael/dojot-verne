# dojot-mqtt-locust
mqtt-locust is a Load testing tool for dojot IoT platform using Locust.io and Paho MQTT client.

The main objective of the load testing is connect 100K devices IoT with dojot plataform

# References
[Material to understand paho-mqtt threads](http://www.steves-internet-guide.com/loop-python-mqtt-client/)

[Locust]( https://locust.io/)

[Paho-MQTT Python Library](https://pypi.org/project/paho-mqtt/)

# Commands

# Docker-Compose

To scale containers using docker compose:

```shell
sudo docker-compose build
sudo docker-compose up -d --scale locust-slave=10
```

# Dockerfile

Currently is been used a docker-compose file, but you can run manually as you want.

```shell
sudo docker build -t locust-mqtt .
sudo docker run -it -d -p 8089:8089 locust-mqtt
sudo docker exec -it <CONTAINER_ID> /bin/bash -c "locust -f main.py Client"
```

## Configuration

These are the environment variables used by tests tool

Key                     | Purpose                                                       | Default Value
----------------------- | --------------------------------------------------------------| --------------
REDIS_HOST              | redis host                                                    | redis
REDIS_PORT              | redis port                                                    | 6379
REDIS_PASSWD            | redis password                                                | none
DOJOT_URL               | URL of dojot                                                  | http://localhost:8000
DOJOT_USER              | user dojot                                                    | admin
DOJOT_PASSWD            | password dojot                                                | admin
DOJOT_MQTT_HOST         | mqqt broker host                                              | 127.0.0.1
DOJOT_MQTT_PORT         | mqqt broker port                                              | 1883
DOJOT_MQTT_TIMEOUT      | mqqt broker timeout                                           | 60
NUMBER_OF_DEVICES       | number of devices used for tests                              | 10000
TASK_MIN_TIME           | min time of each tasks (ms)                                   | 1000
TASK_MAX_TIME           | max time of each tasks (ms)                                   | 1000