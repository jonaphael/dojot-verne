# dojot-mqtt-locust
mqtt-locust is a Load testing tool for dojot IoT platform using Locust.io and Paho MQTT client.

The main objective of the load testing is connect 100K devices IoT with dojot plataform

# References
[Material to understand paho-mqtt threads](http://www.steves-internet-guide.com/loop-python-mqtt-client/)

[Locust]( https://locust.io/)

[Paho-MQTT Python Library](https://pypi.org/project/paho-mqtt/)

# Dockerfile

Currently is been used a docker-compose file, but you can run manually as you want.

```shell
sudo docker build -t locust-mqtt .
sudo docker run -it -d -p 8089:8089 locust-mqtt
sudo docker exec -it <CONTAINER_ID> /bin/bash -c "locust -f main.py Client"
```