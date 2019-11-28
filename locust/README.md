# **Dojot Load Test**
The Dojot Load Test tool is an implementation using the open-source load testing tool Locust to generate traffic
from/to Dojot.

# **Configuration**

## **Environment Variables**

Before running any tests using this tool, make sure you configure the environment variables to match your needs.

When using Docker, you should pass the variables in the Dockerfile for the component you are running.
The Dockerfile for the Locust master and slave are in the `Docker` directory.

### **Locust**

Locust behaviour and Redis configurations.

Key                      | Purpose                                                             | Default Value | Valid Values   |
------------------------ | ------------------------------------------------------------------- | ------------- | -------------- |
CA_CERT_FILE             | CA certificate file                                                 | ca.crt        | file name      |
CERT_DIR                 | certificates and private keys directory                             | cert/         | directory name |
DEBUG_MODE               | show debug messages                                                 | False         | True, False    |
LOCUST_LOG_DIR           | client log directory                                                | /log          | directory name |
LOCUST_LOG_MESSAGE_LIMIT | limit number of messages to write in log files                      | 1000          | integer        |
LOG_IN_FILE              | write log messages in a file                                        | False         | True, False    |
MAP_DEVICE_IDS           | maps the certificates from REDIS_CERTIFICATES_DB to REDIS_MAPPED_DB | False         | True, False    |
REDIS_BACKUP             | use a Redis dump with IDs instead of generating new ones            | y             | y, n           |
REDIS_CERTIFICATES_DB    | database with the certificates                                      | 0             | integer        |
REDIS_HOST               | redis host                                                          | redis         | hostname/IP    |
REDIS_MAPPED_DB          | database with the mapped device IDs from certificates database      | 1             | integer        |
REDIS_PASSWD             | redis password                                                      | none          | passwords      |
REDIS_PORT               | redis port                                                          | 6379          | 1024-65535     |
TASK_MAX_TIME            | max time of each Locust's tasks (ms)                                | 30000         | integer        |
TASK_MIN_TIME            | min time of each Locust's tasks (ms)                                | 29500         | integer        |

### **MQTT**

Configurations related to MQTT communication.

Key                    | Purpose                           | Default Value | Valid Values |
---------------------- | --------------------------------- | ------------- | ------------ |
DOJOT_MQTT_HOST        | MQTT broker host                  | 127.0.0.1     | hostname/IP  |
DOJOT_MQTT_PORT        | MQTT broker port                  | 1883          | 1024-65535   |
DOJOT_MQTT_QOS         | MQTT broker QoS level             | 1             | 0, 1, 2      |
DOJOT_MQTT_TIMEOUT     | MQTT broker timeout               | 60            | integer      |
MQTT_PUBLISH_TIMEOUT   | publish timeout in milliseconds   | 40000         | integer      |
MQTT_SUBSCRIBE_TIMEOUT | subscribe timeout in milliseconds | 40000         | integer      |

### **Dojot**

Dojot integration configuration.

Key          | Purpose                              | Default Value | Valid Values |
------------ | ------------------------------------ | ------------- | ------------ |
DOJOT_ENV    | use a dojot instance                 | n             | y, n         |
DOJOT_PASSWD | dojot user's password                | admin         | passwords    |
DOJOT_URL    | dojot instance address               | 127.0.0.1     | hostname/IP  |
DOJOT_USER   | dojot user                           | admin         | usernames    |
GENERATE_IDS | activate the automatic ID generation | 1             | 0, 1         |

## **Operating System**

While small tests can be run without problems, bigger ones create some obstacles.
To create a lot of clients in only one machine, the default number of ports in the
OS will not accomodate the required number of connections. To increase it, run:

```shell
sudo sysctl -w net/ipv4/ip_local_port_range="1024 65535"
```

# **How to use**

## Certificates

First of all, you will need to generate the certificates for each device. To see how to generate
them, see the `redisinit` module. You will need to generate the `.key` and `.crt` files for each
device, besides the CA certificate file, and move them to the `cert/` directory. You can change
the default values for the CA certificate file and the path to the certificates by changing,
respectively, the variables `CA_CERT_FILE` and `CERT_DIR`. These certificates **must** be in
**each machine** that runs a slave.

After that, you need to export the Redis dump and move it to the `db/` directory. The dump
filename must be `dump.rdb`. This dump **must** be in the machine that runs the master node.

Every time you change the Redis dump file, you will need to map the certificates. This is done
by simply setting the environment variable `MAP_DEVICE_IDS` to `True`. After the mapping has
finished, you can disable it.

## **Docker-Compose**

In Locust root directory, bring up the master node:

```shell
docker-compose -f Docker/docker-compose-master.yml up
```

After the complete initialization of the master node, run the slave:

```shell
docker-compose -f Docker/docker-compose-slave.yml up
```

You can also scale them:

```shell
docker-compose -f Docker/docker-compose-slave.yml up --scale locust-slave=10
```

## **Dockerfile**

If you need to execute the containers individually, run the commands:

```shell
sudo docker build -t locust-mqtt .
sudo docker run -it -d -p 8089:8089 locust-mqtt
sudo docker exec -it <CONTAINER_ID> /bin/bash -c "locust -f main.py Client"
```

## Accessing the GUI

After the initialization of the master, you can access the graphical interface by
typing the address to the server you are running the master followed by the Locust
port: `localhost:8089`.

# **How it works**

Locust works with a master/slave architecture, as you can see in the diagram.

- The Master node is responsible for gathering and showing data from slaves using the
graphical interface
- The Slave node is responsible for making the communication with the server, sending
and receiving messages from Dojot

<img align="center" src="https://github.com/eduardogmisiuk/dojot-verne/blob/update_documentation/locust/docs/diagrams/Locust.png">

The main idea behind Locust is to call **Locust tasks** from time to time to execute tests.
These tasks are run in the slaves only. In our case, we have only one task, responsible for
publishing MQTT messages with QoS 1 to the IoTAgent every 30s.

Aside from publishing, we receive messages too. Every time one client is created by Locust and
successfully connects to the server, it will subscribe to the config topic. The config topic
receives messages from Dojot to actuate in the device, so it is emulating an actuation in the
device - in our case, the device is a Locust client.

The used topics are:
- **Publish**: tenant:deviceid/attrs
- **Subscribe**: tenant:deviceid/config

# Machine specifications for tests

To achieve 100.000 connections with ~3,333 RPS (Requests Per Second), we used distributed Locust
in 4 virtual machines in a cluster. Their configuration were:

Cluster info:
- Intel(R) Xeon(R) Silver 4114 CPU @ 2.20GHz
- 62GB RAM DDR4 2,666 MHz
- 2TB 7.2K RPM HDD

VM1 and VM2 (each):
- 9 CPUs
- 14GB RAM
- 50GB HDD

VM3 and VM4 (each):
- 9 CPUs
- 14GB RAM
- 30GB HDD

In total:
- 36 CPUs
- 56GB RAM
- 160GB HDD

Each machine ran 9 slaves, with the VM1 running the master too.

# **Issues and help**

If you found a problem or need help, leave an issue in the main [Dojot repository](https://github.com/dojot/dojot) and we will help you!

# **References**
[Material to understand paho-mqtt threads](http://www.steves-internet-guide.com/loop-python-mqtt-client/)

[Locust]( https://locust.io/)

[Paho-MQTT Python Library](https://pypi.org/project/paho-mqtt/)
