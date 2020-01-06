# **MQTT Bridge**
The Mqtt Bridge service is the core between the communication of Vernemq broker and the Kafka broker. His basic actuation is to subscribe in "$share/group/+/attrs" topics and publish all the messages obtained to Kafka. The subscribe construction of the service is based in shared subscriptions. This construction enables the "mqtt bridges" services acts like an unique cluster. All the communication between the bridge and the vernemq broker is secure (MQTTS).


<p align="center"> 
<img src="./docs/diagrams/bridge.png">
</p>

# **Configuration**

## **Environment Variables**

Before running the kubernetes environment with the mqtt bridge service, make sure you configure the environment variables to match your needs. The variables can be configured in the .yaml file of the service.

Key                      | Purpose                                                             | Default Value   | Valid Values   |
------------------------ | ------------------------------------------------------------------- | --------------- | -------------- |
BASE_DIR                 | Base directory where the /certs dir will be found                   | opt/mqtt_client | directory name |
HOSTNAME                 | Hostname to be used in the certificate common name                  | broker          | hostname/IP    |
CERT_EJBCA_API_BROKER    | Address of the EJBCA broker                                         | localhost       | hostname/IP    |
CERT_EJBCA_API_PORT      | Port of the EJBCA broker                                            | 5583            | integer        |
AUTH_URL                 | Address of the auth service                                         | http://auth:5000| hostname/IP    |
DATA_BROKER              | Address of the data broker                                          | http://data-broker  | hostname/IP    |
DATA_BROKER_PORT         | Port of the data broker                                             | 80              | integer        |
KAFKA_HOSTS              | Address of the kafka broker                                         |kafka-server:9092| hostname/IP    |
DOJOT_MQTT_HOST          | Address of the verne broker                                         |vernemq-k8s      | hostname/IP    |
DOJOT_MQTT_PORT          | Port of the verne broker                                            |1883             | integer        |
CERT_DNS                 | DNS of the service                                                  |localhost        | hostname       |

# **Issues and help**

If you found a problem or need help, leave an issue in the main [Dojot repository](https://github.com/dojot/dojot) and we will help you!