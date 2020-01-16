# PubKafkaPrometheus

### Latency between publication MQTT and Kafka with Prometheus and Dojot

A [prometheus](https://github.com/prometheus/prometheus) client to measure the latency between publication MQTT and when payload arrives in Apache Kafka for dojot. 

### **How works**

Latency is exposed at *http(s)://ipcontainer:PROMETHEUS_PORT/metrics* to Prometheus.

The latency is measured with the diffence between final timestamp and start timestamp.

The start timestamp in seconds must be within the  payload (publication MQTT) and data as follow:

`{"attrs": {"timestamp": 1575978282.524759 }}`

And the final timestamp in miliseconds is exposed by kafka.


## **Environment variables**

Key                      | Purpose                                                             | Default Value   | Valid Values   |
------------------------ | ------------------------------------------------------------------- | --------------- | -------------- |
AUTH_URL                 | Address of the auth service                                         | http://auth:5000| url    |
DATA_BROKER_URL              | Address of the data broker                                          | http://data-broker  | url    |
KAFKA_HOSTS              | Address of the kafka broker                                         |kafka-server:9092| hostname/IP    |
LOG_LEVEL                 | logger level                                      | info | debug, error, warning, info   |
PROMETHEUS_PORT         | Port of prometheus client                                             | 3000              | integer        |

### Grafana

There is a dashboard configuration for use with this service in [dashboard](./dashboard-grafana/Dojot100kV2.json).

### Node exporter

#### The perf collector may not work by default on all Linux systems due to kernel configuration and security settings. To allow access, set the following sysctl parameter:

`sysctl -w kernel.perf_event_paranoid=X`

- 2 allow only user-space measurements (default since Linux 4.6).
- 1 allow both kernel and user measurements (default before Linux 4.6).
- 0 allow access to CPU-specific data but not raw tracepoint samples.
- -1 no restrictions.
