# 100K Loopback

This service is a helper for 100k-epic, its functionality is to transfer incomming payload from a **device-data** topic to **iotagent-devce** topic.

# Configurations
## Environment Variables

Key                      | Purpose                                                             | Default Value   			| Valid Values   |
------------------------ | ------------------------------------------------------------------- | -------------------------- | -------------- |
DOJOT_USERNAME           | username to login on auth and retrieve token						   | admin           			| string   		 |
DOJOT_USERNAME           | password to login on auth and retrieve token						   | admin           			| string   		 |
AUTH_HOST                | Address of the auth service                                         | http://dojot_auth_1:5000   | hostname/IP    |
DATA_BROKER_HOST         | dojot data broker url to fetch the the topics                       | http://dojot_data-broker_1	| hostname/IP    |
KAFKA_HOSTS              | Address of the kafka broker                                         | dojot_kafka_1				| hostname/IP    |
LOOPBACK_CONSUMER_GROUP  | Kafaka consumer group                                               | 100k-loopback-group        | string         |
DEVICE_DATA  			 | topic to consume from                                               | device-data        		| string         |
DEVICE_CONFIGURE         | topic to produce the messages                                       | iotagent.device            | string         |