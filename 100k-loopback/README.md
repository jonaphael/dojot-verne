# 100K Loopback

This service is a helper for 100k-epic, its functionality is to transfer incomming payload from a **device-data** topic to **iotagent-device** topic.

# Configurations
## Environment Variables

Key                      | Purpose                                                             | Default Value   			| Valid Values   |
------------------------ | ------------------------------------------------------------------- | -------------------------- | -------------- |
DOJOT_USERNAME           | username to login on auth and retrieve token						   | admin           			| string   		 |
DOJOT_PASSWORD           | password to login on auth and retrieve token						   | admin           			| string   		 |
AUTH_HOST                | Address of the auth service                                         | http://dojot_auth_1:5000   | hostname/IP    |
DATA_BROKER_HOST         | dojot data broker url to fetch the the topics                       | http://dojot_data-broker_1	| hostname/IP    |
KAFKA_HOSTS              | Address of the kafka broker                                         | kafka-server:9092			| hostname/IP    |
LOOPBACK_CONSUMER_GROUP  | Kafaka consumer group                                               | 100k-loopback-group        | string         |
DEVICE_DATA  			 | topic to consume from                                               | device-data        		| string         |
DEVICE_CONFIGURE         | topic to produce the messages                                       | dojot.device-manager.device| string         |

# Example
As a specific component for dojot device there is an example

Device data received mesage
```
{
    "metadata": { 
        "deviceid":"e33dada7ce5a4905819d8fb0606c613f",
        "tenant":"admin",
        "timestamp":1583939224
    },
    "attrs": {
        "timestamp": 1583939224072
    }
}
```

Device configure final payload
```
{
    "metadata": {
        "deviceid": "5908d1b9787f4dc9a5bdf94f5122b37e",
        "tenant":"admin",
        "timestamp":1583955993 
    },
    "attrs": {
        "timestamp": 1583955993805
    }
}
```

More infomations can be find [here](https://dojotdocs.readthedocs.io/projects/DeviceManager/en/latest/kafka-messages.html).