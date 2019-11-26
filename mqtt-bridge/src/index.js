const config = require('./config')

const MQTTClient = require('./mqtt-client')
const KafkaMessenger = require('./kafkaMessenger')

const kafkaMessenger = new KafkaMessenger(config)
const client  = new MQTTClient(config, kafkaMessenger)

kafkaMessenger.init()
client.init()