const config = require('./config')

const MQTTClient = require('./mqtt-client')
const client  = new MQTTClient(config)

client.init()
