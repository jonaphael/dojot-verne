const mqtt = require('mqtt')
const defaultConfig = require('./config')
const Utils = require('./utils/utils')
const { logger } = require('@dojot/dojot-module-logger')
const TAG = { filename: "mqtt-client" }
const AgentMessenger = require('./AgentMessenger')
const fs = require('fs');

class MQTTClient {

    constructor(config) {

        this.config = config || defaultConfig;
        this.isConnected = false;
        this.agentMessenger = null;

        this.key = fs.readFileSync(`/opt/mqtt_client/cert/${this.config.mqtt.mqttHost}.key`);
        this.clientCrt = fs.readFileSync(`/opt/mqtt_client/cert/${this.config.mqtt.mqttHost}.crt`);
        this.caCrt = fs.readFileSync('/opt/mqtt_client/cert/ca.crt');

        /* set log level */
        logger.setLevel(this.config.app.mqtt_log_level);
    }

    init() {
        const mqttOptions = {
            username: this.config.mqtt.mqttHost,
            clientId: this.config.mqtt.mqttHost,
            host: this.config.mqtt.host,
            port: this.config.mqtt.port,
            protocol: 'mqtts',
            ca: this.caCrt,
            key: this.key,
            cert: this.clientCrt,
            keepAlive: this.config.mqtt.keepAlive,
            rejectUnauthorized: false
        }

        const onConnectBind = this._onConnect.bind(this);
        const onDisconnectBind = this._onDisconnect.bind(this);
        const onMessageBind = this._onMessage.bind(this);

        this.mqttc = mqtt.connect(mqttOptions);
        this.mqttc.on("connect", onConnectBind);
        this.mqttc.on("disconnect", onDisconnectBind);
        this.mqttc.on("message", onMessageBind);

    }

    _onConnect() {
        this.isConnected = true;
        logger.info(`Client Connected successfully!`, TAG)

        if (this.agentMessenger == null) {
            this.agentMessenger = new AgentMessenger(this.config);
            this.agentMessenger.init(this.mqttc);
        }

    }

    _onDisconnect() {
        this.isConnected = false;
        this.mqttc.reconnect();
    }

    _onMessage(topic, message) {

        try {
            const jsonPayload = JSON.parse(message)
            const generatedData = Utils.generatePayload(topic, jsonPayload)
            const username = `${generatedData.metadata.tenant}:${generatedData.metadata.deviceid}`
            this.agentMessenger.updateAttrs(generatedData.metadata.deviceid, generatedData.metadata.tenant, generatedData.attrs, generatedData.metadata, username);
            logger.info(`Message published to ${username}`, TAG)
        } catch (error) {
            logger.error(`Error : ${error}`, TAG)
        }
    }

}


module.exports = MQTTClient