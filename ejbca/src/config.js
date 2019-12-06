const soap = {
    wsdlAddr: process.env.EJBCA_WSDL_ADDR || 'https://localhost:8443/ejbca/ejbcaws/ejbcaws?wsdl',
    caCrt: process.env.EJBCA_CA_CRT_DIR || '/opt/p12/ca.crt',
    clientP12: process.env.EJBCA_VA_P12_DIR || '/opt/p12/soap_client.p12',
    clientPass: process.env.EJBCA_PASS || 'secret'
};

const messenger = {
    auth: {
        connectionRetries: 5,
        timeoutSleep: 5,
        url: process.env.AUTH_URL || "http://auth:5000",
    },
    databroker: {
        connectionRetries: process.env.DATA_BROKER_CONN_RETRIES || 10,
        timeoutSleep: 2,
        url: process.env.DATA_BROKER_URL || "http://data-broker",
    },
    dojot: {
        events: {
            tenantActionType: {
                CREATE: "create",
                DELETE: "delete",
            },
            tenantEvent: {
                DELETE_TENANT: "delete-tenant",
                NEW_TENANT: "new-tenant",
            },
        },
        management: {
            tenant: process.env.DOJOT_MANAGEMENT_USER || "dojot-management",
            user: process.env.DOJOT_MANAGEMENT_USER || "dojot-management",
        },
        subjects: {
            deviceData: process.env.DOJOT_SUBJECT_DEVICE_DATA || "device-data",
            devices: process.env.DOJOT_SUBJECT_DEVICES || "dojot.device-manager.device",
            tenancy: process.env.DOJOT_SUBJECT_TENANCY || "dojot.tenancy",
        },
    },
    kafka: {
        consumer: {
            "group.id": "vernemq-group",
            "metadata.broker.list": process.env.KAFKA_HOSTS || "kafka-server:9092",
        },
        dojot: {
            connectionRetries: 10,
            subjects: {
                verne: "vernemq-epic-channel",
            },
            timeoutSleep: 2,
        },
        producer: {
            "dr_cb": true,
            "metadata.broker.list": process.env.KAFKA_HOSTS || "kafka-server:9092",
            "socket.keepalive.enable": true,
        },
    },
};


const dojot = {
    ejbcaPort: process.env.EJBCA_PORT || 5583
}

module.exports = {
    soap,
    dojot,
    messenger
};