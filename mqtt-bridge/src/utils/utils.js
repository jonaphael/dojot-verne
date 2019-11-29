const moment = require('moment')

const generatePayload = (topic, payload) => {

    const username = topic.split('/')[0];
    const splitUsername = username.split(':');

    const tenant = splitUsername[0];
    const deviceId = splitUsername[1];

    return {
        metadata: {
            deviceid: deviceId,
            tenant: tenant,
            timestamp: moment().unix()
        },
        attrs: payload
    }
}

module.exports = { generatePayload }