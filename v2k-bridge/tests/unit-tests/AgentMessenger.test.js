jest.mock('@jonaphael/dojot-module');
jest.mock('@dojot/dojot-module-logger');
jest.mock("../../src/utils/utils")

const utils = require('../../src/utils/utils');
const AgentMessenger = require("../../src/AgentMessenger");

async function expectConfigs() {
    mockConfig.Messenger.init.mockReturnValue(Promise.resolve());


    await mockedMessenger.init(mockConfig.mqttConfig);
    expect(mockConfig.Messenger.init).toHaveBeenCalled();
    expect(mockConfig.Messenger.on).toHaveBeenCalled();
    expect(mockConfig.Messenger.generateDeviceCreateEventForActiveDevices).toHaveBeenCalled();
    expect(mockConfig.mqttConfig.subscribe).toHaveBeenCalled();
}

/* MOCKS */
const mockConfig = {
    Messenger: {
        updateAttrs: jest.fn(),
        init: jest.fn(),
        on: jest.fn((arg0, arg1, callback) => callback('tenant', 'device')),
        generateDeviceCreateEventForActiveDevices: jest.fn()
    },

    kafkaConfig: {
        test: "testmock",
        messenger: {
            kafka: {
                dojot: {
                    subjects: {
                        verne: 'verne'
                    }
                }
            }
        },
        mqtt: {
            subscribeTopic: "topic"
        },
        app: {
            mqtt_log_level: "debug"
        }
    },

    mqttConfig: {
        client: "test",
        subscribe: jest.fn()
    }

};

jest.mock("@jonaphael/iotagent-nodejs", () => ({
    IoTAgent: jest.fn(() => mockConfig.Messenger),
}));

let mockedMessenger;

describe("Testing AgentMessenger messenger", () => {

    beforeEach(() => {
        jest.clearAllMocks();
    });

    it("Should init correctly the agent messenger with config and publish data", () => {
        mockedMessenger = new AgentMessenger(mockConfig.kafkaConfig);
        expectConfigs();

    })

    it("Should init correctly the agent messenger without config", () => {
        mockedMessenger = new AgentMessenger();

        expectConfigs();
    })

    it("Should not init correctly the agent messenger", async () => {
        let reason = 'error';
        mockedMessenger = new AgentMessenger(mockConfig.kafkaConfig);
        mockConfig.Messenger.init.mockReturnValue(Promise.reject(reason));

        try {
            await mockedMessenger.init(mockConfig.mqttConfig);
        } catch (error) {
            expect(error).toEqual(reason);
        }

    })

    it("Should send message after onMessage", () => {
        mockedMessenger = new AgentMessenger(mockConfig.kafkaConfig);
        expectConfigs();

        let generateDataFake = {
            metadata: {
                tenant: 'fake',
                deviceid: 'fake'
            }
        }
        utils.generatePayload = jest.fn().mockReturnValue(generateDataFake)

        let fakeMessage = '{ "name":"John", "age":30, "city":"New York"}'
        mockedMessenger.sendMessage("test", fakeMessage);
        expect(mockConfig.Messenger.updateAttrs).toHaveBeenCalled();

    })


    it("Should not send message after onMessage", () => {
        mockedMessenger = new AgentMessenger(mockConfig.kafkaConfig);
        expectConfigs();

        let generateDataFake = {
            metadata: {
                tenant: 'fake',
                deviceid: 'fake'
            }
        }
        utils.generatePayload = jest.fn().mockReturnValue(generateDataFake)
        let fakeMessage = 'error format'

        try {
            mockedMessenger.sendMessage("test", fakeMessage);
        } catch (error) {
            expect(error).toBeDefined();

        }


    })

})