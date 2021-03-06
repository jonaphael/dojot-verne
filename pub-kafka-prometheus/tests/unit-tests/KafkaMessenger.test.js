const metrics = require('../../src/Metrics');
const KafkaMessenger = require("../../src/KafkaMessenger");

jest.mock('@jonaphael/dojot-module');
jest.mock('@dojot/dojot-module-logger');
jest.mock('../../Config.js');
jest.mock('../../src/Metrics');

const mockMessenger = {
    Messenger: {
        on: jest.fn(),
        createChannel: jest.fn(),
    },

};

jest.mock("@jonaphael/dojot-module", () => ({
    Messenger: jest.fn(() => mockMessenger.Messenger),
}));

jest.mock('../../Config.js', () => ({
    messenger: {
        kafka: {
            dojot: {
                subjects: {
                    verne: 'verne'
                }
            }
        }
    },
}));

describe("Testing Dojot Kafka messenger", () => {

    beforeEach(() => {
        jest.clearAllMocks();
        kafkaMessenger = new KafkaMessenger();
    });

    it("Should init correctly ", async () => {

        mockMessenger.Messenger.init = (jest.fn(() => Promise.resolve()));

        await kafkaMessenger.init();

        expect(mockMessenger.Messenger.createChannel).toHaveBeenCalled();
        expect(mockMessenger.Messenger.on).toHaveBeenCalled();
    });

    it("Should not init correctly ", async () => {

        mockMessenger.Messenger.init = (jest.fn(() => Promise.reject("Msg error")));

        await kafkaMessenger.init();

        expect(mockMessenger.Messenger.createChannel).not.toHaveBeenCalled();
        expect(mockMessenger.Messenger.on).not.toHaveBeenCalled();
    });

    it("kafkaOnMessage correctly", () => {

        kafkaMessenger.init();
        const timestamp = Date.now();

        const startSec = timestamp / 1000;

        const msg = {
            attrs: {
                timestamp: startSec
            },
        };

        const endTimeMS = timestamp + 150;
        const extraInfo = {
            timestamp: endTimeMS
        }

        metrics.addTime.mockResolvedValue();

        kafkaMessenger.kafkaOnMessage("admin", JSON.stringify(msg), extraInfo);

        expect(metrics.addTime).toHaveBeenCalledWith(150);

    });

    it("kafkaOnMessage not correctly", () => {

        kafkaMessenger.init();

        metrics.addTime.mockResolvedValue();

        try {
            kafkaMessenger.kafkaOnMessage("admin", {}, {});
        } catch{
            expect(metrics.addTime).not.toHaveBeenCalledWith();
        }

    });

});