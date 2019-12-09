const utils = require("../../src/utils/utils")

describe("Testing utils", () => {

    beforeEach(() => {
        jest.clearAllMocks();
    });

    it("Should generate correctly the payload", async () => {
        topic = "admin:deviceid/topic";
        payload = "data";
        const data = utils.generatePayload(topic, payload);

        let deviceid = data.metadata.deviceid;
        let tenant = data.metadata.tenant;
        let attrs = data.attrs;

        expect(deviceid).toEqual("deviceid");
        expect(tenant).toEqual("admin");
        expect(attrs).toEqual(payload);

    })
})