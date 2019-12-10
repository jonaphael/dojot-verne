
const ExpressApp = require("./express/App");;

jest.mock('@dojot/dojot-module-logger');
jest.mock("body-parser");
jest.mock("express");

jest.mock("./Routes");
jest.mock("./../../Config.js");

//const expressApp = new ExpressApp();
//expressApp.init();

describe("Testing Metrics", () => {

    beforeEach(() => {
        jest.clearAllMocks();
    });

    it("Check instance ", () => {
    });

});