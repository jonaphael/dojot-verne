/**
 * Unit test for app file
 * 
 * This module has the following dependencies
 * 
 * - mqtt
 * - fs
 */

// mocks
jest.mock('fs')

const mockConfig = {
    Messenger: {
        init : jest.fn(() => Promise.resolve() ),
        createChannel: jest.fn(),
        on: jest.fn( (_subject, _msg, callback) => callback())
    },

    mqtt: {
        on: jest.fn(),
        publish: jest.fn(),
        reconnect: jest.fn()
    }
}

jest.mock('mqtt', () => ({ connect: jest.fn(() => mockConfig.mqtt) }))
jest.mock('@jonaphael/dojot-module', () => ({ Messenger: jest.fn(() => mockConfig.Messenger )}))

import App from '../app'

describe("initialize app", () => {
    
    beforeEach(() => {
        jest.clearAllMocks()
    })

    test("create new app instance", () => {

        const app = new App();
        expect(app.initialized).toBe(false);
     })

    it("should init the app", async () => {
        const app = new App();
        await app.initApp();

        expect(app.initialized).toBe(true)
        expect(mockConfig.Messenger.init).toHaveBeenCalled()
        expect(mockConfig.Messenger.createChannel).toHaveBeenCalled()
        expect(mockConfig.Messenger.on).toHaveBeenCalled()
        expect(mockConfig.mqtt.on).toHaveBeenCalledTimes(2)
     })

    it("should not initialize app", async () => {

        mockConfig.Messenger.init.mockReturnValue(Promise.reject())

        const app = new App();
        await app.initApp();
        expect(app.initialized).toBe(false)
        expect(mockConfig.Messenger.init).toHaveBeenCalled()
        expect(mockConfig.Messenger.createChannel).not.toHaveBeenCalled()
        expect(mockConfig.Messenger.on).not.toHaveBeenCalled()
    })

    it("should execute messenger callback", async () => {
        
        mockConfig.Messenger =  {
            init : jest.fn(() => Promise.resolve() ),
            createChannel: jest.fn(),
            on: jest.fn( (_subject, _msg, callback) => callback('abc', `{}`, {}))
        }
        
        const app = new App();
        const publilshMessageSpy = jest.spyOn(app, 'publishMessage')

        await app.initApp();
        expect(publilshMessageSpy).toBeCalled()

    })

    it("should connect to mqtt", async () => {
         mockConfig.mqtt = {
             on: jest.fn((_event, callback) => callback()),
             publish: jest.fn(),
             reconnect: jest.fn()
         }

         const app = new App(); 
         await app.initApp();
         expect(mockConfig.mqtt.on).toHaveBeenCalledTimes(2)
    })

    it("should publish a message", async () => {
        const app = new App();

        mockConfig.mqtt = {
            on: jest.fn((_event, callback) => callback()),
            publish: jest.fn((_topic, _message) => jest.fn()),
            reconnect: jest.fn()
        }

        await app.initApp();
        app.publishMessage('test-topic', 'my-message')
        
        expect(mockConfig.mqtt.publish).toHaveBeenCalledWith('test-topic', 'my-message')
    })

    it("should not publish a message cause mqqt-client is not connected", async () => {

        const app = new App();
        app.publishMessage('test-topic', 'my-message')
        expect(mockConfig.mqtt.publish).not.toHaveBeenCalled()
    })

     it("should stop app", () => {
        const app = new App();
        app.stopApp();
        expect(app.initialized).toBe(false)
     })
 })