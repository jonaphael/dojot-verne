/**
 * Unit test for app file
 *
 * This module has the following dependencies
 *
 * - mqtt
 * - fs
 * - @jonaphael/iotagent-nodejs
 */


const mqtt = require('mqtt');
const App = require('../app');

const fakeArg = (arg1) => ({ meta: { service: arg1 }, data: { id: arg1 } });

/* dependencies mock */
const mockConfig = {
  Messenger: {
    updateAttrs: jest.fn(),
    init: jest.fn(),
    on: jest.fn((arg0, arg1, callback) => callback(arg0, fakeArg(arg1))),
  },
  kafkaConfig: {
    test: 'testMock',
    mesenger: {
      kafka: {
        dojot: {
          subject: {
            verne: 'verne',
          },
        },
      },
    },
  },
};

const mockMqtt = {
  on: jest.fn(),
  reconnect: jest.fn(),
};

jest.mock('mqtt', () => ({
  connect: jest.fn(() => mockMqtt),
  disconnect: jest.fn(),
}));

const mockDefaultConfig = {
  mqtt: {
    host: 'mqtt-host',
    port: 0,
  },
  app: {
    mqtt_log_level: 'debug',
    enable_dojot: true,
  },
  toBoolean: (val) => val,
};


jest.mock('fs');
jest.mock('../../utils/utils');
jest.mock('@dojot/dojot-module-logger');
jest.mock('@dojot//iotagent-nodejs', () => ({
  IoTAgent: jest.fn(() => mockConfig.Messenger),
}));

let mockedApp;

describe('Testing app', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('Should initialize the app correctly', () => {
    mockedApp = new App();
    expect(mockedApp.options).toBeDefined();
  });

  it('Should crash initializing the app correctly', async () => {
    mockConfig.Messenger.init.mockReturnValue(Promise.reject());
    mockedApp = new App();
    try {
      await mockedApp.initApp();
    } catch (error) {
      expect(mockConfig.Messenger.init).toHaveBeenCalled();
    }
  });

  it('should initialize agent correctly for dojot', async () => {
    mockConfig.Messenger.init.mockReturnValue(Promise.resolve());
    mockedApp = new App(mockDefaultConfig);
    await mockedApp.initApp();
  });

  it('should initialize agent correctly for 100k', async () => {
    mockConfig.Messenger.init.mockReturnValue(Promise.resolve());

    mockedApp = new App();

    // -- should not parse json
    await mockedApp.initApp();

    const onMock = jest.fn((arg0, _arg1, callback) => {
      const arg1Modified = JSON.stringify({ attrs: 'attrs ' });
      callback(arg0, arg1Modified, { key: 'key' });
    });
    mockConfig.Messenger.on.mockImplementation(onMock);
    //-------------------------------------------------------
    await mockedApp.initApp();
    expect(mockConfig.Messenger.on).toHaveBeenCalled();
    //-------------------------------------------------------
  });

  it('should publish a message', async () => {
    mockConfig.Messenger.init.mockReturnValue(Promise.resolve());
    mockedApp = new App();
    mockedApp.mqttOnConnect();
    await mockedApp.initApp();

    // -- connect
    expect(mqtt.connect).toHaveBeenCalled();

    mockedApp.mqttOnDisconnect();
    expect(mockMqtt.reconnect).toHaveBeenCalled();
  });
});
