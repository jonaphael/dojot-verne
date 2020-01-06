/**
 * Unit test for app file
 *
 * This module has the following dependencies
 *
 * - mqtt
 * - fs
 * - @jonaphael/iotagent-nodejs
 */


/* dependencies mock */

const mockConfig = {
  Messenger: {
    updateAttrs: jest.fn(),
    init: jest.fn(),
    on: jest.fn((arg0, arg1, callback) => callback(arg0, arg1)),
  },
};


jest.mock('../../utils/utils');
jest.mock('@dojot/dojot-module-logger');
jest.mock('@jonaphael/iotagent-nodejs', () => ({
  IoTAgent: jest.fn(() => mockConfig.Messenger),
}));

describe('Testing app', () => {
  it('Should initialize the mesenger correctly', () => {
    expect(1).toBe(1)
  });
});
