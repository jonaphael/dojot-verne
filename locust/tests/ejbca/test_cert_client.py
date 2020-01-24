"""
    Test CertClient module
"""
import unittest
from mock import patch, MagicMock, mock_open, call
from src.ejbca.cert_client import CertClient

MOCK_CONFIG = {
    'security': {
        'ejbca_url': 'url',
        'ejbca_ca_name': 'name'
    }
}


class TestCertClient(unittest.TestCase):
    """
    CertClient class tests.
    """
    @patch('src.ejbca.cert_client.Utils')
    def test_get_private_key_file(self, mock_utils):
        """
            Should build a correct filename for the key.
        """
        mock_utils.validate_device_id.return_value = MagicMock()
        device_id = "dev-id"
        filename = device_id + ".key"
        self.assertEqual(CertClient.get_private_key_file(device_id), filename)
        mock_utils.reset_mock()

    @patch('src.ejbca.cert_client.Utils')
    def test_get_certificate_file(self, mock_utils):
        """
            Should build a correct filename for the certificate.
        """
        mock_utils.validate_device_id.return_value = MagicMock()
        device_id = "dev-id"
        filename = device_id + ".crt"
        self.assertEqual(CertClient.get_certificate_file(device_id), filename)
        mock_utils.reset_mock()

    @patch('src.ejbca.cert_client.Thing')
    def test_create_cert_files(self, mock_thing):
        """
            Should build a correct filename for the certificate.
        """
        mock_thing.device_id = 'thind-dev-id'
        mock_thing.thing_certificate = 'thing-certificate'
        mock_thing.private_key = 'private-key'
        directory = 'dir-to-create-certs/'
        thing_path = directory + mock_thing.device_id
        with patch('src.ejbca.cert_client.open', mock_open()) as mock_builtin_open:
            CertClient.create_cert_files(mock_thing, directory)

            print(mock_builtin_open.mock_calls)
            calls = [call(thing_path + '.key', "w"),
                     call(thing_path + '.crt', "w"),
                     call().write(mock_thing.thing_certificate),
                     call().write(mock_thing.private_key)]
            mock_builtin_open.assert_has_calls(calls, any_order=True)
            mock_builtin_open.reset_mock()
        mock_thing.reset_mock()

    @patch('src.ejbca.cert_client.Thing')
    def test_create_cert_files_exception(self, mock_thing):
        """
            Should not build a correct filename for the certificate.
        """
        mock_thing.device_id = 'thind-dev-id'
        mock_thing.thing_certificate = 'thing-certificate'
        mock_thing.private_key = 'private-key'
        directory = 'dir-to-create-certs/'
        with patch('src.ejbca.cert_client.open', mock_open()) as mock_builtin_open:
            mock_builtin_open.side_effect = Exception()

            with self.assertRaises(Exception):
                CertClient.create_cert_files(mock_thing, directory)

    @patch('src.ejbca.cert_client.Thing')
    @patch('src.ejbca.cert_client.Utils')
    def test_new_cert(self, mock_utils, mock_thing):
        """
            Test certifcate new cert creation
        """
        tenant = 'tenant'
        dev_id = 'dev-id'
        created_thing = CertClient.new_cert(tenant, dev_id)
        self.assertIsNotNone(created_thing)
        mock_utils.validate_tenant.assert_called()
        mock_utils.validate_device_id.assert_called()
        mock_thing.assert_called_with(tenant, dev_id)
        mock_thing.reset_mock()

    @staticmethod
    @patch('src.ejbca.cert_client.requests')
    @patch('src.ejbca.cert_client.crypto')
    @patch('src.ejbca.cert_client.Thing')
    @patch.dict('src.ejbca.certificate.CONFIG', MOCK_CONFIG)
    def test_revoke_cert(mock_thing, mock_crypto, mock_request):
        """
            Test certifcate revoke cert
        """
        mock_crypto.load_certificate.return_value = MagicMock()
        mock_crypto.load_certificate().get_serial_number.return_value = 0x000111222333
        serial_number = hex(
            mock_crypto.load_certificate().get_serial_number())[2:]

        url = MOCK_CONFIG["security"]["ejbca_url"] + "/ca/CN={0},O=EJBCA/certificate/{1}".format(
            MOCK_CONFIG["security"]["ejbca_ca_name"], serial_number)

        CertClient.revoke_cert(mock_thing)
        mock_request.delete.assert_called_with(url)
        mock_thing.reset_mock()
        mock_crypto.reset_mock()
        mock_request.reset_mock()

    @patch('src.ejbca.cert_client.requests')
    @patch('src.ejbca.cert_client.crypto')
    @patch('src.ejbca.cert_client.Thing')
    @patch.dict('src.ejbca.certificate.CONFIG', MOCK_CONFIG)
    def test_has_been_revoked_true(self, mock_thing, mock_crypto, mock_request):
        mock_crypto.load_certificate.return_value = MagicMock()
        mock_crypto.load_certificate().get_serial_number.return_value = 0x000111222333
        serial_number = hex(
            mock_crypto.load_certificate().get_serial_number())[2:]

        url = "{0}/ca/CN={1},O=EJBCA/certificate/{2}/status".format(
            MOCK_CONFIG["security"]["ejbca_url"],
            MOCK_CONFIG["security"]["ejbca_ca_name"],
            serial_number)

        mock_request.get.return_value = MagicMock()
        mock_request.get().json().get.return_value = MagicMock()
        mock_request.get().json.return_value = {
            'status': {
                'return': {
                    'reason': 0
                },
            },
        }

        result = CertClient.has_been_revoked(mock_thing)
        mock_request.get.assert_called_with(url)
        self.assertTrue(result)

        # print(100 * '6', mock_request.mock_calls, 100 * '6')
        mock_request.get().json.return_value = {}

        result = CertClient.has_been_revoked(mock_thing)
        print(result)
        self.assertFalse(result)


if __name__ == "__main__":
    unittest.main()
