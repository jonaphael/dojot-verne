"""
Tests for the certification module.
"""
import shutil
import os
import unittest
from unittest.mock import MagicMock
import requests
from OpenSSL import crypto

import src.ejbca.certificate as certificate
from src.utils import Utils

# TODO
class TestCertificate(unittest.TestCase):
    """
    Certificate class tests.
    """
    def setUp(self):
        self.pem_key = "-----BEGIN CERTIFICATE-----\nfakePemKey\n-----END CERTIFICATE-----\n"
        self.pem_csr = "-----BEGIN CERTIFICATE-----\nfakePemCsr\n-----END CERTIFICATE-----\n"
        self.pem_crt = "-----BEGIN CERTIFICATE-----\nfakePemCrt\n-----END CERTIFICATE-----\n"

        self.cert = MagicMock(
            c_name="admin:123",
            key={
                "raw": "fakeRawKey",
                "pem": ""
            },
            csr={
                "raw": "fakeRawCsr",
                "pem": ""
            },
            crt={
                "raw": "fakeRawCrt",
                "pem": ""
            }
        )

    def test_save_crt(self):
        """
        Should create the PEM CRT certificate.
        """
        certificate.Certificate = self.cert

        crt = certificate.Certificate.save_crt()

        correct_crt = "-----BEGIN CERTIFICATE-----\n"\
                + self.cert.crt["pem"]\
                + "\n-----END CERTIFICATE-----\n"

        assert crt == correct_crt







class TestCertClient(unittest.TestCase):
    """
    CertClient class tests.
    """

    mock_thing = MagicMock(thing_certificate="test_cert")

    # get_private_key_file() #
    @classmethod
    def test_get_private_key_file(cls):
        """
        Should build a correct filename for the key.
        """
        device_id = "testID"
        filename = device_id + ".key"
        assert filename == certificate.CertClient.get_private_key_file(device_id)

    def test_get_private_key_file_empty_id(self):
        """
        Should not build a key's filename with an empty ID.
        """
        device_id = ""
        with self.assertRaises(ValueError):
            certificate.CertClient.get_private_key_file(device_id)


    # get_certificate_file() #
    @classmethod
    def test_get_certificate_file(cls):
        """
        Should build a correct filename for the certificate.
        """
        device_id = "testID"
        filename = device_id + ".crt"
        assert filename == certificate.CertClient.get_certificate_file(device_id)

    def test_get_certificate_file_empty_id(self):
        """
        Should not build a certificate's filename with an empty ID.
        """
        device_id = ""
        with self.assertRaises(ValueError):
            certificate.CertClient.get_certificate_file(device_id)


    # create_cert_files() #
    def test_create_cert_files(self):
        """
        Should write to the files.
        """
        # Creating a temporary folder to hold the certificate files
        temp_dir = "tests/tmp/"
        cert_dir = temp_dir + "cert/"
        os.makedirs(cert_dir, exist_ok=True)

        certificate.Certificate = MagicMock()
        thing = certificate.Thing("admin", "123")
        certificate.CertClient.create_cert_files(thing, directory=cert_dir)

        assert os.path.exists(cert_dir + thing.device_id + ".key")
        assert os.path.exists(cert_dir + thing.thing_certificate + ".crt")

        # Removing all the temporary files
        shutil.rmtree(temp_dir)


    # new_cert() #
    @classmethod
    def test_new_cert(cls):
        """
        Should build a new Thing instance.
        """
        thing = certificate.CertClient.new_cert("admin", "123")
        assert thing is not None

    def test_new_cert_invalid(self):
        """
        Should not build a new Thing instance with invalid arguments.
        """
        with self.assertRaises(ValueError):
            certificate.CertClient.new_cert("admin", "")

        with self.assertRaises(ValueError):
            certificate.CertClient.new_cert("", "123")


    # revoke_cert() #
    def test_revoke_cert(self):
        """
        Should revoke the certificate.
        """
        requests.delete = MagicMock()
        crypto.load_certificate = MagicMock()
        certificate.CertClient.revoke_cert(self.mock_thing)

        assert crypto.load_certificate.called
        assert requests.delete.called


    # revoke_cert() #
    def test_has_been_revoked(self):
        """
        Should check that the certificate was successfully revoked.
        """
        response: requests.Response = MagicMock()
        response.json = MagicMock(return_value={"status": {"return": {"reason": 0}}})
        requests.get = MagicMock(return_value=response)
        crypto.load_certificate = MagicMock()

        assert certificate.CertClient.has_been_revoked(self.mock_thing)

        assert crypto.load_certificate.called
        assert requests.get.called

    def test_has_not_been_revoked(self):
        """
        Should check that the certificate was not successfully revoked.
        """
        response = MagicMock()
        response.json = MagicMock(return_value={"status": {"return": {"reason": 1}}})
        requests.get = MagicMock(return_value=response)
        crypto.load_certificate = MagicMock()

        assert not certificate.CertClient.has_been_revoked(self.mock_thing)

        assert crypto.load_certificate.called
        assert requests.get.called

    def test_has_been_revoked_invalid_response(self):
        """
        Should check that the response from EJBCA is incorrect.
        """
        # Empty response
        response = MagicMock()
        response.json = MagicMock(return_value={})
        requests.get = MagicMock(return_value=response)
        crypto.load_certificate = MagicMock()

        assert not certificate.CertClient.has_been_revoked(self.mock_thing)

        assert crypto.load_certificate.called
        assert requests.get.called

        # Response with empty status
        response.json = MagicMock(return_value={"status":{}})
        requests.get.reset_mock()
        crypto.load_certificate.reset_mock()

        assert not certificate.CertClient.has_been_revoked(self.mock_thing)

        assert crypto.load_certificate.called
        assert requests.get.called

        # Response with empty status.return
        response.json = MagicMock(return_value={"status":{"return":{}}})
        requests.get.reset_mock()
        crypto.load_certificate.reset_mock()

        assert not certificate.CertClient.has_been_revoked(self.mock_thing)

        assert crypto.load_certificate.called
        assert requests.get.called


class TestThingConstructor(unittest.TestCase):
    """
    Thing class constructor tests.
    """

    @classmethod
    def test_success(cls):
        """
        Should build a correct Thing instance.
        """
        certificate.Certificate = MagicMock()

        tenant = "admin"
        device_id = "123"
        thing_id = Utils.create_thing_id(tenant, device_id)

        thing = certificate.Thing(tenant, device_id)

        assert thing.tenant is not None
        assert thing.device_id is not None
        assert thing.thing_id is not None
        assert thing.cert is not None
        assert thing.private_key is not None
        assert thing.thing_certificate is not None

        assert thing.tenant == tenant
        assert thing.device_id == device_id
        assert thing.thing_id == thing_id

    def test_failure(self):
        """
        Should not build a Thing instance with invalid arguments.
        """
        certificate.Certificate = MagicMock()

        tenant = ""
        device_id = "123"

        with self.assertRaises(ValueError):
            _thing = certificate.Thing(tenant, device_id)

        tenant = "admin"
        device_id = ""

        with self.assertRaises(ValueError):
            _thing = certificate.Thing(tenant, device_id)


class TestThingMethods(unittest.TestCase):
    """
    Thing class methods tests.
    """
    tenant = "admin"
    device_id = "123"

    certificate.Certificate = MagicMock()

    def setUp(self):
        self.thing = certificate.Thing(self.tenant, self.device_id)

    def tearDown(self):
        self.thing = None

    def test_renew_cert(self):
        """
        Should call the renew_cert from Certificate class.
        """
        self.thing.renew_cert()
        assert self.thing.cert.renew_cert.called

    def test_get_args_in_dict(self):
        """
        Should correctly return the arguments in a dictionary.
        """
        args_dict = self.thing.get_args_in_dict()
        correct_dict = {
            "thing_id": self.thing.thing_id,
            "private_key": self.thing.private_key,
            "thing_certificate": self.thing.thing_certificate
        }

        assert args_dict == correct_dict


if __name__ == "__main__":
    unittest.main()
