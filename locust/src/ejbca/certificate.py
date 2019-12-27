"""
Certificate-related classes.
"""
import logging
import json
import requests
from OpenSSL import crypto

from src.config import CONFIG
from src.utils import Utils

LOGGER = logging.getLogger('urllib3')
LOGGER.setLevel(logging.CRITICAL)


class Certificate:
    """
    Generates a certificate and private key for a device.
    """

    def __init__(self, thing_id):
        Utils.validate_thing_id(thing_id)

        self.c_name = thing_id
        self.key = {"raw": "", "pem": self.generate_private_key()}
        self.csr = {"raw": "", "pem": self.generate_csr()}
        self.crt = {"raw": "", "pem": ""}

        self.create_ejbca_user()
        self.sign_cert()
        self.crt["pem"] = self.save_crt()

    def save_crt(self) -> str:
        """
        Generate the CRT in a string.
        """
        return ("-----BEGIN CERTIFICATE-----\n"
                + self.crt["raw"]
                + "\n-----END CERTIFICATE-----\n")

    def generate_private_key(self) -> str:
        """
        Generates the private key in a string.
        """
        try:
            bit_len = 2048
            key = crypto.PKey()
            key.generate_key(crypto.TYPE_RSA, bit_len)
            return crypto.dump_privatekey(crypto.FILETYPE_PEM, key).decode("utf-8")
        except Exception as exception:
            logging.error("Error while generating the private key.")
            logging.error("Error: %s", exception)
            return ""

    def generate_csr(self) -> str:
        """
        Generates the CSR in a string.
        """
        # based on https://github.com/cjcotton/python-csr

        dnsname = CONFIG['security']['dns_cert']
        ipaddr = []

        ss = []
        for i in dnsname:
            ss.append("DNS: %s" % i)
        for i in ipaddr:
            ss.append("IP: %s" % i)
        ss = ", ".join(ss)


        req = crypto.X509Req()
        req.get_subject().CN = self.c_name

        # Add in extensions

        base_constraints = ([
            crypto.X509Extension(b"keyUsage",
                                 False,
                                 b"Digital Signature, Non Repudiation, Key Encipherment"),
            crypto.X509Extension(b"basicConstraints",
                                 False,
                                 b"CA:FALSE"),
        ])

        x509_extensions = base_constraints

        if ss:
            san_constraint = crypto.X509Extension(b"subjectAltName", False, ss.encode("utf-8"))
            x509_extensions.append(san_constraint)

        req.add_extensions(x509_extensions)

        key = crypto.load_privatekey(crypto.FILETYPE_PEM, self.key["pem"])

        req.set_pubkey(key)
        req.sign(key, "sha256")

        return crypto.dump_certificate_request(crypto.FILETYPE_PEM, req).decode("ascii")


    def create_ejbca_user(self) -> None:
        """
        Makes a requisition to EJBCA to create a user.
        """
        # create the ejbca user
        req = json.dumps({
            "username": self.c_name
        })

        default_header = {'content-type': 'application/json',
                          'Accept': 'application/json'}

        response = requests.post(CONFIG['security']['ejbca_url'] + "/user",
                                 headers=default_header,
                                 data=req)

        if response.status_code != 200:
            logging.error("Error while creating the EJBCA user")

        response.connection.close()

    def sign_cert(self) -> None:
        """
        Sign the certificates.
        """
        csr = self.csr["pem"]
        cut_down_clr = (csr[csr.find('-----BEGIN CERTIFICATE REQUEST-----')
                            + len('-----BEGIN CERTIFICATE REQUEST-----'):
                            csr.find("-----END CERTIFICATE REQUEST-----")]
                        .replace("\n", ""))

        req = json.dumps({
            "passwd": "dojot",
            "certificate": cut_down_clr
        })

        default_header = {'content-type': 'application/json',
                          'Accept': 'application/json'}
        url = CONFIG['security']['ejbca_url'] + "/sign/" + self.c_name + "/pkcs10"
        response = requests.post(url,
                                 headers=default_header, data=req)

        if response.status_code == 200:
            self.crt["raw"] = json.loads(response.content)['status']['data']
        else:
            logging.error("Error while signing certificate")

        response.connection.close()

    def renew_cert(self) -> None:
        """
        Renew a certificate.
        """
        self.sign_cert()
        self.crt["pem"] = self.save_crt()


class Thing():
    """
    Device certification data high-level class.
    """
    def __init__(self, tenant: str, device_id: str):
        thing_id = Utils.create_thing_id(tenant, device_id)

        self.tenant = tenant
        self.device_id = device_id
        self.cert = Certificate(thing_id)
        self.thing_id = thing_id
        self.private_key = self.cert.key["pem"]
        self.thing_certificate = self.cert.crt["pem"]

    def renew_cert(self) -> None:
        """
        Renew a certificate.
        """
        self.cert.renew_cert()

    def get_args_in_dict(self) -> dict:
        """
        Returns the ID, private key and certificate in a dict.
        """
        return {
            "thing_id": self.thing_id,
            "private_key": self.private_key,
            "thing_certificate": self.thing_certificate
        }


class CertClient:
    """
    Handles certificate-related operations.
    """

    @staticmethod
    def get_private_key_file(device_id: str) -> str:
        """
        Creates the key filename.
        """
        Utils.validate_device_id(device_id)
        return "{0}.key".format(device_id)

    @staticmethod
    def get_certificate_file(device_id: str) -> str:
        """
        Creates the certificate filename.
        """
        Utils.validate_device_id(device_id)
        return "{0}.crt".format(device_id)

    @staticmethod
    def create_cert_files(thing: Thing, directory: str = "/cert/") -> None:
        """Creates the .key and .crt files for a device.

        Args:
            device_id: device's identification.
            thing: Thing object with certificate's info.
            directory: directory to save the files.
        """
        thing_path = directory + thing.device_id

        try:
            with open(thing_path + ".key", "w") as key_file:
                key_file.write(str(thing.private_key))

            with open(thing_path + ".crt", "w") as key_file:
                key_file.write(str(thing.thing_certificate))

        except Exception as exception:
            logging.error("Error: %s", str(exception))

    @staticmethod
    def new_cert(tenant: str, device_id: str) -> Thing:
        """
        Creates/renovates the certificate for a device.
        """
        Utils.validate_tenant(tenant)
        Utils.validate_device_id(device_id)
        thing = Thing(tenant, device_id)

        return thing

    @staticmethod
    def revoke_cert(thing: Thing) -> None:
        """
        Revokes a certificate for a specific device.
        """
        # Loads the certificate as a X509 object
        cert: crypto.X509 = crypto.load_certificate(
            crypto.FILETYPE_PEM, thing.thing_certificate)
        # Retrieves the Serial Number in Hexadecimal
        serial_number = hex(cert.get_serial_number())[2:]
        # URL to revoke a certificate
        url = CONFIG["security"]["ejbca_url"] + \
            "/ca/CN={0},O=EJBCA/certificate/{1}".format(
                CONFIG["security"]["ejbca_ca_name"], serial_number)

        requests.delete(url)

    @staticmethod
    def has_been_revoked(thing: Thing) -> bool:
        """
        Verifies whether the certificate has been revoked or not.
        """
        # Loads the certificate as a X509 object
        cert: crypto.X509 = crypto.load_certificate(
            crypto.FILETYPE_PEM, thing.thing_certificate)
        # Retrieves the Serial Number in Hexadecimal
        serial_number = hex(cert.get_serial_number())[2:]
        # URL to verify the certificate status
        url = CONFIG["security"]["ejbca_url"] + \
            "/ca/CN={0},O=EJBCA/certificate/{1}/status".format(
                CONFIG["security"]["ejbca_ca_name"], serial_number)

        res = requests.get(url)
        res_json = res.json()

        if res_json.get("status") and res_json["status"].get("return"):
            return res_json["status"]["return"]["reason"] == 0
        else:
            logging.error("Error: invalid response from EJBCA")
            return False
