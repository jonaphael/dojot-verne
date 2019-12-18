"""
EJBCA certificate creation.
"""
import logging
import json
import requests
from OpenSSL import crypto

from src.config import CONFIG

LOGGER = logging.getLogger('urllib3')
LOGGER.setLevel(logging.CRITICAL)

class Certificate:
    """
    Generates a certificate and private key for a device.
    """

    def __init__(self, thing_id):
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


    def create_ejbca_user(self, _certificate_profile_name="CFREE",
                          _end_entity_profile_name="EMPTY_CFREE") -> None:
        """
        Makes a requisition to EJBCA to create a user.
        """
        ca_name = CONFIG['security']['ejbca_ca_name']
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
