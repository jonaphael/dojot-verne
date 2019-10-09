#!/bin/sh

#########################################################
### Required Packages: openssl, curl, jq
### Expected environment variables, example:
: '
export CERT_CANAME='IOTmidCA'
export CERT_EJBCA_API_URL='http://localhost:8000'
export CERT_CNAME='vernemq_2'
export CERT_DNS='localhost'
export CERT_CA_FILE='ca.crt'
export CERT_CERT_FILE='cert.crt'
export CERT_KEY_FILE='cert.key'
'
#########################################################

certCAName=$CERT_CANAME
certEjbcaApiUrl=$CERT_EJBCA_API_URL
certCname=$CERT_CNAME
certDns=$CERT_DNS
certCaFile=$CERT_CA_FILE
certCertFile=$CERT_CERT_FILE
certKeyFile=$CERT_KEY_FILE

keyLength=2048
password=$(openssl rand -hex 3)

_saveFormattedCRT()
{
  nameFile=$1
  rawCRT=$2

  echo "Saving CRT ${nameFile}"

  (echo  "-----BEGIN CERTIFICATE-----"
  echo ${rawCRT}
  echo "-----END CERTIFICATE-----"  ) > tempfile.crt

  openssl x509 -inform pem -in tempfile.crt -out ${nameFile}

}
##Generate key par (private and public key)
_generateKeyPair()
{
  echo "Generating KeyPar in ${certKeyFile}"
  openssl genrsa -out  ${certKeyFile} ${keyLength}
  chmod +x ${certKeyFile}
}

##Create CSR (cert wih some infos and sign with private key )
_createCSR()
{
  echo "Create CSR for ${certCname}"
  openssl req -new  -sha256 -out ${certCname}.csr -key ${certKeyFile} \
            -addext "subjectAltName = DNS:${certDns}" \
	        -addext "keyUsage = Digital Signature, Non Repudiation, Key Encipherment" \
            -addext "basicConstraints  =  CA:FALSE" \
            --subj "/CN=${certCname}"
}

##create entity in ejbca
_createEntity()
{
    echo "Create Entity ${certCname} in ${certCAName} : ${certEjbcaApiUrl}/user"
    CREATE_USER_CA_STATUS=$(curl --silent -X POST ${certEjbcaApiUrl}/user \
    -H "Content-Type:application/json" \
    -H "Accept:application/json" \
    -d  "{
        \"caName\": \"${certCAName}\",
        \"certificateProfileName\": \"CFREE\",
        \"endEntityProfileName\": \"EMPTY_CFREE\",
        \"clearPwd\": true,
        \"keyRecoverable\": false,
        \"password\": \"${password}\",
        \"sendNotification\": false,
        \"status\": 10,
        \"subjectDN\": \"CN=${certCname}\",
        \"tokenType\": \"USERGENERATED\",
        \"username\": \"${certCname}\"
    }" | jq '.status')
}

##sign csr in ejbca
_signCert()
{
    echo "Signing cert for entity ${certCname} in ${certCAName} : ${certEjbcaApiUrl}/sign/${certCname}/pkcs10 "
    csrContent=`cat ${certCname}.csr  | sed '1,1d;$ d' | tr -d '\r\n'`

    signCertCa=$(curl --silent -X POST ${certEjbcaApiUrl}/sign/${certCname}/pkcs10 \
    -H "Content-Type:application/json" \
    -H "Accept:application/json" \
    -d  "{
    \"passwd\": \"${password}\",
    \"certificate\": \"${csrContent}\"
    }"  | jq '.status.data' -r )

    _saveFormattedCRT "${certCertFile}" "${signCertCa}"
}

##Get from PKI the CA certificate and return in PEM format
retrieveCACertificate()
{
    echo "Retrieve cert for  ${certCAName} CA : ${certEjbcaApiUrl}/ca/${certCAName}"
    certCa=$(curl --silent -X GET ${certEjbcaApiUrl}/ca/${certCAName} \
    -H "Content-Type:application/json" \
    -H "Accept:application/json" | jq '.certificate' -r )

    _saveFormattedCRT "${certCaFile}" "${certCa}"
}

##Generate private key and sign certificate crt
generateCertificates()
{
    _generateKeyPair
    _createCSR
    _createEntity
    _signCert
}

generateCertificates

retrieveCACertificate


