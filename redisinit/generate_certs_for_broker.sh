#!/bin/sh

certCAName='IOTmidCA'
certEjbcaApiUrl="http://10.50.11.150:5583"
certDns='localhost'
certCaFile='ca.crt'

N_BROKER=$1
password="dojot"
keyLength=2048

_saveFormattedCRT()
{
  nameFile=$1
  rawCRT=$2

  echo "Saving CRT ${nameFile}"

  (echo  "-----BEGIN CERTIFICATE-----"
  echo ${rawCRT}
  echo "-----END CERTIFICATE-----"  ) > cert/tempfile.crt

  openssl x509 -inform pem -in cert/tempfile.crt -out cert/${nameFile}

}
##Generate key par (private and public key)
_generateKeyPair()
{
  local certKeyFile="$1.key"
  echo "Generating KeyPar in cert/${certKeyFile}"
  openssl genrsa -out  cert/${certKeyFile} ${keyLength}
  chmod +x cert/${certKeyFile}
}

##Create CSR (cert wih some infos and sign with private key )
_createCSR()
{
  local certKeyFile="$1.key"
  local certCname="$1"
  local certCsrFile="$1.csr"

  echo "Create CSR for ${certCname}"
  openssl req -new  -sha256 -out cert/${certCsrFile} -key cert/${certKeyFile} \
            --subj "/CN=${certCname}"
}

##create entity in ejbca
_createEntity()
{
    local certCname="$1"
    echo "Create Entity ${certCname} in ${certCAName} : ${certEjbcaApiUrl}/user"
    CREATE_USER_CA_STATUS=$(curl --silent -X POST ${certEjbcaApiUrl}/user \
    -H "Content-Type:application/json" \
    -H "Accept:application/json" \
    -d  "{\"username\": \"${certCname}\"}")
}

##sign csr in ejbca
_signCert()
{
    local certCname="$1"
    local certCertFile="$1.crt"

    echo "Signing cert for entity ${certCname} in ${certCAName} : ${certEjbcaApiUrl}/sign/${certCname}/pkcs10 "
    csrContent=`cat cert/${certCname}.csr  | sed '1,1d;$ d' | tr -d '\r\n'`

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
  
  local N_BROKER=${1:-"1"}

  for i in $(seq 1 $N_BROKER); do
    _generateKeyPair "vernemq-k8s-$(expr $i - 1)"
    _createCSR "vernemq-k8s-$(expr $i - 1)"
    _createEntity "vernemq-k8s-$(expr $i - 1)"
    _signCert "vernemq-k8s-$(expr $i - 1)"
  done
}

generateCertificates $N_BROKER

#retrieveCACertificate


