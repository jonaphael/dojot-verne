#!/bin/sh

# mosquitto_pub -h localhost -p 1883 -t admin/attrs -m '{"Temperatura": 100}'  -u admin

#########################################################
### Required Packages: openssl, curl, jq
### Expected environment variables, example:
: '
export CERT_EJBCA_API_BROKER = 'ejbca_simple'
export CERT_EJBCA_URL= '5583'
export STATIC_CERT='n'
export K8S_ENV = 'n'
export HOSTNAME = 'vernemq-k8s-0'
'
#########################################################


uuid=$(uuidgen)
CERT_CNAME="${HOSTNAME:-"vernemq"}"
CERT_EJBCA_API_BROKER=${CERT_EJBCA_API_BROKER:-"ejbca_simple"}
CERT_EJBCA_API_PORT=${CERT_EJBCA_API_PORT:-"5583"}
CERT_EJBCA_URL="http://${CERT_EJBCA_API_BROKER}"
CERT_DNS="$HOSTNAME"
CERT_CA_FILE='ca.crt'
CERT_CERT_FILE="$HOSTNAME.crt"
CERT_KEY_FILE="$HOSTNAME.key"
CERT_CSR_FILE="$HOSTNAME.csr"
CERT_CANAME='IOTmidCA'
IS_K8S_ENV=${K8S_ENV:-"y"}

certCAName=$CERT_CANAME
certEjbcaApiUrl="${CERT_EJBCA_URL}:${CERT_EJBCA_API_PORT}"
certCname=$CERT_CNAME
certDns=$CERT_DNS
certCaFile=$CERT_CA_FILE
certCertFile=$CERT_CERT_FILE
certKeyFile=$CERT_KEY_FILE
certCsrFile=$CERT_CSR_FILE

USE_STATIC_CERTS=${STATIC_CERT:-"n"}
CERT_DIRECTORY="cert"

keyLength=2048
password="dojot"

_removeCRTDir()
{
  if [ -d "$CERT_DIRECTORY" ];
  then
    rm -rf cert
  fi
}

_createCRTDir()
{
  mkdir cert
  cd cert
}

_connectEJBCA()
{
  # Waiting for dojot MQTT broker for at most 3 minutes
  START_TIME=$(date +'%s')
  echo "Waiting for dojot EJBCA Broker fully start. Host '${CERT_EJBCA_API_BROKER}', '${CERT_EJBCA_API_PORT}'..."
  echo "Try to connect to dojot EJBCA Broker ... "
  RESPONSE=`curl --fail -s ${certEjbcaApiUrl}/ejbca/version || echo ""`
  echo $RESPONSE
  while [ -z "${RESPONSE}" ]; do
      sleep 30
      echo "Retry to connect to dojot EJBCA broker ... "
      RESPONSE=`curl --fail -s ${certEjbcaApiUrl}/ejbca/version || echo ""`

      ELAPSED_TIME=$(($(date +'%s') - ${START_TIME}))
      if [ ${ELAPSED_TIME} -gt 180 ]
      then
          echo "dojot EJBCA broker is taking too long to fully start. Exiting!"
          exit 3
      fi
  done
  echo "dojot EJBCA broker at host '${CERT_EJBCA_API_BROKER}', port '${CERT_EJBCA_API_PORT}' fully started."

  # give time for EJBCA fully started
  sleep 5
}

_removeUnused()
{
  rm tempfile.crt
  rm cert.csr
}

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
  openssl req -new  -sha256 -out ${certCsrFile} -key ${certKeyFile} \
            --subj "/CN=${certCname}"
}

##create entity in ejbca
_createEntity()
{
    echo "Create Entity ${certCname} in ${certCAName} : ${certEjbcaApiUrl}/user"
    CREATE_USER_CA_STATUS=$(curl --silent -X POST ${certEjbcaApiUrl}/user \
    -H "Content-Type:application/json" \
    -H "Accept:application/json" \
    -d  "{\"username\": \"${certCname}\"}")
}

##sign csr in ejbca
_signCert()
{
    echo "Signing cert for entity ${certCname} in ${certCAName} : ${certEjbcaApiUrl}/sign/${certCname}/pkcs10 "
    csrContent=`cat cert.csr  | sed '1,1d;$ d' | tr -d '\r\n'`

    signCertCa=$(curl --silent -X POST ${certEjbcaApiUrl}/sign/${certCname}/pkcs10 \
    -H "Content-Type:application/json" \
    -H "Accept:application/json" \
    -d  "{
    \"passwd\": \"${password}\",
    \"certificate\": \"${csrContent}\"
    }" | jq '.status.data' -r)

    _saveFormattedCRT "${certCertFile}" "${signCertCa}"
}

##Get from PKI the CA certificate and return in PEM format
_retrieveCACertificate()
{
    echo "Retrieve cert for  ${certCAName} CA : ${certEjbcaApiUrl}/ca/${certCAName}"
    certCa=$(curl --silent -X GET ${certEjbcaApiUrl}/ca/${certCAName} \
    -H "Content-Type:application/json" \
    -H "Accept:application/json" | jq '.certificate' -r )

    _saveFormattedCRT "${certCaFile}" "${certCa}"
}

##Generate private key and sign certificate crt
_generateCertificates()
{
    _createCRTDir
    _generateKeyPair
    _createCSR
    _createEntity
    _signCert

}

main()
{
  if [ "${USE_STATIC_CERTS}" = "n" ]
  then
    ## remove static cert dir
    _removeCRTDir

    ## Try to connect to EJBCA first
    _connectEJBCA

    ## generate the certs from EJBCA
    _generateCertificates

    ## retrieve to host
    _retrieveCACertificate

    ## remove unused certs
    _removeUnused
  else
    echo "Using static certificates... checking if exists.."
    if [ -d "$CERT_DIRECTORY" ];
    then
      echo "checking if certs is correctly installed.."
      if [ -e "cert/$HOSTNAME.crt" -a -e "cert/ca.crt" ]
      then
        echo "All ok!"
      else
        echo "Certs are not correctly installed.. closing application.."
        exit 3
      fi
    else
      echo "certs dir not exists, closing application.."
      exit 3
    fi
  fi
  if [ "${IS_K8S_ENV}" = "n" ]
  then
    start_vernemq
  fi
}


########################
    # MAIN             #
########################
main
