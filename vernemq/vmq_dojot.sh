#!/bin/sh

#########################################################
### Required Packages: openssl, curl, jq
### Expected environment variables, example:
: '
export CERT_EJBCA_API_BROKER='localhost'
export CERT_EJBCA_API_PORT='5583'
export STATIC_CERT='n'
export K8S_ENV='n'
export HOSTNAME='broker'
export CRL_UPDATE_TIME='*/30 * * * *'
export BASE_DIR='/vernemq'
export CHECKEND_EXPIRATION_SEC='43200'
export CHECK_EXPIRATION_TIME='*/30 * * * *'
export CHECK_BROKER_CERT_REVOKED_TIME='*/30 * * * *'
'
#########################################################


########################################################

BASE_DIR=${BASE_DIR:-"/vernemq"}

. ${BASE_DIR}/scripts_tls/_initVariables.sh


_removeCRTDir()
{
  if [ -d "$certDir" ];
  then
    rm -rf ${certDir}
  fi
}

_createCRTDir()
{
  mkdir ${certDir}
  cd ${certDir}
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

##Generate key par (private and public key)
_generateKeyPair()
{
    sh ${BASE_DIR}/scripts_tls/generateKeyPair.sh
}

##Create CSR (cert wih some infos and sign with private key )
_createCSR()
{
    sh ${BASE_DIR}/scripts_tls/createCSR.sh
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
    sh ${BASE_DIR}/scripts_tls/signCert.sh
}

##Get from PKI the CA certificate and return in PEM format
_retrieveCACertificate()
{
    sh ${BASE_DIR}/scripts_tls/retrieveCACertificate.sh
}

##Get from PKI the CRL certificate
_retrieveCRLCertificate()
{
    sh ${BASE_DIR}/scripts_tls/retrieveCRL.sh
}

_cronTabCRL()
{
    echo "$CRL_UPDATE_TIME   ${BASE_DIR}/scripts_tls/retrieveCRL.sh" >> ${BASE_DIR}/crontab.tab
}

_cronTabExpiration()
{
    echo "$CHECK_EXPIRATION_TIME  ${BASE_DIR}/scripts_tls/checkExpirationCertificate.sh" >> ${BASE_DIR}/crontab.tab
}

_cronTabCheckBrokerCertRevoke()
{
    echo "$CHECK_BROKER_CERT_REVOKED_TIME  ${BASE_DIR}/scripts_tls/checkBrokerCertHasRevoke.sh" >> ${BASE_DIR}/crontab.tab
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

_startCronService()
{
   supercronic  ${BASE_DIR}/crontab.tab &
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

    ## retrieve crl
    _retrieveCRLCertificate


    #verifies certificate chains.
    . ${BASE_DIR}/scripts_tls/checkCertificateChain.sh


    ## create cron file
    touch ${BASE_DIR}/crontab.tab

    ## create cron tab to update CRL
    _cronTabCRL

    ## create cron tab to check Expiration
    _cronTabExpiration

    ## create cron tab to check Revoke
    _cronTabCheckBrokerCertRevoke

    _startCronService


  else
    echo "Using static certificates... checking if exists.."
    if [ -d "${BASE_DIR}/$CERT_DIRECTORY" ];
    then
      echo "checking if certs is correctly installed.."
      if [ -e "${BASE_DIR}/cert/$HOSTNAME.crt" -a -e "${BASE_DIR}/cert/ca.crt" ]
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

