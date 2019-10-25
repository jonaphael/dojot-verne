#!/bin/bash

#########################################################

#########################################################

BASE_DIR=${BASE_DIR:-"/vernemq"}

. ${BASE_DIR}/scripts_tls/_initVariables.sh

if openssl x509 -checkend ${CHECKEND_EXPIRATION_SEC} -noout -in ${certDir}/${certCaFile} &&
   openssl x509 -checkend ${CHECKEND_EXPIRATION_SEC} -noout -in ${certDir}/${certCertFile}
then
  echo "Certificate is good for another day!"
else
  echo "Certificate has expired or will do so within 24 hours!"
  echo "(or is invalid/not found)"
  echo "Renew:"
  . ${BASE_DIR}/vmq_dojot.sh
fi





