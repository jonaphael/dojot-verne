#!/bin/bash

#########################################################

#########################################################

BASE_DIR=${BASE_DIR:-"/opt/k2v-bridge"}

. ${BASE_DIR}/security/initVariables.sh


echo "Generating KeyPar in ${certDir}/${certKeyFile}"
openssl genrsa -out ${certDir}/${certKeyFile} ${keyLength}
chmod +x ${certDir}/${certKeyFile}