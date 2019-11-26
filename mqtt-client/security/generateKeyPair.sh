#!/bin/bash

#########################################################

#########################################################

BASE_DIR=${BASE_DIR:-"/opt/mqtt_client"}

. ${BASE_DIR}/security/initVariables.sh


echo "Generating KeyPar in ${certDir}/${certKeyFile}"
openssl genrsa -out ${certDir}/${certKeyFile} ${keyLength}
chmod +x ${certDir}/${certKeyFile}