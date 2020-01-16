#!/bin/sh


BASE_DIR=${BASE_DIR:-"/vernemq"}
HOSTNAME="${HOSTNAME:-"broker"}"
CERT_CNAME="${HOSTNAME:-"broker"}"
CERT_EJBCA_API_BROKER=${CERT_EJBCA_API_BROKER:-"localhost"}
CERT_EJBCA_API_PORT=${CERT_EJBCA_API_PORT:-"5583"}

export CHECKEND_EXPIRATION_SEC="${CHECKEND_EXPIRATION_SEC:-43200}" #12h
export CERT_EJBCA_URL="http://${CERT_EJBCA_API_BROKER}"
export CERT_DNS="${CERT_DNS:-"localhost"}"
export CERT_CA_FILE='ca.crt'
export CERT_CRL_FILE='ca.crl'
export CERT_CERT_FILE="$HOSTNAME.crt"
export CERT_KEY_FILE="$HOSTNAME.key"
export CERT_CSR_FILE="$HOSTNAME.csr"
export CERT_CANAME='IOTmidCA'
#Read up on cron patterns here (http://crontab.org/)
#By default will be updated every 2 hours
export CRL_UPDATE_TIME="${CRL_UPDATE_TIME:-"0 */2 * * *"}"
#By default will be updated every day at 1 am
export CHECK_EXPIRATION_TIME="${CHECK_EXPIRATION_TIME:-"0 1 * * *"}"
#By default will be updated every 3 hours
export CHECK_BROKER_CERT_REVOKED_TIME="${CHECK_BROKER_CERT_REVOKED_TIME:-"0 */3 * * *"}"

# variables for internal use in scripts
export isK8sEnv=${K8S_ENV:-"n"}
export certCAName=$CERT_CANAME
export certEjbcaApiUrl="${CERT_EJBCA_URL}:${CERT_EJBCA_API_PORT}"
export certCname=$CERT_CNAME
export certDns=$CERT_DNS
export certCaFile=$CERT_CA_FILE
export certCertFile=$CERT_CERT_FILE
export certKeyFile=$CERT_KEY_FILE
export certCsrFile=$CERT_CSR_FILE
export certCrlFile=$CERT_CRL_FILE
export certDir="$BASE_DIR/cert"
export keyLength=2048
export password="dojot"

