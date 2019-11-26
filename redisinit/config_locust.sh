#!/bin/bash
# Sends the certificates and Redis dump to Locust machines

MASTER_HOST=$1
SLAVE_HOSTS=$2
HOSTS_PASSWORD=$3

if [ -z $3 ]
then
  echo "usage: $0 master_host slave_hosts hosts_password"
  echo "example: $0 1.2.3.4 '1.1.1.2 1.1.1.3 1.1.1.4' password"
  exit 1
fi

echo "Removing compressed files..."
rm cert.tar.gz
echo

echo "Compressing certs..."
tar -czf cert.tar.gz cert/
echo

echo "Removing Redis dump..."
sshpass -p $HOSTS_PASSWORD ssh cpqd@$MASTER_HOST -t sudo rm /home/cpqd/dojot-verne/locust/db/dump.rdb
echo

echo "Sending Redis dump to Locust master..."
sshpass -p $HOSTS_PASSWORD scp dump.rdb cpqd@$MASTER_HOST:/home/cpqd/dojot-verne/locust
sshpass -p $HOSTS_PASSWORD ssh cpqd@$MASTER_HOST -t 'cd /home/cpqd/dojot-verne/locust/ ; sudo mv dump.rdb db/dump.rdb'
echo

echo "Sending certificates and keys to Locust master..."
sshpass -p $HOSTS_PASSWORD ssh cpqd@$MASTER_HOST -t 'rm -rf /home/cpqd/dojot-verne/locust/cert'
sshpass -p $HOSTS_PASSWORD scp cert.tar.gz cpqd@$MASTER_HOST:/home/cpqd/dojot-verne/locust
sshpass -p $HOSTS_PASSWORD ssh cpqd@$MASTER_HOST -t 'cd /home/cpqd/dojot-verne/locust/ ; tar -xzf cert.tar.gz'
echo

for host in $SLAVE_HOSTS
do
  echo "====================================="
  echo "Configuring slave $host"

  echo "Removing cert dir..."
  sshpass -p $HOSTS_PASSWORD ssh cpqd@$host -t 'rm -rf /home/cpqd/dojot-verne/locust/cert'
  echo

  echo "Removing TAR file..."
  sshpass -p $HOSTS_PASSWORD ssh cpqd@$host -t 'rm /home/cpqd/dojot-verne/locust/cert.tar.gz'
  echo

  echo "Sending data to Locust slave..."
  sshpass -p $HOSTS_PASSWORD scp cert.tar.gz cpqd@$host:/home/cpqd/dojot-verne/locust
  echo

  echo "Extracting TAR file..."
  sshpass -p $HOSTS_PASSWORD ssh cpqd@$host -t 'cd /home/cpqd/dojot-verne/locust/ ; tar -xzf cert.tar.gz'
  echo
done
