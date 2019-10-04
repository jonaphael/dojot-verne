#!/bin/bash

print_usage() {
	echo "usage: $0 -h HOST -i INTERVAL -n NUMBER -p PORT"
	echo "-h: IP to send MQTT messages"
	echo "-i: interval between each message"
	echo "-n: number of messages to send"
	echo "-p: port"
}

# Number of required arguments
nargs=8

host=""
interval=1
n_msgs=1
port=""

echo $#

# Check whether there are the correct number of variables
if [[ $# -ne $nargs ]]
then
	print_usage
	exit
fi

while getopts 'h:i:n:p:' flag
do
	case "${flag}" in
		h) host=${OPTARG} ;;
		i) interval=${OPTARG} ;;
		n) n_msgs=${OPTARG} ;;
		p) port=${OPTARG} ;;
		*) print_usage
		   exit 1 ;;
	esac
done

# Sending n_msgs messages
for i in `seq 1 $n_msgs`
do
	mosquitto_pub -h $host -p $port -t /admin/123abc/device-data -m '{"temperature": -10.8, "location": "-22.817457, -47.073856"}' -u admin:123abc &
	echo "Message $i"
	sleep $interval
done

