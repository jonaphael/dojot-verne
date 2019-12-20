"""
Certificate creation for Locust.
"""

import argparse
import concurrent.futures
import logging
import time
import uuid
import shutil
import sys
import ssl
import os
import redis
import requests

from src.ejbca.thing import Thing
from src.config import CONFIG


def generate_certs(ids: list) -> None:
    """
    Generates the certificates for the IDs.

    Args:
        ids: list of IDs.
    """
    start_time = time.time()

    redis_conn = connect_to_redis()

    for device_id in ids:
        thing = Thing(f"{ARGS.tenant}:{device_id}")
        redis_conn.hmset(device_id, thing.get_args_in_dict())

    end_time = time.time()
    logging.info("Generated %d IDs in %f", len(ids), end_time - start_time)
    redis_conn.close()

def generate_random_certs() -> None:
    """
    Runs threads to generate the certificates.
    """

    start = time.time()
    pool = concurrent.futures.ThreadPoolExecutor(ARGS.threads)
    workload = calculate_thread_load()
    future_list = [pool.submit(register_thing, str(i), workload[i]) for i in range(ARGS.threads)]
    done, _ = concurrent.futures.wait(future_list, return_when=concurrent.futures.FIRST_EXCEPTION)

    for future in done:
        if future.exception() is not None:
            logging.error(str(future.exception()))
            sys.exit(1)

    logging.info(
        "Total inserts %i in %is using %i threads",
        ARGS.cert,
        (time.time() - start),
        ARGS.threads
    )

def register_thing(name: str, n_certs: int) -> None:
    """
    Creates devices and exports them to a Redis database.

    Args:
        name: the thread name.
        n_certs: number of certificates to generate.
    """
    start_time = time.time()

    redis_conn = connect_to_redis()

    pipe = redis_conn.pipeline()
    start_batch_time = start_time
    for i in range(n_certs):

        if i % ARGS.batch == 0:
            end_batch_time = time.time()
            diff = end_batch_time - start_batch_time
            start_batch_time = end_batch_time
            logging.info("Execution time: %f secs by Thread %s with batch %s", diff, name, i)

        thing_id = str(uuid.uuid4().hex)
        thing = Thing(f"{CONFIG['app']['tenant']}:{thing_id}")
        pipe.hmset(thing_id, thing.get_args_in_dict())

    pipe.execute()

    end_time = time.time()
    redis_conn.close()
    logging.info("Thread %s finished in %fs", name, end_time - start_time)

def export_certs() -> None:
    """
    Creates the .crt and .key files from the generated certificates.
    """

    redis_conn = connect_to_redis()

    os.makedirs(CONFIG["security"]["cert_dir"], exist_ok=True)

    # Exporting .key files
    try:
        logging.info("Creating key files...")
        for key in redis_conn.scan_iter(count=2):
            private_key = (redis_conn.hmget(name=key, keys="private_key"))
            filename = f"{CONFIG['security']['cert_dir']}/{key.decode('utf-8')}.key"

            with open(filename, "w") as key_file:
                key_file.write((private_key[0]).decode("utf-8"))

    except Exception as exception:
        logging.error("Error while saving key files: %s", str(exception))
        sys.exit(1)
    else:
        logging.info("Successfully created!")


    # Exporting .crt files
    try:
        logging.info("Creating certificate files...")
        for key in redis_conn.scan_iter(count=2):
            thing_certificate = (redis_conn.hmget(name=key, keys="thing_certificate"))
            filename = f"{CONFIG['security']['cert_dir']}/{key.decode('utf-8')}.crt"

            with open(filename, "w") as crt_file:
                crt_file.write((thing_certificate[0]).decode("utf-8"))

    except Exception as exception:
        logging.error("Error while saving key files: %s", str(exception))
        sys.exit(1)
    else:
        logging.info("Successfully created!")

    redis_conn.close()

def retrieve_ca_cert() -> None:
    """
    Retrieves the CA certificate and exports to a file.
    """

    url = f"{CONFIG['security']['ejbca_url']}/ca/{CONFIG['security']['ejbca_ca_name']}"

    res = requests.get(url).json()

    if res["certificate"] is None:
        logging.error("Error while retrieving the CA certificate.")
        sys.exit(1)

    certificate = "-----BEGIN CERTIFICATE-----\n" +\
                  res["certificate"] +\
                  "\n-----END CERTIFICATE-----\n"

    filename = f"{CONFIG['security']['cert_dir']}{CONFIG['security']['ca_cert_file']}"

    with open(filename, "w") as ca_file:
        ca_file.write(certificate)

def connect_to_redis() -> redis.Redis:
    """
    Connects to Redis.
    """
    try:
        redis_conn = redis.Redis(
            host=CONFIG["locust"]["redis"]["host"],
            port=CONFIG["locust"]["redis"]["port"]
        )
        redis_conn.ping()
        return redis_conn

    except Exception as exception:
        logging.error("Error while connecting to Redis.")
        sys.exit(1)

def calculate_thread_load() -> list:
    """
    Calculates the threads' workloads by dividing them equally between each one.

    Returns: list with the workload of each thread.
    """
    per_thread = ARGS.cert // ARGS.threads
    exceeding = ARGS.cert % ARGS.threads

    workload = [per_thread for _ in range(ARGS.threads)]

    # Distribute the exceeding between the first threads
    for i in range(exceeding):
        workload[i] = workload[i] + 1

    return workload


if __name__ == "__main__":
    # Building the argument parser
    PARSER = argparse.ArgumentParser()
    PARSER.add_argument(
        "--threads",
        metavar="N",
        help=f"number of threads to generate the random certificates, defaults to the number of\
        cores in your machine, which is {os.cpu_count()}",
        type=int,
        default=os.cpu_count()
    )
    PARSER.add_argument(
        "--batch",
        metavar="N",
        help="prints the time spent to generate 'batch_size' certificates each time this number\
            of certificates is generated, defaults to 100",
        type=int,
        default=100
    )
    PARSER.add_argument(
        "--remove",
        help="activates the remotion of the certificates directory before the generation",
        action="store_true",
        default=False
    )

    MEGROUP = PARSER.add_mutually_exclusive_group()
    MEGROUP.add_argument(
        "--cert",
        metavar="N",
        help="number of certificates to be generated",
        type=int
    )
    MEGROUP.add_argument(
        "--ids",
        metavar="ID",
        help="list of IDs to generate the certificates",
        type=str,
        nargs="+"
    )
    ARGS = PARSER.parse_args()

    logging.basicConfig(
        format=CONFIG["app"]["log_format"],
        level=logging.DEBUG,
        datefmt="%H:%M:%S")

    if ARGS.remove and os.path.exists(CONFIG['security']['cert_dir']):
        logging.info("Removing certificates directory...")
        shutil.rmtree(CONFIG['security']['cert_dir'])
        logging.info("... Removed certificates directory")

    if ARGS.cert is not None:
        if ARGS.threads > ARGS.cert:
            logging.error("The number of certificates to be generated must be greather than \
the number of threads!")
            sys.exit(1)

        generate_random_certs()

    if ARGS.ids is not None:
        # Begins the certificate generation
        generate_certs(ARGS.ids)

    # Exports the certificates' files
    export_certs()
    # Retrieving the CA certificate
    retrieve_ca_cert()


#Total inserts 10000 in (s) 1161.1937227249146 thr: 1000 loop: 10 - inseridos +/- 9700
#Total inserts 10000 in (s) 618.5327394008636 thr: 100 loop: 100


# sudo docker run --name ejbca_simple --hostname localhost -p 5583:5583 -d muhamedavila/simple_ejbca_dojot
# docker inspect ejbca_simple
# http://ip:5583
# docker logs -f ejbca_simpl

# sudo docker run --name my-redis-container -d redis
# docker inspect my-redis-container
# ip:6379
# docker container exec --user="root" -it  my-redis-container /bin/bash
# redis-cli
# keys *
# hmget 0143bc0d7fdb4a3399c2dc8a33d52995 thing_certificate
# hmget 0dcab854443243c38bf5406bd7ee03de private_key
