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

def map_device_ids() -> None:
    """
    Maps device IDs from certificate database in Redis to sequential keys in mapping database.

    The certificates database stores the certificates and private keys  by using a Hash Map.
    When testing with Locust, we will read the already created certificates from Redis database,
    but since we are using Hash Maps, the retrieval is not trivial because we need to know the
    device ID.

    The solution is to create another database sequentially mapping the device IDs (i.e., the
    key will be an integer from 0 to the number of devices and the value is the device ID), so
    we can retrieve them safely.
    """
    cert_db = connect_to_redis()
    mapped_db = connect_to_redis(database=CONFIG["locust"]["redis"]["mapped_db"])

    try:
        logging.info("Beginning database mapping...")

        keys = cert_db.keys()
        for i in range(len(keys)):
            mapped_db.set(i+1, keys[i])

    except Exception as exception:
        logging.error(str(exception))

    else:
        logging.info("Finished database mapping.")

    cert_db.close()
    mapped_db.close()

def restore_db_state() -> None:
    """
    Restores the values of variables in the Redis database.
    """
    redis_conn = connect_to_redis(CONFIG["locust"]["redis"]["mapped_db"])

    redis_conn.set("devices_to_revoke", 0)
    redis_conn.set("devices_to_renew", 0)
    redis_conn.set("device_count", 0)

    redis_conn.close()

def clear_db() -> None:
    """
    Removes all entries in Redis databases.
    """
    redis_conn = connect_to_redis()
    redis_conn.flushall()
    redis_conn.close()

def connect_to_redis(database=CONFIG["locust"]["redis"]["certificates_db"]) -> redis.Redis:
    """
    Connects to Redis.

    Args:
        database: the database to be connected.
    """
    try:
        redis_conn = redis.Redis(
            host=CONFIG["locust"]["redis"]["host"],
            port=CONFIG["locust"]["redis"]["port"],
            db=database
        )
        redis_conn.ping()
        return redis_conn

    except Exception as exception:
        logging.error("Error while connecting to Redis: %s", str(exception))
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
    PARSER.add_argument(
        "--map",
        help="activates the mapping of device IDs in Redis",
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
    MEGROUP.add_argument(
        "--onlymap",
        help="applies only the mapping of device IDs in Redis",
        action="store_true",
        default=False
    )
    MEGROUP.add_argument(
        "--restoredb",
        help="restore the Redis database to a fresh state,\
              without removing the certificates",
        action="store_true",
        default=False
    )
    MEGROUP.add_argument(
        "--cleardb",
        help="clears Redis, removing all certificates and mappings",
        action="store_true",
        default=False
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
            logging.error("The number of certificates must be greather than the number of threads!")
            sys.exit(1)

        # Begins the certificate generation
        generate_random_certs()
        # Exports the certificates' files
        export_certs()
        # Retrieving the CA certificate
        retrieve_ca_cert()

    if ARGS.ids is not None:
        # Begins the certificate generation
        generate_certs(ARGS.ids)
        # Exports the certificates' files
        export_certs()
        # Retrieving the CA certificate
        retrieve_ca_cert()

    if ARGS.map or ARGS.onlymap:
        map_device_ids()

    if ARGS.restoredb:
        restore_db_state()

    if ARGS.cleardb:
        clear_db()
