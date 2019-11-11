import redis
import uuid
import concurrent.futures
import conf
import logging
import time
from thing import Thing

NUM_OF_INSERT_BY_THREAD = 100
NUM_OF_THREAD = 10
BATCH_SIZE = 100

#r = redis.Redis(host = conf.redis_host, port = conf.redis_port, db = 0)
#pipe = r.pipeline()

def register_thing(name):
    start = time.time()
    r = redis.Redis(host = conf.redis_host, port = conf.redis_port, db = 0)
    pipe = r.pipeline()
    start_batch_time = start
    for i in range(NUM_OF_INSERT_BY_THREAD):

        if i % BATCH_SIZE == 0:
            end_batch_time = time.time()
            diff = end_batch_time - start_batch_time
            start_batch_time = end_batch_time
            logging.info("Execution time: {} secs by Thread {} with batch {}".format(diff, name, i))

        thing_id = str(uuid.uuid4().hex)
        obj = Thing.Create_Thing("admin:" + thing_id)
        #r.hmset(thing_id,obj)
        pipe.hmset(thing_id,obj)
    pipe.execute()
    end = time.time()

    logging.info(f"Thread {name} finishing (s)" + str(end - start))

if __name__ == "__main__":
    format = "%(asctime)s: %(message)s"
    logging.basicConfig(format=format, level=logging.DEBUG, datefmt="%H:%M:%S")
    start = time.time()
    pool = concurrent.futures.ThreadPoolExecutor(NUM_OF_THREAD) #ThreadPoolExecutor #ProcessPoolExecutor
    myfuturelist = [pool.submit(register_thing, x) for x in range(NUM_OF_THREAD)]

    concurrent.futures.wait(myfuturelist)
    end = time.time()
    #pipe.execute()
    logging.info(f"Total inserts " + str(NUM_OF_INSERT_BY_THREAD * NUM_OF_THREAD)+ " in (s) " + str(end - start) + " thr: " + str(NUM_OF_THREAD) + " loop: "+ str(NUM_OF_INSERT_BY_THREAD) )


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
