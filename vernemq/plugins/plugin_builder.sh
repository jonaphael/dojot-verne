#!/bin/bash

# run the erlang container
docker run -itd --name erlan_int1 --rm dojot/erlang_for_build_plugin_verne:16012020

#transfer all plugins dir to container
for i in */; 
do 
  [[ -e $i ]] || break
  docker cp "${i%%/}" erlan_int1:/"${i%%/}";
  docker exec -it erlan_int1 bash -c "cd ${i%%/}; ./rebar3 compile"; 
  docker cp erlan_int1:/"${i%%/}" .;

done


# stop the erlang container
docker stop erlan_int1