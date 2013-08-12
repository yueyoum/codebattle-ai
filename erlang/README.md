1.  git clone & cd codebattle-ai folder
2.  git submodule init
3.  git submodule update
4.  cp proto/*.proto erlang/src
5.  cd erlang
6.  rebar get-deps
7.  rebar compile
8.  erl -pa ebin -pa deps/protobuffs/ebin

    ```erlang
    ai:start_link(IP, PORT, ROOMID, COLOR).
    ```
