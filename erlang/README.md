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

**NOTE**

项目用rebar来构件

AI的初始化参数

*   IP - 服务器IP，默认为106.186.21.40
*   PORT - 服务器的AI接入PORT， 默认为11012
*   ROOMID - 用GUI Client建立房间后得到的room id
*   Color - 如果没有这个参数，那么默认为红色。

            可选有 "red", "yellow", "blue", "green", "cyan"