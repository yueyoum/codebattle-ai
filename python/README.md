1.  git clone & cd codebattle-ai folder
2.  git submodule init
3.  git submodule update
4.  protoc --python_out=python/ -Iproto proto/*.proto
5.  cd python
6.  start python shell

    ```python
    from ai import AI
    ai = AI(IP, PORT, ROOMID, COLOR)
    ai.run()
    ```

#### NOTE

第四步是要将proto文件编译成*_pb2.py文件。
要求你已经安装 google protobuf 的编译器 protoc

AI的初始化参数

*   IP - 服务器IP，默认为106.186.21.40
*   PORT - 服务器的AI接入PORT， 默认为11012
*   ROOMID - 用GUI Client建立房间后得到的room id
*   Color - 如果没有这个参数，那么默认为红色。

            可选有 "red", "yellow", "blue", "green", "cyan"