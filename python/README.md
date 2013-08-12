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
