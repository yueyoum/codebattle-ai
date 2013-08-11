import time
import struct
from functools import wraps

import gevent
from gevent import socket

import api_pb2
from marine_pb2 import Run, Flares, GunAttack 


class Sdk(object):
    def __init__(self, ip, port, roomid, color="red"):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect((ip, port))

        self.roomid = roomid
        self.color = color

        self.last_receive_time = time.time()
        self.started = False

    def receive_checker(self):
        while True:
            gevent.sleep(2)
            if self.started and time.time() - self.last_receive_time > 5:
                self.flares()

    def sock_recv(self):
        head_t = struct.Struct('>i')
        while True:
            data = self.sock.recv(4)
            if not data:
                print "data_len empty, exit"
                break

            self.last_receive_time = time.time()
            length = head_t.unpack(data)
            length = length[0]
            data = self.sock.recv(length)

            msg = api_pb2.Message()
            msg.ParseFromString(data)

            if msg.msg == api_pb2.cmdresponse:
                if msg.response.ret != 0:
                    print 'cmdresponse, ret = ', msg.response.ret
                else:
                    self.cmdresponse(msg.response)
            elif msg.msg == api_pb2.senceupdate:
                self.senceupdate(msg.update)
            elif msg.msg == api_pb2.startbattle:
                self.started = True
                self.startbattle()
            elif msg.msg == api_pb2.endbattle:
                self.endbattle(msg.endbattle)


    def send(self, data):
        self.sock.sendall(data)


    def cmdresponse(self, data):
        raise NotImplemented()

    def senceupdate(self, data):
        raise NotImplemented()

    def startbattle(self):
        raise NotImplemented()

    def endbattle(self, data):
        raise NotImplemented()


    def flares(self):
        # make flares to get other marine's state
        raise NotImplemented()

    def run(self):
        self.sock.sendall(cmd_joinroom(self.roomid, self.color))
        recv_job = gevent.spawn(self.sock_recv)
        recv_checker_job = gevent.spawn(self.receive_checker)
        gevent.joinall([recv_job, recv_checker_job])






def add_header_length(fun):
    @wraps(fun)
    def deco(*args, **kwargs):
        data = fun(*args, **kwargs)
        data_len = len(data)
        fmt = '>i%ds' % data_len
        data_st = struct.Struct(fmt)
        return data_st.pack(data_len, data)
    return deco



@add_header_length
def cmd_joinroom(roomid, color):
    cmd = api_pb2.Cmd()
    cmd.cmd = api_pb2.joinroom
    cmd.jrm.roomid = roomid
    cmd.jrm.color = color
    return cmd.SerializeToString()

@add_header_length
def cmd_run(marineid, x, z):
    cmd = api_pb2.Cmd()
    cmd.cmd = api_pb2.marineoperate
    cmd.opt.id = marineid
    cmd.opt.status = Run
    cmd.opt.targetPostion.x = x
    cmd.opt.targetPostion.z = z
    return cmd.SerializeToString()

@add_header_length
def cmd_flares(marineid):
    cmd = api_pb2.Cmd()
    cmd.cmd = api_pb2.marineoperate
    cmd.opt.id = marineid
    cmd.opt.status = Flares
    return cmd.SerializeToString()

@add_header_length
def cmd_gunattack(marineid, x, z):
    cmd = api_pb2.Cmd()
    cmd.cmd = api_pb2.marineoperate
    cmd.opt.id = marineid
    cmd.opt.status = GunAttack
    cmd.opt.targetPostion.x = x
    cmd.opt.targetPostion.z = z
    return cmd.SerializeToString()

def cmd_attack_and_run(marineid, ax, az, rx, rz):
    return cmd_gunattack(marineid, ax, az) + cmd_run(marineid, rx, rz)
