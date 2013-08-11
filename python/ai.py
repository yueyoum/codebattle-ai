import time
import random

import gevent

from ai_sdk import Sdk, cmd_run, cmd_flares, cmd_gunattack, cmd_attack_and_run
import api_pb2
from marine_pb2 import Flares, GunAttack, Dead, Injured

class Marine(object):
    mid = 0
    hp = 0
    x = 0
    z = 0
    flares_amount = 0
    last_gun_time = 0


class AI(Sdk):
    mapx = 0
    mapz = 0
    own_marines = {}
    other_marines = {}

    def update_marine(self, _m):
        m = Marine()
        m.mid = _m.id
        m.hp = _m.hp
        m.x = _m.position.x
        m.z = _m.position.z
        m.flares_amount = _m.flaresAmount
        return m


    def cmdresponse(self, data):
        if data.cmd == api_pb2.joinroom:
            self.mapx = data.jrmResponse.size.x
            self.mapz = data.jrmResponse.size.z
            for _m in data.jrmResponse.marines:
                self.own_marines[_m.id] = self.update_marine(_m)

    def senceupdate(self, data):
        for m in data.own:
            # your own marine goes here
            if m.status == Dead:
                # this marine dead
                try:
                    self.own_marines.pop(m.id)
                except KeyError:
                    pass
            elif m.status == Flares:
                # this marine had flares just now, and after 1 seconds,
                # you will got all marine's state.
                # NOTE: when this marine do flares, Enemry AI will also know this marine's state
                pass

            if m.role == Injured:
                # this marine has been attacked! Be careful
                pass

        for m in data.others:
            # this are others marines
            self.other_marines[m.id] = self.update_marine(m)

            if m.status == Dead:
                try:
                    self.other_marines.pop(m.id)
                except KeyError:
                    pass

            elif m.status == Flares:
                # maybe you should run, due to others know your state
                pass

            elif m.status == GunAttack:
                # others make a gun shoot
                pass

        self.attack()


    def startbattle(self):
        self.flares()


    def endbattle(self, data):
        print "Endbattle, reason = {0}, win = {1}".format(data.reason, data.win)


    def choose_flares_id(self):
        ms = [m for m in self.own_marines.values() if m.flares_amount > 0]
        if not ms:
            return None
        ms.sort(key=lambda x: x.hp)
        return ms[0].mid


    def flares(self):
        flares_id = self.choose_flares_id()
        if flares_id:
            self.send( cmd_flares(flares_id) )
        else:
            print "No marine can make flares, make Random GunAttack"
            self.attack()

    def choose_attack_marine(self):
        ms = [m for m in self.own_marines.values() if time.time() - m.last_gun_time > 2]
        return ms

    def choose_attack_position(self):
        ms = [m for m in self.other_marines.values()]
        ms.sort(key=lambda x: x.hp)
        m = ms[0]
        return m.x, m.z

    def attack(self):
        tx, tz = self.choose_attack_position()
        for m in self.choose_attack_marine():
            m.last_gun_time = time.time()
            self.send( cmd_attack_and_run(m.mid, tx, tz, random.randint(0, self.mapx), random.randint(0, self.mapz)) )
