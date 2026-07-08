import utis
from typing import Dict, List
import random
from utis import Pos,Vec
from goal import Goal
from horm import Horm
import math


class Ant:
    
    def __init__(self, x : int, y : int):
        self.pos : Pos = Pos(x,y)
        self.vec : Vec = Vec(long = 0.01, angle = math.pi / 4)
        self.goal : Goal = Goal.FOOD

    def mouve(self, neighbordHorms: List[Horm]):
        elus : Dict[int,self.Horm] = {}

        for horm in neighbordHorms:
            if horm.type != self.goal:
                if abs(Vec(pa=horm.pos,pb=self.pos).angle - self.vec.angle) < math.pi:
                    elus[horm.val] = horm

        if len(elus.keys()) > 0:
            goto = utis.pondchoice(elus)
            self.vec.angle = ( Vec(pa=goto.pos,pb=self.pos).angle + self.vec.angle * 50 ) / 51

        self.vec.angle += (random.random() - 0.5) * .1
        self.pos += self.vec
        
        self.updateGoal()

    def updateGoal(self):
        if self.pos.x > 3 and self.pos.y > 3:
            self.goal = Goal.HOME
            self.vec.angle += math.pi
        elif self.pos.x < 1 and self.pos.y < 1:
            self.goal = Goal.FOOD

    def toHorm(self) -> Horm:
        return Horm(self.pos, self.goal)
        
    def __repr__(self):
        return 'Ant'+str(self.pos)


