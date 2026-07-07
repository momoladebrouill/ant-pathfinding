from typing import Any
from typing import Dict, List, Tuple
import random
from utis import Pos,Vec
from goal import Goal
import math
import drawer
import pygame as pg

def pondchoice(dic : Dict[int,Pos]) -> Pos:
    """{val:pos}"""
    vals=list(dic.keys())
    total=sum(vals)
    choix=random.random()
    marcheur=0
    i=0
    while marcheur<choix<1 and i<len(vals):
        marcheur+=vals[i]/total
        i+=1
    i-=2
    return dic[vals[i]]
    
def best(dic: Dict[int, Any] ) -> Any:
    val = 0
    vainq = 0
    for i in dic:
        if i > val:
            val = i
            vainq = dic[i]
    return vainq


class Ant:
    class Horm:
        def __init__(self, ant):
            self.pos : Pos = ant.pos
            self.type : Goal = ant.goal
            self.val : int = 100
            
        def draw(self, frame):
            self.val -= 1
            if self.type == Goal.FOOD:
                coul=(int(255*self.val/100),0,0)
            else:
                coul=(0,int(255*self.val/100),0)
            pg.draw.circle(frame, coul, drawer.rend(self.pos),1)

    def __init__(self, x : int, y : int):
        self.pos : Pos = Pos(x,y)
        self.vec : Vec = Vec(long=0.01,angle = math.pi/4)
        self.goal : Goal = Goal.FOOD
        
    def mouve(self, omap):
        elus={}
        for i in give_horms(self.pos, omap):
            if i.type!=self.goal:
                if abs(Vec(pa=i.pos,pb=self.pos).angle-self.vec.angle)<math.pi:
                    elus[i.val] = i
        if elus:
            goto = pondchoice(elus)
            self.vec.angle = (Vec(pa=goto.pos,pb=self.pos).angle+self.vec.angle*50)/51
        self.vec.angle += (random.random()-0.5)*.1
        self.pos += self.vec
        if self.pos.x > 3 and self.pos.y > 3:
            self.goal = Goal.HOME
            self.vec.angle += math.pi
        elif self.pos.x < 1 and self.pos.y < 1:
            self.goal = Goal.FOOD
    def toHorm(self) -> Horm:
        return self.Horm(self)
        
    def __repr__(self):
        return 'Ant'+str(self.pos)

OmapType = List[List[Tuple[List[Ant], List[Ant.Horm]]]]

def give_horms(pos : Pos, omap: OmapType) -> List[int]:
    chunk=int(pos.x),int(pos.y)
    place=pos.x-chunk[0],pos.y-chunk[1]
    todo=[]
    #NORD-EST:
    if place[0]>0.5:
        todo.append([1,0])
    else:
        todo.append([-1,0])
    if place[1]>0.5:
        todo.append([0,1])
    else:
        todo.append([0,-1]) 
    todo.append([todo[0][0],todo[1][1]])
    todo.append([0,0])
    fromhere=[]
    for i in todo:
        temp=chunk[0]+i[0],chunk[1]+i[1]
        if 0<=temp[0]<5 and 0<=temp[1]<5:
            fromhere+=omap[temp[0]][temp[1]][1]
    return fromhere
