import utis
from typing import Self
from typing import Dict, List, Tuple
import random
from utis import Pos,Vec
from goal import Goal
import math

MAX_HORM = 1000
GRID_SIZE = 10,10

class Ant:
    class Horm:
        def __init__(self, ant:Self):
            self.pos : Pos = ant.pos
            self.type : Goal = ant.goal
            self.val : int = MAX_HORM
            
        def update(self):
            self.val -= 1
    
    OmapType = List[List[Tuple[List[Self], List[Horm]]]]

    def __init__(self, x : int, y : int):
        self.pos : Pos = Pos(x,y)
        self.vec : Vec = Vec(long = 0.01, angle = math.pi / 4)
        self.goal : Goal = Goal.FOOD

        
    def mouve(self, omap: OmapType):
        elus : Dict[int,self.Horm] = {}

        for i in give_horms(self.pos, omap):
            if i.type != self.goal:
                if abs(Vec(pa=i.pos,pb=self.pos).angle-self.vec.angle) < math.pi:
                    elus[i.val] = i

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
        return self.Horm(self)
        
    def __repr__(self):
        return 'Ant'+str(self.pos)


def give_horms(pos : Pos, omap: Ant.OmapType) -> List[Ant.Horm]:
    """returns the hormns all around this position

    Args:
        pos:the position of reference
        omap:the map of ants,hormones 

    Returns:the list of hormones all around the cell corresponding to the position
    """
    x, y= int(pos.x),int(pos.y)
    dx,dy = pos.x - x,pos.y - y
    todo = []
    #NORD-EST:
    if dx>0.5:
        todo.append([1,0])
    else:
        todo.append([-1,0])
    if dy>0.5:
        todo.append([0,1])
    else:
        todo.append([0,-1]) 
    todo.append([todo[0][0],todo[1][1]])
    todo.append([0,0])
    fromhere = []
    for i in todo:
        temp=x+i[0],y+i[1]
        if 0 <= temp[0] < 5 and 0 <= temp[1] < 5:
            fromhere += omap[temp[0]][temp[1]][1]
    return fromhere
