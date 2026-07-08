from utis import Pos
from horm import Horm
from ant import Ant
from typing import List, Tuple
GRID_SIZE = 5,5


class GridSpace:
    
    def __init__(self):
        OmapType = List[List[Tuple[List[Ant], List[Horm]]]]
        self.omap : OmapType = [ 
                [
                    ([],[]) for i in range(GRID_SIZE[0])
                ]
                for j in range(GRID_SIZE[1])
            ]

    def __get__(self, pos: Tuple[int, int]) -> Tuple[List[Ant],List[Horm]]:
        x, y = pos
        return self.omap[x][y]

    def addAnt(self, pos: Tuple[int, int], ant: Ant):
        self.omap[pos[0]][pos[1]][0].append(ant)

    def addHorm(self, pos: Tuple[int, int], horm: Horm):
        self.omap[pos[0]][pos[1]][1].append(horm)
    
    def addHorms(self, pos: Tuple[int, int], horms: List[Horm]):
        self.omap[pos[0]][pos[1]][1].extend(horms)

    def getAnts(self, pos: Tuple[int, int]) -> Ant:
        return self.omap[pos[0]][pos[1]][0]

    def getHorm(self, pos: Tuple[int, int]) ->  Horm:
        return self.omap[pos[0]][pos[1]][1]


    def give_horms(self, pos : Pos) -> List[Horm]:
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
        if dx > 0.5:
            todo.append([1,0])
        else:
            todo.append([-1,0])
        if dy > 0.5:
            todo.append([0,1])
        else:
            todo.append([0,-1]) 
        todo.append([todo[0][0],todo[1][1]])
        todo.append([0,0])
        fromhere = []
        for i in todo:
            temp = x + i[0], y + i[1]
            if 0 <= temp[0] < 5 and 0 <= temp[1] < 5:
                fromhere += self.getHorm(temp)
        return fromhere
