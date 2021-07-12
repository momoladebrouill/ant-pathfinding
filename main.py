import pygame as pg
from colorsys import hsv_to_rgb
from random import random
from utis import*
pg.init()
f=pg.display.set_mode((500,500))
fps=pg.time.Clock()
B=1

class Ant:
    def __init__(self,x,y):
        self.pos=Pos(x,y)
        self.vec=Vec(long=0.1,angle=0)
    def draw(self):
        pg.draw.circle(f,0xffffff,rend(self.pos),5)
    def __repr__(self):
        return 'Ant'
        
rend=lambda pos:[int(d*100) for d in pos.pg()]
        
omap=[[([],[]) for i in range(5)] for j in range(5)]
omap[0][0]=([Ant(0.1,0.1)],[Pos(0.2,0.2)])
while B:
    fps.tick(60)
    pg.display.update()
    f.fill(0)
    for y in range(5):
        for x in range(5):
            chunk=omap[x][y]
            fourmis=chunk[0][:]
            hormons=chunk[1][:]
            if fourmis:
                for mol in hormons:
                    pg.draw.circle(f,0xffff00,rend(mol),5)
                nextfourmis=[]
                for ant in fourmis:
                    
                    #l'hormone
                    hormons.append(ant.pos)
                    
                    
                    # la fourmi
                    ant.pos+=ant.vec
                    #ant.vec.angle+=random()-0.5
                    if (int(ant.pos.x)!=x or int(ant.pos.y)!=y) :
                        omap[int(ant.pos.x%5)][int(ant.pos.y%5)][0].append(ant)
                    else:
                        nextfourmis.append(ant)
                    ant.draw()
                fourmis=nextfourmis[:]
            elif hormons:
                for mol in hormons:
                    pg.draw.circle(f,0xffff00,rend(mol),5)
            
                
            
            omap[x][y]=(fourmis,hormons)
    for event in pg.event.get():
        if event.type==pg.QUIT:
            pg.quit()
            B=0
        
      
