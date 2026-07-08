from horm import Horm
import pygame as pg
from utis import Pos
from ant import Ant
from drawer import rend
import drawer
from gridMap import GridSpace
from goal import Goal

pg.init()
f = pg.display.set_mode(drawer.SCREEN_DIM)
fps = pg.time.Clock()
    
B = 1
humanWrite = None
grid = GridSpace()
grid.addAnt((0,0), Ant(0.1,0.1))

while B:
    fps.tick(60)
    pg.display.update()
    pg.draw.rect(f, 0, (rend(Pos(0,0)), rend(Pos(5,5)))) # background
    pg.draw.rect(f, 0x550000, (rend(Pos(0,0)), (100,100))) # red area
    pg.draw.rect(f, 0x005500, (rend(Pos(3,3)), (200,200))) # green area
    
    newGrid = GridSpace()
    for y in  range(5):
        for x in range(5):
            
            # Les fourmis

            fourmis = grid.getAnts((x,y))
            for ant in fourmis:
                ant.mouve(grid.give_horms(ant.pos))
                
                nx,ny = int(ant.pos.x), int(ant.pos.y)

                if nx != x or ny != y : #si on est plus dans la même case
                    #si on reste dans la map
                    if 0 <= ant.pos.x < 5 and 0 <= ant.pos.y < 5:
                        newGrid.addAnt((nx,ny), ant)
                    else:
                        #si elle sort, une autre née au point de départ
                        newGrid.addAnt((0,0),Ant(0,0))
                else:
                    newGrid.addAnt((x,y), ant)
                    drawer.drawAnt(f,ant)
                
            # Les hormones
            
            hormons = grid.getHorm((x,y))
            for mol in hormons:
                mol.update()
                drawer.drawHorm(f,mol)

            newGrid.addHorms((x,y), [h for h in hormons if h.val > 0])
            newGrid.addHorms((x,y), [fourmi.toHorm() for fourmi in fourmis])
            del grid
            grid = newGrid
            
    if humanWrite is not None:
        m = [d/100 for d in pg.mouse.get_pos()]

        if humanWrite == 1:
            horm = Horm(Pos(x = m[0], y = m[1]), Goal.FOOD)
            grid.addHorm((int(m[1]), int(m[0])), horm)

        elif humanWrite == 3:
            horm = Horm(Pos(x = m[0], y = m[1]), Goal.HOME)
            grid.addHorm((int(m[1]), int(m[0])), horm)
            
    for event in pg.event.get():

        if event.type==pg.QUIT:
            pg.quit()
            B = 0

        elif event.type==pg.KEYUP:
            match event.key:
                case pg.K_c:
                    breakpoint()
                case pg.K_SPACE:
                    grid.addAnt((0,0), Ant(0.5,0.5))
                case pg.K_ESCAPE | pg.K_q:
                    pg.quit()
                    B = 0

        elif event.type == pg.MOUSEBUTTONUP:
            humanWrite = None 

        elif event.type == pg.MOUSEBUTTONDOWN:
            humanWrite = event.button
