import pygame as pg
from utis import Pos
from ant import Ant, OmapType
from drawer import rend
import drawer
from goal import Goal

pg.init()
f = pg.display.set_mode(drawer.SCREEN_DIM)
fps = pg.time.Clock()
    
omap : OmapType = [ 
        [
            ([],[]) for i in range(5)
        ]
        for j in range(5)
    ]
omap[0][0] = (
        [Ant(0.1,0.1)],
        []
    )

B = 1
humanWrite = None

while B:
    fps.tick(60)
    pg.display.update()
    pg.draw.rect(f, 0, (rend(Pos(0,0)), rend(Pos(5,5))))
    pg.draw.rect(f, 0x550000, (rend(Pos(0,0)), (100,100)))
    pg.draw.rect(f, 0x005500, (rend(Pos(3,3)), (200,200)))

    for y in  range(5):
        for x in range(5):
            # Les fourmis et féromones pour ce chunk 
            fourmis, hormons  = omap[x][y]
           
            nextfourmis = []
            
            # Les fourmis

            for ant in fourmis:
                ant.mouve(omap)
                
                nvlieu = int(ant.pos.x), int(ant.pos.y)

                if nvlieu[0] != x or nvlieu[1] != y or ant.pos.x < 0 or ant.pos.y < 0: #si on est plus dans la même case
                    #si on reste dans la map
                    if 0 <= ant.pos.x < 5 and 0 <= ant.pos.y < 5:
                        omap[nvlieu[0]][nvlieu[1]][0].append(ant)
                        #jean=nvlieu
                    else:
                        #si elle sort, une autre née au point de départ
                        omap[0][0][0].append(Ant(0,0))
                else:
                    nextfourmis.append(ant)
                    pg.draw.circle(f,0xffffff,rend(ant.pos),5) # On dessine la fourmi

                
            # Les hormones
            
            for mol in hormons:
                mol.draw(f)

            nexthormons = [h for h in hormons if h.val >0] + [fourmi.toHorm() for fourmi in fourmis]
            
            omap[x][y] = (nextfourmis, nexthormons)
    if humanWrite is not None:
        m=[d/100 for d in pg.mouse.get_pos()]
        if humanWrite == 1:
            H=Ant.Horm(Ant(m[0],m[1]))
            omap[int(m[1])][int(m[0])][1].append(H)
        elif humanWrite == 3:
            H=Ant.Horm(Ant(m[0],m[1]))
            H.type = Goal.HOME
            omap[int(m[1])][int(m[0])][1].append(H)
            
    for event in pg.event.get():

        if event.type==pg.QUIT:
            pg.quit()
            B = 0

        elif event.type==pg.KEYUP:
            match event.key:
                case pg.K_c:
                    breakpoint()
                case pg.K_SPACE:
                    omap[0][0][0].append(Ant(0.5,0.5))
                case pg.K_ESCAPE:
                    pg.quit()
                    B = 0

        elif event.type == pg.MOUSEBUTTONUP:
            humanWrite = None 

        elif event.type == pg.MOUSEBUTTONDOWN:
            humanWrite = event.button
        
      
