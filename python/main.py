import pygame as pg
import random
from utis import*

pg.init()
f=pg.display.set_mode((700,700))
fps=pg.time.Clock()
font=pg.font.SysFont("consolas",15)

def pondchoice(dic):
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
    
def best(dic):
    val=vainq=0
    for i in dic:
        if i>val:
            val=i
            vainq=dic[i]
    return vainq
def give_horms(pos:Pos):
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

class Ant:
    def __init__(self,x,y):
        self.pos=Pos(x,y)
        self.vec=Vec(long=0.01,angle=math.pi/4)
        self.goal="food"
        
    def mouve(self,horms):
        elus={}
        for i in give_horms(self.pos):
            if i.type!=self.goal:
                if abs(Vec(pa=i.pos,pb=self.pos).angle-self.vec.angle)<math.pi:
                    elus[i.val]=i
        if elus:
            goto=pondchoice(elus)
            self.vec.angle=(Vec(pa=goto.pos,pb=self.pos).angle+self.vec.angle*50)/51
        self.vec.angle+=(random.random()-0.5)*.1
        self.pos+=self.vec
        if self.pos.x>3 and self.pos.y>3:
            self.goal="home"
            self.vec.angle+=math.pi
        elif self.pos.x<1 and self.pos.y<1:
            self.goal="food"  
        
    def __repr__(self):
        return 'Ant'+str(self.pos)

    
class Horm:
    
    def __init__(self,ant):
        self.pos=ant.pos
        self.type=ant.goal
        self.val=100
        
    def draw(self):
        self.val-=1
        if self.type=="food":
            coul=(int(255*self.val/100),0,0)
        else:
            coul=(0,int(255*self.val/100),0)
        pg.draw.circle(f,coul,rend(self.pos),1)

    
rend=lambda pos:[int(d*100) for d in pos.pg()]
omap=[[([],[]) for i in range(5)] for j in range(5)]
omap[0][0]=([Ant(0.1,0.1) for i in range(1)],[])
B=1
human=False
while B:
    fps.tick(60)
    pg.display.update()
    pg.draw.rect(f,0,(rend(Pos(0,0)),rend(Pos(5,5))))
    pg.draw.rect(f,0x550000,(rend(Pos(0,0)),(100,100)))
    pg.draw.rect(f,0x005500,(rend(Pos(3,3)),(200,200)))
    for y in range(5):
        for x in range(5):
            # Les fourmis et féromones pour ce chunk 
            fourmis,hormons=omap[x][y]
           
            nextfourmis=[]
            nexthormons=[]
            
            # Les fourmis

            for ant in fourmis:
                nexthormons.append(Horm(ant))
                ant.vec.angle=ant.vec.angle%math.tau
                ant.mouve(hormons)
                
                nvlieu=int(ant.pos.x),int(ant.pos.y)
                if nvlieu[0]!=x or nvlieu[1]!=y or ant.pos.x<0 or ant.pos.y<0: #si on est plus dans le chunk
                    #si on reste dans la map
                    if 0<=ant.pos.x<5 and 0<=ant.pos.y<5:
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
                mol.draw()
                if mol.val>0:
                    nexthormons.append(mol)
            
            
            # Enregistrer le chunk
            #f.blit(font.render(str((x,y)),1,(255,255,255)),rend(Pos(x,y)))
            omap[x][y]=(nextfourmis,nexthormons)
    if human:
        m=[d/100 for d in pg.mouse.get_pos()]
        if human==1:
            H=Horm(Ant(m[0],m[1]))
            omap[int(m[1])][int(m[0])][1].append(H)
        elif human==3:
            H=Horm(Ant(m[0],m[1]))
            H.type='home'
            omap[int(m[1])][int(m[0])][1].append(H)
            
    for event in pg.event.get():
        if event.type==pg.QUIT:
            pg.quit()
            B=0
        elif event.type==pg.KEYUP:
            if event.key==pg.K_c:breakpoint()
            if event.key==pg.K_SPACE:omap[0][0][0].append(Ant(0.5,0.5))
        elif event.type==pg.MOUSEBUTTONUP:
            human=False
        elif event.type==pg.MOUSEBUTTONDOWN:
            human=event.button
        
      
