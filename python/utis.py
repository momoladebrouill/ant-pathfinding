"""Stuufs utils pour momo, je sais même pas si je vais m'en serveir"""
print("Services offerts par votre bien aimé captiane µ")
import math
class Pos:
    def __init__(self,x,y):
        self.x=x
        self.y=y

    def move(self,vec):
        self.x+=vec.x
        self.y+=vec.y

    def dist(self,autrui):
        return math.sqrt((self.x-autrui.x)**2+(self.y-autrui.y)**2)
    def angle(self,autrui):
        return math.atan2(self.y-autrui.y,self.x-autrui.x)
    def pg(self):
        return (self.x,self.y)
    def __add__(self,other):
        x=self.x+other.x
        y=self.y+other.y
        return Pos(x=x,y=y)
    def __repr__(self):
        return 'pos '+ str([round(d,2) for d in self.pg()])
def addvecs(*vecs):
    """fait la somme des vecteurs"""
    x=0
    y=0
    for vectot in vecs:
        x+=vectot.x
        y+=vectot.y
    return Vec(x=x,y=y)


class Vec:
    def __init__(self,**args):
        """
        duo x=0,y=0 OU angle=0,long=1 OU pa=Pos(),pb=Pos()
        je calcule duo A si tu me donne duo B et viche versa
        peut aussi prendre utis.Pos a utis.Pos b
        """
        if 'x' in args:
            self.x=args['x']
            self.y=args['y']
            self._angle=math.atan2(self.y,self.x)
            self.long=Pos(0,0).dist(Pos(self.x,self.y))
        elif 'long' in args:
            self._angle=args.get('angle',0)
            self.long=args.get('long',1)
            self.x=math.cos(self.angle)*self.long
            self.y=math.sin(self.angle)*self.long
        else:
            self._angle=args['pa'].angle(args['pb'])
            self.long=args['pa'].dist(args['pb'])
            self.x=math.cos(self.angle)*self.long
            self.y=math.sin(self.angle)*self.long
    @property
    def angle(self):
        return self._angle
    @angle.setter
    def angle(self,val):
        self._angle=val
        self.x=round(math.cos(self.angle)*self.long,9)
        self.y=round(math.sin(self.angle)*self.long,9)
    def pointtoo(self,form:Pos,to:Pos):
        self.angle=form.angle(to)
        self.update()
    def __add__(self,other):
        x=self.x+other.x
        y=self.y+other.y
        return Vec(x=x,y=y)
    def __sub__(self,other):
        x=self.x-other.x
        y=self.y-other.y
        return Vec(x=x,y=y)
    def __mul__(self,other):
        return self.x*other.x+self.y*other.y
    def __floordiv__(self,other):
        print(self,other)
        return Vec(self.x/other,self.y/other)
    def draw(self,pg,fen,pos,coul):
        pg.draw.line(fen,coul,pos.pg(),(pos+self).pg(),2)
    def __repr__(self):
        ang=(self.angle/math.pi).as_integer_ratio()
        return 'Vecteur :'+' x = '+str(self.x)+' y = '+str(self.y)+\
               ' angle = '+str(ang[0])+'pi/'+str(ang[1])+' long = '+str(self.long)
if __name__=="__main__":
    a=Vec(x=1,y=0)
    b=Vec(long=1,angle=math.tau)
            
