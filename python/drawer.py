from utis import Pos

SCREEN_DIM = 500,500

def rend(pos: Pos):
    return [int(d*100) for d in pos.pg()]

