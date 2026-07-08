from utis import Pos
from ant import Ant
from horm import Horm,MAX_HORM
from goal import Goal
from gridMap import GRID_SIZE
import pygame as pg


SCREEN_DIM = 500,500

def rend(pos: Pos):
    x,y = pos.pg()
    return [
            x * SCREEN_DIM[0] / GRID_SIZE[0],
            y * SCREEN_DIM[1] / GRID_SIZE[1]
            ]

def drawAnt(f : pg.Surface, ant: Ant):
    pg.draw.circle(f, 0xffffff, rend(ant.pos), 5) # On dessine la fourmi

def drawHorm(frame : pg.Surface, horm: Horm):
    if horm.type == Goal.FOOD:
        coul=(int(255 * horm.val / MAX_HORM), 0, 0)
    else:
        coul=(0, int(255 * horm.val / MAX_HORM), 0)
    pg.draw.circle(frame, coul, rend(horm.pos), 1)
