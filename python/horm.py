from utis import Pos
from goal import Goal

MAX_HORM = 1000

class Horm:
    def __init__(self, pos : Pos, type: Goal):
        self.pos : Pos = pos
        self.type : Goal = type
        self.val : int = MAX_HORM
        
    def update(self):
        self.val -= 1
