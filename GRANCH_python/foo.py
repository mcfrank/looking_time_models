import numpy as np
import itertools
from itertools import repeat
import torch 
import helper
import unittest
import time 


a = torch.tensor([1, 2, 3])
b = torch.tensor([4, 5, 6])
c = torch.tensor([7, 8, 9, 10, 12])
d = torch.tensor([11, 12, 13])


(a1, b1, c1, d1) = torch.meshgrid(
           a, b, c, d)

print(a1)
