import torch 
from itertools import repeat 
import numpy as np
import pandas as pd
import helper
import compute_prob
a = torch.tensor([-1279.1746])

print(torch.exp())
print(torch.exp(a).item() == 0)