import torch 
from itertools import repeat 
import helper


def get_ith_column(t,i):
 return t.gather(1, torch.full([t.size(dim = 0)], fill_value = i, dtype = torch.int64).unsqueeze(1)).squeeze()

a = torch.tensor(
    [[1, 1, 1],
    [2, 3, 4]]
)

print(get_ith_column(a, 0))

print(get_ith_column(a, 1))
print(get_ith_column(a, 2))