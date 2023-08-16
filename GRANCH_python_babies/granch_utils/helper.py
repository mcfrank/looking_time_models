from enum import unique
from pickle import FALSE
import torch 
from itertools import repeat 

import numpy as np

from torch.distributions import constraints
from torch.distributions.transforms import PowerTransform
from pyro.distributions.torch import Gamma, TransformedDistribution
class InverseGamma(TransformedDistribution):
    r"""
    Creates an inverse-gamma distribution parameterized by
    `concentration` and `rate`.
        X ~ Gamma(concentration, rate)
        Y = 1/X ~ InverseGamma(concentration, rate)
    :param torch.Tensor concentration: the concentration parameter (i.e. alpha).
    :param torch.Tensor rate: the rate parameter (i.e. beta).
    """
    arg_constraints = {
        "concentration": constraints.positive,
        "rate": constraints.positive,
    }
    support = constraints.positive
    has_rsample = True

    def __init__(self, concentration, rate, validate_args=None):
        base_dist = Gamma(concentration, rate)
        super().__init__(
            base_dist,
            PowerTransform(-base_dist.rate.new_ones(())),
            validate_args=validate_args,
        )

    def expand(self, batch_shape, _instance=None):
        new = self._get_checked_instance(InverseGamma, _instance)
        return super().expand(batch_shape, _instance=new)

    @property
    def concentration(self):
        return self.base_dist.concentration

    @property
    def rate(self):
        return self.base_dist.rate



def add_singleton_dim(tensor, num_dims):
    """
    Adds the specified number of singleton dimensions to a tensor.

    Args:
        tensor: A PyTorch tensor.
        num_dims: An integer specifying the number of singleton dimensions to add.

    Returns:
        The input tensor with the specified number of singleton dimensions added.
    """
    for i in range(num_dims):
        tensor = tensor.unsqueeze(-1)
    return tensor


def get_grid_parameter_tensors(combination):    
   return (list(map(lambda x: torch.linspace(x[0], x[1], int(x[2])), combination)))

def get_grid_parameter_combination(starts, ends, steps): 
    return np.stack(np.meshgrid(starts, ends, steps), -1).reshape(-1, 3)




def combine(x, y): 
    xx, yy = torch.meshgrid(x, y, indexing='ij')
    return torch.stack([torch.reshape(xx, [-1]), torch.reshape(yy, [-1])], axis=1)


def get_ith_column(t, i): 
    return t.gather(1, torch.full([t.size(dim = 0)], fill_value = i, dtype = torch.int64).unsqueeze(1)).squeeze()


def get_unique_segment(t): 
    unique, idx, counts = torch.unique(t, dim = 0, sorted = True, 
                                        return_inverse = True, return_counts = True)
   
    _, ind_sorted = torch.sort(idx, stable=True)
    cum_sum = counts.cumsum(0)
    cum_sum = torch.cat((torch.tensor([0]), cum_sum[:-1]))

    # get the unique elements and their firstly appeared indices of a pytorch tensor
    first_indicies = ind_sorted[cum_sum]
    sorted_i, _ = torch.sort(first_indicies, stable = True)

    # sorted_indicies, _ = torch.sort(first_indicies)
    return sorted_i






def group_by_logsumexp_improved(grouping_base, target): 
    target = target.unsqueeze(1)
    _, idx, _ = torch.unique(grouping_base, dim = 0, sorted = False, 
                                        return_inverse = True, return_counts = True)
    M = torch.zeros(idx.max()+1, len(target))
    M[idx, torch.arange(len(target))] = 1
    res = torch.log(torch.mm(M, torch.exp(target))).squeeze(1)

    return res 



def group_by_logsumexp(grouping_base, target): 
    unique_grouping_base, idx, _ = torch.unique(grouping_base, dim = 0, sorted = False, 
                                        return_inverse = True, return_counts = True)

    (unique_index, unique_index_position) = (idx == idx.unique().unsqueeze(1)).nonzero(as_tuple = True)
    _, count = torch.unique(unique_index, return_counts = True)
    # figure out what size to chunk unique_index_position with 
    #length = count[0].item()
    length = count.tolist()
    # a tuple of tensor, each indicating the index of slicing lp_z_given_mu_sig_sq_for_y
    splitting_index = torch.split(unique_index_position, length)
    
    # slicing lp_z_given_mu_sig_sq_for_y, resulting also a tuple of tensor
    grouped_val = tuple((torch.index_select(target, dim = 0, index = i) for i in splitting_index))
    # apply torch.logsumexp on each tensor and combine everything into a stack of torch
    res = torch.stack(tuple(map(torch.logsumexp, grouped_val, repeat(0)))) 

    return res 
