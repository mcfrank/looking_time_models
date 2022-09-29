from enum import unique
from pickle import FALSE
import torch 
from itertools import repeat 

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