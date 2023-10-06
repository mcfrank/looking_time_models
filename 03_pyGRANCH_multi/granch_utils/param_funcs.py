import csv
import json

def get_params(param_names_path, param_values_path):

    def get_param_names(param_names_path):
        with open(param_names_path, newline='') as csvfile:
            param_names = csv.reader(csvfile)     
            return list(param_names)[0]

    def get_param_values(param_values_path):
        with open(param_values_path, newline='') as csvfile:
            param_values = csv.reader(csvfile)
            return list(param_values)[0]

    param_names = get_param_names(param_names_path)
    param_values = get_param_values(param_values_path)

    print('param_names')
    print(param_names)

    print('param_values')
    print(param_values)


    # convert values to right format
    param_values = [int(val) if float(val).is_integer() else float(val) for val in param_values]

    params = dict(zip(param_names, param_values)) 

    return params