
import os
import pickle
import pandas as pd

def load_and_concatenate_pickles(directory):
    # List all files in the directory
    detailed_output_file = "nn_try.csv"
    files = [f for f in os.listdir(directory) if f.endswith('.pickle')]
    print(files)
    # Load and append each pickle file's content
    df_list = []
    for idx, file_name in enumerate(files):
        file_path = os.path.join(directory, file_name)
        
        try:
            df = pd.read_pickle(file_path)
            df["batch_id"] = idx
            df_list.append(df)

        except:
             print('error encountered for: ', file_path)
             
        if (idx % 300) == 0: # save every 300 files
            print(idx)
            main_df = pd.concat(df_list)
            main_df = main_df.dropna(subset = ["stimulus_id"])
            if (idx == 300): 
                main_df.to_csv(detailed_output_file, mode = 'a', index=False, header=True)
            else: 
                main_df.to_csv(detailed_output_file, mode = 'a', index=False, header=False)
            df_list = []



    # Concatenate all data (assuming they are pandas DataFrames)
    concatenated_data = pd.concat(df_list, ignore_index=True)

    return concatenated_data

directory = '/scratch/users/anjiecao/cache_results'
result = load_and_concatenate_pickles(directory)
