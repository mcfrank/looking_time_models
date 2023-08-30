import os
import pandas as pd
import ipdb 
import uuid
import datetime

folder_path = "../cache_results/"
df_list = []

file_list = os.listdir(folder_path)

# only get recent files
cut_off_date = datetime.datetime(2023, 8, 26, 22, 19, 31, 882493)
timestamp_cutoff = datetime.datetime.timestamp(cut_off_date)
newer_files = [f for f in file_list if os.path.getmtime(os.path.join(folder_path,f)) >= timestamp_cutoff]

for idx, file_name in enumerate(file_list[::-1]):
    pattern_batch_info = r"batch_(\d+)_cache_([A-Z]+)"
    #pattern_stim_spec = r"b_([\d\.]+)_d_([\d\.]+)"
    if file_name.endswith(".pickle"):
        file_path = os.path.join(folder_path, file_name)
        try:
            df = pd.read_pickle(file_path)
            df["batch_id"] = idx
            df["file_name"] = file_name
            df_list.append(df)
            print(pd.unique(df['stim_squence']))
        except: 
            pass

    if (idx % 300) == 0: # save every 300 files
        main_df = pd.concat(df_list)
        main_df = main_df.dropna()
        counts = main_df.groupby(['batch_id', 'j_i',  "stimulus_id", "stim_squence", "violation_type"]).agg({'epsilon': 'first', 'b_val': 'first', 'd_val': 'first', 'mu_prior' : 'first',
                                                                                                             'mu_prior': 'first','v_prior': 'first','alpha_prior': 'first','beta_prior': 'first',
                                                                                                             'epsilon': 'first'}).reset_index()
        counts["n_sample"] =  main_df.groupby(['batch_id', 'j_i',  "stimulus_id", "stim_squence", "violation_type"], as_index=False).count()['EIG']
        counts.to_csv("summarized_results_detailed_new_files.csv", mode = 'a', index=False, header=False)
        df_list = []

