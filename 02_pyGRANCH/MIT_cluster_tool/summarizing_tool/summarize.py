import os
import pandas as pd
import ipdb

def count_until_look_away(group):
    return group['Look_away'].tolist().index(True) if True in group['Look_away'].tolist() else len(group)

folder_path = "02_pyGRANCH/cache_results/"
df_list = []

# infant or adult runs
paradigm = 'adult'
stims = 'unity'

# get list of file names
file_names = [f for f in os.listdir(folder_path) if f.endswith('.pickle') and paradigm in f and stims in f]

for idx, file_name in enumerate(file_names):
        file_path = os.path.join(folder_path, file_name)
        df = pd.read_pickle(file_path)
        df["batch_id"] = idx
        df_list.append(df)

main_df = pd.concat(df_list)
main_df = main_df.dropna()

counts = main_df.groupby(['batch_id', 'j_i',  "stimulus_id", "stim_squence", "violation_type"]).agg({'epsilon': 'first', 'b_val': 'first', 'd_val': 'first', 'mu_prior' : 'first',
                                                                                                        'mu_prior': 'first','v_prior': 'first','alpha_prior': 'first','beta_prior': 'first',
                                                                                                        'epsilon': 'first'}).reset_index()

counts["n_sample"] =  main_df.groupby(['batch_id', 'j_i',  "stimulus_id", "stim_squence", "violation_type"], as_index=False).count()['EIG']

summary = counts.groupby(["stim_squence", "violation_type", 'stimulus_id', 'epsilon'], as_index = False).mean()
combined_df = summary[["stim_squence", "violation_type", "stimulus_id", "n_sample", "epsilon"]]

counts.to_csv("02_pyGRANCH/MIT_cluster_tool/summarizing_tool/summarized_results_detailed" + "_" + stims + "_" + paradigm + ".csv")
counts.to_csv("02_pyGRANCH/MIT_cluster_tool/summarizing_tool/summarized_results_grouped" + "_" + stims + "_" + paradigm + ".csv")