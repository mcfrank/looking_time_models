import os
import pandas as pd

def count_until_look_away(group):
    return group['Look_away'].tolist().index(True) if True in group['Look_away'].tolist() else len(group)

<<<<<<< HEAD
folder_path = "../cache_results/"
=======
folder_path = "model_results/"
>>>>>>> 3c34b85560e34c035289d9364d47e61e123182dd
df_list = []

for idx, file_name in enumerate(os.listdir(folder_path)):
    pattern_batch_info = r"batch_(\d+)_cache_([A-Z]+)"
    #pattern_stim_spec = r"b_([\d\.]+)_d_([\d\.]+)"
    if file_name.endswith(".pickle"):
        file_path = os.path.join(folder_path, file_name)
        df = pd.read_pickle(file_path)
        df["batch_id"] = idx
        df_list.append(df)

main_df = pd.concat(df_list)
main_df = main_df.dropna()

<<<<<<< HEAD

counts = main_df.groupby(['batch_id', 'j_i',  "stimulus_id", "stim_squence", "violation_type"]).agg({'epsilon': 'first', 'b_val': 'first', 'd_val': 'first'}).reset_index()
counts["n_sample"] =  main_df.groupby(['batch_id', 'j_i',  "stimulus_id", "stim_squence", "violation_type"], as_index=False).count()['EIG']

summary = counts.groupby(["stim_squence", "violation_type", 'stimulus_id', 'epsilon'], as_index = False).mean()
combined_df = summary[["stim_squence", "violation_type", "stimulus_id", "n_sample", "epsilon"]]
=======
counts = main_df.groupby(['batch_id', 'j_i',  "stimulus_id", "stim_squence", "violation_type"], as_index=False).count()
counts["n_sample"] = counts["EIG"]
summary = counts.groupby(["stim_squence", "violation_type", 'stimulus_id'], as_index = False).mean()
combined_df = summary[["stim_squence", "violation_type", "stimulus_id", "n_sample"]]
>>>>>>> 3c34b85560e34c035289d9364d47e61e123182dd

[counts.to_csv("summarized_results_detailed.csv")]