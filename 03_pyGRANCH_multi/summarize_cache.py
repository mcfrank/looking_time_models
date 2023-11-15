import os
import pandas as pd
#import ipdb

def count_until_look_away(group):
    return group['Look_away'].tolist().index(True) if True in group['Look_away'].tolist() else len(group)

folder_path = "cache_results/"
df_list = []

# infant or adult runs
linking_hypothesis = 'EIG'

# get list of file names
file_names = [f for f in os.listdir(folder_path) if f.endswith('.pickle')]
print(os.listdir(folder_path))
# detailed output file name
detailed_output_file = "cache_results/summarized_results_detailed_" + linking_hypothesis + ".csv"

# grouped output file
grouped_output_file = "cache_results/summarized_results_grouped_" + linking_hypothesis + ".csv"

#ipdb.set_trace()

print("file_names:", file_names[0:20])

for idx, file_name in enumerate(file_names):
    file_path = os.path.join(folder_path, file_name)
    try:
        df = pd.read_pickle(file_path)
        df["batch_id"] = idx
        df_list.append(df)
    except:
        print("error on loading file: ", file_name)

    if (idx % 300) == 0: # save every 300 files
        print(idx)
        main_df = pd.concat(df_list)
        main_df = main_df.dropna(subset = ["stimulus_id"])                                                                                                                     
        counts = main_df.groupby(['batch_id', 'j_i',  "stimulus_id", "stim_squence", "violation_type"]).agg({'b_val': 'first', 'd_val': 'first',
                                                                    'v_prior': 'first','alpha_prior': 'first',
                                                                    'beta_prior': 'first',
                                                                    'forced_exposure_max': 'first', 'weig': 'first'}
                                                        ).reset_index()
                
            
        counts["n_sample"] =  main_df.groupby(['batch_id', 'j_i',  "stimulus_id", "stim_squence", "violation_type"], as_index=False).count()['Look_away']
        
        counts.to_csv(detailed_output_file, mode = 'a', index=False, header=False)

        df_list = []

detailed_df = pd.read_csv(detailed_output_file, names=["sub_id", "batch_id", "trial_num", 
                                                           "stim_sequence", "violation", 
                                                            "b_val", "d_val", "v_prior", 
                                                            "alpha_prior", "beta_prior", 
                                                             "forced_exposure_num", "weig", "n_samples"], header=None)


    

detailed_df = detailed_df.groupby(['batch_id', 'sub_id','stim_sequence']).tail(1)

detailed_df['test_type'] = ['nov' if 'D' in seq else 'fam' for seq in detailed_df['stim_sequence']]

detailed_df['fam_duration'] = detailed_df['stim_sequence'].str.len() - 1

detailed_df.to_csv(detailed_output_file, header=True)

