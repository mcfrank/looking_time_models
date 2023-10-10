import os
import pandas as pd
import ipdb

def count_until_look_away(group):
    return group['Look_away'].tolist().index(True) if True in group['Look_away'].tolist() else len(group)

folder_path = "02_pyGRANCH/cache_results/"
df_list = []

# infant or adult runs
paradigm = 'adult'

# spore or unity
stims = 'spore'

# get list of file names
file_names = [f for f in os.listdir(folder_path) if f.endswith('.pickle') and paradigm in f and stims in f]

# detailed output file name
detailed_output_file = "02_pyGRANCH/MIT_cluster_tool/summarizing_tool/summarized_results_detailed_" + stims + "_" + paradigm + ".csv"

# grouped output file
grouped_output_file = "02_pyGRANCH/MIT_cluster_tool/summarizing_tool/summarized_results_grouped_" + stims + "_" + paradigm + ".csv"


print("file_names:", file_names[0:20])

for idx, file_name in enumerate(file_names):
        file_path = os.path.join(folder_path, file_name)
        
        try:
            df = pd.read_pickle(file_path)
            df["batch_id"] = idx
            df_list.append(df)


        except:
             print('error encountered for: ', file_path)
             
        if (idx % 300) == 0: # save every 300 files
            main_df = pd.concat(df_list)
            main_df = main_df.dropna(subset = ["stimulus_id"])

            if (paradigm == 'adult') & (stims == 'unity'):
                counts = main_df.groupby(['batch_id', 'j_i',  "stimulus_id", "stim_squence", "violation_type"]).agg({'epsilon': 'first', 'b_val': 'first', 'd_val': 'first',
                                                                                                                    'mu_prior': 'first','v_prior': 'first','alpha_prior': 'first',
                                                                                                                    'beta_prior': 'first'}
                                                                                                        ).reset_index()
                
            elif (paradigm == 'adult') & (stims == 'spore'):
                counts = main_df.groupby(['batch_id', 'j_i',  "stimulus_id", "stim_squence", "violation_type"]).agg({'epsilon': 'first', 'b_val': 'first', 'd_val': 'first',
                                                                                                'mu_prior': 'first','v_prior': 'first','alpha_prior': 'first',
                                                                                                'beta_prior': 'first', 'complexity_type': 'first'}
                                                                                    ).reset_index()
                 
            else:
                counts = main_df.groupby(['batch_id', 'j_i',  "stimulus_id", "stim_squence", "violation_type"]).agg({'epsilon': 'first', 'b_val': 'first', 'd_val': 'first',
                                                                            'mu_prior': 'first','v_prior': 'first','alpha_prior': 'first',
                                                                            'beta_prior': 'first', 'forced_exposure_max': 'first'}
                                                                ).reset_index()
                 
            
            counts["n_sample"] =  main_df.groupby(['batch_id', 'j_i',  "stimulus_id", "stim_squence", "violation_type"], as_index=False).count()['EIG']
            counts.to_csv(detailed_output_file, mode = 'a', index=False, header=False)

            df_list = []

if paradigm == 'infant':

    detailed_df = pd.read_csv(detailed_output_file, names=["sub_id", "batch_id", "trial_num", 
                                                           "stim_sequence", "violation", 
                                                            "epsilon", "b_val", "d_val", "mu_prior", "v_prior", 
                                                            "alpha_prior", "beta_prior", "forced_exposure_num", 
                                                            "n_samples"],
                  header=None)
    
    detailed_df = detailed_df.groupby(['batch_id', 'sub_id','stim_sequence']).tail(1)

    detailed_df['test_type'] = ['nov' if 'D' in seq else 'fam' for seq in detailed_df['stim_sequence']]
    
    detailed_df['fam_duration'] = detailed_df['stim_sequence'].str.len() - 1

    grouped_df = detailed_df.groupby(
        ['epsilon', 'mu_prior', 'v_prior', 'beta_prior', 'alpha_prior', 'forced_exposure_max', 'stim_sequence']).agg(
        {'n_samples': 'mean', 'forced_exposure_max': 'first', 'fam_duration': 'first', 'test_type': 'first'}).reset_index()

elif stims == 'spore':
     
    detailed_df = pd.read_csv(detailed_output_file, names=["sub_id", "batch_id", "trial_num", "stim_sequence", "violation", 
                         "epsilon", "b_val", "d_val", "mu_prior", "v_prior", "alpha_prior", 
                         "beta_prior", "complexity_type", "n_samples"],
                  header=None)
    
    grouped_df = detailed_df.groupby(
        ['epsilon', 'mu_prior', 'v_prior', 'beta_prior', 'alpha_prior', 'forced_exposure_max', 'stim_sequence','j_i']).agg(
        {'n_samples': 'mean', 'forced_exposure_max': 'first', 'fam_duration': 'first', 'test_type': 'first'}).reset_index()

    grouped_df.to_csv(grouped_output_file)

# adult and unity stim 
else: 
    detailed_df = pd.read_csv(detailed_output_file, names=["sub_id", "batch_id", "trial_num", "stim_sequence", "violation", 
                         "epsilon", "b_val", "d_val", "mu_prior", "v_prior", "alpha_prior", 
                         "beta_prior", "n_samples"],
                  header=None)
    
    grouped_df = detailed_df.groupby(
        ['epsilon', 'mu_prior', 'v_prior', 'beta_prior', 'alpha_prior', 'forced_exposure_max', 'stim_sequence','j_i']).agg(
        {'n_samples': 'mean', 'fam_duration': 'first', 'test_type': 'first'}).reset_index()
