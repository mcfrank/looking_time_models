import pandas as pd
import numpy as np

def scale_data(csv_filename):
    # Load the data from CSV
    df = pd.read_csv(csv_filename)
    
    # Define a scaling function
    def scale(series):
        # Only scale if the column is numeric
        if np.issubdtype(series.dtype, np.number):
            min_val = series.min()
            max_val = series.max()
            range_val = max_val - min_val
            return series.apply(lambda x: ((x - min_val) / range_val) - 0.5)
        else:
            return series

    # Apply the scaling function to each column in the dataframe
    df_scaled = df.apply(scale, axis=0)
    
    # Return the scaled data
    return df_scaled

# Usage
scaled_df = scale_data("all_embeddings_afterPCA.csv")
scaled_df.to_csv("all_embeddings_afterPCA_scaled.csv")