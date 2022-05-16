import numpy as np
import pandas as pd
import json
from scipy.spatial import distance_matrix
from sklearn.metrics.pairwise import cosine_similarity
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt

f = open('new_embeddings.json')
embedding_dict = json.load(f)

# samples in rows, features in columns
embedding_df = pd.DataFrame.from_dict(embedding_dict).transpose() 

embedding_df.to_csv('all_embeddings.csv')

pca = PCA(n_components=3)
components = pca.fit_transform(embedding_df)


np.savetxt("all_embeddings_afterPCA.csv", components, delimiter=",")


# calculate cosine distance
cosine_distances = 1 - cosine_similarity(embedding_df.values, embedding_df.values)
cosine_distances_df = pd.DataFrame(cosine_distances, index=embedding_df.index, columns=embedding_df.index)

cosine_distances_df.to_csv('cosine_distances.csv')


# calculate eucledian distance
embedding_eucledian_distances = distance_matrix(components, components)
eucledian_distances_df = pd.DataFrame(embedding_eucledian_distances, index=embedding_df.index, columns=embedding_df.index)

eucledian_distances_df.to_csv('eucledian_distances.csv')
