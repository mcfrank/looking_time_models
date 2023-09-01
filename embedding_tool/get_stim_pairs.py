import pandas as pd

search_distance = 2

distances = pd.read_csv('eucledian_distances.csv')

distances = distances.round(decimals = 2)

distances.index = distances.iloc[:,0]
distances.drop('Unnamed: 0', axis=1, inplace=True)

match_indices = distances.apply(lambda row: row[row == search_distance].index, axis=1)

# pairs chosen for ResNet50:
# movie_001.png (chick) - movie_072.png (carp)
# movie_012.png (parrot)  - movie_087.png (squid)
# movie_032.png (owl) - movie_081.png (dolphin)
# movie_033.png (raccoon)- movie_070.png (arowana)
# movie_043.png (bighorn) - movie_058.png (wombat)
# movie_042.png (armadillo) - movie_007.png (pig)

# pairs chosen for aligned model:
# movie_002.png (rhino) - movie_011.png (pidgeon)
# movie_003.png (cow)  - movie_034.png (snake)
# movie_022.png (snow weasel) - movie_085.png (sea otter)
# movie_044.png (camel) - movie_032.png (owl))
# movie_048.png (horned lizard) - movie_037.png (flamingo)
# movie_090.png (buffalo) - movie_024.png (sea lion)

print(match_indices[20:50])