# perform cluster analysis



# load external libraries
import random
import numpy as np
from LSHkRepresentatives.LSHkRepresentatives import LSHkRepresentatives
from LSHkRepresentatives.LSHkPrototypes import LSHkPrototypes
import pyreadr
from datetime import datetime



# constants
k_min = 2
k_max = 10
dataset_min = 1
dataset_max = 100
method = 'lsh-k-prototypes'
metric = 'manhattan'
debug = True
verbose = 3 if debug else 0


print(f'Running clustering for {method} method'
      f' with k_min={k_min}, k_max={k_max}, dataset_min={dataset_min}, dataset_max={dataset_max}')



# stability test (remove columns)
def remove_columns(arr, percentage):
    num_cols = arr.shape[1]
    num_cols_to_remove = int(num_cols * percentage / 100)
    cols_to_remove = np.random.choice(num_cols, num_cols_to_remove, replace=False)
    return np.delete(arr, cols_to_remove, axis=1)


# perform clustering
for dataset_i in range(dataset_min, dataset_max + 1):
    # load data
    X = np.asarray(pyreadr.read_r(f'output/rda/cluster-data--{dataset_i}.Rda')['dt_cluster_variables'])
    # print if any of the cells are missing
    if np.isnan(X).any():
        print('Missing values in dataset')
    else:
        print('No missing values in dataset')
    X = X.astype(int)
    # get first 1000 subjects
    # X = X[:1000, :]
    if debug:
        print(f'DATASET: {dataset_i}')
        print(X.shape)
        print(f'Starting time: {datetime.now().strftime("%H:%M:%S")}')
        print(f'Running {method} clustering')
        print('============')
    # list of scores/values
    objective_function_cost_list = []
    for n_clusters in range(k_min, k_max + 1):
        # uncomment below to remove columns
        # X = remove_columns(X, %)
        if debug:
            print(f'Shape: {X.shape}')
        
        # set seed for reproducibility
        random.seed(1)
        np.random.seed(1)
        
        if (method == 'lsh-k-representatives'):
            # perform clustering (LSH k-representatives)
            kreps = LSHkRepresentatives(
                n_clusters = n_clusters,
                n_init = 1
            )
            cluster_labels, cost = kreps.fit(X)
        elif (method == 'lsh-k-prototypes'):
            # perform clustering (LSH k-prototypes)
            kprototypes = LSHkPrototypes(
                n_clusters=n_clusters,
                n_init=1,
                dbname = f'k-proto-{dataset_i}'
            )
            attributeMasks = [1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1]
            cluster_labels, cost = kprototypes.fit(
                X,
                attributeMasks,
                numerical_weight=2,
                categorical_weight=1
            )
        else:
            raise ValueError('Invalid method')

        if debug:
            print(f'k: {n_clusters}')
            print(f'Done at: ({datetime.now().strftime("%H:%M:%S")})')
        # save labels
        print(f'Cluster labels: {cluster_labels}')
        print(f'Cost: {cost}')
        np.save(f'output/npy/cluster-data--labels--{method}--{dataset_i}-k-{n_clusters}.npy', cluster_labels)

        # append objective function cost
        objective_function_cost_list.append(cost)

        if debug:
            print('------')
    # save objective function cost
    np.savetxt(f'output/csv/cost--{method}--dataset-{dataset_i}.csv', objective_function_cost_list, delimiter=',', fmt="%.6f")
    if debug:
        print('===========')