import numpy as np


# Obliczanie średniej geometrycznej dla wiersza
def calculate_geometric_mean(row):
    num_columns = len(row)
    product = np.prod(row)
    geometric_mean = product ** (1 / num_columns)
    return geometric_mean

# Obliczanie średnich geometrycznych dla wszystkich wierszy
def calculate_geometric_means(preferences):
    geometric_means = np.empty(preferences.shape[0], dtype=object)

    for i, row in enumerate(preferences):
        geometric_mean = calculate_geometric_mean(row)
        geometric_means[i] = geometric_mean

    return geometric_means

# Normalizacja tablicy wartości
def normalize(arr):
    arrsum = np.sum(arr)
    norm_arr = np.zeros(arr.shape[0], dtype=float)

    for i, value in enumerate(arr):
        norm_arr[i] = value / arrsum

    return norm_arr

# Główna funkcja obliczeniowa
def calculations(preferences: np.array):
    geometric_means = calculate_geometric_means(preferences)
    geometric_means_sum = np.sum(geometric_means)
    fuzzy_weights = geometric_means * (geometric_means_sum ** -1)
    sharp_weights = np.empty(fuzzy_weights.shape[0], dtype=object)

    for i, value in enumerate(fuzzy_weights):
        sharp_weights[i] = value.defuzzify()

    norm_sharp_weights = normalize(sharp_weights)

    return (np.column_stack((fuzzy_weights, sharp_weights, norm_sharp_weights)))


# Sprawdzanie spójności preferencji
def is_consistent(pref, consistency_dict):
    pref_array = pref
    column_sums = np.sum(pref_array, axis=0)
    normalized_array = pref_array / column_sums
    row_mean = np.mean(normalized_array, axis=1)
    multiplied_array = pref_array * row_mean
    weighetmeans = np.sum(multiplied_array, axis=1)
    result = weighetmeans / row_mean
    consistency_index = (max(result) - len(result)) / (len(result) - 1)

    if consistency_index < consistency_dict[int(len(result))]:
        value = True
    else:
        value = False

    return value