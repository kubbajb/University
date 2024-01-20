import numpy as np

# Słownik do sprawdzania spójności
consistency_dict = {
    1: 0.00, 2: 0.00, 3: 0.58, 4: 0.90,
    5: 1.12, 6: 1.24, 7: 1.32, 8: 1.41,
    9: 1.45, 10: 1.49
}

# Nazwy dla wyników obliczeń
names = np.array(("fuzzy_weight", "sharp_weight", "sharp_norm_weight"))
