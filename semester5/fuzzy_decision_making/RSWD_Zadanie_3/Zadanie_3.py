def triangular(x, lower, peak, upper):
    if lower == 0 and x == 0:
        return 1.0
    elif upper == 0 and x == 0:
        return 1.0
    elif x <= lower or x >= upper:
        return 0.0
    elif lower < x <= peak:
        return (x - lower) / (peak - lower)
    elif peak < x < upper:
        return (upper - x) / (upper - peak)
    else:
        return 0.0

def small_egg_membership(x):
    return triangular(x, 30, 50, 70)

def large_egg_membership(x):
    return triangular(x, 50, 70, 90)

def less_than_5_minutes_membership(size_result):
    return triangular(size_result, 0, 0.5, 1)

def more_than_5_minutes_membership(size_result):
    return triangular(size_result, 0.5, 1, 1.5)

def fuzzy_inference(weight, time_membership, size_membership):
    # Reguła 1: IF jajko jest małe AND czas gotowania mniej niż 5 minut THEN czas gotowania = krótki czas
    rule1 = min(size_membership, time_membership)
    
    # Reguła 2: IF jajko jest duże AND czas gotowania więcej niż 5 minut THEN czas gotowania = długi czas
    rule2 = min(1 - size_membership, time_membership)

    # Kombinacja wyników reguł (maksimum)
    result = max(rule1, rule2)

    return result

# Obliczanie czasu gotowania dla jajka o wadze 55g
weight_55g = 55
size_small = small_egg_membership(weight_55g)
size_large = large_egg_membership(weight_55g)

time_less_than_5_minutes = less_than_5_minutes_membership(size_small)
time_more_than_5_minutes = more_than_5_minutes_membership(size_large)

# Wnioskowanie rozmyte
result_membership = fuzzy_inference(weight_55g, time_less_than_5_minutes, size_small)

# Adjust the cooked time calculation
cooked_time = result_membership * 10  # Assuming a fixed scale for time
print(f'Czas gotowania: {cooked_time} minut')
