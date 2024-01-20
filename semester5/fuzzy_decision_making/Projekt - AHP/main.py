import pandas as pd
from data import config as cfg
from data.data_utilities import read_preferences_data, fuzzify_preferences, check_if_columns_correct
from calculations.calculations import calculations, is_consistent

if __name__ == '__main__':

    # Importowanie danych preferencji, część I
    pref_data = read_preferences_data("./preferencje.csv")
    preferences = fuzzify_preferences(pref_data)

    # Importowanie danych preferencji, część II
    sub_pref_data = read_preferences_data("./preferencje-2.csv")
    sub_preferences = fuzzify_preferences(sub_pref_data)

    # Pobieranie nazw atrybutów z obu zestawów preferencji
    pref_columns = [col for col in pref_data.columns] + [col for col in sub_pref_data.columns]

    # Tworzenie tabli preferencji
    sharp_prefs_all = [pref_data, sub_pref_data]
    fuzzy_prefs_all = [preferences, sub_preferences]

    # Importowanie i sprawdzanie danych
    data = pd.read_csv("./dane.csv", sep=";", index_col=0)
    check_if_columns_correct(pref_columns, data)

    # Sprawdzanie spójności i przeprowadzenie obliczeń
    for i, value in enumerate(sharp_prefs_all):
        if is_consistent(value, cfg.consistency_dict):
            print("\nZestaw danych " + str(i + 1) + " jest spójny!\n")
            print("Poniżej znajduje się zestaw danych posortowany metodą AHP:\n\n")
            calculated_preferences = pd.DataFrame(calculations(fuzzy_prefs_all[i]),sharp_prefs_all[i].columns.transpose(), columns=cfg.names)
            selected_data = data[calculated_preferences.index]
            weighted_data = selected_data * calculated_preferences["sharp_norm_weight"]
            weighted_sum = weighted_data.sum(axis=1)
            weighted_sum = weighted_sum.apply(lambda x: round(x, 2))
            result_df = pd.DataFrame(selected_data)
            result_df['Weighted_Sum'] = weighted_sum
            result_df = result_df.sort_values(by='Weighted_Sum', ascending=False)
            print(result_df)
        else:
            print("\nNiestety, zestaw danych " + str(i + 1) + " nie jest spójny, dlatego preferencje nie zostały przeanalizowane.")
