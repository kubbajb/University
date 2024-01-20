import pandas as pd
import numpy as np
from models.fuzzy_number import FuzzyNumber

# Funkcja do konwersji preferencji na liczby rozmyte
def fuzzify_preferences(preferences: pd.DataFrame):
    fuzzy_preferences = list()

    for col_name in preferences.columns:
        pref_values = list()

        for row in preferences.columns:
            preference_value = preferences[col_name][row]
            fuzzy_value = FuzzyNumber(preference_value)
            pref_values.append(fuzzy_value)

        fuzzy_preferences.append(pref_values)

    return np.array(fuzzy_preferences, dtype=FuzzyNumber).transpose()

# Funkcja do sprawdzenia poprawności danych w DataFrame
def is_data_correct(data: pd.DataFrame):
    rows = sorted([row.lower() for row in data.index])
    cols = sorted([col.lower() for col in data.columns])

    if (rows != cols):
        return False

    # Sprawdzenie czy wartości poszczególnych preferencji są odwrotne
    for row in data.index:
        for col in data.columns:
            if (round(1 / data[row][col], 2) != round(data[col][row], 2)):
                return False

    return True

# Funkcja do wczytywania danych preferencji z pliku
def read_preferences_data(file_path: str):
    data = pd.read_csv(file_path, sep=";", index_col=0)

    if (is_data_correct(data)):
        return data
    else:
        raise Exception("Niepoprawne dane")

# Funkcja do sprawdzania, czy kolumny w danych zgadzają się z preferencjami
def check_if_columns_correct(preferences: list, data: pd.DataFrame):
    prefs = sorted([pref.lower() for pref in preferences])
    cols = sorted([col.lower() for col in data.columns])

    if (prefs != cols):
        raise Exception(f'Niepoprawne dane, kolumny preferencji: {prefs}, kolumny danych: {cols}')

