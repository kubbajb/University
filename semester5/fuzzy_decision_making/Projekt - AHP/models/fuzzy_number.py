class FuzzyNumber:
    def __init__(self, a: float, b: float = None, c: float = None):
        # Inicjalizacja rozmytej liczby w zależności od podanych wartości
        if b == None or c == None:
            if a == 1:
                self.a = 1
                self.b = 1
                self.c = 1
            elif a > 1:
                self.a = a - 1
                self.b = a
                self.c = a + 1
            elif a < 1 and a > 0:
                self.a = round(1 / ((1 / a) + 1), 2)
                self.b = round(a, 2)
                self.c = round(1 / ((1 / a) - 1), 2)
            else:
                raise Exception("Niepoprawne dane")
        elif a >= 0 and a <= b and b <= c:
            self.a = a
            self.b = b
            self.c = c
        else:
            raise Exception("Niepoprawne dane")

    # Metody do obsługi operacji matematycznych na rozmytych liczbach

    # Dodawanie liczb rozmytych
    def __add__(self, other):
        return FuzzyNumber(self.a + other.a, self.b + other.b, self.c + other.c)

    # Mnożenie liczb rozmytych
    def __mul__(self, other):
        return FuzzyNumber(self.a * other.a, self.b * other.b, self.c * other.c)

    # Potęgowanie liczb rozmytych
    def __pow__(self, power):
        if power >= 0:
            return FuzzyNumber(self.a ** power, self.b ** power, self.c ** power)
        elif self.a > 0 and self.b > 0 and self.c > 0:
            return FuzzyNumber(1 / self.c ** abs(power), 1 / self.b ** abs(power), 1 / self.a ** abs(power))
        else:
            raise Exception("Niepoprawne dane")

    # Metody do reprezentacji tekstowej rozmytej liczby
    def __str__(self):
        return f'({self.a}, {self.b}, {self.c})'

    def __repr__(self):
        return f'({self.a}, {self.b}, {self.c})'

    def defuzzify(self):
        # Zdefuzyfikowanie rozmytej liczby
        return (self.a + self.b + self.c) / 3