static void Zadanie1()
{ 
    Console.Write("Podaj a: ");
    if (decimal.TryParse(Console.ReadLine(), out decimal a))
    {
        if (float.TryParse(Console.ReadLine(), out float b))
        {

            Console.WriteLine(a.ToString() + " + " + b.ToString()
                + " = " + (a + (decimal)b).ToString());
            Console.WriteLine("{0} - {1} = {2}", a, b, a - (decimal)b);
            Console.WriteLine($"{a} * {b} = {a * (decimal)b}");
            if (b != 0)
            {
                Console.WriteLine($"{a} / {b} = {a / (decimal)b}");
            }
            else
            {
                Console.WriteLine("Dzielenie przez zero.");
            }
            Console.WriteLine("Niepoprawne dane.");
        }
    }
}

static void Zadanie2(int n)
{
    int suma = 0, licznik = 0;
    
    for(int i=0; i<n; ++i)
    {
        Console.Write($"Podaj liczbę [{i+1}]: ");

        if (!int.TryParse(Console.ReadLine(), out int a))
        {
            --i;
            continue;
        }

        if (a% 3 ==0 || a% 5 ==0)
        {
            suma += a;
            ++licznik;
        }

    }
    Console.WriteLine($"Suma: {suma}\nŚrednia: {(double)suma / licznik:f2}");

}

static void Zadanie2Test()
{
    Console.Write("Podaj n: ");
    if (int.TryParse(Console.ReadLine(), out int n))
    {
        Zadanie2(n);
    }
}

static bool Zadanie3(int a)
{
    //Metoda sprawdza, czy liczba zawiera cyfrę 3
    while (a != 0)
    {
        if (a % 10 == 0)
        {
            return true;
        }
        a /= 10;

    return false;
    }

   return false;
}

static bool Zadanie3a(int a)
{
    return a.ToString().Contains('3');
}


static bool Nwd(int a, int b)
{
    //Oblicza największy wspólny dzielnik
    if ( b == 0)
    {
        return a;
    }
    return Nwd(b, a % b);
}

static void NwdTest()
{
    int a = 13, b = 32;
    Console.WriteLine($"NWD({a}, {b}) = {Nwd(a, b)}");  
}


//Zadanie1();
//Zadanie2Test();
//Zadanie3();
NwdTest()