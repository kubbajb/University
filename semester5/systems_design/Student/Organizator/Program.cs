using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Organizator
{
    class Program
    {
        static void Main(string[] args)
        {
            try{
            // uworzyć 2 osoby z adresami
            SerwisOsob serwisOsob = new SerwisOsob();

            long id1 = serwisOsob.UtworzOsobe("Jan", "Kowalski", "5432312", "2345674", "kowal@poczta.fm.pl");
            serwisOsob.DodajAdresDom(id1, "Mickiewicza", "12", "Kraków", "30-383");
            serwisOsob.DodajAdresPraca(id1, "Lea", "113/12", "Kraków", "30-343");

            long id2 = serwisOsob.UtworzOsobe("Jan", "Nowak", "5432312", "2345674", "nowak@poczta.onet.pl");
            serwisOsob.DodajAdresDom(id2, "Krupnicza", "9", "Kraków", "22-433");
            serwisOsob.DodajAdresPraca(id2, "Grodzka", "4/4a", "Kraków", "30-343");

            // utworzyć wydarzenie z alarmem
            SerwisWydarzen serwisWydarzen = new SerwisWydarzen();

            long id3 = serwisWydarzen.UtworzWydarzenie("Kolokwium ćwiczenia", "sala 207", new DateTime(2015, 6, 11, 15, 0, 0));
            serwisWydarzen.DodajAlarm(id3, new DateTime(2015, 6, 4, 15, 0, 0));

            // do wydarzenia dodać 2 uczestników
            serwisWydarzen.DodajUczestnika(id3, id1);
            serwisWydarzen.DodajUczestnika(id3, id2);

            // jeden uczestnik potwix`erdza uczestnictwo, drugi odwoluje
            SerwisUczestnikow serwisUczestnikow = new SerwisUczestnikow();
            serwisUczestnikow.PotwierdzUczestnictwo(id3, id1);
            serwisUczestnikow.OdwolajUczestnictwo(id3, id2);

             // wydarzenie jest odwolane przez własciciela organizatora
                serwisWydarzen.OdwolajWydarzenie(id3);
            }
            catch (NotImplementedException ex)
            {
                Console.WriteLine("\nNie wszystkie operacje zostały zaimplementowane!\n");
            }
            finally
            {
                Console.WriteLine("Naciśnij dowolny klawisz by wyjść!");
                Console.ReadKey();
            }
        }
    }
}
