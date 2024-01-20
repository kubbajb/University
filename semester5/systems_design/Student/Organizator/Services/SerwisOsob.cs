using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;


namespace Organizator
{
    // w wersji finalnej SerwisABC mo¿e zawieraæ obslugê transakcji i wyj¹tków
    // np.
    //public class SerwisABC
    //{
    //    public void MetodaABC()
    //    {
    //        try
    //        {
    //            BeginTransaction();
    //            // ...
    //            CommittTransaction();
    //        }
    //        catch
    //        { 
    //            // ...
    //            RollbackTransaction(); 
    //        }
    //    }
    //}
    public class SerwisOsob
    {
        public long UtworzOsobe(string imie, string nazwisko, string telDom, string telPraca, string email)
        {
            Osoba osoba = new Osoba(imie, nazwisko, telDom, telPraca, email);
            RepozytoriumOsob.Zapisz(osoba.Id, osoba);

            Console.WriteLine("Utworzono osobê {0} {1} z id = {2}", imie, nazwisko, osoba.Id);

            return osoba.Id;
        }

        public void DodajAdresDom(long idOsoba, string ulica, string numer, string miasto, string kod)
        {
            Osoba osoba = RepozytoriumOsob.Znajdz(idOsoba);
            osoba.DodajAdresDom(ulica, numer, miasto, kod);

            Console.WriteLine("Osobie z id = {0} przypisano adres domowy", idOsoba);

        }

        public void DodajAdresPraca(long idOsoba, string ulica, string numer, string miasto, string kod)
        {
            Osoba osoba = RepozytoriumOsob.Znajdz(idOsoba);
            osoba.DodajAdresDom(ulica, numer, miasto, kod);

            Console.WriteLine("Osobie z id = {0} przypisano adres do pracy", idOsoba);
        }
    }
}
