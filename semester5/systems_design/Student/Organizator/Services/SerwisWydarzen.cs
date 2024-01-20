using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Organizator
{
    // w wersji finalnej SerwisABC mo�e zawiera� obslug� transakcji i wyj�tk�w
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
    public class SerwisWydarzen
    {
        public long UtworzWydarzenie(string co, string gdzie, DateTime kiedy)
        {
            //Podpunkt a)
            Wydarzenie wydarzenie = new Wydarzenie(co, gdzie, kiedy);
            RepozytoriumWydarzen.Zapisz(wydarzenie.Id ,wydarzenie);
            Console.WriteLine($"Zostało utworzone nowe wydarzenie z id {wydarzenie.Id}");
            return wydarzenie.Id;                 
        }

        public void DodajAlarm(long idWydarzenie, DateTime czas)
        {
            //Podpunkt b)
            var wydarzenie = RepozytoriumWydarzen.Znajdz(idWydarzenie);            
            wydarzenie.DodajAlarm(czas);
            RepozytoriumWydarzen.Usun(idWydarzenie);
            RepozytoriumWydarzen.Zapisz(wydarzenie.Id, wydarzenie);
            Console.WriteLine("Do wydarzenia z id = {0} dodano alarm ustawiony na {1}", wydarzenie.Id, czas.ToString());
        }

        public void DodajUczestnika(long idWydarzenie, long idOsoba)
        {
            //Podpunkt c)
            var wydarzenie = RepozytoriumWydarzen.Znajdz(idWydarzenie);
            var osoba = RepozytoriumOsob.Znajdz(idOsoba);

            wydarzenie.DodajUczestnika(osoba);

            RepozytoriumWydarzen.Usun(idWydarzenie);
            RepozytoriumWydarzen.Zapisz(idWydarzenie, wydarzenie);

            Console.WriteLine("Do wydarzenia z id = {0} dodano uczestnika o id = {1}", idWydarzenie, idOsoba);
        }

        public void OdwolajWydarzenie(long idWydarzenie)
        {
            // zaimplementowa� na podstawie diagram�w sekwencji
            throw new NotImplementedException();
            //Console.WriteLine("Wydarzenie z id = {0} anulowano", idWydarzenie);
        }

    }
}
