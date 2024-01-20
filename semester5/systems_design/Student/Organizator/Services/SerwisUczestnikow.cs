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

    public class SerwisUczestnikow
    {
        public void PotwierdzUczestnictwo(long idWydarzenie, long idOsoba)
        {
            var wydarzenie = RepozytoriumWydarzen.Znajdz(idWydarzenie);

            wydarzenie.UstawStatusUczestnika(idOsoba, StatusUczestnik.POTWIERDZONY);

            RepozytoriumWydarzen.Usun(idWydarzenie);
            RepozytoriumWydarzen.Zapisz(idWydarzenie, wydarzenie);
                        
            Console.WriteLine("Osoba z id = {0} potwierdzi�a zaproszenie do udzia�u w wydarzeniu z id = {1}", idOsoba, idWydarzenie);
        }
        
        public void OdwolajUczestnictwo(long idWydarzenie, long idOsoba)
        {
            // zaimplementowa� na podstawie diagram�w sekwencji
            throw new NotImplementedException();
            //Console.WriteLine("Osoba z id = {0} anulowa�a uczestnictwo w wydarzeniu z id = {1}", idOsoba, idWydarzenie);
        }
    }
}
