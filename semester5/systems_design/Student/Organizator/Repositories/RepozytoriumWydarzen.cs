using System;
using System.Collections.Generic;


namespace Organizator
{
    // W rozwiazaniu produkcyjnym repozytoria powinny posiadaæ wspólnego generycznego przodka, np. Repozytorium<T>. 
    // Aby poprawæ testowalnoœæ rozwi¹zania, warto rozwa¿yæ rezygnacjê z metod stycznych.
    // Dostêp do repozytorium powinna zapewniaæ fabryka statyczna
    // W rozwiazaniu produkcyjnym repozytorium bedzie pobieraæ i zapisywac obiekty bizesowe z/do bazy danych
    public class RepozytoriumWydarzen
    {
        private static Dictionary<long, Wydarzenie> listaObiektow = new Dictionary<long, Wydarzenie>();

        public static Wydarzenie Znajdz(long id)
        {
            if (listaObiektow.ContainsKey(id))
                return listaObiektow[id];

            return null;
        }

        public static void Zapisz(long id, Wydarzenie obiekt)
        {
            listaObiektow.Add(id, obiekt);
        }

        public static void Usun(long id)
        {
            if (listaObiektow.ContainsKey(id))
                listaObiektow.Remove(id);
        }
    }
}
