using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Organizator
{
    public enum StatusWydarzenie
    {
        UTWORZONE,
        ANULOWANE,
        ZREALIZOWANE
    }

    public class Wydarzenie : Encja
    {
        private string _co;
        public string Co
        {
            get { return this._co; }
        }

        private string _gdzie;
        public string Gdzie
        {
            get { return this._gdzie; }
        }

        private DateTime _kiedy;
        public DateTime Kiedy
        {
            get { return this._kiedy; }
        }

        private StatusWydarzenie _status;
        public StatusWydarzenie Status
        {
            get { return this._status; }
        }


        private Dictionary<long, Uczestnik> _listaUczestnikow = new Dictionary<long, Uczestnik>();
        private Alarm _alarm;


        public Wydarzenie(string co, string gdzie, DateTime kiedy)
        {
            this._co = co;
            this._gdzie = gdzie;
            this._kiedy = kiedy;
            this._status = StatusWydarzenie.UTWORZONE;
        }

        private Uczestnik ZnajdzUczestnika(long idOsoba)
        {
            if (this._listaUczestnikow.ContainsKey(idOsoba))
                return this._listaUczestnikow[idOsoba];

            return null;
        }
        
        public void DodajAlarm(DateTime czas)
        {
            //To my zrobiliśmy b)
            _alarm = new Alarm(czas);
        }

        public void DodajUczestnika(Osoba osoba)
        {
            var uczestnik = new Uczestnik(osoba);
            _listaUczestnikow.Add(osoba.Id, uczestnik);
        }

        public void UstawStatusUczestnika(long idOsoba, StatusUczestnik status)
        {
            _listaUczestnikow[idOsoba].Status = status;
        }

        public void OdwolajWydarzenie()
        {
            // zaimplementowa� na podstawie diagram�w sekwencji

            // WSKAZ�WKA:
            // Iteracj� po wszystkich uczestnikach mo�na zrobi� np. tak:
            // foreach (KeyValuePair<long, Uczestnik> kvp in this._listaUczestnikow)
        }
    }
}