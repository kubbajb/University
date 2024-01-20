using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Organizator
{
    public class Osoba : Encja
    {
        private string _imie;
        public string Imie
        {
            get { return this._imie; }
        }

        private string _nazwisko;
        public string Nazwisko
        {
            get { return this._nazwisko; }
        }

        private string _telefonDom;
        public string TelefonDom
        {
            get { return this._telefonDom; }
        }

        private string _telefonPraca;
        public string TelefonPraca
        {
            get { return this._telefonPraca; }
        }

        private string _email;
        public string EMail
        {
            get { return this._email; }
        }

        private Adres _adresDom;
        private Adres _adresPraca;
        
        public Osoba(string imie, string nazwisko, string telefonDom, string telefonPraca, string email)
        {
            this._imie = imie;
            this._nazwisko = nazwisko;
            this._telefonDom = telefonDom;
            this._telefonPraca = telefonPraca;
            this._email = email;
        }

        public void DodajAdresDom(string ulica, string numer, string miasto, string kod)
        {
            this._adresDom = new Adres(ulica, numer, miasto, kod);
        }

        public void DodajAdresPraca(string ulica, string numer, string miasto, string kod)
        {
            this._adresPraca = new Adres(ulica, numer, miasto, kod);
        }
    }
}