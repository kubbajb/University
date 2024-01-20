using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Organizator
{
    public enum StatusUczestnik
    {
        UTWORZONY,
        ANULOWANY,
        ANULOWANE_WYDARZENIE,
        POTWIERDZONY
    }

    public class Uczestnik
    {
        private StatusUczestnik _status;
        public StatusUczestnik Status
        {
            get { return this._status; }
            set { this._status = value; }
        }

        private Osoba _osoba;
        
        public Uczestnik(Osoba osoba)
        {
            this._osoba = osoba;
            this._status = StatusUczestnik.UTWORZONY;
        }
        
        
        public void PoinformujUczestnika(string co, string gdzie, DateTime kiedy)
        {
            // zaimplementowaæ na podstawie diagramów sekwencji
        }

        private string PrzygotujTrescZawiadomienia(string co, string gdzie, DateTime kiedy, string imie, string nazwisko)
        {
            string tresc = "Witaj " + imie + nazwisko + "!\n"
                + "Informujemy, ¿e wydarzenie \"" + co + "\" \nplanowane na " + kiedy.ToString() + "\nw " + gdzie
                + "\nzmieni³o status na \"" + this._status.ToString() + "\"";

            return tresc;
        }
    }
}