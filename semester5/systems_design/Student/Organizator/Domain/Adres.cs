using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Organizator
{
    public class Adres
    {
        private string _ulica;
        private string _numer;
        private string _miasto;
        private string _kod;

        public Adres(string ulica, string numer, string miasto, string kod)
        {
            this._ulica = ulica;
            this._numer = numer;
            this._miasto = miasto;
            this._kod = kod;
        }
    }
}
