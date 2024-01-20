using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Organizator
{
    public class Encja
    {
        private static long _counter = 0;

        private long _id;

        public long Id
        {
            get { return _id; }
        }

        public Encja()
        {
            this._id = ++_counter;
        }

    }
}
