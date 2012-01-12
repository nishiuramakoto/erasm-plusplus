/* 
   Copyright (C) 2011,2012 Makoto Nishiura.

   This file is part of ERASM++.

   ERASM++ is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   ERASM++ is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with ERASM++; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */
#include <iostream>
#include <erasm/meta_prelude.hpp>
#include <erasm/meta_leftist_heap.hpp>

using namespace erasm::prelude;
using namespace std;


struct test 
{
typedef Heap::fromList<toList<Two,Three,One,Four,Five,Six> > h;
typedef eval<Heap::min<h> >::result z;
typedef Heap::deleteMin<h> h1;
typedef eval<Heap::min<h1> >::result z2;
};

int
main ()
{
cout << ::test::z::value << endl;
cout << ::test::z2::value << endl;
}
