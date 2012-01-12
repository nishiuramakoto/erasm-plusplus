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
   along with ERASM++; see the file COPYING.  If not see
   <http://www.gnu.org/licenses/>.  */
#ifndef MY_META_PRELUDE_IMPL
#define MY_META_PRELUDE_IMPL

#include <boost/static_assert.hpp>
namespace erasm { namespace prelude { namespace impl {

template<typename F>
struct get_arg1 {};

template<typename R,typename T>
struct get_arg1<R (T)> 
{
   typedef T type;
};

BOOST_MPL_ASSERT(( boost::is_same<int, get_arg1<void (int)>::type > ));

template< int x >
struct abs
{
   enum { value = (x>=0) ? x : -x };
};

template<int x,int y>
struct QuotRem
{
   BOOST_STATIC_ASSERT((y != 0 ));
   enum { a = abs<x>::value,
	  b = abs<y>::value,
	  q = a/b,
	  r = a%b,
	  // note that multiplication does not preserve sign in ZZ/nZZ with arbitrary order
	  sign = ( (x>0 && y>0) || (x<0 && y<0) ) ? 1: -1
   };
   BOOST_STATIC_ASSERT( q*b +r == a);
   BOOST_STATIC_ASSERT( 0<=r && b > r);
   enum { quot = (sign > 0) ? q : -q };
   enum { rem  = (x > 0) ? r : -r };
   BOOST_STATIC_ASSERT( abs<y>::value * abs<quot>::value <= abs<x>::value );
   BOOST_STATIC_ASSERT( x == y * quot + rem );
};



template<int x, int y>
struct DivMod
{
   typedef QuotRem<x,y> quotrem;
   enum { q = quotrem::quot , r = quotrem::rem };
   enum { div= (r==0) ? q : (x>0) ? q : (q-1) ,
	  mod= x - (div*y) };
   BOOST_STATIC_ASSERT( 0<= mod && mod < y );
   BOOST_STATIC_ASSERT( x == y * div + mod );
};


}}}

#endif // MY_META_PRELUDE_IMPL
