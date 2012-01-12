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
#ifndef MY_META_LEFTIST_HEAP
#define MY_META_LEFTIST_HEAP
#include "meta_prelude.hpp"
#include "meta_prelude_ifn.hpp"


#define ERASM_META_QUALIFIED_DATA0(MODULE,DATA)	\
   DEFINE_FOREIGN_DATA0(MODULE::DATA)
#define ERASM_META_QUALIFIED_DATA4(MODULE,DATA)			\
   DEFINE_FOREIGN_DATA4(MODULE::DATA,class,class,class,class)
#define ERASM_META_QUALIFIED_FUNC1_STRICT(MODULE,FUNC)	\
   DEFINE_PATTERN_MATCHER_STRICT(MODULE::FUNC);		\
   DEFINE_CURRYING1(MODULE::FUNC)


#define ERASM_META_QUALIFIED_FUNC1_PATTERN0(MODULE,FUNC,CON,BODY)	\
   template<>								\
   struct eval<MODULE::FUNC<CON> > : MODULE				\
   {									\
      typedef EVAL(BODY) result;				\
   }

#define ERASM_META_QUALIFIED_FUNC1_PATTERN4(MODULE,FUNC,CON,X1,X2,X3,X4,BODY) \
   template<class X1,class X2,class X3,class X4>			\
   struct eval<MODULE::FUNC<CON<X1,X2,X3,X4> > > : MODULE		\
   {									\
      typedef EVAL_TEMPLATE(BODY) result;				\
   }

#define ERASM_META_FUNC2(FUNC,X1,X2,BODY)	\
   DEFINE_FUNC_LAZY_LAZY(FUNC);			\
   DEFINE_FUNC_BODY2(FUNC,X1,X2,BODY)

namespace erasm { namespace prelude {


ERASM_META_FUNC2(iterate,f,x,(Cons<x,iterate<f,apply1<f,x> > >));


// type Elem;
// type Heap;
// empty :: Heap;
// null  :: Heap -> Bool ;
// insert :: Elem -> Heap -> Heap
// merge :: Heap -> Heap -> Heap
// findMin :: Heap -> Elem
// deleteMin :: Heap -> Heap


struct Heap 
{
   template<class X>
   friend class eval;

   struct heap_is_empty;

   struct Empty;
   template<class,class,class ,class> struct T;

   template<class> struct rank;
   template<class> struct elem;
   template<class> struct left;
   template<class> struct right;

   template<class X=_> struct null;
   template<class X1=_,class X2=_> struct merge;
   template<class X1=_,class X2=_> struct insert;
   template<class X1=_> struct min;
   template<class X=_> struct deleteMin;
   template<class X=_> struct fromList;

private:
   template<class,class,class> struct makeT;
};




ERASM_META_QUALIFIED_DATA0(Heap,Empty);
ERASM_META_QUALIFIED_DATA4(Heap,T);

ERASM_META_QUALIFIED_FUNC1_STRICT(Heap,null);
ERASM_META_QUALIFIED_FUNC1_PATTERN0(Heap,null,Heap::Empty,(True));
ERASM_META_QUALIFIED_FUNC1_PATTERN4(Heap,null,Heap::T,Rk,Elem,Left,Right,(and2<False,True>));

ERASM_META_QUALIFIED_FUNC1_STRICT(Heap,rank);
ERASM_META_QUALIFIED_FUNC1_PATTERN0(Heap,rank,Heap::Empty,(Zero));
ERASM_META_QUALIFIED_FUNC1_PATTERN4(Heap,rank,Heap::T,Rk,Elem,Left,Right,(Rk));

ERASM_META_QUALIFIED_FUNC1_STRICT(Heap,elem);
ERASM_META_QUALIFIED_FUNC1_PATTERN0(Heap,elem,Heap::Empty,(error<heap_is_empty>));
ERASM_META_QUALIFIED_FUNC1_PATTERN4(Heap,elem,Heap::T,Rk,Elem,Left,Right,(Elem));

ERASM_META_QUALIFIED_FUNC1_STRICT(Heap,left);
ERASM_META_QUALIFIED_FUNC1_PATTERN0(Heap,left,Heap::Empty,(error<heap_is_empty>));
ERASM_META_QUALIFIED_FUNC1_PATTERN4(Heap,left,Heap::T,Rk,Elem,Left,Right,(Left));

ERASM_META_QUALIFIED_FUNC1_STRICT(Heap,right);
ERASM_META_QUALIFIED_FUNC1_PATTERN0(Heap,right,Heap::Empty,(error<heap_is_empty>));
ERASM_META_QUALIFIED_FUNC1_PATTERN4(Heap,right,Heap::T,Rk,Elem,Left,Right,(Right));


template<class L>
struct eval<Heap::fromList<L> > : Heap
{
   typedef EVAL_TEMPLATE((ifn<Prelude::null<L> , Empty,
			      insert<head<L>,fromList<tail<L> > > >)) result;
};


template<class h1,class h2>
struct eval< Heap::merge <h1,h2 > > : Heap
{
   typedef elem<h1>  x;
   typedef left<h1>  a1;
   typedef right<h1> b1;

   typedef elem<h2>  y;
   typedef left<h2>  a2;
   typedef right<h2> b2;
   
   typedef EVAL_TEMPLATE((ifn< null<h1> , h2 ,
			       null<h2> , h1 ,
			       less<x,y> , makeT <x,a1,merge<b1,h2> > , 
			       makeT< y, a2 , merge<h1,b2> >  > )) result;
};


template<class X,class A,class B>
struct eval < Heap::makeT <X,A,B > >  : Heap
{
   typedef rank<A> ra;
   typedef rank<B> rb;
   
   typedef EVAL_TEMPLATE((ifn< greaterEq<ra,rb> , T<plus<rb,One>,X,A,B > ,
			       T<plus<ra,One>,X,B,A> > )) result;
};


template<class x,class h>
struct eval< Heap::insert<x,h> > : Heap
{
   typedef EVAL_TEMPLATE((merge< T<One,x,Empty,Empty> , h >)) result;
};

template<class H>
struct eval< Heap::min<H> > : Heap
{
   typedef EVAL_TEMPLATE((elem<H>)) result;
};


DEFINE_CURRYING1(Heap::deleteMin);
template<class H>
struct eval< Heap::deleteMin<H> > : Heap
{
   typedef left<H> a;
   typedef right<H> b;
   typedef EVAL_TEMPLATE((merge<a,b>)) result;
};


namespace { namespace test {
typedef Heap::fromList<toList<Two,Three,One,Four,Zero,Five,Six> > h;
typedef Prelude::iterate<Heap::deleteMin<>,h> hs;

ERASM_META_ASSERT_EQUAL((Heap::min<h>),(Zero));
ERASM_META_ASSERT_EQUAL((Heap::min<index<hs,One> >),(One));
ERASM_META_ASSERT_EQUAL((Heap::min<index<hs,Two> >),(Two));
ERASM_META_ASSERT_EQUAL((Heap::min<index<hs,Three> >),(Three));
ERASM_META_ASSERT_EQUAL((Heap::min<index<hs,Four> >),(Four));
ERASM_META_ASSERT_EQUAL((Heap::min<index<hs,Five> >),(Five));
ERASM_META_ASSERT_EQUAL((Heap::min<index<hs,Six> >),(Six));

}}



}}

#endif // MY_META_LEFTIST_HEAP
