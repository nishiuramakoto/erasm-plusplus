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
#ifndef MY_META_POLYNOMIAL
#define MY_META_POLYNOMIAL

// Meta-implementation of Free X-algebra where X=int_,bool_,etc.

#include "meta_prelude.hpp"
 
namespace erasm { namespace prelude {

DEFINE_DATA2(Term);
DEFINE_DATA1(Polynomial);

DEFINE_FUNC_LAZY(monomial);

DEFINE_FUNC_STRICT(totalDegree);
DEFINE_FUNC_STRICT_STRICT(lessLex);
DEFINE_FUNC_STRICT_STRICT(lessRevlex);
DEFINE_FUNC_LAZY_LAZY(lessGrlex);
DEFINE_FUNC_LAZY_LAZY(lessGrevlex);
DEFINE_FUNC_STRICT(termList);
DEFINE_FUNC_STRICT(termDegree);
DEFINE_FUNC_STRICT(termCoefficient);
DEFINE_FUNC2(nthTermDegree,X,Y,
	      (termDegree<index<termList<X>,Y> >));
DEFINE_FUNC2(nthTermCoefficient,X,Y,
	      (termCoefficient<index<termList<X>,Y> >));


// private functions
DEFINE_FUNC_STRICT_STRICT(addSortedTermList);

template<class P>
struct eval<monomial<P> >
{
   typedef Polynomial<Cons<Term<P,One>,Nil> > result;
};

typedef Polynomial<Nil> ZeroPolynomial;

template<class X,class C,class Y,class D>
struct eval<equal<Term<X,C>,Term<Y,D> > >
{
   typedef EVAL_TEMPLATE((and2<equal<X,Y>,equal<C,D> >)) result;
};




template<class X,class Y>
struct eval<termDegree<Term<X,Y> > >
{
   typedef EVAL_TEMPLATE((X)) result;
};



template<class X,class Y>
struct eval<termCoefficient<Term<X,Y> > >
{
   typedef EVAL_TEMPLATE((Y)) result;
};




template<class N,class X,class C>
struct eval<times<N,Term<X,C> > >
{
   ERASM_META_ASSERT(( notEq<C,Zero> ));
   typedef Term<X,times<N,C> > result;
};

template<class X,class C>
struct eval<negate<Term<X,C> > >
{
   typedef Term<X,negate<C> > result;
};




template<class X,class C>
struct eval<totalDegree<Term<X,C> > >
{
   ERASM_META_ASSERT(( notEq<C,Zero> ));
   typedef EVAL_TEMPLATE((totalDegree<X>)) result;
};

template<class I,class J>
struct eval<totalDegree<Pair<I,J> > >
{
   typedef EVAL_TEMPLATE((plus<I,J>)) result;
};


template<class X,class C,class Y,class D>
struct eval<lessLex<Term<X,C>,Term<Y,D> > >
{
   ERASM_META_ASSERT(( notEq<C,Zero> ));
   ERASM_META_ASSERT(( notEq<D,Zero> ));
   typedef EVAL_TEMPLATE((lessLex<X,Y>)) result;
};

template<class i1,class j1,class i2,class j2>
struct eval<lessLex<Pair<i1,j1>,Pair<i2,j2> > >
{
   typedef EVAL_TEMPLATE((if2< less<i1,i2> , True,
			       less<i2,i1> , False,
			       less<j1,j2> >)) result;
};

template<int x,int y>
struct eval<lessLex<Int<x>,Int<y> > >
{
   typedef Bool< COMPARE_LESS(x,y)  > result; 
};



template<class i1,class j1,class i2,class j2>
struct eval<lessRevlex<Pair<i1,j1>,Pair<i2,j2> > >
{
   typedef EVAL_TEMPLATE((if2< less<j1,j2> , False,
			       less<j2,j1> , True,
			       less<i2,i1> >)) result;
};



template<class X,class Y>
struct eval<lessGrlex<X,Y> >
{
   typedef totalDegree<X> degX;
   typedef totalDegree<Y> degY;

   typedef EVAL_TEMPLATE((if2<less<degX,degY> , True,
			      greater<degX,degY> , False,
			      lessLex<X,Y> >)) result;
};


template<class X,class Y>
struct eval<lessGrevlex<X,Y> >
{
   typedef totalDegree<X> degX;
   typedef totalDegree<Y> degY;

   typedef EVAL_TEMPLATE((if2<less<degX,degY> , True,
			      greater<degX,degY> , False,
			      lessRevlex<X,Y> >)) result;
};


namespace test{
ERASM_META_ASSERT(( lessLex< Pair<Zero,One > , Pair<Zero,Two> > ));
ERASM_META_ASSERT(( lessLex< Pair<Zero,One > , Pair<One,Zero> > ));
ERASM_META_ASSERT(( lessLex< Term<Pair<Zero,One >,One> , Term<Pair<One,Zero>,Two>  > ));
}

template<class X,class C,class Y,class D>
struct eval<less<Term<X,C>,Term<Y,D> > >
{
   ERASM_META_ASSERT(( notEq<C,Zero> ));
   ERASM_META_ASSERT(( notEq<D,Zero> ));
   typedef EVAL_TEMPLATE((lessLex<X,Y>)) result;
};



template<class N,class L>
struct eval<times<N,Polynomial<L> > >
{
   typedef EVAL_TEMPLATE((if_< equal<N,Zero> , Polynomial<Nil>,
			       Polynomial<map<Function2_1<times,N>,L> > >)) result;

};



template<class L1>
struct eval<termList<Polynomial<L1> > >
{
   typedef EVAL_TEMPLATE((L1)) result;
};


template<>
struct eval<addSortedTermList<Nil,Nil> >
{
   typedef Nil result;
};

template<class X>
struct eval<addSortedTermList<Nil,X> >
{
   typedef EVAL_TEMPLATE((X)) result;
};
template<class X>
struct eval<addSortedTermList<X,Nil> >
{
   typedef EVAL_TEMPLATE((X)) result;
};

template<class M,class MS,class N,class NS>
struct eval<addSortedTermList<Cons<M,MS>,Cons<N,NS> > >
{
   typedef termCoefficient<M> cm;
   typedef termCoefficient<N> cn;
   typedef termDegree<M> xd;

   ERASM_META_ASSERT(( notEq<cm,Zero> ));
   ERASM_META_ASSERT(( notEq<cn,Zero> ));

   typedef EVAL_TEMPLATE((if3<less<N,M>, Cons<M, addSortedTermList<MS,Cons<N,NS> > >,
			      less<M,N>, Cons<N, addSortedTermList<Cons<M,MS>,NS> >,
			      equal<Zero,plus<cm,cn> > , addSortedTermList<MS,NS>,
			      Cons<Term<xd, plus<cm,cn> >,
				   addSortedTermList<MS,NS> > >)) result;
};



template<class L1,class L2>
struct eval<plus<Polynomial<L1>,Polynomial<L2> > >
{
   typedef Polynomial<addSortedTermList<L1,L2> > result;
};

template<class X,class C,class Y,class D>
struct eval<plus<Term<X,C>,Term<Y,D> > >
{
   typedef EVAL_TEMPLATE((plus<Polynomial<toList<Term<X,C> > >,Polynomial<toList<Term<Y,D> > > >)) result;
};


template<class L1,class L2>
struct eval<equal<Polynomial<L1>,Polynomial<L2> > >
{
   typedef EVAL_TEMPLATE((equal<L1,L2>)) result;
};

template<class X,class C,class L>
struct eval<plus<Term<X,C>,Polynomial<L> > >
{
   typedef EVAL_TEMPLATE((plus<Polynomial<toList<Term<X,C> > >,Polynomial<L> >)) result;
};

template<class X,class C,class L>
struct eval<plus<Polynomial<L>,Term<X,C> > >
{
   typedef EVAL_TEMPLATE((plus<Polynomial<L>,Polynomial<toList<Term<X,C> > > >)) result;
};


template<class L1,class X,class C>
struct eval<equal<Polynomial<L1>,Term<X,C> > >
{
   typedef EVAL_TEMPLATE((equal<L1,toList<Term<X,C> > >)) result;
};

template<class L1,class X,class C>
struct eval<equal<Term<X,C>,Polynomial<L1> > >
{
   typedef EVAL_TEMPLATE((equal<L1,toList<Term<X,C> > >)) result;
};

}}


// testing
namespace erasm { namespace prelude {

namespace test {
typedef Term<Pair<One,Zero>,One > x;
typedef Term<Pair<Zero,One>,One > y;

typedef ZeroPolynomial  p_0;
typedef x p_x;
typedef times<Two,x> p_2x;
typedef negate<p_2x> p_m2x;
typedef times<Three,x>  p_3x;
typedef times<Four,x>  p_4x;
typedef times<Five,x>  p_5x;

typedef y p_y;
typedef times<Two,y> p_2y;
typedef negate<p_2y> p_m2y;
typedef times<Three,y>  p_3y;
typedef times<Four,y>  p_4y;
typedef times<Five,y>  p_5y;

typedef plus<p_2x,p_2y> p_2x_2y;
typedef plus<p_x,p_y> p_x_y;

ERASM_META_ASSERT_EQUAL((plus<p_2x,p_3x>),(p_5x));
ERASM_META_ASSERT_EQUAL((plus<p_0,p_2x>),(p_2x));
ERASM_META_ASSERT_EQUAL((head<termList<p_2x_2y> >),(p_2x));
ERASM_META_ASSERT_EQUAL((head<tail<termList<p_2x_2y> > >),(p_2y));
ERASM_META_ASSERT_EQUAL((length<termList<p_2x_2y> >),(Two));
ERASM_META_ASSERT_EQUAL((plus<p_2x,p_m2x>),(p_0));

ERASM_META_ASSERT_EQUAL((times<Zero,p_x_y>),(p_0));
ERASM_META_ASSERT_EQUAL((times<Two,p_x_y>),(p_2x_2y));

ERASM_META_ASSERT_EQUAL((plus<plus <p_2x_2y,p_m2x>,p_m2y>),(p_0));

}

}}


#endif // MY_META_POLYNOMIAL
