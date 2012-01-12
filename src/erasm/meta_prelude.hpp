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
#ifndef MY_META_PRELUDE_HPP
#define MY_META_PRELUDE_HPP

/*
   Set ERASM_NO_META_ASSERT to disable static error checking
   #define ERASM_NO_META_ASSERT
*/

#if defined NDEBUG && ! defined ERASM_NO_META_ASSERT
#define ERASM_NO_META_ASSERT
#endif

#if ! defined(ERASM_NO_META_ASSERT)
#warning  Static checking of meta_prelude is enabled.
#warning  It will take time,so consider disabling it with
#warning  #define ERASM_NO_META_ASSERT
#warning  or just
#warning  #define NDEBUG
#warning  before #include-ing meta_prelude.hpp.
#endif


#include "meta_prelude_core.hpp"


namespace erasm { namespace prelude {

/*
  Bool (defined in the core library)
*/


// Int type

DEFINE_DATA1_BUILTIN(Int,int);
template<int n>
struct Int
{
   Int() {}
   enum { value= n };
   typedef Int type;
};


// A desperate attempt to reduce the amount of silly angle brackets..
typedef Int<-1> MinusOne;
typedef Int<0> Zero;
typedef Int<1> One;
typedef Int<2> Two;
typedef Int<3> Three;
typedef Int<4> Four;
typedef Int<5> Five;
typedef Int<6> Six;
typedef Int<7> Seven;
typedef Int<8> Eight;
typedef Int<9> Nine;
typedef Int<10> Ten;



/*
  Tuples
*/

DEFINE_DATA2(Pair);

DEFINE_FUNC_STRICT(fst);

template<class X,class Y>
struct eval<fst<Pair<X,Y> > >
{
   typedef EVAL_TEMPLATE((X)) result;
};


DEFINE_FUNC_STRICT(snd);

template<class X,class Y>
struct eval<snd<Pair<X,Y> > >
{
   typedef EVAL_TEMPLATE((Y)) result;
};




/*
   List
*/

DEFINE_DATA2(Cons);
DEFINE_DATA0(Nil);



// class Eq where
DEFINE_FUNC_STRICT_STRICT(equal);
DEFINE_FUNC2(notEq,X,Y,
	     (not_<equal<X,Y> >));


// instance Eq Bool where
template<bool b,bool c>
struct eval<equal<Bool<b>,Bool<c> > >
{
   typedef Bool<b==c> result;
};

// Let's hope this helps the compiler (although strictly unnecessary)
template<bool b,bool c>
struct eval<notEq<Bool<b>,Bool<c> > >
{
   typedef Bool<b!=c> result;
};

ERASM_META_ASSERT_EQUAL((True),(True));
ERASM_META_ASSERT_NOT_EQUAL((True),(False));


// instance Eq Int where
template<int x,int y>
struct eval<equal<Int<x>,Int<y> > >
{
   typedef Bool<x==y> result;
};

// TODO:Check whether this actually decreases compilation time
template<int x,int y>
struct eval<notEq<Int<x>,Int<y> > >
{
   typedef Bool<x!=y> result;
};

ERASM_META_ASSERT_EQUAL((Int<0>),(Int<0>));
ERASM_META_ASSERT_NOT_EQUAL((Int<0>),(Int<1>));


// instance Eq Tuple where

template<class X,class Y,class Z,class W>
struct eval<equal<Pair<X,Y>,Pair<Z,W> > >
{
   typedef  EVAL_TEMPLATE((and2<equal<X,Z>,equal<Y,W> >)) result;
};

ERASM_META_ASSERT_EQUAL((fst<Pair<True,Bottom> >),(True));
ERASM_META_ASSERT_EQUAL((snd<Pair<Bottom,True> >),(True));



// instance Eq List where
template<>
struct eval<equal<Nil,Nil> >
{
   typedef True result;
};

template<class X,class Y>
struct eval<equal<Cons<X,Y>,Nil> >
{
   typedef False result;
};

template<class X,class Y>
struct eval<equal<Nil,Cons<X,Y> > >
{
   typedef False result;
};


template<class X,class Y,class Z,class W>
struct eval<equal<Cons<X,Y>,Cons<Z,W> > >
{
   typedef typename eval<and2<equal<X,Z>,equal<Y,W> > >::result result;
};



// class Ord
DEFINE_FUNC_STRICT_STRICT(compare);
DEFINE_FUNC_STRICT_STRICT(less);
DEFINE_FUNC_STRICT_STRICT(lessEq);
DEFINE_FUNC_STRICT_STRICT(greater);
DEFINE_FUNC_STRICT_STRICT(greaterEq);
DEFINE_FUNC_STRICT_STRICT(max);
DEFINE_FUNC_STRICT_STRICT(min);


// instance Ord Int where
template<int x,int y>
struct eval<less<Int<x>,Int<y> > >
{
   typedef Bool<COMPARE_LESS(x,y)> result;
};

template<int x,int y>
struct eval<lessEq<Int<x>,Int<y> > >
{
   typedef Bool<COMPARE_LESS_EQ(x,y)> result;
};

template<int x,int y>
struct eval<greater<Int<x>,Int<y> > >
{
   typedef Bool<COMPARE_GREATER(x,y)> result;
};

template<int x,int y>
struct eval<greaterEq<Int<x>,Int<y> > >
{
   typedef Bool<COMPARE_GREATER_EQ(x,y)> result;
};


// class Num
// This class should clearly be devived into several subclassess
// e.g. Group,Abelian,Ring,etc.
DEFINE_FUNC_STRICT_STRICT(plus);
DEFINE_FUNC_STRICT_STRICT(minus);
DEFINE_FUNC_STRICT(negate);

DEFINE_FUNC_STRICT_STRICT(times);

DEFINE_FUNC_STRICT(abs);
DEFINE_FUNC_STRICT(signum);
DEFINE_FUNC_STRICT(fromInteger);


//Class Real
DEFINE_FUNC_STRICT(toRational);

// Class Integral
// Minimul complete definition: quotRem and toInteger
DEFINE_FUNC_STRICT(toInteger);
DEFINE_FUNC_STRICT_STRICT(quotRem);
DEFINE_FUNC_STRICT_STRICT(divMod);

DEFINE_FUNC2(quot,X,Y,(fst<quotRem<X,Y> >));
DEFINE_FUNC2(rem, X,Y,(snd<quotRem<X,Y> >));
DEFINE_FUNC2(div, X,Y,(fst<divMod<X,Y> >));
DEFINE_FUNC2(mod, X,Y,(snd<divMod<X,Y> >));



// instance Num Int where

template<int x,int y>
struct eval<plus<Int<x>,Int<y> > >
{
   typedef Int<x+y> result;
};


template<int x,int y>
struct eval<minus<Int<x>,Int<y> > >
{
   typedef Int<x-y> result;
};


template<int x,int y>
struct eval<times<Int<x>,Int<y> > >
{
   typedef Int<x*y> result;
};

template<int x>
struct eval<negate<Int<x> > >
{
   typedef Int<-x> result;
};

template<int x,int y>
struct eval<divMod<Int<x>,Int<y> > >
{
   typedef impl::DivMod<x,y> divmod;
   typedef Pair<Int<divmod::div>,Int<divmod::mod> > result;
};

template<int x,int y>
struct eval<quotRem<Int<x>,Int<y> > >
{
   ERASM_META_ASSERT((notEq<Int<y>,Zero>));
   BOOST_STATIC_ASSERT((y!=0));

   typedef impl::QuotRem<x,y> quotrem;
   typedef Pair<Int<quotrem::quot>,Int<quotrem::rem> > result;
};



namespace test {
ERASM_META_ASSERT_EQUAL((One),(One));
ERASM_META_ASSERT_EQUAL((plus<One,Two>),(Three));
ERASM_META_ASSERT_EQUAL((minus<Two,One>),(One));
ERASM_META_ASSERT_EQUAL((times<Two,Two>),(Four));
ERASM_META_ASSERT(( less<One,Two> ));
ERASM_META_ASSERT(( lessEq<One,One> ));
ERASM_META_ASSERT(( greater<Two,One> ));
ERASM_META_ASSERT(( greaterEq<Two,Two> ));
}

DERIVING_ABELIAN2(Pair);
DERIVING_Z_MODULE2(Pair);

namespace test {
ERASM_META_ASSERT_EQUAL((plus<Pair<One,Two>,Pair<Three,Four> >),
		       (Pair<Four,Six>));
ERASM_META_ASSERT_EQUAL((minus<Pair<One,Two>,Pair<Three,Four> >),
		       (Pair<negate<Two>,negate<Two> >));

ERASM_META_ASSERT_EQUAL((minus<Pair<One,Two>,Pair<Three,Four> >),
		       (negate<Pair<Two,Two> >));

ERASM_META_ASSERT_EQUAL((times<Two,Pair<One,Two> >),
		       (Pair<Two,Four> ));
ERASM_META_ASSERT_EQUAL((times<Pair<One,Two>,Two>),
		       (Pair<Two,Four> ));
}



// Class Bounded
// Unfortunately , we have to define these two methods as 
// constant functions.
DEFINE_FUNC_STRICT(minBound);
DEFINE_FUNC_STRICT(maxBound);

// instance Bool
// instance Char
// instance Int
// instance Int8
// instance Int32
// instance Int64
// instance Ordering <- I hate it
// instance Word8
// instance Word16
// instance Word32
// instance Word64
// instance (Bound a,Bounded b) =>  Bounded (a,b)
// etc..

// Class Enum
//      The calls succ maxBound and pred minBound should result in a runtime error.
// fromEnum and toEnum should give a runtime error if the result value is not representable in the result type. For example, toEnum 7 :: Bool is an error.
//    enumFrom and enumFromThen should be defined with an implicit bound, thus: 

//    enumFrom     x   = enumFromTo     x maxBound
//    enumFromThen x y = enumFromThenTo x y bound
//    where
//    bound | fromEnum y >= fromEnum x = maxBound
//    | otherwise                = minBound


DEFINE_FUNC_STRICT(succ);
DEFINE_FUNC_STRICT(pred);
DEFINE_FUNC_STRICT(toEnum);
DEFINE_FUNC_STRICT(fromEnum);
DEFINE_FUNC_STRICT(enumFrom);
DEFINE_FUNC_STRICT(enumFromThen);
DEFINE_FUNC_STRICT(enumFromTo);
DEFINE_FUNC_STRICT(enumFromThenTo);

// instances
// Enum Bool	 
// Enum Char	 
// Enum Double	 
// Enum Float	 
// Enum Int	 
// Enum Int8	 
// Enum Int16	 
// Enum Int32	 
// Enum Int64	 
// Enum Integer	 
// Enum Ordering	 
// Enum Word	 
// Enum Word8	 
// Enum Word16	 
// Enum Word32	 
// Enum Word64	 
// Enum ()	 
// Enum IOMode	 
// Enum SeekMode	 
// Enum CUIntMax	 
// Enum CIntMax	 
// Enum CUIntPtr	 
// Enum CIntPtr	 
// Enum CSUSeconds	 
// Enum CUSeconds	 
// Enum CTime	 
// Enum CClock	 
// Enum CSigAtomic	 
// Enum CWchar	 
// Enum CSize	 
// Enum CPtrdiff	 
// Enum CDouble	 
// Enum CFloat	 
// Enum CULLong	 
// Enum CLLong	 
// Enum CULong	 
// Enum CLong	 
// Enum CUInt	 
// Enum CInt	 
// Enum CUShort	 
// Enum CShort	 
// Enum CUChar	 
// Enum CSChar	 
// Enum CChar	 
// Enum GeneralCategory	 
// Enum IntPtr	 
// Enum WordPtr	 
// Enum Fd	 
// Enum CRLim	 
// Enum CTcflag	 
// Enum CSpeed	 
// Enum CCc	 
// Enum CUid	 
// Enum CNlink	 
// Enum CGid	 
// Enum CSsize	 
// Enum CPid	 
// Enum COff	 
// Enum CMode	 
// Enum CIno	 
// Enum CDev	 
// Integral a => Enum (Ratio a)	 
//    Enum (Fixed a)	 







template<class X0=Nil,class X1=Nil,class X2=Nil,class X3=Nil,class X4=Nil,class X5=Nil,class X6=Nil,class X7=Nil>
struct toList;


template<>
struct eval<toList<> >
{
   typedef Nil result;
};

template<class X0,class X1,class X2,class X3,class X4,class X5,class X6,class X7>
struct eval<toList<X0,X1,X2,X3,X4,X5,X6,X7> >
{
   typedef Cons<X0,toList<X1,X2,X3,X4,X5,X6,X7> > result;
};


namespace test {
ERASM_META_ASSERT_EQUAL((toList<>),(Nil));
ERASM_META_ASSERT_EQUAL((toList<True>),(Cons<True,Nil>));
ERASM_META_ASSERT_NOT_EQUAL((toList<True>),(Cons<True,Cons<False,Nil> >));
ERASM_META_ASSERT_EQUAL((toList<True,False>),(Cons<True,Cons<False,Nil> >));
ERASM_META_ASSERT_NOT_EQUAL((toList<True,False>),(toList<True>));
}

DEFINE_FUNC_STRICT(head);

template<>
struct eval<head<Nil> >
{
   typedef EVAL((undefined)) result;
};

template<class X,class Y>
struct eval<head<Cons<X,Y> > >
{
   typedef typename  eval<X>::result result;
};




DEFINE_FUNC_STRICT(tail);

template<>
struct eval<tail<Nil> >
{
   typedef EVAL((undefined)) result;
};

template<class X,class Y>
struct eval<tail<Cons<X,Y> > >
{
   typedef typename eval<Y>::result result;
};



DEFINE_FUNC_STRICT(null);

template<>
struct eval<null<Nil> >
{
   typedef True result;
};

template<class X,class Y>
struct eval<null<Cons<X,Y> > >
{
   typedef False result;
};


namespace test {
ERASM_META_ASSERT_EQUAL((null<toList<> >),(True));
ERASM_META_ASSERT_EQUAL((null<toList<True> >),(False));
}


DEFINE_FUNC_STRICT_STRICT(append);

template<class YS>
struct eval<append<Nil,YS> >
{
   typedef EVAL_TEMPLATE((YS)) result;
};

template<class X,class XS,class YS>
struct eval<append<Cons<X,XS>,YS> >
{
   typedef Cons<X,append<XS,YS> > result;
};



DEFINE_FUNC_LAZY_STRICT(elem);


template<class X>
struct eval<elem<X,Nil> >
{
   typedef False result;
};

template<class X,class C,class CS>
struct eval<elem<X,Cons<C,CS> > >
{
   typedef EVAL_TEMPLATE((if_<equal<X,C>,True,elem<X,CS> >)) result;
};

namespace test {
ERASM_META_ASSERT((elem<Four,toList<One,Two,Three,Four,Bottom> >));
}



DEFINE_FUNC_STRICT(length);

template<>
struct eval<length<Nil> >
{
   typedef Zero result;
};

template<class X,class XS>
struct eval<length<Cons<X,XS> > >
{
   typedef EVAL_TEMPLATE((plus<One,length<XS> >)) result;
};

namespace test{
ERASM_META_ASSERT_EQUAL((Two),(length<toList<One,Two> > ));
ERASM_META_ASSERT_EQUAL((Zero),(length<toList<> > ));
}


DEFINE_FUNC_STRICT_STRICT(index);
template<class N>
struct eval<index<Nil,N> >
{
   struct index_out_of_range;
   typedef error<index_out_of_range> result;
};

template<class X,class XS>
struct eval<index<Cons<X,XS>,Zero> >
{
   typedef EVAL_TEMPLATE((X)) result;
};

template<class X,class XS,int n>
struct eval<index<Cons<X,XS>,Int<n> > >
{
   typedef EVAL_TEMPLATE((index<XS,Int<n-1> >)) result;
};

namespace test {
ERASM_META_ASSERT_EQUAL((index<Cons<One,Nil> , Zero>) ,(One));
ERASM_META_ASSERT_EQUAL((index<toList<One,Two,Three> , One>) ,(Two));
}


DEFINE_FUNC_STRICT(and_);

template<>
struct eval<and_<Nil> >
{
   typedef True  result;
};

template<class X,class XS>
struct eval<and_<Cons<X,XS> > >
{
   typedef EVAL_TEMPLATE((if_<X , and_<XS> , False >)) result;
};


DEFINE_FUNC_STRICT(or_);

template<>
struct eval<or_<Nil> >
{
   typedef False  result;
};

template<class X,class XS>
struct eval<or_<Cons<X,XS> > >
{
   typedef EVAL_TEMPLATE((if_<X , True , or_<XS>  >)) result;
};


DEFINE_FUNC_LAZY_STRICT(map);

template<class F>
struct eval<map<F,Nil> >
{
   typedef Nil  result;
};

template<class F,class X,class XS>
struct eval<map<F,Cons<X,XS> > >
{
   typedef Cons<apply1<F,X>,map<F,XS> > result;
};


DEFINE_FUNC_LAZY_LAZY_STRICT(foldr);

template<class K,class Z>
struct eval<foldr<K,Z,Nil> >
{
   typedef Z result;
};

template<class K,class Z,class Y,class YS>
struct eval<foldr<K,Z,Cons<Y,YS> > >
{
   typedef  apply2<K,Y,foldr<K,Z,YS> > result;
};



namespace impl {
template<class X,class A,class B> struct inRange;
}
template<class X,class A,class B>
struct eval<impl::inRange<X,A,B> >
{
   typedef EVAL_TEMPLATE((and2<greaterEq<X,A>,less<X,B> >)) result;
};

namespace impl {
ERASM_META_ASSERT((inRange<Zero,Zero,One>));
ERASM_META_ASSERT((not_<inRange<One,Zero,One> >));
ERASM_META_ASSERT_IN_RANGE((One),(Zero),(Two));
}



// stupid,but can reduce brackets a bit
DEFINE_FUNC3(and3,X,Y,Z,
	     (and2<X,and2<Y,Z> >));


// Numeric functions

DEFINE_FUNC2(subtract,X,Y,(minus<Y,X>));
DEFINE_FUNC(even,X,(equal<mod<X,Two>,Zero>));
DEFINE_FUNC(odd, X,(notEq<mod<X,Two>,Zero>));
DEFINE_FUNC(gcd,X,(undefined));
DEFINE_FUNC(lcm,X,(undefined));

namespace test {
ERASM_META_ASSERT(( even<Two> ));
ERASM_META_ASSERT(( odd<One> ));
ERASM_META_ASSERT(( odd<Three> ));
ERASM_META_ASSERT(( even<times<Two,Three> > ));
}


// Some trivial functions
DEFINE_FUNC(id,X,(X));
DEFINE_FUNC2(const_,C,X,(C));









namespace test {
struct CompileError;
ERASM_META_ASSERT_EQUAL((if_<id<True>,One,error<Two> >),(One));
ERASM_META_ASSERT_EQUAL((if_<id<False>,error<CompileError>,Two >),(Two));

}

BOOST_MPL_ASSERT(( boost::is_same<EVAL((apply1<Function1_0<id>,One>)),One> ));
BOOST_MPL_ASSERT(( boost::is_same<EVAL((apply1<Function2_1<const_,One>,Two>)),One> ));
BOOST_MPL_ASSERT(( boost::is_same<EVAL((apply2<Function2_0<const_>,One,Two>)),One> ));


namespace test {

ERASM_META_ASSERT_EQUAL((id<One>),(One));
ERASM_META_ASSERT_NOT_EQUAL((id<One>),(Two));
ERASM_META_ASSERT_EQUAL((apply1<id<>,One>),(One));

ERASM_META_ASSERT_EQUAL((const_<One,Two>),(One));
ERASM_META_ASSERT_EQUAL((apply1<const_<One>,Two>),(One));
ERASM_META_ASSERT_EQUAL((apply2<const_<>,One,Two>),(One));

ERASM_META_ASSERT_EQUAL(( or2<False,False>)		,(False));
ERASM_META_ASSERT_EQUAL(( or2<True,True>)		,(True));
ERASM_META_ASSERT_EQUAL(( or2<False,True>)		,(True));
ERASM_META_ASSERT_EQUAL(( or2<True,False>)		,(True));
ERASM_META_ASSERT_EQUAL(( or2<id<True>,Bottom >)	,(True));

ERASM_META_ASSERT_EQUAL(( and2<False,False>) ,(False));
ERASM_META_ASSERT_EQUAL(( and2<True ,True >) ,(True));
ERASM_META_ASSERT_EQUAL(( and2<False,True >) ,(False));
ERASM_META_ASSERT_EQUAL(( and2<True ,False> ),(False));
ERASM_META_ASSERT_EQUAL(( and2<id<True> ,id<False> >) ,(False));

ERASM_META_ASSERT_EQUAL(( and_<Nil> ) , (True) );
ERASM_META_ASSERT_EQUAL(( and_<toList<equal<One,One>,equal<One,One> > > ) ,
		       (True) );
ERASM_META_ASSERT_EQUAL(( and_<toList<equal<One,One>,equal<One,Two> > > ) ,
		       (False) );

ERASM_META_ASSERT_EQUAL(( or_<Nil> ) , (False) );
ERASM_META_ASSERT_EQUAL(( or_<toList<equal<One,One>,equal<One,Two> > > ) ,
		       (True) );
ERASM_META_ASSERT_EQUAL(( or_<toList<equal<One,Two>,equal<One,Two> > > ) ,
		       (False) );


ERASM_META_ASSERT_EQUAL((map<Function1_0<id>,toList<One,Two> >),
		       (toList<One,Two>));
ERASM_META_ASSERT_EQUAL((map<id<>,toList<One,Two> >),(toList<One,Two>));
ERASM_META_ASSERT_EQUAL((map<plus<One>,toList<One,Two> >),(toList<Two,Three>));
ERASM_META_ASSERT_EQUAL((map<and3<True,True>,toList<True,False> >),(toList<True,False>));


ERASM_META_ASSERT_EQUAL((foldr<plus<>,Zero,toList<One,Two,Three> >),
		       (Six));
}



DEFINE_FUNC2(any,P,XS,(if_<null<XS>,
			   False,
			   or2<apply1<P,head<XS> >,any<P,tail<XS> > > >));

DEFINE_FUNC2(all,P,XS,(if_<null<XS>,
			   True,
			   and2<apply1<P,head<XS> >,all<P,tail<XS> > > >));


namespace test {
ERASM_META_ASSERT_EQUAL((less<Two,Four>),(True));
ERASM_META_ASSERT_EQUAL((less<Two,Two>),(False));
ERASM_META_ASSERT_EQUAL((any<less<Two>  , toList <One,Three> >),(True));
ERASM_META_ASSERT_EQUAL((any<less<Two>  , toList <Two,Three> >),(True));
ERASM_META_ASSERT_EQUAL((any<less<Two>  , toList <Two,One> >),(False));
ERASM_META_ASSERT_EQUAL((all<less<Two>  , toList <Two,Three> >),(False));
ERASM_META_ASSERT_EQUAL((all<less<Two>  , toList <Three,Four> >),(True));


}


/* Maybe */
DEFINE_DATA1(Just);
DEFINE_DATA0(Nothing);

// Eq a => instance Eq (Maybe a) where

template<class b,class c>
struct eval<equal<Just<b>,Just<c> > >
{
   typedef typename eval<equal<b,c> >::result result;
};

template<class b>
struct eval<equal<Just<b>,Nothing > >
{
   typedef False result;
};

template<class b>
struct eval<equal<Nothing,Just<b> > >
{
   typedef False result;
};

template<>
struct eval<equal<Nothing,Nothing > >
{
   typedef True result;
};

namespace test {

ERASM_META_ASSERT_EQUAL((Just<One>),(Just<One>));
ERASM_META_ASSERT_NOT_EQUAL((Just<One>),(Just<Two>));
ERASM_META_ASSERT_NOT_EQUAL((Just<One>),(Nothing));
ERASM_META_ASSERT_EQUAL((Nothing),(Nothing));

}


}}


#include "meta_prelude_ifn.hpp"

#endif // MY_META_PRELUDE_HPP
