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
#ifndef MY_X64_ADDR64
#define MY_X64_ADDR64

#include  <boost/type_traits/is_same.hpp>
#include "erasm/x64_common.hpp"

#include "meta_polynomial.hpp"
#include "meta_prelude_ifn.hpp"
#include <limits.h>
#include <assert.h>

#define X64_DEFINE_DATA1(DATACON)			\
  namespace x64 {					\
  template<class X=_> struct DATACON;			\
  }							\
  DEFINE_FOREIGN_DATA1(x64::DATACON,class)

#define X64_DEFINE_DATA2(DATACON)				\
  namespace x64 {						\
  template<class X=_,class Y=_> struct DATACON;			\
  }								\
  DEFINE_FOREIGN_DATA2(x64::DATACON,class,class)

#define X64_DEFINE_DATA3(DATACON)					\
  namespace x64 {							\
  template<class X=_,class Y=_,class Z=_> struct DATACON;		\
  }									\
  DEFINE_FOREIGN_DATA3(x64::DATACON,class,class,class)

#define X64_DEFINE_DATA4(DATACON)				\
   namespace x64 {						\
   template<class X=_,class Y=_,class Z=_,class W=_> struct DATACON;	\
   }								\
   DEFINE_FOREIGN_DATA4(x64::DATACON,class,class,class,class)


#define X64_DEFINE_DATA1_DERIVING_EQ(DATACON)	\
  X64_DEFINE_DATA1(DATACON);			\
  DERIVING_EQ1(x64::DATACON)

#define X64_DEFINE_DATA2_DERIVING_EQ(DATACON)	\
  X64_DEFINE_DATA2(DATACON);			\
  DERIVING_EQ2(x64::DATACON)

#define X64_DEFINE_DATA3_DERIVING_EQ(DATACON)	\
  X64_DEFINE_DATA3(DATACON);			\
  DERIVING_EQ3(x64::DATACON)

#define X64_DEFINE_DATA4_DERIVING_EQ(DATACON)	\
   X64_DEFINE_DATA4(DATACON);			\
   DERIVING_EQ4(x64::DATACON)

#define X64_DECLARE_FUNC1(FUNC)			\
   namespace x64 {				\
   template<class X=_>  struct FUNC;		\
   }						

#define X64_DEFINE_FUNC_STRICT(FUNC)			\
  namespace x64 {					\
  template<class X=_>  struct FUNC;			\
  }							\
  DEFINE_PATTERN_MATCHER_STRICT(x64::FUNC)

#define X64_DEFINE_FUNC X64_DEFINE_FUNC1
#define X64_DEFINE_FUNC1(FUNC,ARG,PAREN_BODY)		\
  namespace x64 {					\
  template<class X=_>  struct FUNC;			\
  }							\
  DEFINE_FUNC_BODY1(x64::FUNC,ARG,PAREN_BODY)

#define X64_DEFINE_FUNC_STRICT_STRICT(FUNC)			\
  namespace x64 {						\
  template<class X=_,class Y=_>  struct FUNC;			\
  }								\
  DEFINE_PATTERN_MATCHER_STRICT_STRICT(x64::FUNC)

#define X64_DEFINE_FUNC2(FUNC,ARG1,ARG2,PAREN_BODY)			\
  namespace x64 {							\
  template<class X=_,class Y=_>  struct FUNC;				\
  }									\
  DEFINE_FUNC_BODY2(x64::FUNC,ARG1,ARG2,PAREN_BODY)

#define X64_DEFINE_FUNC3(FUNC,ARG1,ARG2,ARG3,PAREN_BODY)	\
  namespace x64 {						\
  template<class X=_,class Y=_,class Z=_>  struct FUNC;		\
  }								\
  DEFINE_FUNC_BODY3(x64::FUNC,ARG1,ARG2,ARG3,PAREN_BODY)


// Precondition for this module to work
// I'm not sure whether this can be relaxed in a meaningful way

BOOST_STATIC_ASSERT(CHAR_MIN == -128);
BOOST_STATIC_ASSERT(CHAR_MAX ==  127);
BOOST_STATIC_ASSERT(SHRT_MIN ==	-32768);
BOOST_STATIC_ASSERT(SHRT_MAX ==	 32767);
BOOST_STATIC_ASSERT(INT_MIN  ==	(-INT_MAX - 1));
BOOST_STATIC_ASSERT(INT_MAX  ==	2147483647);


namespace erasm { namespace prelude {
namespace x64 {
using namespace erasm::x64;

template<int i>
struct Disp;

template<>
struct Disp<0>
{
   enum { size = 0 };
   typedef int value_type;
};

template<>
struct Disp<8>
{
   enum { size = 1 };
   typedef disp8_type value_type;
};

template<>
struct Disp<16>
{
   enum { size = 2 };
   typedef disp16_type value_type;
};

template<>
struct Disp<32>
{
   enum { size = 4 };
   typedef disp32_type value_type;
};

typedef Disp<0>  NoDisp;
typedef Disp<8>  Disp8;
typedef Disp<16> Disp16;
typedef Disp<32> Disp32;

}

DEFINE_FOREIGN_DATA1(x64::Disp,int);

template<int n,int m>
struct eval<plus<x64::Disp<n>,x64::Disp<m> > >
{
   typedef EVAL_TEMPLATE((ifn< less<Int<n>,Int<m> > , 
			       x64::Disp<m> , 
			       x64::Disp<n> > )) result;
};

template<int n,int m>
struct eval<times<Int<n>,x64::Disp<m> > >
{
   typedef x64::Disp<m> result ;
};

template<int n,int m>
struct eval<times<x64::Disp<m> , Int<n> > >
{
   typedef x64::Disp<m> result ;
};



namespace x64 {
struct Error_invalid_displacement_type;
}
X64_DEFINE_FUNC(dispSize,D,(
		   ifn<equal<D,x64::NoDisp> , Zero,
		       equal<D,x64::Disp8> , One,
		       equal<D,x64::Disp32> , Four,
		       error<x64::Error_invalid_displacement_type> >));


DEFINE_FOREIGN_DATA1(x64::Register16,int);
DEFINE_FOREIGN_DATA1(x64::Register32,int);
DEFINE_FOREIGN_DATA1(x64::Register64,int);
DEFINE_FOREIGN_DATA1(x64::RegNoneT,int);

X64_DEFINE_FUNC_STRICT(regCode);
X64_DEFINE_FUNC_STRICT(register16);
X64_DEFINE_FUNC_STRICT(register32);
X64_DEFINE_FUNC_STRICT(register64);

template<int x>
struct eval<x64::regCode<x64::RegNoneT<x> > >
{
   typedef Int<x> result;
};

template<int x>
struct eval<x64::regCode<x64::Register16<x> > >
{
   typedef Int<x> result;
};

template<int x>
struct eval<x64::regCode<x64::Register32<x> > >
{
   typedef Int<x> result;
};

template<int x>
struct eval<x64::regCode<x64::Register64<x> > >
{
   typedef Int<x> result;
};


namespace x64 {
typedef EVAL((regCode<RegNone>)) RegNoneCode;
typedef EVAL((regCode<RegRIP>)) RegRIPCode;
typedef EVAL((regCode<RegRAX>)) RegRAXCode;
typedef EVAL((regCode<RegRCX>)) RegRCXCode;
typedef EVAL((regCode<RegRDX>)) RegRDXCode;
typedef EVAL((regCode<RegRBX>)) RegRBXCode;
typedef EVAL((regCode<RegRSP>)) RegRSPCode;
typedef EVAL((regCode<RegRBP>)) RegRBPCode;
typedef EVAL((regCode<RegRSI>)) RegRSICode;
typedef EVAL((regCode<RegRDI>)) RegRDICode;

typedef EVAL((regCode<RegR8>)) RegR8Code;
typedef EVAL((regCode<RegR9>)) RegR9Code;
typedef EVAL((regCode<RegR10>)) RegR10Code;
typedef EVAL((regCode<RegR11>)) RegR11Code;
typedef EVAL((regCode<RegR12>)) RegR12Code;
typedef EVAL((regCode<RegR13>)) RegR13Code;
typedef EVAL((regCode<RegR14>)) RegR14Code;
typedef EVAL((regCode<RegR15>)) RegR15Code;

}

}}

namespace erasm { namespace prelude {



X64_DEFINE_FUNC(isExtendedRegCode,X,(greaterEq<X,Eight>));
template<>
struct eval<x64::isExtendedRegCode<x64::RegNoneCode> >
{
   typedef False result;
};

X64_DEFINE_FUNC(isRSPCode,X,
		(equal<X,Four>));
X64_DEFINE_FUNC(isRegNoneCode,X,
		(equal<X,x64::RegNoneCode>));
X64_DEFINE_FUNC2(isRegNonePair,X,Y,
		 (and2<equal<X,x64::RegNoneCode>,equal<Y,x64::RegNoneCode> >));

X64_DEFINE_DATA2(Expr);
DERIVING_EQ2(x64::Expr);
DERIVING_ABELIAN2(x64::Expr);
DERIVING_Z_MODULE2(x64::Expr);

X64_DEFINE_DATA2(Expr32);
DERIVING_EQ2(x64::Expr32);
DERIVING_ABELIAN2(x64::Expr32);
DERIVING_Z_MODULE2(x64::Expr32);

X64_DEFINE_DATA4_DERIVING_EQ(EffectiveAddress);
namespace x64 {
typedef EffectiveAddress<x64::RegNoneCode,
			 x64::RegNoneCode,
			 Zero,
			 NoDisp> emptyEffectiveAddress ;
}

X64_DEFINE_FUNC(minDispRep,X,
		( ifn<equal<X,Zero> , x64::NoDisp ,
		      and2<lessEq<Int<CHAR_MIN>,X> , 
			   lessEq<X,Int<CHAR_MAX> > > ,
		      x64::Disp8,
		      x64::Disp32 > ));

X64_DEFINE_FUNC_STRICT_STRICT(setDisp);
template<class NewDisp,class Base,class Index,class Scale,class Disp>
struct eval<x64::setDisp<NewDisp,
			 x64::EffectiveAddress<Base,Index,Scale,Disp> > >
{
   typedef x64::EffectiveAddress<Base,Index,Scale,NewDisp> result;
};

X64_DEFINE_FUNC_STRICT(exprFromReg);
X64_DEFINE_FUNC_STRICT_STRICT(exprFromRegDisp);
X64_DEFINE_FUNC_STRICT(expr32FromReg);
X64_DEFINE_FUNC_STRICT_STRICT(expr32FromRegDisp);
X64_DEFINE_FUNC_STRICT(polynomialFromExpr);
X64_DEFINE_FUNC_STRICT(realDispType);
X64_DEFINE_FUNC_STRICT(realDispSize);
X64_DEFINE_FUNC_STRICT(modrmMod);
X64_DEFINE_FUNC_STRICT(modrmRM);
X64_DEFINE_FUNC_STRICT(sib);
X64_DEFINE_FUNC_STRICT(sibSS);
X64_DEFINE_FUNC_STRICT(sibIndex);
X64_DEFINE_FUNC_STRICT(sibBase);
X64_DEFINE_FUNC_STRICT(rexX);
X64_DEFINE_FUNC_STRICT(rexB);
X64_DEFINE_FUNC(isRexAddress,E,(or2<x64::rexX<E>,x64::rexB<E> >));

X64_DEFINE_FUNC_STRICT(effectiveAddressFromExpr);
X64_DEFINE_FUNC(isRexExpr,Expr,
		(x64::isRexAddress<x64::effectiveAddressFromExpr<Expr> >));
X64_DEFINE_FUNC(isRexReg,R,(greater<x64::regCode<R>,Seven>));

ERASM_META_ASSERT_EQUAL((x64::isRexReg<x64::RegRIP>),(False));

X64_DEFINE_FUNC_STRICT(effectiveAddressBase);
template<class Base,class Index,class Scale,class Disp>
struct eval<x64::effectiveAddressBase<x64::EffectiveAddress<Base,Index,Scale,Disp> > >
{
   typedef EVAL_TEMPLATE((Base)) result;
};

X64_DEFINE_FUNC_STRICT(effectiveAddressIndex);
template<class Base,class Index,class Scale,class Disp>
struct eval<x64::effectiveAddressIndex<x64::EffectiveAddress<Base,Index,Scale,Disp> > >
{
   typedef EVAL_TEMPLATE((Index)) result;
};

X64_DEFINE_FUNC_STRICT(effectiveAddressScale);
template<class Base,class Index,class Scale,class Disp>
struct eval<x64::effectiveAddressScale<x64::EffectiveAddress<Base,Index,Scale,Disp> > >
{
   typedef EVAL_TEMPLATE((Scale)) result;
};


X64_DECLARE_FUNC1(effectiveAddressFromPolynomial);
X64_DEFINE_FUNC_STRICT(effectiveAddressFromTerm);
X64_DEFINE_FUNC_STRICT_STRICT(effectiveAddressFromTerm2);
X64_DEFINE_FUNC2(effectiveAddressFromScaledReg,R,S,
		 (x64::effectiveAddressFromTerm<Term<x64::regCode<R>,S> >));
X64_DEFINE_FUNC(effectiveAddressFromReg,X,
		(x64::effectiveAddressFromTerm<Term<x64::regCode<X>,One> >));
X64_DEFINE_FUNC3(effectiveAddressFromReg2,R1,R2,Scale,
		 (x64::EffectiveAddress<x64::regCode<R1>,x64::regCode<R2>,Scale,x64::NoDisp>));

X64_DEFINE_FUNC_STRICT_STRICT(nthRegister);

X64_DEFINE_FUNC_STRICT(noIndex);



template<int X,int Y>
struct eval<equal< x64::Disp<X>,x64::Disp<Y> > >
{
   typedef Bool<X==Y> result;
};

template<class X,class A,class B>
struct inRange;

template<class X,class A,class B>
struct eval<inRange<X,A,B> >
{
   typedef EVAL_TEMPLATE(( and2<greaterEq<X,A> , less<X,B> > )) result;
};

ERASM_META_ASSERT((inRange<One,One,Two>));
ERASM_META_ASSERT_NOT((inRange<One,Zero,One>));



namespace x64 {
ERASM_META_ASSERT((equal<Disp32,Disp32>));
ERASM_META_ASSERT_NOT((equal<Disp32,NoDisp>));
ERASM_META_ASSERT((equal<NoDisp,NoDisp>));

ERASM_META_ASSERT_EQUAL((regCode<RegNone>),(RegNoneCode));
ERASM_META_ASSERT_NOT((isExtendedRegCode<RegRAXCode>));
ERASM_META_ASSERT_NOT((isExtendedRegCode<RegNoneCode>));
ERASM_META_ASSERT((isExtendedRegCode<RegR8Code>));
ERASM_META_ASSERT((isRSPCode<regCode<RegRSP> > ));
ERASM_META_ASSERT((isRSPCode<RegRSPCode>));

}

template<int n>
struct eval<x64::register16<Int<n> > >
{
   typedef x64::Register16<n> result;
};

template<int n>
struct eval<x64::register32<Int<n> > >
{
   typedef x64::Register32<n> result;
};

template<int n>
struct eval<x64::register64<Int<n> > >
{
   typedef x64::Register64<n> result;
};

template<>
struct eval<equal<x64::RegNone,x64::RegNone> >
{
   typedef True result;
};

template<int i>
struct eval<equal<x64::Register64<i>,x64::RegNone> >
{
   typedef False result;
};
template<int i>
struct eval<equal<x64::RegNone,x64::Register64<i> > >
{
   typedef False result;
};

template<int n,int m>
struct eval<equal<x64::Register64<n>,x64::Register64<m> > >
{
   typedef Bool<n==m> result;
};

template<int i>
struct eval<equal<x64::Register32<i>,x64::RegNone> >
{
   typedef False result;
};
template<int i>
struct eval<equal<x64::RegNone,x64::Register32<i> > >
{
   typedef False result;
};

template<int n,int m>
struct eval<equal<x64::Register32<n>,x64::Register32<m> > >
{
   typedef Bool<n==m> result;
};


template<int i>
struct eval<equal<x64::Register16<i>,x64::RegNone> >
{
   typedef False result;
};
template<int i>
struct eval<equal<x64::RegNone,x64::Register16<i> > >
{
   typedef False result;
};


template<int n,int m>
struct eval<equal<x64::Register16<n>,x64::Register16<m> > >
{
   typedef Bool<n==m> result;
};


template<class P,class D>
struct eval<x64::effectiveAddressFromExpr<x64::Expr<P,D> > >
{
   typedef EVAL_TEMPLATE((x64::setDisp<D,x64::effectiveAddressFromPolynomial<P> >)) result;
};

template<class P>
struct eval<x64::effectiveAddressFromPolynomial<P> >
{
   typedef length<termList<P> > len;
   typedef ifn< equal<len,Zero> , 
		x64::emptyEffectiveAddress , 
		equal<len,One>  , 
		x64::effectiveAddressFromTerm <head<termList<P> > >,
		equal<len,Two>  , 
		x64::effectiveAddressFromTerm2<index<termList<P>,Zero>,
					       index<termList<P>,One> >,
		error<x64::Error_Invalid_address_expression,P> > r;
   typedef EVAL_TEMPLATE((r)) result;
};


template<class Deg,class Coeff>
struct eval<x64::effectiveAddressFromTerm< Term<Deg,Coeff> > >
{
   typedef Deg regCode;
   typedef Coeff scale;
   typedef ifn< and2<equal<regCode,x64::RegRSPCode>,greater<scale,One> > ,  
		error<x64::Error_ESP_and_RSP_cannot_be_used_as_the_index_register,
		      regCode,scale>,
                // On the other hand,R12 can be used as the index register
		equal<scale,Zero>  , error<x64::Error_Invalid_address_expression,
					   regCode,scale>,
		equal<scale,One>   , 
		x64::EffectiveAddress<regCode,x64::RegNoneCode,Zero,x64::NoDisp> ,
		equal<scale,Two>   , 
		x64::EffectiveAddress<x64::RegNoneCode,regCode,Two,x64::NoDisp> ,
		equal<scale,Four>  , 
		x64::EffectiveAddress<x64::RegNoneCode,regCode,Four,x64::NoDisp> ,
		equal<scale,Eight> , 
		x64::EffectiveAddress<x64::RegNoneCode,regCode,Eight,x64::NoDisp> ,
		equal<scale,Three> , 
		x64::EffectiveAddress<regCode,regCode,Two,x64::NoDisp> ,
		equal<scale,Five>  , 
		x64::EffectiveAddress<regCode,regCode,Four,x64::NoDisp> ,
		equal<scale,Nine>  , 
		x64::EffectiveAddress<regCode,regCode,Eight,x64::NoDisp> ,
		error<x64::Error_Invalid_address_expression,regCode,scale> > r;
   typedef EVAL_TEMPLATE((r)) result;
};

namespace x64 { namespace addr64 {
ERASM_META_ASSERT_EQUAL((effectiveAddressFromTerm<Term<One,Two> >),
		       (EffectiveAddress<RegNoneCode,RegRCXCode,Two,NoDisp>));
ERASM_META_ASSERT_ERROR((effectiveAddressFromTerm<Term<Four,Two> >));
}}


template<class Deg1,class Coeff1,class Deg2,class Coeff2>
struct eval<x64::effectiveAddressFromTerm2< Term<Deg1,Coeff1> ,
					    Term<Deg2,Coeff2> > >
{
   typedef Deg1 reg1Code;
   typedef Deg2 reg2Code;
   typedef Coeff1 scale1;
   typedef Coeff2 scale2;
   typedef Term<Deg1,Coeff1> t1;
   typedef Term<Deg2,Coeff2> t2;

   typedef ifn< equal<times<scale1,scale2>,Zero> , 
		error<x64::Error_Invalid_address_expression,
		      Term<reg1Code,scale1>,Term<reg2Code,scale2> >,
		x64::isRSPCode<reg1Code> , 
		ifn<equal<scale1,One> , 
		    x64::EffectiveAddress<reg1Code,reg2Code,scale2,x64::NoDisp>,
		    error<x64::Error_ESP_and_RSP_cannot_be_used_as_the_index_register,t1,t2> > ,
		x64::isRSPCode<reg2Code> , 
		ifn<equal<scale2,One> , 
		    x64::EffectiveAddress<reg2Code,reg1Code,scale1,x64::NoDisp>,
		    error<x64::Error_ESP_and_RSP_cannot_be_used_as_the_index_register,t1,t2> >,
		equal<scale1,One> , 
		x64::EffectiveAddress<reg1Code,reg2Code,scale2,x64::NoDisp>,
		equal<scale2,One> , 
		x64::EffectiveAddress<reg2Code,reg1Code,scale1,x64::NoDisp>,
		error<x64::Error_Invalid_address_expression,t1,t2> >  r;
   typedef EVAL_TEMPLATE((r)) result;
};

namespace x64 { namespace addr64 {
ERASM_META_ASSERT_EQUAL((effectiveAddressFromTerm2<Term<Zero,One>,
			                          Term<One,Two > >),
		       (EffectiveAddress<RegRAXCode,RegRCXCode,Two,NoDisp>));
ERASM_META_ASSERT_ERROR((effectiveAddressFromTerm2<Term<Zero,One>,
			                          Term<Four,Two > >));
}}

template<int n>
struct eval<x64::exprFromReg<x64::Register64<n> > >
{
   typedef x64::Expr<monomial<Int<n> >,x64::NoDisp > result;
};

template<int n,class Disp>
struct eval<x64::exprFromRegDisp<x64::Register64<n> ,Disp> >
{
   typedef x64::Expr<monomial<Int<n> >,Disp > result;
};

template<int n>
struct eval<x64::expr32FromReg<x64::Register32<n> > >
{
   typedef x64::Expr32<monomial<Int<n> >,x64::NoDisp > result;
};

template<int n,class Disp>
struct eval<x64::expr32FromRegDisp<x64::Register32<n>,Disp > >
{
   typedef x64::Expr32<monomial<Int<n> >,Disp > result;
};

template<class P>
struct eval<x64::polynomialFromExpr<x64::Expr<P> > >
{
   typedef EVAL_TEMPLATE((P)) result;
};


template<class BaseCode,class IndexCode,class Scale,class DispType>
struct eval<x64::modrmMod<x64::EffectiveAddress<BaseCode,
						IndexCode,
						Scale,
						DispType> > >
{
   typedef ifn<and2<equal<BaseCode,x64::RegRBPCode>,
		    equal<DispType,x64::NoDisp> >,
	       One,
	       and2<equal<BaseCode,x64::RegR13Code>,
		    equal<DispType,x64::NoDisp> >,
	       One,
	       equal<BaseCode,x64::RegRIPCode > , 
	       Zero,
	       equal<BaseCode,x64::RegNoneCode> , 
	       Zero ,
	       equal<DispType,x64::NoDisp> , 
	       Zero,
	       equal<DispType,x64::Disp8> , 
	       One,
	       equal<DispType,x64::Disp32> , 
	       Two,
	       error<x64::Error_Invalid_displacement> > r;
   typedef EVAL_TEMPLATE((r)) result;
};


template<class BaseCode,class IndexCode,class Scale,class DispType>
struct eval<x64::modrmRM<x64::EffectiveAddress<BaseCode,IndexCode,Scale,DispType> > >
{
   typedef mod<BaseCode,Eight> baseCodeMod8;
   typedef ifn< x64::isRegNonePair<BaseCode,IndexCode> , Four ,
		equal<BaseCode,x64::RegRIPCode > , Five,
		equal<IndexCode,x64::RegNoneCode> , baseCodeMod8,
		Four> r;
   typedef EVAL_TEMPLATE((r)) result;
};

template<class BaseCode,class IndexCode,class Scale,class DispType>
struct eval<x64::realDispType<x64::EffectiveAddress<BaseCode,IndexCode,Scale,DispType> > >
{
   typedef x64::EffectiveAddress<BaseCode,IndexCode,Scale,DispType> E;
   typedef x64::modrmMod<E> mod;
   typedef ifn< and2< equal<mod,Zero> , equal<BaseCode,x64::RegNoneCode> > , x64::Disp32,
		equal<BaseCode,x64::RegRIPCode> , x64::Disp32 , 
		equal<mod,Zero> , x64::NoDisp,
		equal<mod,One>  , x64::Disp8,
		equal<mod,Two>  , x64::Disp32,
		error<x64::Error_Invalid_address_expression> >  r;
   typedef EVAL_TEMPLATE((r)) result;
};

template<class BaseCode,class IndexCode,class Scale,class DispType>
struct eval<x64::realDispSize<x64::EffectiveAddress<BaseCode,IndexCode,Scale,DispType> > >
{
   typedef x64::EffectiveAddress<BaseCode,IndexCode,Scale,DispType> E;
   typedef EVAL_TEMPLATE((x64::dispSize < x64::realDispType<E> >)) result;
};



namespace UNIQUEID(addr64_test) {
   using namespace ::erasm::prelude::x64;
   using namespace ::erasm::prelude::x64::addr64;
ERASM_META_ASSERT_EQUAL((modrmMod<EffectiveAddress<RegRAXCode , RegNoneCode,Zero , 
						  NoDisp> >),
		       (Zero));
ERASM_META_ASSERT_EQUAL((modrmMod<EffectiveAddress<RegRBPCode ,RegNoneCode,Zero ,
						  NoDisp> >),
		       (One));
ERASM_META_ASSERT_EQUAL((modrmMod<EffectiveAddress<RegNoneCode,RegNoneCode,Zero ,
						  Disp8> >),
		       (Zero));
ERASM_META_ASSERT_EQUAL((modrmMod<EffectiveAddress<RegRBXCode ,RegNoneCode,Zero ,
						  Disp8> >) ,
		       (One));
ERASM_META_ASSERT_EQUAL((modrmMod<EffectiveAddress<RegRCXCode ,RegRDICode ,Zero ,
						  Disp32> >),
		       (Two));
ERASM_META_ASSERT_EQUAL((modrmMod<EffectiveAddress<RegRIPCode ,RegNoneCode,Zero ,
						  Disp8> >),
		       (Zero));

ERASM_META_ASSERT_EQUAL((modrmMod<EffectiveAddress<RegNoneCode,RegNoneCode,Zero ,
						  Disp32> >),
		       (Zero));
ERASM_META_ASSERT_EQUAL((modrmRM <EffectiveAddress<RegNoneCode,RegNoneCode,Zero,NoDisp> >),
		       (Four));

ERASM_META_ASSERT_EQUAL((modrmMod<EffectiveAddress<RegNoneCode,RegR12Code,One ,
						  Disp32> >),
		       (Zero));
ERASM_META_ASSERT_EQUAL((modrmRM<EffectiveAddress<RegNoneCode,RegR12Code,One,NoDisp> >),
		       (Four));
} 


template<class BaseCode,class IndexCode,class Scale,class DispType>
struct eval<x64::noIndex<x64::EffectiveAddress<BaseCode,IndexCode,Scale,DispType> > >
{
   typedef EVAL_TEMPLATE((or2<equal<IndexCode,x64::RegNoneCode>,
			      equal<Scale,Zero> >)) result;
};

template<class BaseCode,class IndexCode,class Scale,class DispType>
struct eval<x64::sib<x64::EffectiveAddress<BaseCode,IndexCode,Scale,DispType> > >
{
   typedef not_<x64::noIndex<x64::EffectiveAddress<BaseCode,IndexCode,Scale,DispType> > > 
           has_index;
   typedef ifn< equal<BaseCode,x64::RegRSPCode> , True,
		equal<BaseCode,x64::RegR12Code > , True,
		x64::isRegNonePair<BaseCode,IndexCode> , True,
		has_index >  r;
   typedef EVAL_TEMPLATE((r)) result;
};

template<class BaseCode,class IndexCode,class Scale,class DispType>
struct eval<x64::sibSS<x64::EffectiveAddress<BaseCode,IndexCode,Scale,DispType> > > 
{
   typedef ifn< x64::isRegNonePair<BaseCode,IndexCode> , Zero ,
		equal<Scale,Zero> , Zero ,  //  BaseCode might be  RSP or R12
		equal<Scale,One>  , Zero,
		equal<Scale,Two>  , One,
		equal<Scale,Four> , Two,
		equal<Scale,Eight>, Three,
		error<x64::Error_Invalid_Scale,Scale> > r;
   typedef EVAL_TEMPLATE((r)) result;
};

template<class BaseCode,class IndexCode,class Scale,class DispType>
struct eval<x64::sibBase<x64::EffectiveAddress<BaseCode,IndexCode,Scale,DispType> > >
{
   typedef if_< equal<BaseCode,x64::RegNoneCode> , Five ,
		mod<BaseCode,Eight> > r;
   typedef EVAL_TEMPLATE((r)) result;
};

template<class BaseCode,class IndexCode,class Scale,class DispType>
struct eval<x64::sibIndex<x64::EffectiveAddress<BaseCode,IndexCode,Scale,DispType> > >
{
   typedef x64::EffectiveAddress<BaseCode,IndexCode,Scale,DispType> e;
   ERASM_META_ASSERT_NE((IndexCode),(x64::RegRSPCode));
   typedef EVAL_TEMPLATE((if_<x64::noIndex<e> , 
			      Four, 
			      mod<IndexCode,Eight> >)) result;
};


namespace UNIQUEID(x64_addr64) {
using namespace x64;
using namespace x64::addr64;

ERASM_META_ASSERT_EQUAL((sib<EffectiveAddress<RegRSPCode,RegNoneCode,One,NoDisp> >),
		       (True));
ERASM_META_ASSERT_EQUAL((sib<EffectiveAddress<RegRSPCode,RegNoneCode,Zero,NoDisp> >),
		       (True));
ERASM_META_ASSERT_EQUAL((effectiveAddressFromExpr<exprFromReg<RegRSP> >  ),
		       (EffectiveAddress<RegRSPCode,RegNoneCode,Zero,NoDisp>));
ERASM_META_ASSERT_EQUAL((sib<effectiveAddressFromExpr<exprFromReg<RegRSP> > > ),
		       (True));
ERASM_META_ASSERT_EQUAL((sibIndex<EffectiveAddress<RegRAXCode,RegRAXCode,One,NoDisp> >),
		       (Zero));
}

template<class BaseCode,class IndexCode,class Scale,class DispType>
struct eval<x64::rexX<x64::EffectiveAddress<BaseCode,IndexCode,Scale,DispType> > >
{
   typedef EVAL_TEMPLATE((x64::isExtendedRegCode<IndexCode>)) result;
};

template<class BaseCode,class IndexCode,class Scale,class DispType>
struct eval<x64::rexB<x64::EffectiveAddress<BaseCode,IndexCode,Scale,DispType> > >
{
   typedef EVAL_TEMPLATE((x64::isExtendedRegCode<BaseCode>)) result;
};


				 

namespace x64 { 

struct VoidDisp 
{
   VoidDisp() {}
   VoidDisp(int n) {}
};

template<class P,typename Disp>
struct Expr
{
   typedef EVAL_TEMPLATE((Disp)) D;
   typedef typename D::value_type disp_type;
   typedef Expr type;

   explicit Expr(disp_type d=0) : disp(d) {}
   typedef P  regExpr;


   disp_type get_disp() const
      { return disp ; }
   disp_type   disp;
};

template<class P,typename Disp>
struct Expr32
{
   typedef EVAL_TEMPLATE((Disp)) D;
   typedef typename D::value_type disp_type;

   typedef Expr32 type;
   explicit Expr32(disp_type d=0) : disp(d) {}
   typedef P  regExpr;
   
   disp_type get_disp() const
      { return disp ; }
   disp_type   disp;
};

template<class BaseCode,class IndexCode,class Scale ,typename Disp>
struct EffectiveAddress
{
   typedef EffectiveAddress this_type;

   typedef EVAL_TEMPLATE((modrmMod<this_type>)) mod_type;
   typedef EVAL_TEMPLATE((modrmRM<this_type>))       rm_type;
   typedef EVAL_TEMPLATE((realDispType<this_type>))  realDisp;

   typedef EVAL_TEMPLATE((sib<this_type>))  need_sib_type;
   typedef EVAL_TEMPLATE((rexX<this_type>)) rex_x_type;
   typedef EVAL_TEMPLATE((rexB<this_type>)) rex_b_type;

   typedef EVAL_TEMPLATE((if_<sib<this_type>,sibSS   <this_type>,Zero>)) 
    sib_ss_type;
   typedef EVAL_TEMPLATE((if_<need_sib_type,sibIndex<this_type>,Zero>)) 
    sib_index_type;
   typedef EVAL_TEMPLATE((if_<need_sib_type,sibBase <this_type>,Zero>)) 
    sib_base_type;

   typedef typename Disp::value_type         disp_type;
   typedef typename realDisp::value_type     real_disp_type;

   ERASM_META_ASSERT_IN_RANGE((Zero)             ,(Zero) ,(Four));
   ERASM_META_ASSERT_IN_RANGE((Zero)             ,(Zero) ,(One));
   ERASM_META_ASSERT_IN_RANGE((mod_type)         ,(Zero) ,(Four));
   ERASM_META_ASSERT_IN_RANGE((rm_type)          ,(Zero) ,(Eight));
   ERASM_META_ASSERT_IN_RANGE((sib_ss_type)      ,(Zero) ,(Four));
   ERASM_META_ASSERT_IN_RANGE((sib_index_type)   ,(Zero) ,(Eight));
   ERASM_META_ASSERT_IN_RANGE((sib_base_type)    ,(Zero) ,(Eight));
   

   explicit EffectiveAddress(real_disp_type d=0)
      : disp(d)
      {}

   real_disp_type get_disp() const
      { return disp ; }
   
   size_t get_disp_size() const
      {	 return realDisp::size ; }

   int get_mod() const
      {  return mod_type::value ; }

   int  get_rm() const
      { return rm_type::value;  }
   bool has_sib() const
      { return need_sib_type::value;  }

   int get_sib_ss() const
      { return sib_ss_type::value;  }

   int get_sib_index() const
      { return sib_index_type::value;  }

   int get_sib_base() const
      { return sib_base_type::value;  }

   bool get_rex_b () const
      { return rex_b_type::value ; }
   bool get_rex_x () const
      { return rex_x_type::value ; }

   bool is16bit() const
      { return false ; }
	    
protected:
   real_disp_type disp;
};

template<class BaseCode,class IndexCode,class Scale ,typename Disp>
struct EffectiveAddress32
   : public EffectiveAddress<BaseCode,IndexCode,Scale,Disp>
{
   typedef  EffectiveAddress<BaseCode,IndexCode,Scale,Disp> base_type;
   EffectiveAddress32(typename base_type::real_disp_type d=0)
      : base_type(d)
      { }
};

template<class Scale,typename Disp>
struct EffectiveAddress32<RegNoneCode,RegNoneCode,Scale,Disp> 
   : public EffectiveAddress<RegRIPCode,RegNoneCode,Scale,Disp>
{
   typedef  EffectiveAddress<RegRIPCode,RegNoneCode,Scale,Disp> base_type;
   EffectiveAddress32(typename base_type::real_disp_type d=0)
      : base_type(d)
      { }
};



typedef EffectiveAddress32<RegNoneCode,RegNoneCode,Zero,Disp32>
   EffectiveAddress32Disp;

ERASM_META_ASSERT_EQUAL((EffectiveAddress32Disp::mod_type),(Zero));
ERASM_META_ASSERT_EQUAL((EffectiveAddress32Disp::rm_type),(Five));
ERASM_META_ASSERT_EQUAL((EffectiveAddress32Disp::need_sib_type),(False));

typedef EffectiveAddress<RegRIPCode,RegNoneCode,Zero,Disp32>  
EffectiveAddress64RIP;

ERASM_META_ASSERT_EQUAL((EffectiveAddress64RIP::mod_type),(Zero));
ERASM_META_ASSERT_EQUAL((EffectiveAddress64RIP::rm_type),(Five));
ERASM_META_ASSERT_EQUAL((EffectiveAddress64RIP::need_sib_type),(False));


typedef EffectiveAddress<RegNoneCode,RegNoneCode,Zero,Disp32>  
EffectiveAddress64Disp;

ERASM_META_ASSERT_EQUAL((EffectiveAddress64Disp::mod_type),(Zero));
ERASM_META_ASSERT_EQUAL((EffectiveAddress64Disp::rm_type),(Four));
ERASM_META_ASSERT_EQUAL((EffectiveAddress64Disp::need_sib_type),(True));
ERASM_META_ASSERT_EQUAL((EffectiveAddress64Disp::sib_base_type),(Five));
ERASM_META_ASSERT_EQUAL((EffectiveAddress64Disp::sib_index_type),(Four));
ERASM_META_ASSERT_EQUAL((EffectiveAddress64Disp::sib_ss_type),(Zero));


ERASM_META_ASSERT_EQUAL((modrmMod<EffectiveAddress<RegNoneCode,
						  RegNoneCode,
						  Zero,
						  Disp8> >),
		       (Zero));
ERASM_META_ASSERT_EQUAL((modrmMod<EffectiveAddress<RegNoneCode,
						  RegNoneCode,
						  Zero,
						  NoDisp> >),
		       (Zero));
ERASM_META_ASSERT_EQUAL((modrmMod<EffectiveAddress<RegNoneCode,
						  RegNoneCode,
						  Zero,
						  Disp8> >),
		       (Zero));
ERASM_META_ASSERT_EQUAL((modrmMod<EffectiveAddress<RegNoneCode,
						  RegNoneCode,
						  Zero,
						  Disp32> >),
		       (Zero));
ERASM_META_ASSERT_EQUAL((sibSS<EffectiveAddress<RegRAXCode,
					       RegRAXCode,
					       One,
					       NoDisp> >),
		       (Zero));

}}}

namespace erasm { namespace prelude {
// TODO: Any identifier that begins with an underscore followed by
//       a capital letter is reserved for the imlementation.
//       Perhaps I need a reasonable module system for my meta_prelude.
//       Currently,defining a type function in a namespace other than
//       erasm::prelude is just too painful.

DEFINE_DATA2(_Modrm);
DEFINE_ACCESSORS2(_Modrm,_mod,_rm);
ERASM_META_ASSERT_EQUAL((_mod<_Modrm<One,Two> >),(One)); 
ERASM_META_ASSERT_EQUAL((_rm <_Modrm<One,Two> >),(Two));

DEFINE_DATA1(_Dispsize);
DEFINE_ACCESSORS1(_Dispsize,_dsize);
ERASM_META_ASSERT_EQUAL((_dsize <_Dispsize<One> >),(One));

DEFINE_DATA3(_Sib);
DEFINE_ACCESSORS3(_Sib,_base,_index,_ss);
ERASM_META_ASSERT_EQUAL((_base  <_Sib<One,Two,Three> >),(One));
ERASM_META_ASSERT_EQUAL((_index <_Sib<One,Two,Three> >),(Two));
ERASM_META_ASSERT_EQUAL((_ss    <_Sib<One,Two,Three> >),(Three));

DEFINE_DATA2(_Rex);
DEFINE_ACCESSORS2(_Rex,_b,_x);
ERASM_META_ASSERT_EQUAL((_b<_Rex<True,False> >),(True));
ERASM_META_ASSERT_EQUAL((_x<_Rex<True,False> >),(False));


#define TEST_EFFECTIVE_ADDRESS64_NOSIB(BASE,DISP,MODRM,DSIZE,REX)	\
namespace UNIQUEID(addr64_test) {					\
   using namespace erasm::prelude::x64;					\
   typedef ERASM_META_REMOVE_PAREN(BASE) base;				\
   typedef ERASM_META_REMOVE_PAREN(MODRM) modrm;				\
   typedef ERASM_META_REMOVE_PAREN(DISP) d;				\
   typedef ERASM_META_REMOVE_PAREN(DSIZE) dsize;				\
   typedef ERASM_META_REMOVE_PAREN(REX) rex;				\
   typedef EffectiveAddress<regCode<base>,RegNoneCode,Zero,d> e;	\
									\
   ERASM_META_ASSERT_EQUAL((modrmMod<e>),(_mod<modrm>));			\
   ERASM_META_ASSERT_EQUAL((modrmRM<e>),(_rm<modrm>));			\
   ERASM_META_ASSERT_EQUAL((realDispSize<e>),(_dsize<dsize>));		\
   ERASM_META_ASSERT_EQUAL((sib<e>), (False));				\
   ERASM_META_ASSERT_EQUAL((rexB<e>), (_b<rex>));			\
   ERASM_META_ASSERT_EQUAL((rexX<e>), (_x<rex>));			\
}


#define TEST_EFFECTIVE_ADDRESS64_SIB(BASE,INDEX,SCALE,DISP, MODRM , DISPSIZE , SIB , REX) \
namespace UNIQUEID(addr64_test2) {					\
   using namespace erasm::prelude::x64;					\
   typedef ERASM_META_REMOVE_PAREN(BASE) base;				\
   typedef ERASM_META_REMOVE_PAREN(INDEX) index;				\
   typedef ERASM_META_REMOVE_PAREN(SCALE) scale;				\
   typedef ERASM_META_REMOVE_PAREN(DISP) d;				\
   typedef ERASM_META_REMOVE_PAREN(MODRM) modrm;				\
   typedef ERASM_META_REMOVE_PAREN(DISPSIZE) size;			\
   typedef ERASM_META_REMOVE_PAREN(SIB) sib_;				\
   typedef ERASM_META_REMOVE_PAREN(REX) rex;				\
   typedef EffectiveAddress<regCode<base>,regCode<index>,scale,d> e;	\
   ERASM_META_ASSERT_EQUAL((modrmMod<e>),(_mod<modrm>));			\
   ERASM_META_ASSERT_EQUAL((modrmRM<e>),(_rm<modrm>));			\
   ERASM_META_ASSERT_EQUAL((realDispSize<e>),(_dsize<size>));		\
   ERASM_META_ASSERT_EQUAL((sib<e>), (True));				\
   ERASM_META_ASSERT_EQUAL((sibIndex<e>), (_index<sib_>));		\
   ERASM_META_ASSERT_EQUAL((sibBase<e>), (_base<sib_>));			\
   ERASM_META_ASSERT_EQUAL((sibSS<e>), (_ss<sib_>));			\
   ERASM_META_ASSERT_EQUAL((rexB<e>), (_b<rex>));			\
   ERASM_META_ASSERT_EQUAL((rexX<e>), (_x<rex>));			\
}									

TEST_EFFECTIVE_ADDRESS64_NOSIB((RegRAX),
			       (NoDisp),
			       (_Modrm<Zero,Zero>),
			       (_Dispsize<Zero>),
			       (_Rex<False,False>));

TEST_EFFECTIVE_ADDRESS64_NOSIB((RegRAX),
			       (Disp8) ,
			       (_Modrm<One ,Zero>),
			       (_Dispsize<One>) ,
			       (_Rex<False,False>));

TEST_EFFECTIVE_ADDRESS64_NOSIB((RegRAX),
			       (Disp32),
			       (_Modrm<Two ,Zero>),
			       (_Dispsize<Four>),
			       (_Rex<False,False>));

TEST_EFFECTIVE_ADDRESS64_NOSIB((RegR8) ,
			       (NoDisp),
			       (_Modrm<Zero,Zero>),
			       (_Dispsize<Zero>),
			       (_Rex<True,False>));
TEST_EFFECTIVE_ADDRESS64_NOSIB((RegR8) ,
			       (Disp8) ,
			       (_Modrm<One ,Zero>),
			       (_Dispsize<One>) ,
			       (_Rex<True,False>));
TEST_EFFECTIVE_ADDRESS64_NOSIB((RegR8) ,
			       (Disp32),
			       (_Modrm<Two ,Zero>),
			       (_Dispsize<Four>),
			       (_Rex<True,False>));
TEST_EFFECTIVE_ADDRESS64_NOSIB((RegRSI),
			       (NoDisp),
			       (_Modrm<Zero,Six>),
			       (_Dispsize<Zero>),
			       (_Rex<False,False>));
TEST_EFFECTIVE_ADDRESS64_NOSIB((RegRSI),
			       (Disp8) ,
			       (_Modrm<One ,Six>),
			       (_Dispsize<One>) ,
			       (_Rex<False,False>));
TEST_EFFECTIVE_ADDRESS64_NOSIB((RegRSI),
			       (Disp32),
			       (_Modrm<Two ,Six>),
			       (_Dispsize<Four>),
			       (_Rex<False,False>));

TEST_EFFECTIVE_ADDRESS64_NOSIB((RegR14),
			       (NoDisp),
			       (_Modrm<Zero,Six>),
			       (_Dispsize<Zero>),
			       (_Rex<True,False>));
TEST_EFFECTIVE_ADDRESS64_NOSIB((RegR14),
			       (Disp8) ,
			       (_Modrm<One ,Six>),
			       (_Dispsize<One>) ,
			       (_Rex<True,False>));
TEST_EFFECTIVE_ADDRESS64_NOSIB((RegR14),
			       (Disp32),
			       (_Modrm<Two ,Six>),
			       (_Dispsize<Four>),
			       (_Rex<True,False>));

TEST_EFFECTIVE_ADDRESS64_NOSIB((RegRBP),
			       (NoDisp),
			       (_Modrm<One ,Five>),
			       (_Dispsize<One>) ,
			       (_Rex<False,False>));
TEST_EFFECTIVE_ADDRESS64_NOSIB((RegRBP),
			       (Disp8) ,
			       (_Modrm<One ,Five>),
			       (_Dispsize<One>) ,
			       (_Rex<False,False>));
TEST_EFFECTIVE_ADDRESS64_NOSIB((RegRBP),
			       (Disp32),
			       (_Modrm<Two ,Five>),
			       (_Dispsize<Four>),
			       (_Rex<False,False>));
TEST_EFFECTIVE_ADDRESS64_NOSIB((RegRIP),
			       (NoDisp),
			       (_Modrm<Zero ,Five>),
			       (_Dispsize<Four>),
			       (_Rex<False,False>));
TEST_EFFECTIVE_ADDRESS64_NOSIB((RegRIP),
			       (Disp8) ,
			       (_Modrm<Zero ,Five>),
			       (_Dispsize<Four>),
			       (_Rex<False,False>));
TEST_EFFECTIVE_ADDRESS64_NOSIB((RegRIP),
			       (Disp32),
			       (_Modrm<Zero ,Five>),
			       (_Dispsize<Four>),
			       (_Rex<False,False>));

TEST_EFFECTIVE_ADDRESS64_SIB((RegNone),
			     (RegNone),
			     (Zero),
			     (NoDisp),
			     (_Modrm<Zero ,Four>),
			     (_Dispsize<Four>),
			     (_Sib<Five,Four,Zero>) ,
			     (_Rex<False,False>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegRAX),
			     (RegRAX),
			     (Two),
			     (NoDisp) ,
			     (_Modrm<Zero,Four>),
			     (_Dispsize<Zero>),
			     (_Sib<Zero,Zero,One>),
			     (_Rex<False,False>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegRAX),
			     (RegRAX),
			     (Two),
			     (Disp8)  ,
			     (_Modrm<One,Four>) ,
			     (_Dispsize<One>) ,
			     (_Sib<Zero,Zero,One>),
			     (_Rex<False,False>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegRAX),
			     (RegRAX),
			     (Two),
			     (Disp32) ,
			     (_Modrm<Two,Four>) ,
			     (_Dispsize<Four>),
			     (_Sib<Zero,Zero,One>),
			     (_Rex<False,False>));

TEST_EFFECTIVE_ADDRESS64_SIB((RegRAX),
			     (RegR8),
			     (Two),
			     (NoDisp)  ,
			     (_Modrm<Zero,Four>),
			     (_Dispsize<Zero>),
			     (_Sib<Zero,Zero,One>),
			     (_Rex<False,True>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegRAX),
			     (RegR8),
			     (Two),
			     (Disp8)   ,
			     (_Modrm<One,Four>) ,
			     (_Dispsize<One>) ,
			     (_Sib<Zero,Zero,One>),
			     (_Rex<False,True>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegRAX),
			     (RegR8),
			     (Two),
			     (Disp32)  ,
			     (_Modrm<Two,Four>) ,
			     (_Dispsize<Four>),
			     (_Sib<Zero,Zero,One>),
			     (_Rex<False,True>));

TEST_EFFECTIVE_ADDRESS64_SIB((RegR8),
			     (RegRAX),
			     (Two),
			     (NoDisp) ,
			     (_Modrm<Zero,Four>),
			     (_Dispsize<Zero>),
			     (_Sib<Zero,Zero,One>),
			     (_Rex<True,False>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegR8),
			     (RegRAX),
			     (Two),
			     (Disp8)  ,
			     (_Modrm<One,Four>) ,
			     (_Dispsize<One>) ,
			     (_Sib<Zero,Zero,One>),
			     (_Rex<True,False>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegR8),
			     (RegRAX),
			     (Two),
			     (Disp32) ,
			     (_Modrm<Two,Four>) ,
			     (_Dispsize<Four>),
			     (_Sib<Zero,Zero,One>),
			     (_Rex<True,False>));

TEST_EFFECTIVE_ADDRESS64_SIB((RegR8),
			     (RegR8),
			     (Two),
			     (NoDisp) ,
			     (_Modrm<Zero,Four>),
			     (_Dispsize<Zero>),
			     (_Sib<Zero,Zero,One>),
			     (_Rex<True,True>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegR8),
			     (RegR8),
			     (Two),
			     (Disp8)  ,
			     (_Modrm<One,Four>) ,
			     (_Dispsize<One>) ,
			     (_Sib<Zero,Zero,One>),
			     (_Rex<True,True>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegR8),
			     (RegR8),
			     (Two),
			     (Disp32) ,
			     (_Modrm<Two,Four>) ,
			     (_Dispsize<Four>),
			     (_Sib<Zero,Zero,One>),
			     (_Rex<True,True>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegRCX),
			     (RegR15),
			     (Eight),
			     (NoDisp) ,
			     (_Modrm<Zero,Four>),
			     (_Dispsize<Zero>),
			     (_Sib<One,Seven,Three>),
			     (_Rex<False,True>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegRCX),
			     (RegR15),
			     (Eight),
			     (Disp8)  ,
			     (_Modrm<One,Four>) ,
			     (_Dispsize<One>) ,
			     (_Sib<One,Seven,Three>),
			     (_Rex<False,True>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegRCX),
			     (RegR15),
			     (Eight),
			     (Disp32) ,
			     (_Modrm<Two,Four>) ,
			     (_Dispsize<Four>),
			     (_Sib<One,Seven,Three>),
			     (_Rex<False,True>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegNone),
			     (RegR12),
			     (Eight),
			     (NoDisp) ,
			     (_Modrm<Zero,Four>),
			     (_Dispsize<Four>),
			     (_Sib<Five,Four,Three>),
			     (_Rex<False,True>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegNone),
			     (RegR12),
			     (Eight),
			     (Disp8)  ,
			     (_Modrm<Zero,Four>),
			     (_Dispsize<Four>),
			     (_Sib<Five,Four,Three>),
			     (_Rex<False,True>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegNone),
			     (RegR12),
			     (Eight),
			     (Disp32) ,
			     (_Modrm<Zero,Four>),
			     (_Dispsize<Four>),
			     (_Sib<Five,Four,Three>),
			     (_Rex<False,True>));

TEST_EFFECTIVE_ADDRESS64_SIB((RegRBP) ,
			     (RegR12),
			     (Eight),
			     (NoDisp) ,
			     (_Modrm<One ,Four>),
			     (_Dispsize<One>) ,
			     (_Sib<Five,Four,Three>),
			     (_Rex<False,True>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegRBP) ,
			     (RegR12),
			     (Eight),
			     (Disp8)  ,
			     (_Modrm<One ,Four>),
			     (_Dispsize<One>) ,
			     (_Sib<Five,Four,Three>),
			     (_Rex<False,True>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegRBP) ,
			     (RegR12),
			     (Eight),
			     (Disp32)  ,
			     (_Modrm<Two ,Four>),
			     (_Dispsize<Four>) ,
			     (_Sib<Five,Four,Three>),
			     (_Rex<False,True>));

TEST_EFFECTIVE_ADDRESS64_SIB((RegRSP) ,
			     (RegNone),
			     (Zero),
			     (NoDisp) ,
			     (_Modrm<Zero,Four>),
			     (_Dispsize<Zero>) ,
			     (_Sib<Four,Four,Zero>),
			     (_Rex<False,False>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegRSP) ,
			     (RegR12) ,
			     (Four),
			     (NoDisp) ,
			     (_Modrm<Zero,Four>),
			     (_Dispsize<Zero>) ,
			     (_Sib<Four,Four,Two>) ,
			     (_Rex<False,True>));

TEST_EFFECTIVE_ADDRESS64_SIB((RegR12) ,
			     (RegNone),
			     (Zero),
			     (NoDisp) ,
			     (_Modrm<Zero,Four>),
			     (_Dispsize<Zero>) ,
			     (_Sib<Four,Four,Zero>),
			     (_Rex<True,False>));
TEST_EFFECTIVE_ADDRESS64_SIB((RegR12) ,
			     (RegR12) ,
			     (Four),
			     (NoDisp) ,
			     (_Modrm<Zero,Four>),
			     (_Dispsize<Zero>) ,
			     (_Sib<Four,Four,Two >),
			     (_Rex<True,True>));

namespace UNIQUEID(test_x64_addr64) {
   using namespace x64;
   typedef exprFromReg<RegRAX> rax;
   typedef exprFromReg<RegRCX> rcx;
   typedef exprFromReg<RegRDX> rdx;
   typedef exprFromReg<RegRBX> rbx;
   typedef exprFromReg<RegRSP> rsp;
   typedef exprFromReg<RegRBP> rbp;
   typedef exprFromReg<RegRSI> rsi;

   typedef exprFromReg<RegR8>  r8;
   typedef exprFromReg<RegR9>  r9;
   typedef exprFromReg<RegR10> r10;
   typedef exprFromReg<RegR11> r11;
   typedef exprFromReg<RegR12> r12;
   typedef exprFromReg<RegR13> r13;
   typedef exprFromReg<RegR13> r14;
   typedef exprFromReg<RegR13> r15;


   ERASM_META_ASSERT_EQUAL((effectiveAddressFromExpr<rax>),
			  (effectiveAddressFromReg2<RegRAX,RegNone,Zero>));
   ERASM_META_ASSERT_EQUAL((effectiveAddressFromExpr<rcx>),
			  (effectiveAddressFromReg2<RegRCX,RegNone,Zero>));
   ERASM_META_ASSERT((elem<effectiveAddressFromExpr<plus<rax,rcx> >,
			  toList<effectiveAddressFromReg2<RegRCX,RegRAX,One>,
				 effectiveAddressFromReg2<RegRAX,RegRCX,One> > >));
   ERASM_META_ASSERT_EQUAL((effectiveAddressFromExpr<plus<rax,times<rcx,Two> > >),
			  (effectiveAddressFromReg2<RegRAX,RegRCX,Two>));
   ERASM_META_ASSERT_EQUAL((effectiveAddressFromScaledReg<RegRAX,Two>),
			  (effectiveAddressFromReg2<RegNone,RegRAX,Two>));
   ERASM_META_ASSERT_ERROR((sibIndex<effectiveAddressFromExpr<times<Two,rsp> > > ));
   ERASM_META_ASSERT_ERROR((effectiveAddressFromExpr<times<Two,rsp> > ));
   ERASM_META_ASSERT_ERROR((effectiveAddressFromExpr<times<Six,rax> > ));
   
   ERASM_META_ASSERT_ERROR((effectiveAddressFromScaledReg<RegRAX,Zero>));
   ERASM_META_ASSERT_ERROR((effectiveAddressFromScaledReg<RegRAX,Six>));
   ERASM_META_ASSERT((notEq<Nine,
			   sibIndex<effectiveAddressFromExpr<times<One,
								   rsp> > > >));

   ERASM_META_ASSERT_NOT((greaterEq<Zero,Eight>));
   ERASM_META_ASSERT_NOT((isExtendedRegCode<regCode<Register64<0> > >));
   ERASM_META_ASSERT_NOT((isRexAddress<effectiveAddressFromReg<Register64<0> > >));
   ERASM_META_ASSERT_NOT((isRexAddress<effectiveAddressFromReg<Register32<0> > >));
}

}}



namespace erasm { namespace prelude { namespace x64 {


template<class P,class D,class Q,class E>
inline
EVAL_TEMPLATE((plus<Expr<P,D>,Expr<Q,E> >))
operator+(const Expr<P,D>& e1,const Expr<Q,E>& e2)
{
   typedef EVAL_TEMPLATE((plus<Expr<P,D>,Expr<Q,E> >)) result;
   return result(e1.get_disp()+e2.get_disp());
}

template<int n,class P,class D>
inline
EVAL_TEMPLATE((plus<Expr<P,D>,exprFromReg<Register64<n> > >))
operator+(const Expr<P,D>& e2,const Register64<n>& e1)
{
   typedef EVAL_TEMPLATE((plus<Expr<P,D>,exprFromReg<Register64<n> > >)) 
      result;
   return result(e2.get_disp());
}


template<class P,class D>
inline
Expr<P,Disp32>
operator+(const Expr<P,D>& e1,disp32_type d)
{
   typedef Expr<P,Disp32> result;
   return result(e1.get_disp()+d);
}


template<class P,class D>
inline
Expr<P,EVAL_TEMPLATE((plus<D,Disp8>)) >
operator+(const Expr<P,D>& e1,disp8_type d)
{
   typedef Expr<P,EVAL_TEMPLATE((plus<D,Disp8>)) > result;
   return result(e1.get_disp()+d);
}

template<class P,class D,int i>
inline
Expr<P,EVAL_TEMPLATE((plus<D,minDispRep<Int<i> > >)) >
operator+(const Expr<P,D>& e1,Int<i> d)
{
   typedef Expr<P,EVAL_TEMPLATE((plus<D,minDispRep<Int<i> > >)) > result;
   return result(e1.get_disp()+i);
}


///////////////////////////////////////////////////////////////////////////

template<int n,int m>
inline
EVAL_TEMPLATE((plus<exprFromReg<Register64<n> >,
		    exprFromReg<Register64<m> > >))
operator+(const Register64<n>& e1,const Register64<m>& e2)
{
   typedef EVAL_TEMPLATE((plus<exprFromReg<Register64<n> >,
			       exprFromReg<Register64<m> > >)) result;
   return result();
}

template<int i>
inline
EVAL_TEMPLATE((exprFromRegDisp<Register64<i>,Disp32>))
operator+(const Register64<i>& e1,disp32_type d)
{
   typedef EVAL_TEMPLATE((exprFromRegDisp<Register64<i>,Disp32>))  result;
   return result(d);
}

template<int i>
inline
EVAL_TEMPLATE((exprFromRegDisp<Register64<i>,Disp8>))
operator+(const Register64<i>& e1,disp8_type d)
{
   typedef EVAL_TEMPLATE((exprFromRegDisp<Register64<i>,Disp8>))  result;
   return result(d);
}

template<int i,int n>
inline
EVAL_TEMPLATE((exprFromRegDisp<Register64<i>,minDispRep<Int<n> > >))
operator+(const Register64<i>& e1,Int<n>& d)
{
   typedef EVAL_TEMPLATE((exprFromRegDisp<Register64<i>,minDispRep<Int<n> > >))
      result;
   return result(n);
}

template<int n,class P,class D>
inline
EVAL_TEMPLATE((times<Int<n>,Expr<P,D> >))
operator*(const Int<n>&,const Expr<P,D>& e)
{
   typedef EVAL_TEMPLATE((times<Int<n>,Expr<P,D> >)) result;
   return result(e.get_disp()*n);
}

template<int n,int i>
inline
EVAL_TEMPLATE((times<Int<n>,exprFromReg<Register64<i> > >))
operator*(const Int<n>&,const Register64<i>& e)
{
   typedef EVAL_TEMPLATE((times<Int<n>,exprFromReg<Register64<i> > >)) 
      result;
   return result();
}


template<int n,class P,class D>
inline
EVAL_TEMPLATE((times<Int<n>,Expr<P,D> >))
operator*(const Expr<P,D>& e,const Int<n>&)
{
   typedef EVAL_TEMPLATE((times<Int<n>,Expr<P,D> >)) result;
   return result(e.get_disp()*n);
}

template<int n,int i>
inline
EVAL_TEMPLATE((times<Int<n>,exprFromReg<Register64<i> > >))
operator*(const Register64<i>& e,const Int<n>&)
{
   typedef EVAL_TEMPLATE((times<Int<n>,exprFromReg<Register64<i> > >)) 
      result;
   return result();
}



/////////////////////////////////////////////////////////////////////////
// 32 bit addressing mode
/////////////////////////////////////////////////////////////////////////

template<class P,class D,class Q,class E>
inline
EVAL_TEMPLATE((plus<Expr32<P,D>,Expr32<Q,E> >))
operator+(const Expr32<P,D>& e1,const Expr32<Q,E>& e2)
{
   typedef EVAL_TEMPLATE((plus<Expr32<P,D>,Expr32<Q,E> >)) result;
   return result(e1.get_disp()+e2.get_disp());
}

template<int n,class P,class D>
inline
EVAL_TEMPLATE((plus<Expr32<P,D>,expr32FromReg<Register32<n> > >))
operator+(const Expr32<P,D> & e1,const Register32<n>& e2)
{
   typedef EVAL_TEMPLATE((plus<Expr32<P,D>,expr32FromReg<Register32<n> > >))
      result;
   return result(e1.get_disp());
}

template<class P,class D>
inline
Expr32<P,Disp32>
operator+(const Expr32<P,D>& e1,disp32_type d)
{
   typedef Expr32<P,Disp32> result;
   return result(e1.get_disp()+d);
}

template<class P,class D>
inline
Expr32<P,EVAL_TEMPLATE((plus<D,Disp8>)) >
operator+(const Expr32<P,D>& e1,disp8_type d)
{
   typedef Expr32<P,EVAL_TEMPLATE((plus<D,Disp8>)) > result;
   return result(e1.get_disp()+d);
}

template<class P,class D,int n>
inline
Expr32<P,EVAL_TEMPLATE((minDispRep<Int<n> >)) >
operator+(const Expr32<P,D>& e1,Int<n> d)
{
   typedef Expr32<P,EVAL_TEMPLATE((minDispRep<Int<n> >)) > result;
   return result(e1.get_disp()+n);
}

template<int n,int m>
inline
EVAL_TEMPLATE((plus<expr32FromReg<Register32<n> >,
	            expr32FromReg<Register32<m> > >))
operator+(const Register32<n>& e1,const Register32<m>& e2)
{
   typedef EVAL_TEMPLATE((plus<expr32FromReg<Register32<n> >,
			       expr32FromReg<Register32<m> > >)) result;
   return result();
}

template<int i>
inline
EVAL_TEMPLATE((expr32FromRegDisp<Register32<i>,Disp32 >))
operator+(const Register32<i>& e1,disp32_type d)
{
   typedef EVAL_TEMPLATE((expr32FromRegDisp<Register32<i>,Disp32 >)) result;
   return result(d);
}

template<int i>
inline
EVAL_TEMPLATE((expr32FromRegDisp<Register32<i>,Disp8 >))
operator+(const Register32<i>& e1,disp8_type d)
{
   typedef EVAL_TEMPLATE((expr32FromRegDisp<Register32<i>,Disp8 >)) result;
   return result(d);
}

template<int i,int n>
inline
EVAL_TEMPLATE((expr32FromRegDisp<Register32<i>,minDispRep<Int<n> > >))
operator+(const Register32<i>& e1,Int<n> d)
{
   typedef EVAL_TEMPLATE((expr32FromRegDisp<Register32<i>,
					    minDispRep<Int<n> > >))
      result;
   return result(n);
}


template<int n,class P,class D>
inline
EVAL_TEMPLATE((times<Int<n>,Expr32<P,D> >))
operator*(const Int<n>&,const Expr32<P,D>& e)
{
   typedef EVAL_TEMPLATE((times<Int<n>,Expr32<P,D> >)) result;
   return result(e.get_disp()*n);
}

template<int n,int i>
inline
EVAL_TEMPLATE((times<Int<n>,expr32FromReg<Register32<i> > >))
operator*(const Int<n>&,const Register32<i>& e)
{
   typedef EVAL_TEMPLATE((times<Int<n>,expr32FromReg<Register32<i> > >)) 
      result;
   return result();
}

template<int n,class P,class D>
inline
EVAL_TEMPLATE((times<Int<n>,Expr32<P,D> >))
operator*(const Expr32<P,D>& e,const Int<n>&)
{
   typedef EVAL_TEMPLATE((times<Int<n>,Expr32<P,D> >)) result;
   return result(e.get_disp()*n);
}

template<int n,int i>
inline
EVAL_TEMPLATE((times<Int<n>,expr32FromReg<Register32<i> > >))
operator*(const Register32<i>& e,const Int<n>&)
{
   typedef EVAL_TEMPLATE((times<Int<n>,expr32FromReg<Register32<i> > >)) 
      result;
   return result();
}

/////////////////////////////////////////////////////////////////////////
// let's make it commutative 

template<int n,class P,class D>
inline
EVAL_TEMPLATE((plus<Expr<P,D>,exprFromReg<Register64<n> > >))
operator+(const Register64<n>& e1,const Expr<P,D>& e2)
{
   typedef EVAL_TEMPLATE((plus<Expr<P,D>,exprFromReg<Register64<n> > >)) 
      result;
   return result(e2.get_disp());
}


template<class P,class D>
inline
Expr<P,Disp32>
operator+(disp32_type d,const Expr<P,D>& e1)
{
   typedef Expr<P,Disp32> result;
   return result(e1.get_disp()+d);
}


template<class P,class D>
inline
Expr<P,EVAL_TEMPLATE((plus<D,Disp8>)) >
operator+(disp8_type d,const Expr<P,D>& e1)
{
   typedef Expr<P,EVAL_TEMPLATE((plus<D,Disp8>)) > result;
   return result(e1.get_disp()+d);
}

template<class P,class D,int i>
inline
Expr<P,EVAL_TEMPLATE((plus<D,minDispRep<Int<i> > >)) >
operator+(Int<i> d,const Expr<P,D>& e1)
{
   typedef Expr<P,EVAL_TEMPLATE((plus<D,minDispRep<Int<i> > >)) > result;
   return result(e1.get_disp()+i);
}


///////////////////////////////////////////////////////////////////////////


template<int i>
inline
EVAL_TEMPLATE((exprFromRegDisp<Register64<i>,Disp32>))
operator+(disp32_type d,const Register64<i>& e1)
{
   typedef EVAL_TEMPLATE((exprFromRegDisp<Register64<i>,Disp32>))  result;
   return result(d);
}

template<int i>
inline
EVAL_TEMPLATE((exprFromRegDisp<Register64<i>,Disp8>))
operator+(disp8_type d,const Register64<i>& e1)
{
   typedef EVAL_TEMPLATE((exprFromRegDisp<Register64<i>,Disp8>))  result;
   return result(d);
}

template<int i,int n>
inline
EVAL_TEMPLATE((exprFromRegDisp<Register64<i>,minDispRep<Int<n> > >))
operator+(Int<n>& d,const Register64<i>& e1)
{
   typedef EVAL_TEMPLATE((exprFromRegDisp<Register64<i>,minDispRep<Int<n> > >))
      result;
   return result(n);
}


/////////////////////////////////////////////////////////////////////////
// 32 bit addressing mode
/////////////////////////////////////////////////////////////////////////

template<int n,class P,class D>
inline
EVAL_TEMPLATE((plus<Expr32<P,D>,expr32FromReg<Register32<n> > >))
operator+(const Register32<n>& e2,const Expr32<P,D> & e1)
{
   typedef EVAL_TEMPLATE((plus<Expr32<P,D>,expr32FromReg<Register32<n> > >))
      result;
   return result(e1.get_disp());
}

template<class P,class D>
inline
Expr32<P,Disp32>
operator+(disp32_type d,const Expr32<P,D>& e1)
{
   typedef Expr32<P,Disp32> result;
   return result(e1.get_disp()+d);
}

template<class P,class D>
inline
Expr32<P,EVAL_TEMPLATE((plus<D,Disp8>)) >
operator+(disp8_type d,const Expr32<P,D>& e1)
{
   typedef Expr32<P,EVAL_TEMPLATE((plus<D,Disp8>)) > result;
   return result(e1.get_disp()+d);
}

template<class P,class D,int n>
inline
Expr32<P,EVAL_TEMPLATE((minDispRep<Int<n> >)) >
operator+(Int<n> d,const Expr32<P,D>& e1)
{
   typedef Expr32<P,EVAL_TEMPLATE((minDispRep<Int<n> >)) > result;
   return result(e1.get_disp()+n);
}


template<int i>
inline
EVAL_TEMPLATE((expr32FromRegDisp<Register32<i>,Disp32 >))
operator+(disp32_type d,const Register32<i>& e1)
{
   typedef EVAL_TEMPLATE((expr32FromRegDisp<Register32<i>,Disp32 >)) result;
   return result(d);
}

template<int i>
inline
EVAL_TEMPLATE((expr32FromRegDisp<Register32<i>,Disp8 >))
operator+(disp8_type d,const Register32<i>& e1)
{
   typedef EVAL_TEMPLATE((expr32FromRegDisp<Register32<i>,Disp8 >)) result;
   return result(d);
}

template<int i,int n>
inline
EVAL_TEMPLATE((expr32FromRegDisp<Register32<i>,minDispRep<Int<n> > >))
operator+(Int<n> d,const Register32<i>& e1)
{
   typedef EVAL_TEMPLATE((expr32FromRegDisp<Register32<i>,
					    minDispRep<Int<n> > >))
   result;
   return result(n);
}


}}}


#endif // MY_X64_ADDR64
