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
#ifndef MY_ERASM_X86_ADDR16_HPP
#define MY_ERASM_X86_ADDR16_HPP

#include <assert.h>

#include "meta_polynomial.hpp"
#include "erasm/x86_common.hpp"
#include "erasm/x64_addr64.hpp"


#define X86_ADDR16_DEFINE_DATA1(DATACON)		\
   namespace x86 {					\
   template<class X=_> struct DATACON;			\
   }							\
   DEFINE_FOREIGN_DATA1(x86::DATACON,class);

#define X86_ADDR16_DEFINE_DATA2(DATACON)			\
   namespace x86 {						\
   template<class X=_,class Y=_> struct DATACON;		\
   }								\
   DEFINE_FOREIGN_DATA2(x86::DATACON,class,class);

#define X86_ADDR16_DEFINE_DATA3(DATACON)				\
   namespace x86 {							\
   template<class X=_,class Y=_,class Z=_> struct DATACON;		\
   }									\
   DEFINE_FOREIGN_DATA3(x86::DATACON,class,class,class);

#define X86_ADDR16_DEFINE_DATA1_DERIVING_EQ(DATACON)	\
   X86_ADDR16_DEFINE_DATA1(DATACON);			\
   DERIVING_EQ1(x86::DATACON);

#define X86_ADDR16_DEFINE_DATA2_DERIVING_EQ(DATACON)	\
   X86_ADDR16_DEFINE_DATA2(DATACON);			\
   DERIVING_EQ2(x86::DATACON);

#define X86_ADDR16_DEFINE_DATA3_DERIVING_EQ(DATACON)	\
   X86_ADDR16_DEFINE_DATA3(DATACON);			\
   DERIVING_EQ3(x86::DATACON);


#define X86_DEFINE_FUNC X86_DEFINE_FUNC1
#define X86_DEFINE_FUNC1(FUNC,ARG,PAREN_BODY)	\
   namespace x86 {				\
   template<class X=_>  struct FUNC;		\
   }						\
   DEFINE_FUNC_BODY1(x86::FUNC,ARG,PAREN_BODY)


#define X86_ADDR16_DEFINE_FUNC_STRICT(FUNC)			\
   namespace x86 {						\
   template<class X=_>  struct FUNC;				\
   }								\
   DEFINE_PATTERN_MATCHER_STRICT(x86::FUNC);



#define X86_ADDR16_DEFINE_FUNC_STRICT_STRICT(FUNC)	\
   namespace x86 {					\
   template<class X=_,class Y=_>  struct FUNC;		\
   }							\
   DEFINE_PATTERN_MATCHER_STRICT_STRICT(x86::FUNC);


#define X86_ADDR16_DEFINE_FUNC_STRICT_STRICT_STRICT(FUNC)	\
   namespace x86 {						\
   template<class X=_,class Y=_,class Z=_>  struct FUNC;	\
   }								\
   DEFINE_PATTERN_MATCHER_STRICT_STRICT_STRICT(x86::FUNC);

namespace erasm { namespace prelude { namespace x86 { 
using x64::register16;
using x64::regCode;
}}}


namespace erasm { namespace prelude {
namespace x86 {
using namespace erasm::x86;
using x64::NoDisp;
using x64::Disp8;
using x64::Disp16;
using x64::Disp32;
using x64::Disp;
}


X86_ADDR16_DEFINE_DATA2(Expr16);
DERIVING_EQ2(x86::Expr16);
DERIVING_ABELIAN2(x86::Expr16);
DERIVING_Z_MODULE2(x86::Expr16);

X86_ADDR16_DEFINE_DATA3(EffectiveAddress16);
DERIVING_EQ3(x86::EffectiveAddress16);

X86_ADDR16_DEFINE_FUNC_STRICT(effectiveAddress16FromExpr16);
X86_ADDR16_DEFINE_FUNC_STRICT(effectiveAddress16FromTerm);
X86_ADDR16_DEFINE_FUNC_STRICT_STRICT(effectiveAddress16FromTerm2);
X86_ADDR16_DEFINE_FUNC_STRICT(effectiveAddress16FromReg);
X86_ADDR16_DEFINE_FUNC_STRICT_STRICT(effectiveAddress16FromReg2);

X86_ADDR16_DEFINE_FUNC_STRICT(exprFromReg);
X86_ADDR16_DEFINE_FUNC_STRICT_STRICT(exprFromRegDisp);
X86_ADDR16_DEFINE_FUNC_STRICT(polynomialFromExpr16);

X86_ADDR16_DEFINE_FUNC_STRICT_STRICT(modrmRM);
X86_ADDR16_DEFINE_FUNC_STRICT_STRICT_STRICT(modrmMod);
X86_ADDR16_DEFINE_FUNC_STRICT_STRICT_STRICT(dispSize);
X86_ADDR16_DEFINE_FUNC_STRICT_STRICT_STRICT(dispType);

X86_ADDR16_DEFINE_FUNC_STRICT_STRICT(setDisp);

X86_DEFINE_FUNC(minDispRep,X,
		( ifn<equal<X,Zero> , x86::NoDisp ,
		      and2<lessEq<Int<CHAR_MIN>,X> , 
			   lessEq<X,Int<CHAR_MAX> > > ,
		      x86::Disp8,
		      x86::Disp16 > ));


namespace x86 { 
ERASM_META_ASSERT_EQUAL((register16<Zero>),(RegAX));
ERASM_META_ASSERT_EQUAL((register16<One>) ,(RegCX));
}

struct x86_16_names
{
   typedef x64::Error_Invalid_displacement       Error_Invalid_displacement;
   typedef x64::Error_Invalid_address_expression Error_Invalid_address_expression;
   typedef x64::RegNone RegNone;
   typedef x64:: RegAX RegAX;
   typedef x64:: RegCX RegCX;
   typedef x64:: RegDX RegDX;
   typedef x64:: RegBX RegBX;
   typedef x64:: RegSP RegSP;
   typedef x64:: RegBP RegBP;
   typedef x64:: RegSI RegSI;
   typedef x64:: RegDI RegDI;
   typedef x64:: NoDisp NoDisp;
   typedef x64:: Disp8 Disp8;
   typedef x64:: Disp16 Disp16;
};

template<class P,class D>
struct eval< x86::effectiveAddress16FromExpr16<x86::Expr16<P,D> > > 
   : x86_16_names
{
   typedef termList<P> L;
   typedef length<L>   len;
   typedef if3< equal<len,Zero> , 
		x86::EffectiveAddress16<RegNone,RegNone,D>,
		equal<len,One>  , 
		x86::setDisp<D,x86::effectiveAddress16FromTerm<head<L> > >,
		equal<len,Two>  , 
		x86::setDisp<D,x86::effectiveAddress16FromTerm2<index<L,Zero>,
								index<L,One> > >,
		error<Error_Invalid_address_expression,P> > r;
   typedef EVAL_TEMPLATE((r)) result;
};



template<class Deg,class Coeff>
struct eval< x86::effectiveAddress16FromTerm<Term<Deg,Coeff> > >
{
   ERASM_META_ASSERT_EQUAL((Coeff),(One));
   typedef EVAL_TEMPLATE(( x86::effectiveAddress16FromReg<x86::register16<Deg> > )) result;
};


template<class Deg1,class Coeff1,class Deg2,class Coeff2>
struct eval< x86::effectiveAddress16FromTerm2<Term<Deg1,Coeff1>,Term<Deg2,Coeff2> > >
{
   ERASM_META_ASSERT_EQUAL((Coeff1),(One));
   ERASM_META_ASSERT_EQUAL((Coeff2),(One));
   typedef x86::register16<Deg1> reg1;
   typedef x86::register16<Deg2> reg2;

   typedef EVAL_TEMPLATE(( x86::effectiveAddress16FromReg2<reg1,reg2> )) result;
};




template<int i>
struct eval< x86::effectiveAddress16FromReg< x86::Register16<i> > > 
   : x86_16_names
{
   typedef x86::Register16<i> reg;
   typedef if_< elem<reg,toList<RegSI,RegDI,RegBP,RegBX> > , 
		x86::EffectiveAddress16<reg,RegNone,NoDisp>,
		error<Error_Invalid_address_expression,reg> >  r;
   typedef EVAL_TEMPLATE((r)) result;
};


template<int i,int j>
struct eval< x86::effectiveAddress16FromReg2<x86::Register16<i> ,
					     x86::Register16<j> > > 
: x86_16_names
{
   typedef x86::Register16<i> reg1;
   typedef x86::Register16<j> reg2;
   typedef if2< and2<elem<reg1,toList<RegBX,RegBP> > ,
		     elem<reg2,toList<RegSI,RegDI> > > , 
		x86::EffectiveAddress16<reg1,reg2,NoDisp>,
		and2<elem<reg2,toList<RegBX,RegBP> > ,
		     elem<reg1,toList<RegSI,RegDI> > > , 
		x86::EffectiveAddress16<reg2,reg1,NoDisp>,
		error<Error_Invalid_address_expression,reg1,reg2> > r;
   typedef EVAL_TEMPLATE((r)) result;
};


template<int i>
struct eval<x86::exprFromReg<x86::Register16<i> > >
   : x86_16_names
{
   typedef monomial<Int<i> > p;
   typedef x86::Expr16<p,NoDisp> result;
};

template<int i,class D>
struct eval<x86::exprFromRegDisp<x86::Register16<i>,D> >
{
   typedef monomial<Int<i> > p;
   typedef x86::Expr16<p,D> result;
};


template<class P,class D>
struct eval<x86::polynomialFromExpr16<x86::Expr16<P,D> > >
{
   typedef EVAL_TEMPLATE((P)) result;
};


template<> struct eval<x86::modrmRM<x86::RegBX,x86::RegSI> > 
{ typedef Zero result ; };
template<> struct eval<x86::modrmRM<x86::RegBX,x86::RegDI> > 
{ typedef One result ; };
template<> struct eval<x86::modrmRM<x86::RegBP,x86::RegSI> > 
{ typedef Two result ; } ;
template<> struct eval<x86::modrmRM<x86::RegBP,x86::RegDI> > 
{ typedef Three result ; } ;
template<> struct eval<x86::modrmRM<x86::RegSI,x86::RegNone> > 
{ typedef Four result ; } ;
template<> struct eval<x86::modrmRM<x86::RegDI,x86::RegNone> > 
{ typedef Five result ; } ;
template<> struct eval<x86::modrmRM<x86::RegBP,x86::RegNone> > 
{ typedef Six result ; } ;
template<> struct eval<x86::modrmRM<x86::RegBX,x86::RegNone> > 
{ typedef Seven result ; } ;
template<> struct eval<x86::modrmRM<x86::RegNone,x86::RegNone> > 
{ typedef Six result ; } ;


ERASM_META_ASSERT_EQUAL(( x86::modrmRM<x86::RegBX,x86::RegSI> ),(Zero));
ERASM_META_ASSERT_EQUAL(( x86::modrmRM<x86::RegBP,x86::RegNone>),(Six));
ERASM_META_ASSERT_EQUAL(( x86::modrmRM<x86::RegNone,x86::RegNone>),(Six));
ERASM_META_ASSERT_EQUAL(( x86::modrmRM<x86::RegBX,x86::RegNone>),(Seven));

template<> 
struct eval<x86::modrmMod<x86::RegBP,x86::RegNone,x86::NoDisp> > 
{ typedef One result ; };

template<class Base,class Index> 
struct eval<x86::modrmMod<Base,Index,x86::NoDisp>  >  
{ typedef Zero result ; };

template<> 
struct eval<x86::modrmMod<x86::RegNone,x86::RegNone,x86::Disp8> >
{ typedef Zero result ; };

template<class Base,class Index> 
struct eval<x86::modrmMod<Base,Index,x86::Disp8>   >  
{ typedef One  result; };

template<>
struct eval<x86::modrmMod<x86::RegNone,x86::RegNone,x86::Disp16>  >
{ typedef Zero result; };

template<class Base,class Index> 
struct eval<x86::modrmMod<Base,Index,x86::Disp16> >      
{ typedef Two result ; };


ERASM_META_ASSERT_EQUAL((x86::modrmMod<x86::RegBX,x86::RegSI,x86::NoDisp>),
		       (Zero));
ERASM_META_ASSERT_EQUAL((x86::modrmMod<x86::RegNone,x86::RegNone,x86::NoDisp>),
		       (Zero));
ERASM_META_ASSERT_EQUAL((x86::modrmMod<x86::RegBP,x86::RegNone,x86::NoDisp>),
		       (One));
ERASM_META_ASSERT_EQUAL((x86::modrmMod<x86::RegNone,x86::RegNone,x86::Disp8>),
		       (Zero));
ERASM_META_ASSERT_EQUAL((x86::modrmMod<x86::RegBX,x86::RegNone,x86::Disp8>),
		       (One));
ERASM_META_ASSERT_EQUAL((x86::modrmMod<x86::RegNone,x86::RegNone,x86::Disp16>),
		       (Zero));
ERASM_META_ASSERT_EQUAL((x86::modrmMod<x86::RegBX,x86::RegNone,x86::Disp16>),
		       (Two));

template<class Base,class Index>
struct eval<x86::dispSize<Base,Index,x86::NoDisp> >
{ typedef Zero result ; };

template<>
struct eval<x86::dispSize<x86::RegBP,x86::RegNone,x86::NoDisp> >
{ typedef One result ;};

template<>
struct eval<x86::dispSize<x86::RegNone,x86::RegNone,x86::NoDisp> >	
{ typedef Two result ;};

template<class Base,class Index>
struct eval<x86::dispSize<Base,Index,x86::Disp8> >	
{ typedef One  result ;};

template<>
struct eval<x86::dispSize<x86::RegNone,x86::RegNone,x86::Disp8> >	
{ typedef Two result ;};

template<class Base,class Index>
struct eval<x86::dispSize<Base,Index,x86::Disp16> >	
{ typedef Two result ;};

template<>
struct eval<x86::dispSize<x86::RegNone,x86::RegNone,x86::Disp16> >	
{ typedef Two result ;};

template<class Base,class Index,int n>
struct eval<x86::dispType<Base,Index,x86::Disp<n> > >
   : x86_16_names
{
   struct Error_invalid_displacement_type;
   typedef x86::Disp<n> D;
   typedef x86::dispSize<Base,Index,D> Size;
   typedef ifn<equal<Size,Zero> , NoDisp ,
	       equal<Size,One>  , Disp8 ,
	       equal<Size,Two>  , Disp16 ,
	       error<Error_invalid_displacement_type> > r;
   typedef EVAL_TEMPLATE((r)) result;
};


template<class NewDisp,class Base,class Index,class Disp>
struct eval<x86::setDisp<NewDisp,x86::EffectiveAddress16<Base,Index,Disp> > >
{
   typedef x86::EffectiveAddress16<Base,Index,NewDisp> result;
};


namespace x86 { 


template<class P,class D>
struct Expr16
{
   typedef EVAL_TEMPLATE((D)) Disp;
   typedef typename Disp::value_type disp_type;

   typedef Expr16 type;
   typedef P  regExpr16;

   explicit Expr16(disp_type d=0) : disp(d) {}
   
   disp_type get_disp() const
      { return disp ; }
   disp_type   disp;
};

template<class Base,class Index,class Disp>
struct EffectiveAddress16
{
   typedef EffectiveAddress16 this_type;
   typedef EVAL_TEMPLATE((modrmMod<Base,Index,Disp>)) mod_type;
   typedef EVAL_TEMPLATE((modrmRM<Base,Index>))  rm_type;
   typedef EVAL_TEMPLATE((dispType<Base,Index,Disp>))    realDisp;
   typedef typename realDisp::value_type    disp_type;
   enum { disp_size =  realDisp::size  };

   typedef False need_sib;


   explicit EffectiveAddress16(disp_type d=0) : disp(d) {}

   bool has_sib() const
      { return false ; }

   int get_mod() const
      {	 return mod_type::value;  }

   int get_rm() const 
      {  return rm_type::value ;   }

   int get_disp_size() const
      { return disp_size ; }
   

   disp_type get_disp() const
      { return disp ; }

   bool get_rex_x() const
      { return false ; }
   bool get_rex_b() const
      { return false ; }

   bool is16bit() const
      { return true ; }


private:
   disp_type disp;
};


typedef EVAL((effectiveAddress16FromExpr16<Expr16<ZeroPolynomial,NoDisp> >  )) EffectiveAddress16Disp;

namespace UNIQUEID(test_addr16) {
   using namespace x86;
   ERASM_META_ASSERT_EQUAL((EffectiveAddress16Disp::mod_type),(Zero));
   ERASM_META_ASSERT_EQUAL((EffectiveAddress16Disp::rm_type)  ,(Six));
}

template<class P1,class D1,class P2,class D2>
inline
EVAL_TEMPLATE((plus<Expr16<P1,D1>,Expr16<P2,D2> >))
operator+ (const Expr16<P1,D1>& e1,const Expr16<P2,D2>& e2)
{
   typedef EVAL_TEMPLATE((plus<Expr16<P1,D1>,Expr16<P2,D2> >)) result;
   return result(e1.get_disp()+e2.get_disp());
}

template<int i,class P1,class D1>
inline
EVAL_TEMPLATE((plus<exprFromReg<Register16<i> >,Expr16<P1,D1> >))
operator+ (const Expr16<P1,D1>& e2,const Register16<i>& e1)
{
   typedef EVAL_TEMPLATE((plus<exprFromReg<Register16<i> >,Expr16<P1,D1> >)) 
      result;
   return result(e2.get_disp());
}

template<class P1,class D1>
inline
Expr16<P1,Disp16>
operator+ (const Expr16<P1,D1>& e1,disp16_type d)
{
   typedef Expr16<P1,Disp16> result;
   return result(e1.get_disp() + d);
}

template<class P1,class D1>
inline
Expr16<P1,plus<D1,Disp8> >
operator+ (const Expr16<P1,D1>& e1,disp8_type d)
{
   typedef Expr16<P1,plus<D1,Disp8> > result;
   return result(e1.get_disp() + d);
}

template<class P1,class D1,int n >
inline
Expr16<P1,plus<D1,minDispRep<Int<n> > > >
operator+ (const Expr16<P1,D1>& e1,Int<n>)
{
   typedef Expr16<P1,plus<D1,minDispRep<Int<n> > > > result;
   return result(e1.get_disp() + n);
}


template<int i,int j>
inline
EVAL_TEMPLATE((plus<exprFromReg<Register16<i> >,
		    exprFromReg<Register16<j> > >))
operator+ (const Register16<i>& e1,const Register16<j>& e2)
{
   typedef EVAL_TEMPLATE((plus<exprFromReg<Register16<i> >,
			       exprFromReg<Register16<j> > >)) result;
   return result();
}

template<int i>
inline
EVAL_TEMPLATE((exprFromRegDisp<Register16<i>,Disp16>))
operator+ (const Register16<i>& e1,disp16_type d)
{
   typedef EVAL_TEMPLATE((exprFromRegDisp<Register16<i>,Disp16>)) result;
   return result(d);
}

template<int i>
inline
EVAL_TEMPLATE((exprFromRegDisp<Register16<i>,Disp8>))
operator+ (const Register16<i>& e1,disp8_type d)
{
   typedef EVAL_TEMPLATE((exprFromRegDisp<Register16<i>,Disp8>)) result;
   return result(d);
}

template<int i,int n>
inline
EVAL_TEMPLATE((exprFromRegDisp<Register16<i>,minDispRep<Int<n> > >))
operator+ (const Register16<i>& e1,Int<n>)
{
   typedef EVAL_TEMPLATE((exprFromRegDisp<Register16<i>,
					  minDispRep<Int<n> > >)) result;
   return result(n);
}


////////////////////////////////////////////////////////////////////
// For commutativity 
////////////////////////////////////////////////////////////////////

template<int i,class P1,class D1>
inline
EVAL_TEMPLATE((plus<exprFromReg<Register16<i> >,Expr16<P1,D1> >))
operator+ (const Register16<i>& e1,const Expr16<P1,D1>& e2)
{
   typedef EVAL_TEMPLATE((plus<exprFromReg<Register16<i> >,Expr16<P1,D1> >)) 
      result;
   return result(e2.get_disp());
}

template<class P1,class D1>
inline
Expr16<P1,Disp16>
operator+ (disp16_type d,const Expr16<P1,D1>& e1)
{
   typedef Expr16<P1,Disp16> result;
   return result(e1.get_disp() + d);
}

template<class P1,class D1>
inline
Expr16<P1,plus<D1,Disp8> >
operator+ (disp8_type d,const Expr16<P1,D1>& e1)
{
   typedef Expr16<P1,plus<D1,Disp8> > result;
   return result(e1.get_disp() + d);
}

template<class P1,class D1,int n>
inline
Expr16<P1,plus<D1,minDispRep<Int<n> > > >
operator+ (Int<n>,const Expr16<P1,D1>& e1)
{
   typedef Expr16<P1,plus<D1,minDispRep<Int<n> > > > result;
   return result(e1.get_disp() + n);
}


template<int i>
inline
EVAL_TEMPLATE((exprFromRegDisp<Register16<i>,Disp16>))
operator+ (disp16_type d,const Register16<i>& e1)
{
   typedef EVAL_TEMPLATE((exprFromRegDisp<Register16<i>,Disp16>)) result;
   return result(d);
}

template<int i>
inline
EVAL_TEMPLATE((exprFromRegDisp<Register16<i>,Disp8>))
operator+ (disp8_type d,const Register16<i>& e1)
{
   typedef EVAL_TEMPLATE((exprFromRegDisp<Register16<i>,Disp8>)) result;
   return result(d);
}

template<int i,int n>
inline
EVAL_TEMPLATE((exprFromRegDisp<Register16<i>,minDispRep<Int<n> > >))
operator+ (Int<n>,const Register16<i>& e1)
{
   typedef EVAL_TEMPLATE((exprFromRegDisp<Register16<i>,
					  minDispRep<Int<n> > >)) result;
   return result(n);
}


}}}

namespace erasm { namespace prelude {
namespace UNIQUEID(test_addr16) {
   using namespace x86;

   typedef monomial<regCode<RegAX> > ax;
   typedef monomial<regCode<RegCX> > cx;
   typedef monomial<regCode<RegDX> > dx;
   typedef monomial<regCode<RegBX> > bx;
   typedef monomial<regCode<RegSP> > sp;
   typedef monomial<regCode<RegBP> > bp;
   typedef monomial<regCode<RegSI> > si;
   typedef monomial<regCode<RegDI> > di;


   ERASM_META_ASSERT_EQUAL((effectiveAddress16FromTerm2< head<termList<si> > ,
			   head<termList<bx> > >) , 
			  (EffectiveAddress16<RegBX,RegSI,NoDisp>));

   ERASM_META_ASSERT_EQUAL((effectiveAddress16FromExpr16<plus<exprFromReg<RegBX>,exprFromReg<RegSI> > >),
			  (EffectiveAddress16<RegBX,RegSI,NoDisp>));

   ERASM_META_ASSERT_EQUAL((effectiveAddress16FromExpr16<exprFromReg<RegBX> >),
			  (EffectiveAddress16<RegBX,RegNone,NoDisp>));
   ERASM_META_ASSERT_EQUAL((effectiveAddress16FromExpr16<exprFromReg<RegSI> >),
			  (EffectiveAddress16<RegSI,RegNone,NoDisp>));

   ERASM_META_ASSERT_ERROR((effectiveAddress16FromExpr16<exprFromReg<RegAX> >));

   ERASM_META_ASSERT_EQUAL((effectiveAddress16FromExpr16<exprFromReg<RegSI> >),
			  (EffectiveAddress16<RegSI,RegNone,NoDisp>));
   ERASM_META_ASSERT_EQUAL((effectiveAddress16FromExpr16<plus<exprFromReg<RegBX>,exprFromReg<RegSI> > >),
			  (EffectiveAddress16<RegBX,RegSI,NoDisp>));

   ERASM_META_ASSERT_ERROR((effectiveAddress16FromTerm< head<termList<monomial<Zero> > > >));
   ERASM_META_ASSERT_EQUAL((effectiveAddress16FromTerm< head<termList<monomial<Seven> > > >),(EffectiveAddress16<RegDI,RegNone,NoDisp>));
   ERASM_META_ASSERT_EQUAL((effectiveAddress16FromTerm< head<termList<monomial<Six> > > >),(EffectiveAddress16<RegSI,RegNone,NoDisp>));

   typedef plus<bx,si> p;
   ERASM_META_ASSERT_EQUAL((effectiveAddress16FromTerm2< index<termList<p>,Zero>,index<termList<p>,One> >),
			  (EffectiveAddress16<RegBX,RegSI,NoDisp>));

   ERASM_META_ASSERT_EQUAL((effectiveAddress16FromReg<RegDI>),
			  (EffectiveAddress16<RegDI,RegNone,NoDisp>));

   ERASM_META_ASSERT_EQUAL((effectiveAddress16FromReg2<RegBP,RegDI>),
			  (EffectiveAddress16<RegBP,RegDI,NoDisp>));
   ERASM_META_ASSERT_EQUAL((effectiveAddress16FromReg2<RegSI,RegBX>),
			  (EffectiveAddress16<RegBX,RegSI,NoDisp>));

   ERASM_META_ASSERT_EQUAL((polynomialFromExpr16<exprFromReg<RegAX> >) ,
			  (monomial<Zero>));
   ERASM_META_ASSERT_EQUAL((polynomialFromExpr16<exprFromReg<RegCX> >) ,
			  (monomial<One>));
   ERASM_META_ASSERT_EQUAL((polynomialFromExpr16<exprFromReg<RegSP> >) ,
			  (monomial<Four>));


   ERASM_META_ASSERT_ERROR(( effectiveAddress16FromReg<RegAX> ));
   ERASM_META_ASSERT_ERROR(( effectiveAddress16FromReg<RegCX> ));
   ERASM_META_ASSERT_ERROR(( effectiveAddress16FromReg<RegDX> ));
   ERASM_META_ASSERT_ERROR(( effectiveAddress16FromReg<RegSP> ));
   ERASM_META_ASSERT_ERROR(( effectiveAddress16FromExpr16< plus<exprFromReg<RegBX> ,
							       exprFromReg<RegAX> > > ));
}


}}


#endif // MY_ERASM_X86_ADDR16_HPP
