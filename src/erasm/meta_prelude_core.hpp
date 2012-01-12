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
#ifndef MY_META_HASKELL_CORE_HPP
#define MY_META_HASKELL_CORE_HPP


#include <boost/static_assert.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/preprocessor/cat.hpp>
#include "meta_prelude_impl.hpp"


#ifdef __COUNTER__
#define UNIQUEID(id) BOOST_PP_CAT(id,__COUNTER__)
#else
#warning __COUNTER__ not implemented in this preprocessor
#warning Expect some token collissions
#define UNIQUEID(id) BOOST_PP_CAT(id,__LINE__)
#endif


#ifndef ERASM_NO_META_ASSERT

#define ERASM_META_ASSERT_WITH_ID(PAREN_EXP,ID)				\
template<class> struct BOOST_PP_CAT(ID,_struct) {			\
   enum { v = sizeof (::erasm::prelude::impl::assert_fail( (void (*) PAREN_EXP ) 0 , \
							   (void (*) (EVAL_TEMPLATE(PAREN_EXP))) 0 )) }; \
};							\
enum { BOOST_PP_CAT(ID,_enum) = BOOST_PP_CAT(ID,_struct)<void> :: v }

#define ERASM_META_ASSERT(PAREN_EXP) ERASM_META_ASSERT_WITH_ID(PAREN_EXP,UNIQUEID(assertion))

#else
#define ERASM_META_ASSERT(PAREN_EXP)
#endif

#define ERASM_META_ASSERT_NOT(PAREN_EXP) ERASM_META_ASSERT((not_< ERASM_META_REMOVE_PAREN(PAREN_EXP) >))

#define ERASM_META_ASSERT_ERROR(PAREN_EXP)     ERASM_META_ASSERT((::erasm::prelude::impl::isError<typename ERASM_META_REMOVE_PAREN(PAREN_EXP)> ))
#define ERASM_META_ASSERT_NOT_ERROR(PAREN_EXP) ERASM_META_ASSERT_NOT((::erasm::prelude::impl::isError<typename ERASM_META_REMOVE_PAREN(PAREN_EXP)> ))

#define EVAL(PAREN_EXP)            ::erasm::prelude::eval< ERASM_META_REMOVE_PAREN(PAREN_EXP) >::result
#define EVAL_TEMPLATE(PAREN_EXP)  typename ::erasm::prelude::eval<typename ERASM_META_REMOVE_PAREN(PAREN_EXP) >::result

#define ERASM_META_REMOVE_PAREN(PAREN_EXP) ::erasm::prelude::impl::get_arg1<void PAREN_EXP >::type


#define ERASM_META_ASSERT_PRED(PAREN_X,PAREN_Y,PRED)  ERASM_META_ASSERT((PRED<typename ERASM_META_REMOVE_PAREN(PAREN_X),typename ERASM_META_REMOVE_PAREN(PAREN_Y) > ))
#define ERASM_META_ASSERT_PRED3(PAREN_X,PAREN_Y,PAREN_Z,PRED)  ERASM_META_ASSERT((PRED<typename ERASM_META_REMOVE_PAREN(PAREN_X),typename ERASM_META_REMOVE_PAREN(PAREN_Y),typename ERASM_META_REMOVE_PAREN(PAREN_Z) > ))

#define ERASM_META_ASSERT_EQ(PAREN_X,PAREN_Y)  ERASM_META_ASSERT_PRED(PAREN_X,PAREN_Y,equal)
#define ERASM_META_ASSERT_NE(PAREN_X,PAREN_Y)  ERASM_META_ASSERT_PRED(PAREN_X,PAREN_Y,notEq)
#define ERASM_META_ASSERT_LT(PAREN_X,PAREN_Y)  ERASM_META_ASSERT_PRED(PAREN_X,PAREN_Y,less)
#define ERASM_META_ASSERT_LE(PAREN_X,PAREN_Y)  ERASM_META_ASSERT_PRED(PAREN_X,PAREN_Y,lessEq)
#define ERASM_META_ASSERT_GT(PAREN_X,PAREN_Y)  ERASM_META_ASSERT_PRED(PAREN_X,PAREN_Y,greater)
#define ERASM_META_ASSERT_GE(PAREN_X,PAREN_Y)  ERASM_META_ASSERT_PRED(PAREN_X,PAREN_Y,greaterEq)
#define ERASM_META_ASSERT_IN_RANGE(PAREN_X,PAREN_A,PAREN_B) ERASM_META_ASSERT_PRED3(PAREN_X,PAREN_A,PAREN_B,::erasm::prelude::impl::inRange)

#define ERASM_META_ASSERT_EQUAL ERASM_META_ASSERT_EQ
#define ERASM_META_ASSERT_NOT_EQUAL ERASM_META_ASSERT_NE



// Just to calm highlight-paren-mode
#define COMPARE_LESS(x,y) ((x) < (y))
#define COMPARE_GREATER(x,y) ((x) > (y))
#define COMPARE_LESS_EQ(x,y) ((x) <= (y))
#define COMPARE_GREATER_EQ(x,y) ((x) >= (y))

#define DEFINE_CURRYING1(FUNCNAME)		\
   template<>					\
   struct eval<FUNCNAME<_> >			\
   {						\
      typedef Function1_0<FUNCNAME> result;	\
	 }

#define DEFINE_CURRYING2(FUNCNAME)			\
   template<>					\
   struct eval<FUNCNAME<_,_> >			\
   {						\
      typedef Function2_0<FUNCNAME> result;	\
	 };					\
   template<class X>				\
   struct eval<FUNCNAME<X,_> >			\
   {						\
      typedef Function2_1<FUNCNAME,X> result;	\
   }


#define DEFINE_CURRYING3(FUNCNAME)				\
   template<>						\
   struct eval<FUNCNAME<_,_,_> >			\
   {							\
      typedef Function3_0<FUNCNAME> result;		\
   };							\
							\
   template<class X>					\
   struct eval<FUNCNAME<X,_,_> >			\
   {							\
      typedef Function3_1<FUNCNAME,X> result;		\
	 };						\
   template<class X,class Y>				\
   struct eval<FUNCNAME<X,Y,_> >			\
   {							\
      typedef Function3_2<FUNCNAME,X,Y> result;		\
	 }

#define DEFINE_CURRYING4(FUNCNAME)		\
template<>					\
struct eval<FUNCNAME<_,_,_,_> >			\
{						\
   typedef Function4_0<FUNCNAME> result;	\
};						\
						\
template<class X>				\
struct eval<FUNCNAME<X,_,_,_> >			\
{						\
   typedef Function4_1<FUNCNAME,X> result;	\
};						\
template<class X,class Y>			\
struct eval<FUNCNAME<X,Y,_,_> >			\
{						\
   typedef Function4_2<FUNCNAME,X,Y> result;	\
};						\
template<class X,class Y,class Z>		\
struct eval<FUNCNAME<X,Y,Z,_> >			\
{						\
   typedef Function4_3<FUNCNAME,X,Y,Z> result;	\
}


#define DEFINE_FUNC_STRICT(FUNCNAME)		\
   template<class X=_> struct FUNCNAME;		\
   DEFINE_PATTERN_MATCHER_STRICT(FUNCNAME);	\
   DEFINE_CURRYING1(FUNCNAME)

#define DEFINE_FUNC1_STRICT DEFINE_FUNC_STRICT

#define DEFINE_FUNC_LAZY(FUNCNAME)		\
   template<class X=_> struct FUNCNAME;		\
   DEFINE_CURRYING1(FUNCNAME)

#define DEFINE_FUNC(FUNCNAME,ARG1,PAREN_BODY)	\
   template<class ARG1=_> struct FUNCNAME;		\
   DEFINE_FUNC_BODY1(FUNCNAME,ARG1,PAREN_BODY);		\
   DEFINE_CURRYING1(FUNCNAME)

#define DEFINE_FUNC_LAZY_LAZY(FUNCNAME)		\
   template<class X=_,class Y=_> struct FUNCNAME;	\
   DEFINE_CURRYING2(FUNCNAME)

#define DEFINE_FUNC_STRICT_STRICT(FUNCNAME)		\
   template<class X=_,class Y=_> struct FUNCNAME;	\
   DEFINE_PATTERN_MATCHER_STRICT_STRICT(FUNCNAME);	\
   DEFINE_CURRYING2(FUNCNAME)

#define DEFINE_FUNC_STRICT_LAZY(FUNCNAME)		\
   template<class X0,class X1> struct FUNCNAME;		\
   DEFINE_PATTERN_MATCHER_STRICT_LAZY(FUNCNAME);	\
   DEFINE_CURRYING2(FUNCNAME)

#define DEFINE_FUNC_LAZY_STRICT(FUNCNAME)		\
   template<class X0,class X1> struct FUNCNAME;		\
   DEFINE_PATTERN_MATCHER_LAZY_STRICT(FUNCNAME);	\
   DEFINE_CURRYING2(FUNCNAME)

#define DEFINE_FUNC2(FUNCNAME,ARG1,ARG2,PAREN_BODY)	\
   template<class ARG1=_,class ARG2=_> struct FUNCNAME;	\
   DEFINE_FUNC_BODY2(FUNCNAME,ARG1,ARG2,PAREN_BODY);	\
   DEFINE_CURRYING2(FUNCNAME)


#define DEFINE_FUNC_STRICT_STRICT_STRICT(FUNCNAME)		\
   template<class X=_,class Y=_,class Z=_> struct FUNCNAME;	\
   DEFINE_PATTERN_MATCHER_STRICT_STRICT_STRICT(FUNCNAME);	\
   DEFINE_CURRYING3(FUNCNAME)

#define DEFINE_FUNC_LAZY_LAZY_STRICT(FUNCNAME)			\
   template<class X=_,class Y=_,class Z=_> struct FUNCNAME;	\
   DEFINE_PATTERN_MATCHER_LAZY_LAZY_STRICT(FUNCNAME);		\
   DEFINE_CURRYING3(FUNCNAME)


#define DEFINE_FUNC_LAZY_LAZY_LAZY(FUNCNAME)			\
   template<class X=_,class Y=_,class Z=_> struct FUNCNAME;		\
   DEFINE_CURRYING3(FUNCNAME)


#define DEFINE_FUNC3(FUNCNAME,ARG1,ARG2,ARG3,PAREN_BODY)		\
   template<class ARG1=_,class ARG2=_,class ARG3=_> struct FUNCNAME;	\
   DEFINE_FUNC_BODY3(FUNCNAME,ARG1,ARG2,ARG3,PAREN_BODY);		\
   DEFINE_CURRYING3(FUNCNAME)

#define DEFINE_FUNC4(FUNCNAME,ARG1,ARG2,ARG3,ARG4,PAREN_BODY)	\
template<class ARG1=_,class ARG2=_,class ARG3=_,class ARG4=_>	\
struct FUNCNAME;						\
DEFINE_FUNC_BODY4(FUNCNAME,ARG1,ARG2,ARG3,ARG4,PAREN_BODY);	\
DEFINE_CURRYING4(FUNCNAME)


#define DEFINE_PATTERN_MATCHER_STRICT(FUNCNAME)		\
   template<class X>					\
   struct eval<FUNCNAME<X> >				\
   {							\
      typedef EVAL_TEMPLATE((X)) x;			\
	 typedef EVAL_TEMPLATE((errorOr1<x,FUNCNAME<x> >)) result;	\
   }

#define DEFINE_PATTERN_MATCHER_STRICT_LAZY(FUNCNAME)		\
   template<class X0,class X1>					\
   struct eval<FUNCNAME<X0,X1> >				\
   {								\
      typedef EVAL_TEMPLATE((X0)) x0;				\
      typedef EVAL_TEMPLATE((errorOr1<x0,FUNCNAME<x0,X1> >)) result;	\
   }


#define DEFINE_PATTERN_MATCHER_LAZY_STRICT(FUNCNAME)	\
template<class X0,class X1>				\
struct eval<FUNCNAME<X0,X1> >				\
{							\
   typedef EVAL_TEMPLATE((X1)) x1;			\
   typedef EVAL_TEMPLATE((errorOr1<x1,FUNCNAME<X0,x1> >)) result;	\
}

#define DEFINE_PATTERN_MATCHER_STRICT_STRICT(FUNCNAME)			\
   template<class X,class Y>					\
   struct eval<FUNCNAME<X,Y> >					\
   {								\
      typedef EVAL_TEMPLATE((X)) x;					\
	 typedef EVAL_TEMPLATE((Y)) y;					\
	    typedef EVAL_TEMPLATE((errorOr2<x,y,FUNCNAME<x,y> >)) result; \
	       }


#define DEFINE_PATTERN_MATCHER_STRICT_STRICT_STRICT(FUNCNAME)		\
   template<class X,class Y,class Z>				\
   struct eval<FUNCNAME<X,Y,Z> >				\
   {								\
      typedef EVAL_TEMPLATE((X)) x;				\
	 typedef EVAL_TEMPLATE((Y)) y;				\
	    typedef EVAL_TEMPLATE((Z)) z;			\
	    typedef EVAL_TEMPLATE((errorOr3<x,y,z,FUNCNAME<x,y,z> >)) result; \
		  }

#define DEFINE_PATTERN_MATCHER_LAZY_LAZY_STRICT(FUNCNAME)		\
   template<class X,class Y,class Z>					\
   struct eval<FUNCNAME<X,Y,Z> >					\
   {									\
      typedef EVAL_TEMPLATE((Z)) z;					\
      typedef EVAL_TEMPLATE((errorOr1<z,FUNCNAME<X,Y,z> >)) result;	\
   }




//// Utility macros for Data declaration

#define DEFINE_DATA0(DATACON)			\
   struct DATACON ;				\
   template<>					\
   struct eval<DATACON >			\
   {						\
      typedef DATACON result;			\
	 }


#define DEFINE_DATA1(DATACON)		\
   template<class X=_>   struct DATACON;		\
   template<class X>				\
   struct eval<DATACON<X> >			\
   {						\
      typedef DATACON<X> result;		\
   }

#define DEFINE_DATA2(DATACON)	\
   template<class X=_,class Y=_>   struct DATACON;	\
   template<class X,class Y>				\
   struct eval<DATACON<X,Y> >			\
   {						\
      typedef DATACON<X,Y> result;		\
   }

#define DEFINE_DATA3(DATACON)	\
   template<class X=_,class Y=_,class Z=_>   struct DATACON;	\
   template<class X,class Y,class Z>			\
   struct eval<DATACON<X,Y,Z> >			\
   {						\
      typedef DATACON<X,Y,Z> result;		\
   }

#define DEFINE_DATA4(DATACON)						\
template<class X=_,class Y=_,class Z=_,class W=_>   struct DATACON;	\
template<class X,class Y,class Z,class W>				\
struct eval<DATACON<X,Y,Z,W> >						\
{									\
   typedef DATACON<X,Y,Z,W> result;					\
}


#define DEFINE_DATA1_BUILTIN(DATACON,T)		\
   template<T X>   struct DATACON;		\
   template<T X>				\
   struct eval<DATACON<X> >			\
   {						\
      typedef DATACON<X> result;		\
   }

#define DEFINE_DATA2_BUILTIN(DATACON,T1,T2)	\
   template<T1 X,T2 Y>   struct DATACON;	\
   template<T1 X,T2 Y>				\
   struct eval<DATACON<X,Y> >			\
   {						\
      typedef DATACON<X,Y> result;		\
   }

#define DEFINE_DATA3_BUILTIN(DATACON,T1,T2,T3)	\
   template<T1 X,T2 Y,T3 Z>   struct DATACON;	\
   template<T1 X,T2 Y,T3 Z>			\
   struct eval<DATACON<X,Y,Z> >			\
   {						\
      typedef DATACON<X,Y,Z> result;		\
   }

#define DEFINE_DATA4_BUILTIN(DATACON,T1,T2,T3,T4)	\
   template<T1 X,T2 Y,T3 Z,T4 W>   struct DATACON;	\
   template<T1 X,T2 Y,T3 Z,T4 W>			\
   struct eval<DATACON<X,Y,Z,W> >			\
   {							\
      typedef DATACON<X,Y,Z,W> result;			\
   }


#define DEFINE_FOREIGN_DATA0(DATACON)		\
   template<>					\
   struct eval<DATACON>				\
   {						\
      typedef DATACON result;			\
   }

#define DEFINE_FOREIGN_DATA1(DATACON,T1)	\
   template<T1 X>				\
   struct eval<DATACON<X> >			\
   {						\
      typedef DATACON<X> result;		\
   }

#define DEFINE_FOREIGN_DATA2(DATACON,T1,T2)	\
   template<T1 X,T2 Y>				\
   struct eval<DATACON<X,Y> >			\
   {						\
      typedef DATACON<X,Y> result;		\
	 }

#define DEFINE_FOREIGN_DATA3(DATACON,T1,T2,T3)	\
   template<T1 X,T2 Y,T3 Z>				\
   struct eval<DATACON<X,Y,Z> >			\
   {						\
      typedef DATACON<X,Y,Z> result;		\
	 }

#define DEFINE_FOREIGN_DATA4(DATACON,T1,T2,T3,T4)	\
   template<T1 X1,T2 X2,T3 X3,T4 X4>			\
   struct eval<DATACON<X1,X2,X3,X4> >			\
   {							\
      typedef DATACON<X1,X2,X3,X4> result;		\
   }

#define DERIVING_EQ1(DATA)			\
template<class X,class Y>			\
struct eval<equal<DATA<X>,DATA<Y> > >		\
{						\
   typedef EVAL_TEMPLATE((equal<X,Y>)) result;	\
}

#define DERIVING_EQ2(DATA)						\
template<class X0,class Y0,class X1,class Y1>				\
struct eval<equal<DATA<X0,Y0>,DATA<X1,Y1> > >				\
{									\
   typedef EVAL_TEMPLATE((and2<equal<X0,X1>,equal<Y0,Y1> >)) result;	\
}

#define DERIVING_EQ3(DATA)						\
template<class X0,class Y0,class X1,class Y1,class Z0,class Z1>		\
struct eval<equal<DATA<X0,Y0,Z0>,DATA<X1,Y1,Z1> > >			\
{									\
   typedef EVAL_TEMPLATE((and_<toList<equal<X0,X1>,equal<Y0,Y1>,equal<Z0,Z1> > >)) result; \
}

#define DERIVING_EQ4(DATA)						\
template<class X0,class Y0,class X1,class Y1,class Z0,class Z1,class W0,class W1> \
struct eval<equal<DATA<X0,Y0,Z0,W0>,DATA<X1,Y1,Z1,W1> > >		\
{									\
   typedef EVAL_TEMPLATE((and_<toList<equal<X0,X1>,equal<Y0,Y1>,equal<Z0,Z1> ,equal<W0,W1> >  >)) result; \
}


#define DERIVING_ABELIAN1(DATACON)		\
template<class X,class Y>			\
struct eval<plus<DATACON<X>,DATACON<Y> > >	\
{						\
   typedef DATACON<plus<X,Y> > result;		\
};						\
template<class X,class Y>			\
struct eval<minus<DATACON<X>,DATACON<Y> > >	\
{						\
   typedef DATACON<minus<X,Y> > result;		\
};						\
						\
template<class X>				\
struct eval<negate<DATACON<X> > >		\
{						\
   typedef DATACON<negate<X> > result;		\
}

#define DERIVING_ABELIAN2(DATACON)			\
template<class X0,class Y0,class X1,class Y1>		\
struct eval<plus<DATACON<X0,Y0>,DATACON<X1,Y1> > >	\
{							\
   typedef DATACON<plus<X0,X1>,plus<Y0,Y1> > result;	\
};							\
template<class X0,class Y0,class X1,class Y1>		\
struct eval<minus<DATACON<X0,Y0>,DATACON<X1,Y1> > >	\
{							\
   typedef DATACON<minus<X0,X1>,minus<Y0,Y1> > result;	\
};							\
template<class X,class Y>				\
struct eval<negate<DATACON<X,Y> > >			\
{							\
   typedef DATACON<negate<X>,negate<Y> > result;	\
}


#define DERIVING_Z_MODULE1(DATACON)		\
template<int x,class Y>				\
struct eval<times<Int<x>,DATACON<Y> > >		\
{						\
   typedef DATACON<times<Int<x>,Y> > result;	\
};						\
						\
template<class Y,int x>				\
struct eval<times<DATACON<Y> ,Int<x> > >	\
{						\
   typedef DATACON<times<Int<x>,Y > >result;	\
}

#define DERIVING_Z_MODULE2(DATACON)				\
template<int x,class X,class Y>					\
struct eval<times<Int<x>,DATACON<X,Y> > >			\
{								\
   typedef DATACON<times<Int<x>,X>,times<Int<x>,Y> > result;	\
};								\
								\
template<class X,class Y,int x>					\
struct eval<times<DATACON<X,Y> ,Int<x> > >			\
{								\
   typedef DATACON<times<Int<x>,X >,times<Int<x>,Y> > result;	\
}


#define DEFINE_FUNC_BODY1(FUNCNAME,ARG1,PAREN_BODY)	\
template<class ARG1>					\
struct eval<FUNCNAME<ARG1> >				\
{							\
   typedef EVAL_TEMPLATE(PAREN_BODY) result;		\
}


#define DEFINE_FUNC_BODY2(FUNCNAME,ARG1,ARG2,PAREN_BODY)	\
template<class ARG1,class ARG2>					\
struct eval<FUNCNAME<ARG1,ARG2> >				\
{								\
   typedef EVAL_TEMPLATE(PAREN_BODY) result;			\
}

#define DEFINE_FUNC_BODY3(FUNCNAME,ARG1,ARG2,ARG3,PAREN_BODY)	\
template<class ARG1,class ARG2,class ARG3>			\
struct eval<FUNCNAME<ARG1,ARG2,ARG3> >				\
{								\
   typedef EVAL_TEMPLATE(PAREN_BODY) result;			\
}


#define DEFINE_FUNC_BODY4(FUNCNAME,ARG1,ARG2,ARG3,ARG4,PAREN_BODY)	\
template<class ARG1,class ARG2,class ARG3,class ARG4>			\
struct eval<FUNCNAME<ARG1,ARG2,ARG3,ARG4> >				\
{									\
   typedef EVAL_TEMPLATE(PAREN_BODY) result;				\
}


#define DEFINE_ACCESSORS1(DATACON,FX)		\
template<class X=_> struct FX;			\
DEFINE_PATTERN_MATCHER_STRICT(FX);		\
template<class X>				\
struct eval<FX<DATACON<X> > >			\
{						\
   typedef EVAL_TEMPLATE((X)) result;		\
}


#define DEFINE_ACCESSORS2(DATACON,FX,FY)	\
template<class X=_> struct FX;			\
template<class X=_> struct FY;			\
DEFINE_PATTERN_MATCHER_STRICT(FX);		\
DEFINE_PATTERN_MATCHER_STRICT(FY);		\
template<class X,class Y>			\
struct eval<FX<DATACON<X,Y> > >			\
{						\
   typedef EVAL_TEMPLATE((X)) result;		\
};						\
template<class X,class Y>			\
struct eval<FY<DATACON<X,Y> > >			\
{						\
   typedef EVAL_TEMPLATE((Y)) result;		\
}


#define DEFINE_ACCESSORS3(DATACON,FX,FY,FZ)	\
template<class X=_> struct FX;			\
template<class X=_> struct FY;			\
template<class X=_> struct FZ;			\
DEFINE_PATTERN_MATCHER_STRICT(FX);		\
DEFINE_PATTERN_MATCHER_STRICT(FY);		\
DEFINE_PATTERN_MATCHER_STRICT(FZ);		\
template<class X,class Y,class Z>		\
struct eval<FX<DATACON<X,Y,Z> > >		\
{						\
   typedef EVAL_TEMPLATE((X)) result;		\
};						\
template<class X,class Y,class Z>		\
struct eval<FY<DATACON<X,Y,Z> > >		\
{						\
   typedef EVAL_TEMPLATE((Y)) result;		\
};						\
template<class X,class Y,class Z>		\
struct eval<FZ<DATACON<X,Y,Z> > >		\
{						\
   typedef EVAL_TEMPLATE((Z)) result;		\
}



namespace erasm { namespace prelude {
namespace Prelude = prelude;

// The core expression reducer
template<class X>
struct eval;

template<typename X0,typename X1=void,typename X2=void,typename X3=void,typename X4=void>
struct errorMassage;

template<typename X0,typename X1=void,typename X2=void,typename X3=void,typename X4=void>
struct error
{
   typedef typename errorMassage<X0,X1,X2,X3,X4>::msg result;
};

#define ERROR_TEMPLATE_PARAM typename ERR0,typename ERR1,typename ERR2,typename ERR3,typename ERR4
#define ERROR_PARAM  ERR0,ERR1,ERR2,ERR3,ERR4

template<ERROR_TEMPLATE_PARAM>
struct eval<error<ERROR_PARAM> >
{
   typedef error<ERROR_PARAM>  result;
};

template<class X,class D>
struct errorOr1;

template<class X,class Y,class D>
struct errorOr2;

template<class X,class Y,class Z,class D>
struct errorOr3;

template<class X,class D>
struct eval<errorOr1<X,D> >
{
   typedef EVAL_TEMPLATE((D)) result;
};

template<ERROR_TEMPLATE_PARAM,class D>
struct eval<errorOr1<error<ERROR_PARAM>,D> >
{
   typedef error<ERROR_PARAM> result;
};

template<class X,class Y,class D>
struct eval<errorOr2<X,Y,D> >
{
   typedef EVAL_TEMPLATE((D)) result;
};

template<ERROR_TEMPLATE_PARAM,class Y,class D>
struct eval<errorOr2<error<ERROR_PARAM>,Y,D> >
{
   typedef error<ERROR_PARAM> result;
};

template<ERROR_TEMPLATE_PARAM,class X,class D>
struct eval<errorOr2<X,error<ERROR_PARAM>,D> >
{
   typedef error<ERROR_PARAM> result;
};

template<class X,class Y,class Z,class D>
struct eval<errorOr3<X,Y,Z,D> >
{
   typedef EVAL_TEMPLATE((D)) result;
};

template<ERROR_TEMPLATE_PARAM,class Y,class Z,class D>
struct eval<errorOr3<error<ERROR_PARAM>,Y,Z,D> >
{
   typedef error<ERROR_PARAM> result;
};

template<ERROR_TEMPLATE_PARAM,class X,class Z,class D>
struct eval<errorOr3<X,error<ERROR_PARAM>,Z,D> >
{
   typedef error<ERROR_PARAM> result;
};

template<ERROR_TEMPLATE_PARAM,class X,class Y,class D>
struct eval<errorOr3<X,Y,error<ERROR_PARAM>,D> >
{
   typedef error<ERROR_PARAM> result;
};



// Defines common constants and data structures

// Models an unterminating computation
struct Bottom;
typedef Bottom undefined;

struct error_unterminating_computation;

template<>
struct eval<Bottom>
{
   typedef error<error_unterminating_computation> result;
};

// Function type and its operations

struct ParameterPlaceholder;
typedef ParameterPlaceholder _;

template<template<class> class F>
struct Function1_0;
template<template<class,class> class F>
struct Function2_0;
template<template<class,class> class F,class X>
struct Function2_1;
template<template<class,class,class> class F>
struct Function3_0;
template<template<class,class,class> class F,class X>
struct Function3_1;
template<template<class,class,class> class F,class X,class Y>
struct Function3_2;

template<template<class,class,class,class> class F>
struct Function4_0;
template<template<class,class,class,class> class F,class X>
struct Function4_1;
template<template<class,class,class,class> class F,class X,class Y>
struct Function4_2;
template<template<class,class,class,class> class F,class X,class Y,class Z>
struct Function4_3;


template<class F=_,class X=_>
struct apply1;
template<class F=_,class X0=_,class X1=_>
struct apply2;
template<class F=_,class X0=_,class X1=_,class X2=_>
struct apply3;
template<class F=_,class X0=_,class X1=_,class X2=_,class X3=_>
struct apply4;


template<class F,class Y>
struct eval<apply1<F,Y> >
{
   typedef EVAL_TEMPLATE((F)) f;
   typedef EVAL_TEMPLATE((apply1<f,Y>)) result;
};


template<template <class> class F,class X>
struct eval<apply1<Function1_0<F>,X> >
{
   typedef EVAL_TEMPLATE((F<X>)) result;
};

template<template <class,class> class F,class X0,class X1>
struct eval<apply1<Function2_1<F,X0>,X1> >
{
   typedef EVAL_TEMPLATE((F<X0,X1>)) result;
};

template<template <class,class,class> class F,class X0,class X1,class X2>
struct eval<apply1<Function3_2<F,X0,X1>,X2> >
{
   typedef EVAL_TEMPLATE((F<X0,X1,X2>)) result;
};

template<template <class,class,class,class> class F,
	 class X0,class X1,class X2,class X3>
struct eval<apply1<Function4_3<F,X0,X1,X2>,X3> >
{
   typedef EVAL_TEMPLATE((F<X0,X1,X2,X3>)) result;
};


template<class F,class X0,class X1>
struct eval<apply2<F,X0,X1> >
{
   typedef EVAL_TEMPLATE((F)) f;
   typedef EVAL_TEMPLATE((apply2<f,X0,X1>)) result;
};

template<template <class,class> class F,class X0,class X1>
struct eval<apply2<Function2_0<F>,X0,X1> >
{
   typedef EVAL_TEMPLATE((F<X0,X1>)) result;
};

template<template <class,class,class> class F,class X0,class X1,class X2>
struct eval<apply2<Function3_1<F,X0>,X1,X2> >
{
   typedef EVAL_TEMPLATE((F<X0,X1,X2>)) result;
};

template<template <class,class,class,class> class F,
	 class X0,class X1,class X2,class X3>
struct eval<apply2<Function4_2<F,X0,X1>,X2,X3> >
{
   typedef EVAL_TEMPLATE((F<X0,X1,X2,X3>)) result;
};

template<template <class,class,class,class> class F,
	 class X0,class X1,class X2,class X3>
struct eval<apply3<Function4_3<F,X0,X1,X2>,X3> >
{
   typedef EVAL_TEMPLATE((F<X0,X1,X2,X3>)) result;
};



DEFINE_DATA1_BUILTIN(Bool,bool);
template<bool b>
struct Bool
{
   enum { value= b };
   typedef Bool type;
};

typedef Bool<true>  True;
typedef Bool<false> False;
typedef Bool<true>  otherwise;

namespace impl {
template<class X>
struct isError;
template<class X>
struct isError0;
template<class X>
int assert_fail( void (*) (X) , void (*) (True) );

}

BOOST_MPL_ASSERT(( True ));
ERASM_META_ASSERT(( True ));

template<class X>
struct eval<impl::isError<X> >
{
   typedef EVAL_TEMPLATE((X)) x;
   typedef EVAL_TEMPLATE((impl::isError0<x>)) result;
};

template<class X0,class X1,class X2,class X3,class X4>
struct eval<impl::isError0<error<X0,X1,X2,X3,X4> > >
{
   typedef True result;
};
template<class X>
struct eval<impl::isError0<X> >
{
   typedef False result;
};


ERASM_META_ASSERT_ERROR((error<True>));



// Syntactic expressions

template<class p1,class then_exp,class else_exp>
struct if_;

template<class P,class Then,class Else>
struct eval<if_<P,Then,Else> >
{
   typedef EVAL_TEMPLATE((P)) b;
   typedef EVAL_TEMPLATE((if_<b,Then,Else>)) result;
};

template<class Then,class Else>
struct eval<if_<True,Then,Else> >
{
   typedef EVAL_TEMPLATE((Then)) result;
};

template<class Then,class Else>
struct eval<if_<False,Then,Else> >
{
   typedef EVAL_TEMPLATE((Else)) result;
};


// Premitive logical functions
DEFINE_FUNC_STRICT(not_);
template<bool b>
struct eval<not_<Bool<b> > >
{
   typedef Bool<not b> result;
};

DEFINE_FUNC_STRICT_LAZY(or2);
template<class Y>
struct eval<or2<True,Y> >
{
   typedef True result;
};

template<class Y>
struct eval<or2<False,Y> >
{
   typedef typename eval<Y>::result result;
};


DEFINE_FUNC_STRICT_LAZY(and2);
template<class Y>
struct eval<and2<True,Y> >
{
   typedef typename eval<Y>::result result;
};

template<class Y>
struct eval<and2<False,Y> >
{
   typedef False result;
};




}}



#endif // MY_META_HASKELL_CORE_HPP
