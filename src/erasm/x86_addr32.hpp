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
#ifndef MY_ERASM_X86_ADDR32_HPP
#define MY_ERASM_X86_ADDR32_HPP

#include "erasm/x86_common.hpp"
#include "erasm/x64_addr64.hpp"
#include "meta_polynomial.hpp"
#include <assert.h>

#define X86_ADDR32_DEFINE_DATA1(DATACON)	\
   namespace x86 { namespace addr32 {		\
template<class X=_> struct DATACON;		\
   }}						\
   DEFINE_FOREIGN_DATA1(x86::addr32::DATACON,class);

#define X86_ADDR32_DEFINE_DATA2(DATACON)		\
   namespace x86 { namespace addr32 {			\
template<class X=_,class Y=_> struct DATACON;			\
   }}							\
   DEFINE_FOREIGN_DATA2(x86::addr32::DATACON,class,class);

#define X86_ADDR32_DEFINE_DATA3(DATACON)			\
   namespace x86 { namespace addr32 {				\
template<class X=_,class Y=_,class Z=_> struct DATACON;			\
   }}								\
   DEFINE_FOREIGN_DATA3(x86::addr32::DATACON,class,class,class);

#define X86_ADDR32_DEFINE_DATA1_DERIVING_EQ(DATACON)	\
   X86_ADDR32_DEFINE_DATA1(DATACON);			\
   DERIVING_EQ1(x86::addr32::DATACON);

#define X86_ADDR32_DEFINE_DATA2_DERIVING_EQ(DATACON)	\
   X86_ADDR32_DEFINE_DATA2(DATACON);			\
   DERIVING_EQ2(x86::addr32::DATACON);

#define X86_ADDR32_DEFINE_DATA3_DERIVING_EQ(DATACON)	\
   X86_ADDR32_DEFINE_DATA3(DATACON);			\
   DERIVING_EQ3(x86::addr32::DATACON);


#define X86_ADDR32_DEFINE_FUNC_STRICT(FUNC)		\
   namespace x86 { namespace addr32 {			\
template<class X=_>  struct FUNC;			\
   }}							\
   DEFINE_PATTERN_MATCHER_STRICT(x86::addr32::FUNC);


#define X86_ADDR32_DEFINE_FUNC1 X86_ADDR32_DEFINE_FUNC
#define X86_ADDR32_DEFINE_FUNC(FUNC,ARG,PAREN_BODY)	\
   namespace x86 { namespace addr32 {			\
template<class X=_>  struct FUNC;			\
   }}							\
   DEFINE_FUNC_BODY1(x86::addr32::FUNC,ARG,PAREN_BODY);


#define X86_ADDR32_DEFINE_FUNC_STRICT_STRICT(FUNC)		\
namespace x86 { namespace addr32 {				\
template<class X=_,class Y=_>  struct FUNC;			\
}}								\
DEFINE_PATTERN_MATCHER_STRICT_STRICT(x86::addr32::FUNC);


namespace erasm { namespace prelude { namespace x86 { 
using x64::RegNoneCode;
typedef Zero RegEAXCode;
struct Error_Extended_registers_not_available_in_x86_mode;
}}}

namespace erasm { namespace prelude {


// namespace x86 { namespace addr32 {
// ERASM_META_ASSERT_EQUAL((polynomialFromExpr<exprFromReg<RegEAX> >) , (monomial<Zero>));
// ERASM_META_ASSERT_EQUAL((polynomialFromExpr<exprFromReg<RegECX> >) , (monomial<One>));
// ERASM_META_ASSERT_EQUAL((polynomialFromExpr<exprFromReg<RegESP> >) , (monomial<Four>));
// }}

// namespace UNIQUEID(addr32_test) {
//    using namespace ::erasm::prelude::x86;
//    using namespace ::erasm::prelude::x86::addr32;
//    ERASM_META_ASSERT_EQUAL((modrmMod<x64::effectiveAddressFromReg2<RegEAX,RegNone,Zero> ,NoDisp>),(Zero));
//    ERASM_META_ASSERT_EQUAL((modrmMod<x64::effectiveAddressFromReg2<RegEBP,RegNone,Zero> ,NoDisp>),(One));
//    ERASM_META_ASSERT_EQUAL((modrmMod<x64::effectiveAddressFromReg2<RegNone,RegNone,Zero> ,Disp8>),(Zero));
//    ERASM_META_ASSERT_EQUAL((modrmMod<x64::effectiveAddressFromReg2<RegEBX ,RegNone,Zero> ,Disp8>) ,(One));
//    ERASM_META_ASSERT_EQUAL((modrmMod<x64::effectiveAddressFromReg2<RegNone,RegNone,Zero> ,Disp32>),(Zero));
//    ERASM_META_ASSERT_EQUAL((modrmMod<x64::effectiveAddressFromReg2<RegECX,RegEDI,Zero> ,Disp32>),(Two));
// }

// namespace UNIQUEID(x86_addr32) {
// using namespace erasm::prelude::x86;
// using namespace erasm::prelude::x86::addr32;
// ERASM_META_ASSERT_EQUAL((sib<x64::effectiveAddressFromReg2<RegESP,RegNone,One> >),(True));
// ERASM_META_ASSERT_EQUAL((sib<x64::effectiveAddressFromReg2<RegESP,RegNone,Zero> >),(True));
// ERASM_META_ASSERT_EQUAL((effectiveAddressFromExpr<exprFromReg<RegESP> >  ),(x64::effectiveAddressFromReg2<RegESP,RegNone,Zero>));
// //ERASM_META_ASSERT_EQUAL((sib<effectiveAddressFromReg2<RegESP,RegRAX,Two> > ),(False));
// ERASM_META_ASSERT_EQUAL((sib<effectiveAddressFromExpr<exprFromReg<RegESP> > > ),(True));
// ERASM_META_ASSERT_EQUAL((sibIndex<x64::effectiveAddressFromReg2<RegEAX,RegEAX,One> >),(Zero));
// }

// }}

// namespace erasm { namespace prelude { namespace x86 { namespace addr32 {

// namespace UNIQUEID(test) {
//    typedef exprFromReg<RegEAX> eax;
//    typedef exprFromReg<RegEBX> ebx;
//    typedef exprFromReg<RegECX> ecx;
//    typedef exprFromReg<RegEDX> edx;
//    typedef exprFromReg<RegESP> esp;
//    typedef exprFromReg<RegEBP> ebp;
//    typedef exprFromReg<RegEDI> edi;
//    typedef exprFromReg<RegESI> esi;

//    using x64::effectiveAddressFromTerm;
//    using x64::effectiveAddressFromReg2;

//    ERASM_META_ASSERT_EQUAL((effectiveAddressFromTerm<Term<RegEAXCode,One> >),(effectiveAddressFromReg2<RegEAX,RegNone,Zero>));
//    ERASM_META_ASSERT((elem<effectiveAddressFromExpr<plus<eax,ecx> >,
// 		     List<effectiveAddressFromReg2<RegECX,RegEAX,One>,effectiveAddressFromReg2<RegEAX,RegECX,One> > >));
//    ERASM_META_ASSERT_EQUAL((effectiveAddressFromExpr<plus<eax,times<Two,ecx> > >),(effectiveAddressFromReg2<RegEAX,RegECX,Two>));
//    ERASM_META_ASSERT_EQUAL((effectiveAddressFromExpr<plus<eax,esp> >),(effectiveAddressFromReg2<RegESP,RegEAX,One>));
//    ERASM_META_ASSERT_EQUAL((effectiveAddressFromExpr<plus<esp,eax> >),(effectiveAddressFromReg2<RegESP,RegEAX,One>));
//    ERASM_META_ASSERT_EQUAL((effectiveAddressFromExpr<times<Four,eax> >),(effectiveAddressFromReg2<RegNone,RegEAX,Four>));
//    ERASM_META_ASSERT_EQUAL((effectiveAddressFromExpr<times<Nine,eax> >),(effectiveAddressFromReg2<RegEAX,RegEAX,Eight>));
//    ERASM_META_ASSERT_EQUAL((effectiveAddressFromExpr<esp>),(effectiveAddressFromReg2<RegESP,RegNone,Zero>));
//    ERASM_META_ASSERT_EQUAL((effectiveAddressFromReg2<RegEAX,RegEBX,Four>),(effectiveAddressFromReg2<RegEAX,RegEBX,Four>));

//    typedef effectiveAddressFromExpr<eax> a1;
//    typedef effectiveAddressFromExpr<plus<eax,eax> > a2;
//    typedef effectiveAddressFromExpr<plus<ebp,times<Eight,esi> > > a3;
//    typedef effectiveAddressFromExpr<plus<eax,esp> > a4;

//    ERASM_META_ASSERT_EQUAL((effectiveAddressBase<a1>),(RegEAXCode));
//    ERASM_META_ASSERT_EQUAL((effectiveAddressIndex<a1>),(RegNoneCode));
//    ERASM_META_ASSERT_EQUAL((effectiveAddressScale<a1>),(Zero));
//    ERASM_META_ASSERT_EQUAL((effectiveAddressBase<a2>),(RegNoneCode));
//    ERASM_META_ASSERT_EQUAL((effectiveAddressIndex<a2>),(RegEAXCode));
//    ERASM_META_ASSERT_EQUAL((effectiveAddressScale<a2>),(Two));

//    ERASM_META_ASSERT_EQUAL((sib<a1>),(False));
//    ERASM_META_ASSERT_EQUAL((sib<a2>),(True));
//    ERASM_META_ASSERT_EQUAL((sibSS<a2>),(One));
//    ERASM_META_ASSERT_EQUAL((sibIndex<a2>),(Zero));
//    ERASM_META_ASSERT_EQUAL((sibBase<a2>),(Five));
//    ERASM_META_ASSERT_EQUAL((sib<a3>),(True));
//    ERASM_META_ASSERT_EQUAL((sibSS<a3>),(Three));
//    ERASM_META_ASSERT_EQUAL((sibIndex<a3>),(Six));
//    ERASM_META_ASSERT_EQUAL((sibBase<a3>),(Five));
//    ERASM_META_ASSERT_EQUAL((sib<a4>),(True));
//    ERASM_META_ASSERT_EQUAL((sibSS<a4>),(Zero));
//    ERASM_META_ASSERT_EQUAL((sibIndex<a4>),(Zero));
//    ERASM_META_ASSERT_EQUAL((sibBase<a4>),(Four));

//    ERASM_META_ASSERT_EQUAL((modrmMod<effectiveAddressFromReg2<RegNone,RegNone,Zero>,Disp32>),(Zero));

// }

// namespace UNIQUEID(test_x86_addr32) {
//    typedef exprFromReg<RegEAX> eax;
//    typedef exprFromReg<RegESP> esp;

//    using x64::effectiveAddressFromTerm;

//    ERASM_META_ASSERT_ERROR(( sibIndex<effectiveAddressFromExpr<times<Two,esp> > > ));
//    ERASM_META_ASSERT_ERROR(( effectiveAddressFromExpr<times<Two,esp> > ));
//    ERASM_META_ASSERT_ERROR(( effectiveAddressFromExpr<times<Six,eax> > ));

//    ERASM_META_ASSERT_ERROR((effectiveAddressFromTerm<Term<RegEAXCode,Zero> >));
//    ERASM_META_ASSERT_ERROR((effectiveAddressFromTerm<Term<RegEAXCode,Six> >));
//    ERASM_META_ASSERT((notEq<Nine,sibIndex<effectiveAddressFromExpr<times<One,esp> > > >));
// }

}} // namespace erasm


#endif // MY_ERASM_X86_ADDR32_HPP
