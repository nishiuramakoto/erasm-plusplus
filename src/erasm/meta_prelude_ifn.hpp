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
#ifndef MY_META_PRELUDE_IFN_HPP
#define MY_META_PRELUDE_IFN_HPP

#include "meta_prelude.hpp"

#include <boost/preprocessor/repetition.hpp>
#include <boost/preprocessor/punctuation/comma_if.hpp>
#include <boost/static_assert.hpp>


#define ERASM_PP_IF_PARAM_DEF(Z,N,DATA) class P ## N = EndIf , class Then ## N = EndIf,
#define ERASM_PP_IF_PARAM_DEF_LIST(N) class P0,class Then0,BOOST_PP_REPEAT_FROM_TO(1,N,ERASM_PP_IF_PARAM_DEF,~) class Else = EndIf

#define ERASM_PP_IF_PARAM(Z,N,DATA) class P ## N  , class Then ## N ,
#define ERASM_PP_IF_PARAM_LIST(N) BOOST_PP_REPEAT(N,ERASM_PP_IF_PARAM,~) class Else

#define ERASM_PP_IF_ARG(Z,N,DATA) P ## N , Then ## N ,
#define ERASM_PP_IF_ARG_LIST(N)   BOOST_PP_REPEAT(N,ERASM_PP_IF_ARG,~) Else 
#define ERASM_PP_TAIL_IF_ARG_LIST(N) BOOST_PP_REPEAT_FROM_TO(1,N,ERASM_PP_IF_ARG,~) Else

#define ERASM_PP_IF_N 10

namespace erasm { namespace prelude {

struct EndIf;

template<class p1,class then1,class p2,class then2,class else_exp>
struct if2;

template<class p1,class then1,class p2,class then2,class else_exp>
struct eval<if2<p1,then1,p2,then2,else_exp> >
{
   typedef EVAL_TEMPLATE((if_<p1,then1,if_<p2,then2,else_exp> >)) result;
};

template<class p1,class then1,class p2,class then2,class p3,class then3,class else_exp>
struct if3;

template<class p1,class then1,class p2,class then2,class p3,class then3,class else_exp>
struct eval<if3<p1,then1,p2,then2,p3,then3,else_exp> >
{
   typedef EVAL_TEMPLATE((if_<p1,then1,if2<p2,then2,p3,then3,else_exp> >)) result;
};

template<class p1,class then1,class p2,class then2,class p3,class then3,class p4,class then4,class else_exp>
struct if4;

template<class p1,class then1,class p2,class then2,class p3,class then3,class p4,class then4,class else_exp>
struct eval<if4<p1,then1,p2,then2,p3,then3,p4,then4,else_exp> >
{
   typedef EVAL_TEMPLATE((if_<p1,then1,if3<p2,then2,p3,then3,p4,then4,else_exp> >)) result;
};

template<class p1,class then1,class p2,class then2,class p3,class then3,class p4,class then4,class p5,class then5,class else_exp>
struct if5;
template<class p1,class then1,class p2,class then2,class p3,class then3,class p4,class then4,class p5,class then5,class else_exp>
struct eval<if5<p1,then1,p2,then2,p3,then3,p4,then4,p5,then5,else_exp> >
{
   typedef EVAL_TEMPLATE((if_<p1,then1,if4<p2,then2,p3,then3,p4,then4,p5,then5,else_exp> >)) result;
};

namespace test {
ERASM_META_ASSERT_EQUAL((if2<True,One,error<CompileError>,error<CompileError>,error<CompileError> >),(One));
ERASM_META_ASSERT_EQUAL((if2<False,One,False,Two,Three>),(Three));
ERASM_META_ASSERT_EQUAL((if3<False,One,False,Two,False,Three,Four>),(Four));
ERASM_META_ASSERT_EQUAL((if4<False,One,False,Two,False,Three,False,Four,One>),(One));
ERASM_META_ASSERT_EQUAL((if5<False,One,False,Two,False,Three,False,Four,False,Five,One>),(One));
}




template<ERASM_PP_IF_PARAM_DEF_LIST(ERASM_PP_IF_N)>
struct ifn;

template<ERASM_PP_IF_PARAM_LIST(ERASM_PP_IF_N)>
struct eval<ifn<ERASM_PP_IF_ARG_LIST(ERASM_PP_IF_N)> >
{
   typedef EVAL_TEMPLATE((if_<P0,Then0,ifn<ERASM_PP_TAIL_IF_ARG_LIST(ERASM_PP_IF_N)> >)) result;
};

template<class P0,class Then0,class Else>
struct eval<ifn<P0,Then0,Else> >
{
   typedef EVAL_TEMPLATE((if_<P0,Then0,Else>)) result;
};

ERASM_META_ASSERT_EQUAL((ifn<True,Zero,One>),(Zero));
ERASM_META_ASSERT_EQUAL((ifn<False,Zero,One>),(One));
ERASM_META_ASSERT_EQUAL((ifn<True,Zero,False,One,Two>),(Zero));
ERASM_META_ASSERT_EQUAL((ifn<True,Zero,False ,One,False,Two,Three>),(Zero));
ERASM_META_ASSERT_EQUAL((ifn<False,Zero,False,One,False,Two,Three>),(Three));
ERASM_META_ASSERT_EQUAL((ifn<False,Zero,False,One,True,Two,Three>),(Two));


}}

#endif // MY_META_PRELUDE_IFN_HPP
