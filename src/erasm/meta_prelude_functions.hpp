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

// TODO:automatically generate this file with haskell

// Function types and its operations

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


template<class F=_,class X=_>
struct apply1;
template<class F=_,class X0=_,class X1=_>
struct apply2;
template<class F,class X0,class X1,class X2>
struct apply3;

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

