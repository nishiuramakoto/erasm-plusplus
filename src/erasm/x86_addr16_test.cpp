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
#include "erasm/x86.hpp"
#include <gtest/gtest.h>

#define CONCAT(x,y) CONCAT_(x,y)
#define CONCAT_(x,y)  x ## y

#define TEST_EXPR(EXPR,MOD,RM,DSIZE,DISP)		\
   TEST(x86_addr16_test,CONCAT(test_expr_,__LINE__))	\
   {							\
      DwordPtr  p(dword_ptr[ (EXPR) ]);			\
      EXPECT_EQ(true,p.is_addr16());     		\
      EXPECT_EQ((MOD),(int)p.get_mod());		\
      EXPECT_EQ((RM) ,(int)p.get_rm());			\
      EXPECT_EQ((DSIZE),(int)p.get_disp_size());	\
      EXPECT_EQ((DISP) ,(int)p.get_disp16());		\
   }


using namespace erasm::x86;

TEST_EXPR( bx + si	,0,0,0,0);
TEST_EXPR( bx + di	,0,1,0,0);
TEST_EXPR( bp + si	,0,2,0,0);
TEST_EXPR( bp + di	,0,3,0,0);
TEST_EXPR( si		,0,4,0,0);
TEST_EXPR( di		,0,5,0,0);
TEST_EXPR((uint16_t)0	,0,6,2,0);
TEST_EXPR( bx		,0,7,0,0);
TEST_EXPR((uint16_t)200 ,0,6,2,200);

TEST_EXPR( bx + si+_1	,1,0,1,1);
TEST_EXPR( bx + si+(int8_t)1	,1,0,1,1);
TEST_EXPR( bx + di+_1	,1,1,1,1);
TEST_EXPR( bx + di+(int8_t)1	,1,1,1,1);
TEST_EXPR( bp + si+_1	,1,2,1,1);
TEST_EXPR( bp + si+(int8_t)1	,1,2,1,1);
TEST_EXPR( bp + di+_1	,1,3,1,1);
TEST_EXPR( bp + di+(int8_t)1	,1,3,1,1);
TEST_EXPR( si	+_1	,1,4,1,1);
TEST_EXPR( si	+(int8_t)1	,1,4,1,1);
TEST_EXPR( di	+_1	,1,5,1,1);
TEST_EXPR( di	+(int8_t)1	,1,5,1,1);
TEST_EXPR( bp		,1,6,1,0);
TEST_EXPR( bp	+_1	,1,6,1,1);
TEST_EXPR( bp	+(int8_t)1	,1,6,1,1);
TEST_EXPR( bx	+_1	,1,7,1,1);
TEST_EXPR( bx	+(int8_t)1	,1,7,1,1);

const disp16_type disp16 = 200;

TEST_EXPR( bx + si+ disp16	,2,0,2,200);
TEST_EXPR( bx + di+ disp16	,2,1,2,200);
TEST_EXPR( bp + si+ disp16	,2,2,2,200);
TEST_EXPR( bp + di+ disp16	,2,3,2,200);
TEST_EXPR( si	+   disp16	,2,4,2,200);
TEST_EXPR( di	+   disp16	,2,5,2,200);
TEST_EXPR( bp	+   disp16	,2,6,2,200);
TEST_EXPR( bx	+   disp16	,2,7,2,200);

int main(int argc,char**argv)
{
   ::testing::InitGoogleTest(&argc,argv);
   return RUN_ALL_TESTS();
}
