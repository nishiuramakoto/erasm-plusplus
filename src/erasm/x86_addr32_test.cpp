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
TEST(x86_addr32_test,CONCAT(test_expr_,__LINE__))	\
{							\
   DwordPtr  p(dword_ptr[ (EXPR) ]);			\
   EXPECT_EQ(false,p.is_addr16());			\
   EXPECT_EQ((MOD),(int)p.get_mod());			\
   EXPECT_EQ((RM) ,(int)p.get_rm());				\
   EXPECT_EQ((DSIZE),(int)p.get_disp_size());		\
   EXPECT_EQ((DISP) ,(int)p.get_disp32());			\
   EXPECT_EQ(false,p.has_sib());			\
}

#define TEST_EXPR_SIB(EXPR,MOD,RM,DSIZE,DISP,SS,INDEX,BASE)	\
TEST(x86_addr32_test,CONCAT(test_expr_sib_,__LINE__))		\
{								\
   DwordPtr  p(dword_ptr[(EXPR)]);				\
   EXPECT_EQ(false,p.is_addr16());				\
   EXPECT_EQ((MOD),(int)(p.get_mod()));				\
   EXPECT_EQ((RM),(int)(p.get_rm()));				\
   EXPECT_EQ((DSIZE),(int)(p.get_disp_size()));			\
   EXPECT_EQ((DISP),(int)(p.get_disp32()));				\
   EXPECT_EQ(true,(p.has_sib()));				\
   EXPECT_EQ((SS),(int)(p.get_sib_ss()));				\
   EXPECT_EQ((INDEX),(int)(p.get_sib_index()));			\
   EXPECT_EQ((BASE) ,(int)(p.get_sib_base()));			\
}


using namespace erasm::x86;

TEST_EXPR(eax   ,0,0,0,0);
TEST_EXPR(ecx+(disp8_type)1 ,1,1,1,1);
TEST_EXPR(edx+200,2,2,4,200);
TEST_EXPR(0U     ,0,5,4,0);
TEST_EXPR(1U     ,0,5,4,1);
TEST_EXPR(1000U  ,0,5,4,1000);
TEST_EXPR(0x1fffffffU   ,0,5,4,0x1fffffffU);
TEST_EXPR(ebp    ,1,5,1,0);
TEST_EXPR(ebp+(disp8_type)1  ,1,5,1,1);
TEST_EXPR(ebp+200,2,5,4,200);

TEST_EXPR_SIB(eax+ecx*_2      , 0,4,0,0,1,1,0);
TEST_EXPR_SIB(eax+ecx*_2+_1    , 1,4,1,1,1,1,0);
TEST_EXPR_SIB(eax+ecx*_2+(disp8_type)1    , 1,4,1,1,1,1,0);
TEST_EXPR_SIB(eax+ecx*_2+200  , 2,4,4,200,1,1,0);
TEST_EXPR_SIB(ebx+edi*_4      , 0,4,0,0,2,7,3);
TEST_EXPR_SIB(ebx+edi*_4+_1    , 1,4,1,1,2,7,3);
TEST_EXPR_SIB(ebx+edi*_4+(disp8_type)1    , 1,4,1,1,2,7,3);
TEST_EXPR_SIB(ebx+edi*_4+200  , 2,4,4,200,2,7,3);
TEST_EXPR_SIB(edx+ebp*_8      , 0,4,0,0,3,5,2);
TEST_EXPR_SIB(edx+ebp*_8+_1    , 1,4,1,1,3,5,2);
TEST_EXPR_SIB(edx+ebp*_8+(disp8_type)1    , 1,4,1,1,3,5,2);
TEST_EXPR_SIB(edx+ebp*_8+200  , 2,4,4,200,3,5,2);
TEST_EXPR_SIB(eax*_3          , 0,4,0,0,1,0,0);
TEST_EXPR_SIB(eax*_5          , 0,4,0,0,2,0,0);
TEST_EXPR_SIB(eax*_9          , 0,4,0,0,3,0,0);
TEST_EXPR_SIB(esp             , 0,4,0,0,0,4,4);
TEST_EXPR_SIB(esp+_1           , 1,4,1,1,0,4,4);
TEST_EXPR_SIB(esp+(disp8_type)1           , 1,4,1,1,0,4,4);
TEST_EXPR_SIB(esp+200         , 2,4,4,200,0,4,4);
TEST_EXPR_SIB(_2*edx          , 0,4,4,0,1,2,5);
TEST_EXPR_SIB(_2*edx+_1        , 0,4,4,1,1,2,5);
TEST_EXPR_SIB(_2*edx+(disp8_type)1        , 0,4,4,1,1,2,5);
TEST_EXPR_SIB(_2*edx+200      , 0,4,4,200,1,2,5);
TEST_EXPR_SIB(_2*edx + ebp    , 1,4,1,0,1,2,5);
TEST_EXPR_SIB(_2*edx + ebp+_1  , 1,4,1,1,1,2,5);
TEST_EXPR_SIB(_2*edx + ebp+(disp8_type)1  , 1,4,1,1,1,2,5);
TEST_EXPR_SIB(_2*edx + ebp+200, 2,4,4,200,1,2,5);

int main(int argc,char**argv)
{
   ::testing::InitGoogleTest(&argc,argv);
   return RUN_ALL_TESTS();
}

