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
#include "erasm/x64.hpp"
#include <gtest/gtest.h>
#define CONCAT(x,y) CONCAT_(x,y)
#define CONCAT_(x,y)  x ## y


#define TEST_EXPR_NOREX(EXPR,MOD,RM,DSIZE,DISP)	\
   TEST_EXPR(EXPR,MOD,RM,DSIZE,DISP,false,false)

#define TEST_EXPR_SIB_NOREX(EXPR,MOD,RM,DSIZE,DISP,SS,INDEX,BASE)  \
   TEST_EXPR_SIB(EXPR,MOD,RM,DSIZE,DISP,SS,INDEX,BASE,false,false)


#define TEST_EXPR(EXPR,MOD,RM,DSIZE,DISP,REX_B,REX_X)	\
TEST(x64_addr64_test,CONCAT(test_expr_,__LINE__))		\
{								\
   DwordPtr  p(dword_ptr[ (EXPR) ]);				\
   EXPECT_FALSE(p.is_addr16());				\
   EXPECT_EQ((MOD),(int)p.get_mod());				\
   EXPECT_EQ((RM) ,(int)p.get_rm());				\
   EXPECT_EQ((DSIZE),(int)p.get_disp_size());			\
   EXPECT_EQ((DISP) ,(int)p.get_disp32());			\
   EXPECT_EQ((bool)(REX_X) ,(bool)p.get_rex_x());		\
   EXPECT_EQ((bool)(REX_B) ,(bool)p.get_rex_b());		\
   EXPECT_FALSE(p.has_sib());				\
}

#define TEST_EXPR_SIB(EXPR,MOD,RM,DSIZE,DISP,SS,INDEX,BASE,REX_B,REX_X) \
TEST(x64_addr64_test,CONCAT(test_expr_sib_,__LINE__))			\
{									\
   DwordPtr  p(dword_ptr[(EXPR)]);					\
   EXPECT_FALSE(p.is_addr16());					\
   EXPECT_EQ((MOD),(int)(p.get_mod()));					\
   EXPECT_EQ((RM),(int)(p.get_rm()));					\
   EXPECT_EQ((DSIZE),(int)(p.get_disp_size()));				\
   EXPECT_EQ((DISP),(int)(p.get_disp32()));				\
   EXPECT_EQ((bool)(REX_X) ,(bool)p.get_rex_x());			\
   EXPECT_EQ((bool)(REX_B) ,(bool)p.get_rex_b());			\
   EXPECT_EQ(true,(p.has_sib()));					\
   EXPECT_EQ((SS),(int)(p.get_sib_ss()));				\
   EXPECT_EQ((INDEX),(int)(p.get_sib_index()));				\
   EXPECT_EQ((BASE) ,(int)(p.get_sib_base()));				\
}

using namespace erasm::prelude;
using namespace erasm::prelude::x64;
using namespace erasm::prelude::x64::addr64;

TEST(test0,test0)
{
   typedef EVAL((exprFromRegDisp<Register64<1>,x64::Disp32 >))    rt;
   rt r(rcx + 1);
   EXPECT_EQ(1,r.get_disp());
}

TEST_EXPR_NOREX(rax   ,0,0,0,0);
TEST_EXPR_NOREX(rcx+(int8_t)1 ,1,1,1,1);
TEST_EXPR_NOREX(rcx+_1 ,1,1,1,1);
TEST_EXPR_NOREX(rdx+200,2,2,4,200);
TEST_EXPR_NOREX(rip      ,0,5,4,0);
TEST_EXPR_NOREX(rip+_1   ,0,5,4,1);
TEST_EXPR_NOREX(rip+1000 ,0,5,4,1000);
TEST_EXPR_NOREX(rbp    ,1,5,1,0);
TEST_EXPR_NOREX(rbp+(int8_t)1  ,1,5,1,1);
TEST_EXPR_NOREX(rbp+_1  ,1,5,1,1);
TEST_EXPR_NOREX(rbp+200,2,5,4,200);


TEST_EXPR_SIB_NOREX(0u              , 0,4,4,0,0,4,5);
TEST_EXPR_SIB_NOREX(1u              , 0,4,4,1,0,4,5);
TEST_EXPR_SIB_NOREX(1000u           , 0,4,4,1000,0,4,5);
TEST_EXPR_SIB_NOREX(rax+rcx*_2      , 0,4,0,0,1,1,0);
TEST_EXPR_SIB_NOREX(rax+rcx*_2+_1   , 1,4,1,1,1,1,0);
TEST_EXPR_SIB_NOREX(rax+rcx*_2+200  , 2,4,4,200,1,1,0);
TEST_EXPR_SIB_NOREX(rbx+rdi*_4      , 0,4,0,0,2,7,3);
TEST_EXPR_SIB_NOREX(rbx+rdi*_4+_1   , 1,4,1,1,2,7,3);
TEST_EXPR_SIB_NOREX(rbx+rdi*_4+200  , 2,4,4,200,2,7,3);
TEST_EXPR_SIB_NOREX(rdx+rbp*_8      , 0,4,0,0,3,5,2);
TEST_EXPR_SIB_NOREX(rdx+rbp*_8+_1   , 1,4,1,1,3,5,2);
TEST_EXPR_SIB_NOREX(rdx+rbp*_8+200  , 2,4,4,200,3,5,2);
TEST_EXPR_SIB_NOREX(rax*_3          , 0,4,0,0,1,0,0);
TEST_EXPR_SIB_NOREX(rax*_5          , 0,4,0,0,2,0,0);
TEST_EXPR_SIB_NOREX(rax*_9          , 0,4,0,0,3,0,0);
TEST_EXPR_SIB_NOREX(rsp             , 0,4,0,0,0,4,4);
TEST_EXPR_SIB_NOREX(rsp+_1          , 1,4,1,1,0,4,4);
TEST_EXPR_SIB_NOREX(rsp+200         , 2,4,4,200,0,4,4);
TEST_EXPR_SIB_NOREX(rsp +rax        , 0,4,0,0  ,0,0,4);
TEST_EXPR_SIB_NOREX(rsp +rax + _1   , 1,4,1,1  ,0,0,4);
TEST_EXPR_SIB_NOREX(rsp +rax + 200  , 2,4,4,200,0,0,4);
TEST_EXPR_SIB_NOREX(rax +rsp        , 0,4,0,0  ,0,0,4);
TEST_EXPR_SIB_NOREX(rax +rsp + _1   , 1,4,1,1  ,0,0,4);
TEST_EXPR_SIB_NOREX(rax +rsp + 200  , 2,4,4,200,0,0,4);
TEST_EXPR_SIB_NOREX(_2*rdx          , 0,4,4,0,1,2,5);
TEST_EXPR_SIB_NOREX(_2*rdx+ _1      , 0,4,4,1,1,2,5);
TEST_EXPR_SIB_NOREX(_2*rdx+200      , 0,4,4,200,1,2,5);
TEST_EXPR_SIB_NOREX(_2*rdx + rbp    , 1,4,1,0,1,2,5);
TEST_EXPR_SIB_NOREX(_2*rdx + rbp+_1 , 1,4,1,1,1,2,5);
TEST_EXPR_SIB_NOREX(_2*rdx + rbp+200, 2,4,4,200,1,2,5);

TEST_EXPR(r8		,0	,0	,0	,0	,true	,false);
TEST_EXPR(r9+_1		,1	,1	,1	,1	,true	,false);
TEST_EXPR(r10+200	,2	,2	,4	,200	,true	,false);
TEST_EXPR(r13		,1	,5	,1	,0	,true	,false);
TEST_EXPR(r13+_1	,1	,5	,1	,1	,true	,false);
TEST_EXPR(r13+200	,2	,5	,4	,200	,true	,false);

TEST_EXPR_SIB(r8+rcx*_2      , 0,4,0,0,1,1,0,true,false);
TEST_EXPR_SIB(rax+r9*_2      , 0,4,0,0,1,1,0,false,true);
TEST_EXPR_SIB(r8+r9*_2       , 0,4,0,0,1,1,0,true,true);

TEST_EXPR_SIB(r11+r15*_4      , 0,4,0,0,2,7,3,true,true);
TEST_EXPR_SIB(rbx+r15*_4      , 0,4,0,0,2,7,3,false,true);
TEST_EXPR_SIB(r11+rdi*_4      , 0,4,0,0,2,7,3,true,false);

TEST_EXPR_SIB(r9+r13*_8       , 0,4,0,0,3,5,1,true,true);
TEST_EXPR_SIB(r8*_3           , 0,4,0,0,1,0,0,true,true);
TEST_EXPR_SIB(r12             , 0,4,0,0,0,4,4,true,false);
TEST_EXPR_SIB(_2*r10          , 0,4,4,0,1,2,5,false,true);
TEST_EXPR_SIB(_2*r10 + r13    , 1,4,1,0,1,2,5,true,true);
TEST_EXPR_SIB(r12*_2          , 0,4,4,0,1,4,5,false,true);

Segment seg(const PtrBase& p)
{
   return p.get_segment();
}

template<class X>
Segment seg2(const Ptr<X>& p)
{
   return p.get_segment();
}

TEST(addr64,esi)
{
   
   EXPECT_EQ(seg(dword_ptr.ss[esi]), Segment_SS);
   EXPECT_EQ(seg2(dword_ptr.ss[esi]), Segment_SS);
   EXPECT_EQ((dword_ptr.ss[edi]).get_segment(), Segment_SS);
}

int main(int argc,char**argv)
{
   ::testing::InitGoogleTest(&argc,argv);
   return RUN_ALL_TESTS();
}
