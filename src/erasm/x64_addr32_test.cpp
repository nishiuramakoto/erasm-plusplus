#include "erasm/x64.hpp"
#include <gtest/gtest.h>
#define CONCAT(x,y) CONCAT_(x,y)
#define CONCAT_(x,y)  x ## y


#define TEST_EXPR_NOREX(EXPR,MOD,RM,DSIZE,DISP)	\
   TEST_EXPR(EXPR,MOD,RM,DSIZE,DISP,false,false)

#define TEST_EXPR_SIB_NOREX(EXPR,MOD,RM,DSIZE,DISP,SS,INDEX,BASE)  \
   TEST_EXPR_SIB(EXPR,MOD,RM,DSIZE,DISP,SS,INDEX,BASE,false,false)


#define TEST_EXPR(EXPR,MOD,RM,DSIZE,DISP,REX_B,REX_X)	\
TEST(x64_addr32_test,CONCAT(test_expr_,__LINE__))		\
{								\
   DwordPtr  p(dword_ptr[ (EXPR) ]);				\
   EXPECT_EQ((MOD),(int)p.get_mod());				\
   EXPECT_EQ((RM) ,(int)p.get_rm());				\
   EXPECT_EQ((DSIZE),(int)p.get_disp_size());			\
   EXPECT_EQ((DISP) ,(int)p.get_disp32());			\
   EXPECT_EQ((bool)(REX_X) ,(bool)p.get_rex_x());		\
   EXPECT_EQ((bool)(REX_B) ,(bool)p.get_rex_b());		\
   EXPECT_EQ(false,p.has_sib());				\
}

#define TEST_EXPR_SIB(EXPR,MOD,RM,DSIZE,DISP,SS,INDEX,BASE,REX_B,REX_X) \
TEST(x64_addr32_test,CONCAT(test_expr_sib_,__LINE__))			\
{									\
   DwordPtr  p(dword_ptr[(EXPR)]);					\
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

using namespace erasm::prelude::x64::addr64;
using namespace erasm::prelude::x64;
using namespace erasm::prelude;


TEST(test0,test0)
{
   typedef EVAL((expr32FromRegDisp<Register32<1>,Disp32 >))    rt;
   rt r(ecx + 1);
   EXPECT_EQ(1,r.get_disp());
}


TEST_EXPR_NOREX(eax   ,0,0,0,0);
TEST_EXPR_NOREX(ecx+(int8_t)1 ,1,1,1,1);
TEST_EXPR_NOREX(ecx+_1 ,1,1,1,1);
TEST_EXPR_NOREX(edx+200,2,2,4,200);
TEST_EXPR_NOREX(ebp    ,1,5,1,0);
TEST_EXPR_NOREX(ebp+_1  ,1,5,1,1);
TEST_EXPR_NOREX(ebp+(int8_t)1  ,1,5,1,1);
TEST_EXPR_NOREX(ebp+200,2,5,4,200);
TEST_EXPR_NOREX(rip    ,0,5,4,0);
TEST_EXPR_NOREX(rip+1  ,0,5,4,1);
TEST_EXPR_NOREX(rip+1000,0,5,4,1000);


TEST_EXPR_SIB_NOREX(0u              , 0,4,4,0,0,4,5);
TEST_EXPR_SIB_NOREX(1u              , 0,4,4,1,0,4,5);
TEST_EXPR_SIB_NOREX(1000u           , 0,4,4,1000,0,4,5);
TEST_EXPR_SIB_NOREX(eax+ecx*_2      , 0,4,0,0,1,1,0);
TEST_EXPR_SIB_NOREX(eax+ecx*_2+_1    , 1,4,1,1,1,1,0);
TEST_EXPR_SIB_NOREX(eax+ecx*_2+(int8_t)1    , 1,4,1,1,1,1,0);
TEST_EXPR_SIB_NOREX(eax+ecx*_2+200  , 2,4,4,200,1,1,0);
TEST_EXPR_SIB_NOREX(ebx+edi*_4      , 0,4,0,0,2,7,3);
TEST_EXPR_SIB_NOREX(ebx+edi*_4+_1    , 1,4,1,1,2,7,3);
TEST_EXPR_SIB_NOREX(ebx+edi*_4+(int8_t)1    , 1,4,1,1,2,7,3);
TEST_EXPR_SIB_NOREX(ebx+edi*_4+200  , 2,4,4,200,2,7,3);
TEST_EXPR_SIB_NOREX(edx+ebp*_8      , 0,4,0,0,3,5,2);
TEST_EXPR_SIB_NOREX(edx+ebp*_8+_1    , 1,4,1,1,3,5,2);
TEST_EXPR_SIB_NOREX(edx+ebp*_8+(int8_t)1    , 1,4,1,1,3,5,2);
TEST_EXPR_SIB_NOREX(edx+ebp*_8+200  , 2,4,4,200,3,5,2);
TEST_EXPR_SIB_NOREX(eax*_3          , 0,4,0,0,1,0,0);
TEST_EXPR_SIB_NOREX(eax*_5          , 0,4,0,0,2,0,0);
TEST_EXPR_SIB_NOREX(eax*_9          , 0,4,0,0,3,0,0);
TEST_EXPR_SIB_NOREX(esp             , 0,4,0,0,0,4,4);
TEST_EXPR_SIB_NOREX(esp+_1          , 1,4,1,1,0,4,4);
TEST_EXPR_SIB_NOREX(esp+(int8_t)1           , 1,4,1,1,0,4,4);
TEST_EXPR_SIB_NOREX(esp+200         , 2,4,4,200,0,4,4);
TEST_EXPR_SIB_NOREX(_2*edx          , 0,4,4,0,1,2,5);
TEST_EXPR_SIB_NOREX(_2*edx+1        , 0,4,4,1,1,2,5);
TEST_EXPR_SIB_NOREX(_2*edx+200      , 0,4,4,200,1,2,5);
TEST_EXPR_SIB_NOREX(_2*edx + ebp    , 1,4,1,0,1,2,5);
TEST_EXPR_SIB_NOREX(_2*edx + ebp+_1  , 1,4,1,1,1,2,5);
TEST_EXPR_SIB_NOREX(_2*edx + ebp+(int8_t)1  , 1,4,1,1,1,2,5);
TEST_EXPR_SIB_NOREX(_2*edx + ebp+200, 2,4,4,200,1,2,5);

TEST_EXPR(r8d   ,0,0,0,0,true,false);
TEST_EXPR(r9d+_1 ,1,1,1,1,true,false);
TEST_EXPR(r9d+(int8_t)1 ,1,1,1,1,true,false);
TEST_EXPR(r10d+200,2,2,4,200,true,false);
TEST_EXPR(r13d    ,1,5,1,0,true,false);
TEST_EXPR(r13d+_1  ,1,5,1,1,true,false);
TEST_EXPR(r13d+(int8_t)1  ,1,5,1,1,true,false);
TEST_EXPR(r13d+200,2,5,4,200,true,false);

TEST_EXPR_SIB(r8d+ecx*_2      , 0,4,0,0,1,1,0,true,false);
TEST_EXPR_SIB(eax+r9d*_2      , 0,4,0,0,1,1,0,false,true);
TEST_EXPR_SIB(r8d+r9d*_2       , 0,4,0,0,1,1,0,true,true);

TEST_EXPR_SIB(r11d+r15d*_4      , 0,4,0,0,2,7,3,true,true);
TEST_EXPR_SIB(ebx+r15d*_4      , 0,4,0,0,2,7,3,false,true);
TEST_EXPR_SIB(r11d+edi*_4      , 0,4,0,0,2,7,3,true,false);

TEST_EXPR_SIB(r9d+r13d*_8       , 0,4,0,0,3,5,1,true,true);
TEST_EXPR_SIB(r8d*_3           , 0,4,0,0,1,0,0,true,true);
TEST_EXPR_SIB(r12d             , 0,4,0,0,0,4,4,true,false);
TEST_EXPR_SIB(_2*r10d          , 0,4,4,0,1,2,5,false,true);
TEST_EXPR_SIB(_2*r10d + r13d    , 1,4,1,0,1,2,5,true,true);
TEST_EXPR_SIB(r12d*_2          , 0,4,4,0,1,4,5,false,true);



int main(int argc,char**argv)
{
   ::testing::InitGoogleTest(&argc,argv);
   return RUN_ALL_TESTS();
}
