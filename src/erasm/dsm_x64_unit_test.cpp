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
#include <gtest/gtest.h>
#include "erasm/intel_prefix_bitset.hpp"
#include "erasm/x64_implementation_defined.hpp"

using namespace erasm::x64;

#define CONCAT(x,y) CONCAT_(x,y)
#define CONCAT_(x,y)  x ## y

#define TEST_FLAGS(set,cond)					\
TEST(erasm_dsm_flags_test,CONCAT(dsm_flags_test,__LINE__))	\
{								\
   prefix_bitset flags;						\
   flags.flags = 0 ;						\
   RexByte rex;						\
   rex.byte = 0 ;						\
   set;								\
   EXPECT_EQ(true,cond);					\
}

TEST(erasm_dsm_flags_test,init_test)
{
   prefix_bitset prefix;
   prefix.not_branch=1;
   prefix.branch=1;
   prefix.lock=1;
   prefix.repne=1;
   prefix.rep=1;
   prefix.cs=1;
   prefix.ds=1;
   prefix.es=1;
   prefix.fs=1;
   prefix.gs=1;
   prefix.operand_size=1;
   prefix.address_size=1;


   prefix.flags = 0;

   EXPECT_EQ(prefix.not_branch,0);
   EXPECT_EQ(prefix.branch,0);
   EXPECT_EQ(prefix.lock,0);
   EXPECT_EQ(prefix.repne,0);
   EXPECT_EQ(prefix.rep,0);
   EXPECT_EQ(prefix.cs,0);
   EXPECT_EQ(prefix.ds,0);
   EXPECT_EQ(prefix.es,0);
   EXPECT_EQ(prefix.fs,0);
   EXPECT_EQ(prefix.gs,0);
   EXPECT_EQ(prefix.operand_size,0);
   EXPECT_EQ(prefix.address_size,0);
}


TEST(erasm_dsm_flags_test,rex_init_test)
{
   RexByte rex;

   rex.b=1;
   rex.x=1;
   rex.r=1;
   rex.w=1;
   rex.rex=1;

   rex.byte=0x00;
   EXPECT_EQ(rex.byte,0);
   EXPECT_EQ(rex.b,0);
   EXPECT_EQ(rex.x,0);
   EXPECT_EQ(rex.r,0);
   EXPECT_EQ(rex.w,0);
   EXPECT_EQ(rex.rex,0);

}

// TEST_FLAGS( flags.lock  = 1 , flags.flags == PREFIX_FLAG_LOCK )
// TEST_FLAGS( flags.repne = 1 , flags.flags == PREFIX_FLAG_REPNE )
// TEST_FLAGS(  flags.rep   = 1 , flags.flags == PREFIX_FLAG_REP   )
// TEST_FLAGS(  flags.cs    = 1 , flags.flags == PREFIX_FLAG_CS   )
// TEST_FLAGS(  flags.ss    = 1 , flags.flags == PREFIX_FLAG_SS   )
// TEST_FLAGS(  flags.ds    = 1 , flags.flags == PREFIX_FLAG_DS   )
// TEST_FLAGS(  flags.es    = 1 , flags.flags == PREFIX_FLAG_ES   )
// TEST_FLAGS(  flags.fs    = 1 , flags.flags == PREFIX_FLAG_FS   )
// TEST_FLAGS(  flags.gs    = 1 , flags.flags == PREFIX_FLAG_GS   )
// TEST_FLAGS(  flags.operand_size    = 1 , flags.flags == PREFIX_FLAG_OPERAND_SIZE   )
// TEST_FLAGS(  flags.address_size    = 1 , flags.flags == PREFIX_FLAG_ADDRESS_SIZE   )
// TEST_FLAGS(  flags.rex_byte = 0x40 , flags.flags == PREFIX_FLAG_REX  && flags.rex )
// TEST_FLAGS(  flags.rex_byte = 0x48 , flags.flags == PREFIX_FLAG_REX_W && flags.rex_w  )
// TEST_FLAGS(  flags.rex_byte = 0x44 , flags.flags == PREFIX_FLAG_REX_R  && flags.rex_r )
// TEST_FLAGS(  flags.rex_byte = 0x42 , flags.flags == PREFIX_FLAG_REX_X  && flags.rex_x )
// TEST_FLAGS(  flags.rex_byte = 0x41 , flags.flags == PREFIX_FLAG_REX_B  && flags.rex_b )


TEST_FLAGS(  flags.cs = 1 ,  flags.not_branch )
TEST_FLAGS(  flags.ds = 1 ,  flags.branch )

TEST_FLAGS(  rex.byte = 0x40 ,  rex.rex )
TEST_FLAGS(  rex.byte = 0x48 ,  rex.w  )
TEST_FLAGS(  rex.byte = 0x44 ,  rex.r )
TEST_FLAGS(  rex.byte = 0x42 ,  rex.x )
TEST_FLAGS(  rex.byte = 0x41 ,  rex.b )


int main(int argc,char**argv)
{
   ::testing::InitGoogleTest(&argc,argv);
   return RUN_ALL_TESTS();
}
