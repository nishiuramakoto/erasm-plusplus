#    Copyright (C) 2011,2012 Makoto Nishiura

#    This file is part of ERASM++.

#    ERASM++ is free software; you can redistribute it and/or modify it under
#    the terms of the GNU General Public License as published by the Free
#    Software Foundation; either version 3, or (at your option) any later
#    version.

#    ERASM++ is distributed in the hope that it will be useful, but WITHOUT ANY
#    WARRANTY; without even the implied warranty of MERCHANTABILITY or
#    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#    for more details.

#    You should have received a copy of the GNU General Public License
#    along with ERASM++; see the file COPYING.  If not see
#    <http://www.gnu.org/licenses/>.  

include $(srcdir)/haskell/Makefile.am
export CPP
export CXX

if HAVE_LIBINTL
# Some strange (e.g. cygwin) systems will need this
GNU_DSM_LIB	= libgnu_dsm.la  -lopcodes -lbfd -lintl -liberty
GTEST_LIB       = -lgtest
else
GNU_DSM_LIB	= libgnu_dsm.la  -lopcodes -lbfd
GTEST_LIB       = -lgtest -lpthread
endif

CPPFLAGS_DR  =
LDFLAGS_DR  =

if HOST_IS_64BIT
CPPFLAGS_DR  += -DX86_64
else
CPPFLAGS_DR  += -DX86_32
endif

if WINDOWS
CPPFLAGS_DR += -DWINDOWS
LDFLAGS_DR  += dynamorio.dll
endif

if LINUX
CPPFLAGS_DR += -DLINUX
LDFLAGS_DR  += -ldynamorio
endif

CXXFLAGS_DR = $(CPPFLAGS_DR)


bin_PROGRAMS =

TESTS64_ADDR = x64_addr64_test x64_addr32_test
TESTS86_ADDR = x86_addr16_test x86_addr32_test

TESTS64 = \
	erasm_dsm_x64_unit_test \
	test_meta_leftist_heap \
	x64_addr64_test x64_addr32_test \
	test_x64_assembler_64_32.sh \
	test_x64_disassembler_64_32.sh \
	check_illegal_code_x64.sh

TESTS86 = \
	test_meta_leftist_heap \
	x86_addr16_test x86_addr32_test \
	test_x86_assembler_32_32.sh \
	test_x86_disassembler_32_32.sh \
	check_illegal_code_x86_32.sh 


TESTS_ALL = \
	erasm_dsm_x64_unit_test \
	test_meta_leftist_heap \
	x64_addr64_test x64_addr32_test \
	x86_addr16_test x86_addr32_test \
	test_x64_assembler_64_32.sh \
	test_x86_assembler_32_32.sh \
	test_x64_disassembler_64_32.sh \
	test_x86_disassembler_32_32.sh \
	check_illegal_code_x86_32.sh \
	check_illegal_code_x64.sh


TESTS = $(TESTS_ALL)

check_PROGRAMS 	= \
	erasm_dsm_x64_unit_test \
	test_meta_leftist_heap \
	x64_addr64_test x64_addr32_test \
	x86_addr16_test x86_addr32_test \
	x64_assembler_test_64_32 \
	x86_assembler_test_32_32 \
	erasm_dsm_x64_test \
	erasm_dsm_x86_test \
	erasm_demo86 \
	erasm_checker \
	preprocess

lib_LTLIBRARIES 	= liberasm.la
check_LTLIBRARIES 	= liberasm64_test.la liberasm86_test.la libgnu_dsm.la

ERASM64_TEST_LDADD = liberasm64_test.la liberasm.la 
ERASM86_TEST_LDADD = liberasm86_test.la liberasm.la 

EXTRA_PROGRAMS	= \
	iotest \
	erasm_dsm_x86_profile \
	erasm_dsm_x86_demo \
	erasm_dsm_x86_formatA \
	erasm_dsm_x86_formatB \
	erasm_dsm_x86_formatC \
	erasm_dsm_x86_performance_test \
	erasm_dsm_x86_performance_test_noio \
	erasm_dsm_x86_count_ret \
	erasm_dsm_x86_count_cti \
	erasm_dsm_x64_performance_test \
	erasm_gdsm_x86_formatC \
	erasm_gdsm_x86_performance_test \
	erasm_gdsm_x64_performance_test \
	erasm_udis_x86_formatA \
	erasm_udis_x86_formatB \
	erasm_udis_x86_formatC \
	erasm_udis_x86_test \
	erasm_udis_x86_performance_test \
	erasm_dr_x86_formatB \
	erasm_dr_x86_performance_test \
	erasm_dr_x86_test \
	erasm_dr_x86_count_ret \
	erasm_dr_x86_count_cti \
	test_meta_binomial_heap \
	instruction_manual_parser \
	myghcpp meta_haskell \
	test_gnudsm hook_test \
	test_faststream

nobase_include_HEADERS 	= \
	erasm/dsm_x64.hpp  \
	erasm/dsm_x64_util.hpp \
	erasm/dsm_x64_auto_stm.hpp \
	erasm/dsm_x86.hpp \
	erasm/dsm_x86_util.hpp \
	erasm/dsm_x86_auto_stm.hpp \
	erasm/intel_common.hpp \
	erasm/intel_prefix_bitset.hpp \
	erasm/x64.hpp \
	erasm/x64_addr32_data16.hpp \
	erasm/x64_addr32_data32.hpp \
	erasm/x64_addr64.hpp \
	erasm/x64_addr64_data16.hpp \
	erasm/x64_addr64_data32.hpp \
	erasm/x64_assembler_impl.hpp \
	erasm/x64_common.hpp \
	erasm/x64_implementation_defined.hpp \
	erasm/x64_instruction_definition.hpp \
	erasm/x64_instruction_definition_common.hpp \
	erasm/x64_io.hpp \
	erasm/x64_prefix.hpp \
	erasm/x64_ptr.hpp \
	erasm/x86.hpp \
	erasm/x86_addr16.hpp \
	erasm/x86_addr16_data16.hpp \
	erasm/x86_addr16_data32.hpp \
	erasm/x86_addr32.hpp \
	erasm/x86_addr32_data16.hpp \
	erasm/x86_addr32_data32.hpp \
	erasm/x86_assembler_impl.hpp \
	erasm/x86_common.hpp \
	erasm/x86_instruction_definition.hpp \
	erasm/x86_instruction_definition_common.hpp \
	erasm/x86_io.hpp \
	erasm/x86_ptr.hpp \
	erasm/meta_prelude_core.hpp \
	erasm/meta_prelude.hpp \
	erasm/meta_prelude_ifn.hpp \
	erasm/meta_prelude_impl.hpp \
	erasm/meta_prelude_functions.hpp \
	erasm/meta_leftist_heap.hpp \
	erasm/meta_polynomial.hpp \
	erasm/common_macros.hpp \
	erasm/faststream.hpp

dist_noinst_HEADERS	= \
	erasm/x64_assembler_test_code_64_32.hpp \
	erasm/x86_assembler_test_code_32_32.hpp \
	gnu_disassembler.hpp \
	dprintf.h \
	memory_protection.hpp


dist_noinst_SCRIPTS	= test_assembler_common.sh \
			  test_x86_assembler_32_32.sh \
			  test_x64_assembler_64_32.sh \
			  check_illegal_code.sh \
			  check_illegal_code_x86_32.sh \
			  check_illegal_code_x64.sh \
			  test_x64_disassembler_64_32.sh \
			  test_x86_disassembler_32_32.sh

EXTRA_DIST	= erasm/x64_assembler_illegal_code.cpp \
		  erasm/x64_assembler_manual_test.cpp \
		  erasm/x64_assembler_auto_test.cpp \
		  erasm/x86_assembler_illegal_code_32_32.cpp \
		  erasm/x86_assembler_manual_test.cpp \
		  erasm/x86_assembler_auto_test.cpp \
		  parse_all.stamp \
		  haskell/erasm.cabal \
		  haskell/my_ghc_testframework.m4 \
		  $(ERASM_GENERATED_FILES) 

#nodist_data_DATA	=  $(REFERENCES) $(TEST_REFERENCE)

AM_CPPFLAGS		= -include config.h -I $(srcdir) 
BUILT_SOURCES		= $(ERASM_GENERATED_FILES)

liberasm_la_SOURCES 	= erasm/x64.cpp \
			  erasm/x64_assembler_impl.cpp \
			  erasm/x64_addr64_data32.cpp \
			  erasm/x64_instruction_definition.cpp \
			  erasm/x86.cpp \
			  erasm/x86_assembler_impl.cpp \
			  erasm/x86_addr32_data32.cpp \
			  erasm/x86_instruction_definition.cpp \
			  erasm/x86_io.cpp \
			  erasm/x64_io.cpp \
			  erasm/faststream.cpp

liberasm64_test_la_SOURCES= erasm/x64_assembler_test_code_64_32.cpp 

liberasm64_test_la_DEPENDENCIES = erasm/x64_assembler_test_code_64_32.hpp \
				  erasm/x64_assembler_manual_test.cpp 

liberasm86_test_la_SOURCES= erasm/x86_assembler_test_code_32_32.cpp 

liberasm86_test_la_DEPENDENCIES = erasm/x86_assembler_test_code_32_32.hpp \
			    	  erasm/x86_assembler_manual_test.cpp


erasm_demo86_SOURCES= erasm/demo86.cpp
erasm_demo86_LDADD  = liberasm.la


x64_assembler_test_64_32_SOURCES  = erasm/x64_assembler_test_main_64_32.cpp
x64_assembler_test_64_32_LDADD	  = $(GNU_DSM_LIB) $(ERASM64_TEST_LDADD)
x64_assembler_test_64_32_CPPFLAGS = # -DUSE_SAMPLE_HEADER

x86_assembler_test_32_32_LDADD		= $(GNU_DSM_LIB) $(ERASM86_TEST_LDADD)
x86_assembler_test_32_32_SOURCES	= erasm/x86_assembler_test_main_32_32.cpp

test_dsm :  erasm_dsm_x64_unit_test$(EXEEXT) \
	    erasm_dsm_x64_test$(EXEEXT) \
	    erasm_checker$(EXEEXT)
	./erasm_dsm_x64_unit_test && \
	srcdir=. ./test_x64_disassembler_64_32.sh

dump_dsm :  erasm_dsm_x64_test$(EXEEXT)
	./erasm_dsm_x64_test

time_gdsm : erasm_gdsm_performance_test$(EXEEXT)
	time ./erasm_gdsm_performance_test$(EXEEXT)

time_dsm : erasm_dsm_x64_performance_test$(EXEEXT)
	(echo "revision="`bzr revno`;\
	for x in {0..10};do \
		./erasm_dsm_x64_performance_test; \
	done) >> timings.txt

erasm_dsm_x64_performance_test_SOURCES = \
	erasm/dsm_x64_performance_test.cpp \
	erasm/dsm_x64_auto_stm.hpp

erasm_dsm_x64_performance_test_CXXFLAGS = -O3 -DTEST_MYDSM
erasm_dsm_x64_performance_test_LDADD = $(GNU_DSM_LIB) $(ERASM64_TEST_LDADD)


erasm_gdsm_x64_performance_test_SOURCES = \
	erasm/dsm_x64_performance_test.cpp
erasm_gdsm_x64_performance_test_CXXFLAGS = -O3
erasm_gdsm_x64_performance_test_LDADD = $(GNU_DSM_LIB) $(ERASM64_TEST_LDADD)



erasm_dsm_x64_test_SOURCES 	= erasm/dsm_x64_test_main.cpp \
				  erasm/dsm_x64_auto_stm.hpp 

erasm_dsm_x64_test_LDADD	= $(GNU_DSM_LIB) $(ERASM64_TEST_LDADD) liberasm.la
erasm_dsm_x64_test_CXXFLAGS	= $(AM_CXXFLAGS)

erasm_dsm_x64_unit_test_LDADD 	= $(GTEST_LIB)
erasm_dsm_x64_unit_test_SOURCES = erasm/dsm_x64_unit_test.cpp
erasm_dsm_x64_unit_test_CXXFLAGS = $(AM_CXXFLAGS)




test_dsm86 :  erasm_dsm_x86_test$(EXEEXT) \
	      erasm_checker$(EXEEXT) \
	      test_x86_disassembler_32_32.sh
	srcdir=$(srcdir) $(srcdir)/test_x86_disassembler_32_32.sh


dump_dsm86 :  erasm_dsm_x86_test$(EXEEXT)
	./erasm_dsm_x86_test

time_dsm86 : 	erasm_dsm_x86_performance_test$(EXEEXT) \
		erasm_dsm_x86_performance_test_noio$(EXEEXT) \
		erasm_dsm_x86_formatA$(EXEEXT) \
		erasm_dsm_x86_formatB$(EXEEXT) \
		erasm_dsm_x86_formatC$(EXEEXT) \
		erasm_dsm_x86_count_ret$(EXEEXT) \
		erasm_dsm_x86_count_cti$(EXEEXT) \
		erasm_gdsm_x86_performance_test$(EXEEXT) \
		erasm_gdsm_x86_formatC$(EXEEXT) \
		erasm_udis_x86_performance_test$(EXEEXT) \
		erasm_udis_x86_formatA$(EXEEXT) \
		erasm_udis_x86_formatB$(EXEEXT) \
		erasm_udis_x86_formatC$(EXEEXT) \
		erasm_dr_x86_formatB$(EXEEXT) \
		erasm_dr_x86_performance_test$(EXEEXT) \
		erasm_dr_x86_test$(EXEEXT) \
		erasm_dr_x86_count_ret$(EXEEXT) \
		erasm_dr_x86_count_cti$(EXEEXT) 
	for x in $^ ;do echo $$x;time (./$$x | wc)  ;done


dsm86.prof : erasm_dsm_x86_profile$(EXEEXT)
	./$^ | wc; \
	gprof .libs/$^ > $@

reprof : clean_erasm_dsm_x86_profile dsm86.prof

clean_erasm_dsm_x86_prof : FORCE
	rm erasm_dsm_x86_profile.o erasm_dsm_x86_profile$(EXEEXT) ; true

# CXXFLAGS_O3= -DNDEBUG  -O3 -finline-limit=1000000 \
# 	--param max-inline-insns-single=1000000 \
# 	--param max-inline-insns-auto=1000000 \
# 	--param inline-unit-growth=10000 \
# 	--param early-inlining-insns=10000 \
# 	--param max-early-inliner-iterations=10000

#CXXFLAGS_O3 = -DALWAYS_INLINE="" -DNDEBUG -O3 # -finline-limit=100000
CXXFLAGS_O3 =  -DNDEBUG -O3 # -finline-limit=100000
CXXFLAGS_PROF = $(CXXFLAGS_O3) # -pg

iotest_SOURCES = iotest.cpp
iotest_LDADD = liberasm.la

erasm_dsm_x86_profile_SOURCES 	= erasm/dsm_x86_test_main.cpp  \
				  erasm/x86_io.cpp \
				  erasm/x64_io.cpp \
				  erasm/faststream.cpp
erasm_dsm_x86_profile_LDADD	= $(ERASM86_TEST_LDADD) 
erasm_dsm_x86_profile_CXXFLAGS	= -DFORMAT=A -DPROFILING $(CXXFLAGS_PROF) -pg
erasm_dsm_x86_profile_LDFLAGS	= $(CXXFLAGS_PROF)


erasm_dsm_x86_performance_test_SOURCES  = erasm/dsm_x86_performance_test.cpp
erasm_dsm_x86_performance_test_CXXFLAGS = -DTEST_MYDSM  $(CXXFLAGS_PROF)
erasm_dsm_x86_performance_test_LDFLAGS  = $(CXXFLAGS_PROF)
erasm_dsm_x86_performance_test_LDADD    = $(ERASM86_TEST_LDADD) 

erasm_dsm_x86_performance_test_noio_SOURCES  = erasm/dsm_x86_test_noio.cpp
erasm_dsm_x86_performance_test_noio_CXXFLAGS = -DPROFILING -DTEST_MYDSM  $(CXXFLAGS_PROF)
erasm_dsm_x86_performance_test_noio_LDFLAGS  = $(CXXFLAGS_PROF)
erasm_dsm_x86_performance_test_noio_LDADD    = $(ERASM86_TEST_LDADD)

erasm_dsm_x86_count_ret_SOURCES = erasm/dsm_x86_count_ret.cpp
erasm_dsm_x86_count_ret_CXXFLAGS =  -DTEST_MYDSM  $(CXXFLAGS_PROF)
erasm_dsm_x86_count_ret_LDFLAGS  =  $(CXXFLAGS_PROF)
erasm_dsm_x86_count_ret_LDADD = $(ERASM86_TEST_LDADD)

erasm_dsm_x86_count_cti_SOURCES  = erasm/dsm_x86_count_cti.cpp
erasm_dsm_x86_count_cti_CXXFLAGS =  -DTEST_MYDSM  $(CXXFLAGS_PROF)
erasm_dsm_x86_count_cti_LDFLAGS  =  $(CXXFLAGS_PROF)
erasm_dsm_x86_count_cti_LDADD    = $(ERASM86_TEST_LDADD)


test_faststream_SOURCES = test_faststream.cpp
test_faststream_LDADD	= liberasm.la

erasm_dsm_x86_test_SOURCES 	= erasm/dsm_x86_test_main.cpp 
erasm_dsm_x86_test_LDADD	= $(ERASM86_TEST_LDADD) 
erasm_dsm_x86_test_CXXFLAGS	= $(AM_CXXFLAGS)

erasm_dsm_x86_formatA_SOURCES 	= erasm/dsm_x86_test_main.cpp 
erasm_dsm_x86_formatA_LDADD	= $(ERASM86_TEST_LDADD) 
erasm_dsm_x86_formatA_CXXFLAGS	= -DFORMAT=A -DPROFILING $(CXXFLAGS_PROF)
erasm_dsm_x86_formatA_LDFLAGS	= $(CXXFLAGS_PROF)

erasm_dsm_x86_formatB_SOURCES 	= erasm/dsm_x86_test_main.cpp 
erasm_dsm_x86_formatB_LDADD	= $(ERASM86_TEST_LDADD) 
erasm_dsm_x86_formatB_CXXFLAGS	= -DFORMAT=B -DPROFILING $(CXXFLAGS_PROF)
erasm_dsm_x86_formatB_LDFLAGS	= $(CXXFLAGS_PROF)

erasm_dsm_x86_formatC_SOURCES 	= erasm/dsm_x86_test_main.cpp 
erasm_dsm_x86_formatC_LDADD	= $(ERASM86_TEST_LDADD) 
erasm_dsm_x86_formatC_CXXFLAGS	= -DFORMAT=C -DPROFILING $(CXXFLAGS_PROF)
erasm_dsm_x86_formatC_LDFLAGS	= $(CXXFLAGS_PROF)

erasm_dsm_x86_demo_SOURCES 	=  erasm/dsm_x86_demo.cpp 
erasm_dsm_x86_demo_LDADD	= $(ERASM86_TEST_LDADD) 
erasm_dsm_x86_demo_LDFLAGS	= $(CXXFLAGS_PROF)

erasm_gdsm_x86_performance_test_SOURCES  = erasm/dsm_x86_performance_test.cpp
erasm_gdsm_x86_performance_test_CXXFLAGS = -DTEST_GDSM  $(CXXFLAGS_PROF)
erasm_gdsm_x86_performance_test_LDFLAGS  = $(CXXFLAGS_PROF)
erasm_gdsm_x86_performance_test_LDADD    = $(GNU_DSM_LIB) $(ERASM86_TEST_LDADD)

erasm_gdsm_x86_formatC_SOURCES 	= erasm/dsm_x86_test_main.cpp 
erasm_gdsm_x86_formatC_LDADD	= $(ERASM86_TEST_LDADD) $(GNU_DSM_LIB) 
erasm_gdsm_x86_formatC_CXXFLAGS	= -DFORMAT=C -DPROFILING -DTEST_GDSM $(CXXFLAGS_PROF)
erasm_gdsm_x86_formatC_LDFLAGS	= $(CXXFLAGS_PROF)


erasm_udis_x86_performance_test_SOURCES	= erasm/dsm_x86_performance_test.cpp
erasm_udis_x86_performance_test_LDADD	= $(ERASM86_TEST_LDADD) $(GNU_DSM_LIB) 
erasm_udis_x86_performance_test_CXXFLAGS= -DPROFILING -DTEST_UDIS $(CXXFLAGS_PROF)
erasm_udis_x86_performance_test_LDFLAGS	= $(CXXFLAGS_PROF) -ludis86

erasm_udis_x86_formatA_SOURCES 	= erasm/dsm_x86_test_main.cpp 
erasm_udis_x86_formatA_LDADD	= $(ERASM86_TEST_LDADD) $(GNU_DSM_LIB) 
erasm_udis_x86_formatA_CXXFLAGS	= -DFORMAT=A -DPROFILING -DTEST_UDIS $(CXXFLAGS_PROF)
erasm_udis_x86_formatA_LDFLAGS	= $(CXXFLAGS_PROF) -ludis86

erasm_udis_x86_formatB_SOURCES 	= erasm/dsm_x86_test_main.cpp 
erasm_udis_x86_formatB_LDADD	= $(ERASM86_TEST_LDADD) $(GNU_DSM_LIB) 
erasm_udis_x86_formatB_CXXFLAGS	= -DFORMAT=B -DPROFILING -DTEST_UDIS $(CXXFLAGS_PROF)
erasm_udis_x86_formatB_LDFLAGS	= $(CXXFLAGS_PROF) -ludis86

erasm_udis_x86_formatC_SOURCES 	= erasm/dsm_x86_test_main.cpp 
erasm_udis_x86_formatC_LDADD	= $(ERASM86_TEST_LDADD) $(GNU_DSM_LIB) 
erasm_udis_x86_formatC_CXXFLAGS	= -DFORMAT=C -DPROFILING -DTEST_UDIS $(CXXFLAGS_PROF)
erasm_udis_x86_formatC_LDFLAGS	= $(CXXFLAGS_PROF) -ludis86

erasm_udis_x86_test_SOURCES 	= erasm/dsm_x86_test_main.cpp 
erasm_udis_x86_test_LDADD	= $(ERASM86_TEST_LDADD) $(GNU_DSM_LIB) 
erasm_udis_x86_test_CXXFLAGS	= -DTEST_UDIS $(CXXFLAGS_PROF)
erasm_udis_x86_test_LDFLAGS	= $(CXXFLAGS_PROF) -ludis86

erasm_dr_x86_formatB_SOURCES 	= erasm/dsm_x86_test_main.cpp 
erasm_dr_x86_formatB_LDADD	        = $(ERASM86_TEST_LDADD) 
erasm_dr_x86_formatB_CXXFLAGS	= -DPROFILING -DTEST_DR $(CXXFLAGS_PROF) $(CXXFLAGS_DR)
erasm_dr_x86_formatB_LDFLAGS	= $(CXXFLAGS_PROF)  $(LDFLAGS_DR)

erasm_dr_x86_test_SOURCES 	= erasm/dsm_x86_test_main.cpp 
erasm_dr_x86_test_LDADD	= $(ERASM86_TEST_LDADD) $(GNU_DSM_LIB) 
erasm_dr_x86_test_CXXFLAGS	= -DTEST_DR $(CXXFLAGS_PROF) $(CXXFLAGS_DR)
erasm_dr_x86_test_LDFLAGS	= $(CXXFLAGS_PROF) $(LDFLAGS_DR)

erasm_dr_x86_performance_test_SOURCES	= erasm/dsm_x86_performance_test.cpp
erasm_dr_x86_performance_test_LDADD	= $(ERASM86_TEST_LDADD) $(GNU_DSM_LIB) 
erasm_dr_x86_performance_test_CXXFLAGS  = -DPROFILING -DTEST_DR $(CXXFLAGS_PROF) $(CXXFLAGS_DR)
erasm_dr_x86_performance_test_LDFLAGS	= $(CXXFLAGS_PROF) $(LDFLAGS_DR)

erasm_dr_x86_count_ret_SOURCES 	= erasm/dsm_x86_count_ret.cpp 
erasm_dr_x86_count_ret_LDADD	= $(ERASM86_TEST_LDADD) $(GNU_DSM_LIB) 
erasm_dr_x86_count_ret_CXXFLAGS	= -DTEST_DR $(CXXFLAGS_PROF) $(CXXFLAGS_DR)
erasm_dr_x86_count_ret_LDFLAGS	= $(CXXFLAGS_PROF) $(LDFLAGS_DR)

erasm_dr_x86_count_cti_SOURCES 	= erasm/dsm_x86_count_cti.cpp 
erasm_dr_x86_count_cti_LDADD	= $(ERASM86_TEST_LDADD) $(GNU_DSM_LIB) 
erasm_dr_x86_count_cti_CXXFLAGS	= -DTEST_DR $(CXXFLAGS_PROF) $(CXXFLAGS_DR)
erasm_dr_x86_count_cti_LDFLAGS	= $(CXXFLAGS_PROF) $(LDFLAGS_DR)


x64_addr64_test_LDADD		= $(GTEST_LIB) -lpthread
x64_addr64_test_SOURCES		= \
	erasm/x64_addr64_test.cpp \
	erasm/x64_addr64.hpp \
	erasm/x64.cpp
x64_addr64_test_CXXFLAGS	= $(AM_CXXFLAGS)

x64_addr32_test_LDADD		= $(GTEST_LIB) -lpthread
x64_addr32_test_SOURCES		= erasm/x64_addr32_test.cpp  erasm/x64.cpp
x64_addr32_test_CXXFLAGS	= $(AM_CXXFLAGS)

x86_addr32_test_LDADD		= $(GTEST_LIB) -lpthread
x86_addr32_test_SOURCES		= \
	erasm/x86_addr32_test.cpp \
	erasm/x86.cpp \
	erasm/x64.cpp
x86_addr32_test_CXXFLAGS	= $(AM_CXXFLAGS)

x86_addr16_test_LDADD		= $(GTEST_LIB) -lpthread
x86_addr16_test_SOURCES		= \
	erasm/x86_addr16_test.cpp  \
	erasm/x86.cpp \
	erasm/x64.cpp
x86_addr16_test_CXXFLAGS	= $(AM_CXXFLAGS)

test_gnudsm_SOURCES		= unit_test_main.cpp \
				  test_gnudsm.cpp \
				  test_gnudsm.hpp
test_gnudsm_CXXFLAGS		= $(CPPUNIT_CFLAGS)
test_gnudsm_LDADD		= $(GNU_DSM_LIB)
test_gnudsm_LDFLAGS		= $(CPPUNIT_LIBS)


hook_test_SOURCES		= hook_test.cpp
hook_test_LDADD			=   $(DSM_LIB) $(GNU_DSM_LIB)


TEST_LIB	= libunit_test.la

libgnu_dsm_la_SOURCES	= gnu_disassembler.cpp dprintf.cpp

#test_x86_SOURCES		= meta_prelude.hpp x86_addr16.hpp x86_common.hpp x86.hpp x86.cpp
#test_lazy_SOURCES		= meta_prelude.hpp meta_polynomial.hpp test_lazy.cpp
test_meta_leftist_heap_SOURCES	= test_meta_leftist_heap.cpp
test_meta_binomial_heap_SOURCES	= test_meta_binomial_heap.cpp


AM_HCFLAGS=-cpp -v0 # -fglasgow-exts
#AM_GHCFLAGS=-v0
#HAPPY	= happy -a -g -c
HAPPY	= happy -a -g
RUNGHC	= runghc $(AM_HCFLAGS) $(HCFLAGS)
GHCI	= ghci   $(AM_HCFLAGS) $(HCFLAGS)
HC      = ghc
HCCOMPILE = $(HC) $(AM_HCFLAGS) $(HCFLAGS)


test_asm_performance.s  : test_asm_performance.cpp Makefile
	g++ -c $<  -S -masm=intel  -O3

.c.s:
	$(COMPILE) $< -S -masm=intel
.cpp.s:
	$(CXXCOMPILE) $< -S -masm=intel

.pdf.txt:
	pstotext $< > $@

%64.o : %64.s
	$(AS) --64 $^ -o $@

%32.o : %.s
	$(AS) --32 $^ -o $@

objdump_% : %.o
	$(OBJDUMP) -M intel -d $^

AS=as
OBJDUMP=objdump

REFERENCES	= $(srcdir)/instruction_ref_a_m.txt  \
		  $(srcdir)/instruction_ref_n_z.txt
TEST_REFERENCE  = $(srcdir)/inst_ref_test.txt


ERASM_GENERATED_FILES= 	\
	erasm/dsm_x64_auto_stm.hpp \
	erasm/dsm_x86_auto_stm.hpp \
	erasm/x64_addr32_data16.cpp \
	erasm/x64_addr32_data16.hpp \
	erasm/x64_addr32_data32.cpp \
	erasm/x64_addr32_data32.hpp \
	erasm/x64_addr64_data16.cpp \
	erasm/x64_addr64_data16.hpp \
	erasm/x64_addr64_data32.cpp \
	erasm/x64_addr64_data32.hpp \
	erasm/x64_assembler_auto_test.cpp \
	erasm/x64_instruction_definition.cpp \
	erasm/x64_instruction_definition.hpp \
	erasm/x86_addr16_data16.cpp \
	erasm/x86_addr16_data16.hpp \
	erasm/x86_addr16_data32.cpp \
	erasm/x86_addr16_data32.hpp \
	erasm/x86_addr32_data16.cpp \
	erasm/x86_addr32_data16.hpp \
	erasm/x86_addr32_data32.cpp \
	erasm/x86_addr32_data32.hpp \
	erasm/x86_assembler_auto_test.cpp \
	erasm/x86_instruction_definition.cpp \
	erasm/x86_instruction_definition.hpp 


$(ERASM_GENERATED_FILES) : parse_all.stamp
	@if test -f $@;then :; else \
		rm -f parse_all.stamp; \
		$(MAKE) $(AM_MAKEFLAGS) parse_all.stamp ; \
	fi

%.hs :
	mkdir -p `dirname $@` && touch --date='1999-1-1 0:0:0'  $@

parse_all.stamp : $(nodist_instruction_manual_parser_SOURCES)
	@rm -f parse_all.tmp
	@touch parse_all.tmp
	$(MAKE) $(AM_MAKEFLAGS) instruction_manual_parser$(EXEEXT) && \
	LC_ALL=en_US.utf-8 ./instruction_manual_parser  $(REFERENCES)
	@mv -f parse_all.tmp $@


regen : instruction_manual_parser$(EXEEXT) FORCE
	LC_ALL=en_US.utf-8 ./$<  $(REFERENCES)


run_test64 : x64_assembler_test_64_32$(EXEEXT)
	./$<
run_test32 : x86_assembler_test_32_32$(EXEEXT)
	./$<


FORCE:

CLEANFILES	= 	opcode_app.txt opcode.txt p2inst.txt seed.txt \
			encdef.tmp.txt \
			out1.txt \
			out2.txt \
			out3.txt \
			parse_all.tmp \
			parse_test \
			disassembler_impl.cpp \
			instruction_code.hpp \
			assembler_converter.hpp \
			codetest.cpp \
			assembler.hpp \
			x.cpp


MAINTAINERCLEANFILES = 	parse_all.stamp $(ERASM_GENERATED_FILES) 			

