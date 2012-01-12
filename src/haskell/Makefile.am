#    Copyright (C) 2011,2012 Makoto Nishiura.

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
CABAL_FILE=$(srcdir)/haskell/erasm.cabal

haskell_sources= $(nodist_instruction_manual_parser_SOURCES) \
		 $(dist_erasm_checker_SOURCES) \
		 $(dist_preprocess_SOURCES) \
		 $(nodist_meta_haskell_SOURCES)

nodist_instruction_manual_parser_SOURCES = \
	haskell/InstructionParserMain.hs\
	haskell/InstructionManualParser.hs \
	haskell/InstructionManualLexer.hs \
	haskell/InstructionDefinition.hs \
	haskell/Operands.hs \
	haskell/EncodedInstructionDefinition.hs \
	haskell/EncodedInstructionDefinitionErrata.hs \
	haskell/AssemblerGen.hs \
	haskell/AssemblerFunction.hs \
	haskell/AssemblerInterface.hs \
	haskell/GenTestCommon.hs \
	haskell/GenTest64.hs \
	haskell/Equivalence.hs \
	haskell/PartialOrder.hs \
	haskell/ParamType.hs \
	haskell/OpcodeTrie64.hs \
	haskell/OpcodeTrie86.hs \
	haskell/Trie.hs \
	haskell/Pretty.hs \
	haskell/Prefix.hs \
	haskell/DisassemblerGen.hs \
	haskell/CPlusPlusSemantics.hs \
	haskell/CPlusPlusSemanticsDef.hs \
	haskell/CPlusPlusSemanticsPretty.hs \
	haskell/LaxCPlusPlusGrammar.hs \
	haskell/CPlusPlusLexical.hs \
	haskell/CPlusPlusPretty.hs \
	haskell/GrammarCommon.hs \
	haskell/ErasmCommon.hs \
	haskell/ErasmIO.hs \
	haskell/ErasmMain.hs 

haskell/ErasmTest.hs : haskell/GenErasmTest.hs \
		     $(nodist_instruction_manual_parser_SOURCES)
	runhaskell $^ > $@

erasm_test : haskell/ErasmTest.hs
	cd $(srcdir)/haskell && runhaskell ErasmTest.hs 

nodist_instruction_manual_parser_test_SOURCES = haskell/ErasmTest.hs


nodist_meta_haskell_SOURCES 	= haskell/MetaHaskell.hs

nodist_myghcpp_SOURCES		= haskell/MyGhcpp.hs \
				  haskell/MyProcess.hs

dist_erasm_checker_SOURCES	= haskell/ErasmChecker.hs \
				  haskell/ErasmCommon.hs \
				  haskell/ErasmParseCommon.hs \
				  haskell/Equivalence.hs \
				  haskell/Polynomial.hs \
				  haskell/EncodedInstructionDefinition.hs \
				  haskell/Operands.hs \
				  haskell/PartialOrder.hs \
				  haskell/Prefix.hs \
				  haskell/Combinators.hs

dist_preprocess_SOURCES	= 	  haskell/Preprocessor.hs \
				  haskell/ShellCommon.hs




cabal_builddir=$(abs_builddir)/haskell/dist
CABAL= cd $(srcdir)/haskell && PATH="$(abs_builddir):$(PATH)" cabal --builddir=$(cabal_builddir) 
#CABAL_BUILD_FLAGS=  --ghc-options="-F -pgmF preprocess"
CABAL_BUILD_FLAGS = 

haskell/TAGS : $(haskell_sources)
	hasktags -e $(haskell_sources) && mv TAGS haskell/TAGS


cabal_clean : 
	rm instruction_manual_parser$(EXEEXT) ; \
	$(CABAL) clean

cabal_configure : erasm_cabal_conf.timestamp 
cabal_build     : erasm_cabal_build.timestamp

%_cabal_conf.stamp:  Setup.hs  %.cabal 
	( $(CABAL) configure --ghc ) && touch $@

%_cabal_build.stamp:  Setup.hs %_cabal_conf.timestamp  %.cabal $(haskell_sources)
	( $(CABAL) build )  && touch $@




%.hs : %.y
	$(HAPPY) $< 

CPlusPlusGrammarUse.hs : GenCPlusPlusGrammarUse.hs 
	runhaskell $< > $@

haskell_sources : $(haskell_sources)


hunit_all : $(subst .hs,.hs.hunit,$(haskell_sources))

%.hs.hunit : %.hs
	echo "runTests" | ghci $^

quickcheck_all : $(subst .hs,.hs.quickcheck,$(haskell_sources))

%.hs.quickcheck : %.hs
	cd haskell && runghc quickCheck.hs `basename $^`


erasm_checker$(EXEEXT):  $(CABAL_FILE) preprocess$(EXEEXT) $(dist_erasm_checker_SOURCES) 
	($(CABAL) configure --flags="Checker -AsmGen" ) && ($(CABAL) build ) && \
	cp $(cabal_builddir)/build/erasm_checker/$@ $(builddir)

instruction_manual_parser$(EXEEXT):  \
		$(CABAL_FILE) preprocess$(EXEEXT) $(nodist_instruction_manual_parser_SOURCES) 
	($(CABAL) configure --flags="AsmGen" ) && ($(CABAL) build $(CABAL_BUILD_FLAGS) ) && \
	cp $(cabal_builddir)/build/instruction_manual_parser/$@  $(builddir)

meta_haskell :  $(CABAL_FILE) $(meta_haskell_SOURCES) 
	($(CABAL) configure --flags="MetaHaskell") && ($(CABAL) build) && \
	cp $(cabal_builddir)/build/meta_haskell_meta_haskell $(builddir)

preprocess$(EXEEXT):  $(CABAL_FILE) $(dist_preprocess_SOURCES)
	($(CABAL) configure --flags="Preprocess -AsmGen" ) && ($(CABAL) build ) && \
	cp $(cabal_builddir)/build/preprocess/$@ $(builddir)



runghc_%: %.hs
	$(RUNGHC)  $<
ghci_%: %.hs
	$(GHCI)    $<

qcheck_% : %.hs
	quickCheck +names $<

distclean-local: cabal_clean

