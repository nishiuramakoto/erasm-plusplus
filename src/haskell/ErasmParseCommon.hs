--    Copyright (C) 2011,2012 Makoto Nishiura.

--    This file is part of ERASM++.

--    ERASM++ is free software; you can redistribute it and/or modify it under
--    the terms of the GNU General Public License as published by the Free
--    Software Foundation; either version 3, or (at your option) any later
--    version.

--    ERASM++ is distributed in the hope that it will be useful, but WITHOUT ANY
--    WARRANTY; without even the implied warranty of MERCHANTABILITY or
--    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--    for more details.

--    You should have received a copy of the GNU General Public License
--    along with ERASM++; see the file COPYING.  If not see
--    <http://www.gnu.org/licenses/>.  

{-# OPTIONS_GHC -F -pgmF preprocess  #-}
--m4_include(my_ghc_testframework.m4)
module ErasmParseCommon ( stringParser
                       , runTests
                       , module Text.ParserCombinators.Parsec , module Text.ParserCombinators.Parsec.Expr,module  Text.ParserCombinators.Parsec.Language

                       )
where       
import Test.HUnit  
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language


stringParser ::  Parser a -> String -> a
stringParser  p input =
  case (parse p  "" input) of
    Left err -> ERROR("parse error at " ++ show err ++ " in: " ++ input )
    Right x  ->  x
