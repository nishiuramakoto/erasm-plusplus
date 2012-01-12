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
--    along with ERASM++; see the file COPYING3.  If not see
--    <http://www.gnu.org/licenses/>.  

{-# OPTIONS_GHC -F -pgmF preprocess  #-}
--m4_include(my_ghc_testframework.m4)

{-# LANGUAGE RankNTypes,FlexibleContexts #-}

-- Sparse univariate polynomial
module Polynomial( polyP
                 , coeff
                 , Polynomial.runTests
                 , module Math.CommutativeAlgebra.Polynomial
                 )
where
       
import Test.HUnit
import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import qualified Data.Map as Map
import Math.Algebra.Field.Base
import Math.Algebras.Structures
import Math.Algebras.VectorSpace
import Math.CommutativeAlgebra.Polynomial

  --  It looks like the library is currently under active development,
  --  and the interface is really unstable now .

[x,y] = map var ["x","y"] :: [ GlexPoly Q String ]
[rax,rbx] = map var ["rax","rbx"] :: [ GlexPoly Q String ]


f = 2 * x^2  + y^3 :: GlexPoly Q String

ASSERT_EQ((coeff (lm (x^2)) f ),(2))
ASSERT_EQ((coeff (lm x)     f ),(0))
ASSERT_EQ((coeff (lm (y^3)) f ),(1))

polyP :: (Num k ,Ord b,Monomial b,Algebra k b) => Parser (Vect k b) -> Parser (Vect k b)
polyP  termP = buildExpressionParser table termP
        <?> "polynomial"
        
table :: (Num k,Ord b,Monomial b,Algebra k b) => OperatorTable Char () (Vect k b)
table = [[op "*" (*) AssocLeft] --  , op "/" (/) AssocLeft ]
        ,[op "+" (+) AssocLeft , op "-" (-) AssocLeft]
        ]
  where
    op s f assoc = Infix (reservedOp s >> return f) assoc
    

polyP' = polyP $ (identifier >>= return.var) <|> (natural >>= return.fromInteger)


ASSERT_EQ((stringParser polyP' "x + 2*y"),(x + 2 *y)) 
ASSERT_EQ((stringParser polyP' "rax + 2*rbx"),(var "rax" + 2 * var "rbx")) 
ASSERT_EQ((stringParser polyP' "rax + 2/3*rbx"),(var "rax" + 2/3 * var "rbx")) 


stringParser ::  Parser a -> String -> a
stringParser  p input =
  case (parse p  "" input) of
    Left err -> ERROR("parse error at " ++ show err ++ " in: " ++ input )
    Right x  ->  x


    
lexer :: P.TokenParser ()
lexer = P.makeTokenParser
        (haskellDef 
         { P.reservedNames   = [ ]
         , P.reservedOpNames = [ "*","+","-" ] 
         })

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
decimal   = P.decimal lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
semi      = P.semi lexer
comma     = P.comma lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer
