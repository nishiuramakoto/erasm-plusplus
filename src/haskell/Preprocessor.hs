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

{-# LANGUAGE DeriveDataTypeable , DoAndIfThenElse  #-}
module Main
where

import ShellCommon

data MyArgs = MyArgs { rest :: [ String ] , debug :: String} deriving (Show, Data, Typeable)

--sample = MyArgs{ hello = def &= help "World argument" &= opt "world"}
sample = MyArgs{ rest = def &= args  , debug = "no" &= opt "yes" }
         &= summary "my ghc custom preprocessor. Currently it calls m4."


m4cmd :: String -> String -> String -> [String] -> String
m4cmd src input output args 
--      = printf "m4 --synclines -P  --define=m4__file__='%s' --define=__file__='%s' '%s'  %s " src src input  (intercalate " " args)
      = printf "m4  --synclines --prefix-builtins  --define=m4__file__='%s' --define=__file__='%s' '%s'  %s " src src input  (intercalate " " args)


-- sed =  "sed -e 's/^#line \\+[0-9]\\+ *$//'"

sed :: [String] -> [String]
sed = map f
    where f s | s =~ "^#line +[0-9]+ *$" = "-- empty line"
              | otherwise = s


writeStringsToFile :: Bool -> FilePath -> [ String ] -> IO ()
writeStringsToFile debug file xs = 
    let s =  (unlines xs)
    in
      do writeFile file s
         when debug $ do (tmp,h) <- openTempFile "/tmp" "tmp.hs"
                         writeTmp s tmp h
                         hClose h

writeTmp :: String -> FilePath -> Handle -> IO ()
writeTmp s file h =
    do  print $ "temporary file = " ++ file
        hPutStr h s
                

main = do args <- cmdArgs sample
          process args

process :: MyArgs -> IO ()
process args =
    do   when isDebug $ do print $ "src=" ++ src
                           print $ "in =" ++ input
                           print $ "out=" ++ output
                           print $ restArg

         xs <- run (m4cmd src input output restArg)
         if not . null  $ output
         then writeStringsToFile isDebug output $ sed xs
         else mapM_ print xs

    where
        src     = replaceBS $ rest args !! 0
        input   = replaceBS $ rest args !! 1
        output  = replaceBS $ rest args !! 2
        restArg = drop 3 $ rest args
        isDebug = debug args /= "no"

replaceBS :: String -> String
replaceBS = map f
    where f '\\' = '/'
          f x = x
