{- True if the string contains balanced parens -}

import Test.HUnit
import Test.HUnit.Text

balanced :: String -> Bool
balanced = balanced' []

balanced' :: String -> String -> Bool
balanced' [] [] = True
balanced' _ [] = False
-- Handle '(' and ')'
balanced' ps ('(':xs) = balanced' ('(':ps) xs
balanced' ('(':ps) (')':xs) = balanced' ps xs
balanced' _ (')':xs) = False
-- Handle '[' and ']'
balanced' ps ('[':xs) = balanced' ('[':ps) xs
balanced' ('[':ps) (']':xs) = balanced' ps xs
balanced' _ (']':xs) = False
-- Handle '{' and '}'
balanced' ps ('{':xs) = balanced' ('{':ps) xs
balanced' ('{':ps) ('}':xs) = balanced' ps xs
balanced' _ ('}':xs) = False
balanced' ps xs = balanced' ps (tail xs)

tests :: Test
tests = TestList
      [ balanced "" ~?= True
      , balanced "(" ~?= False
      , balanced ")" ~?= False
      , balanced "()" ~?= True
      , balanced "[" ~?= False
      , balanced "]" ~?= False
      , balanced "[]" ~?= True
      , balanced "{" ~?= False
      , balanced "}" ~?= False
      , balanced "{}" ~?= True
      , balanced "(())" ~?= True
      , balanced "(()" ~?= False
      , balanced "())" ~?= False
      , balanced "{[()]}" ~?= True
      , balanced "(asdf)" ~?= True
      , balanced "[asdf]" ~?= True
      , balanced "{asdf}" ~?= True
      , balanced "()[]{}" ~?= True
      ]

main :: IO ()
main = runTestTT tests >> return ()
