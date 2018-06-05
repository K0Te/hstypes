import           Test.HUnit
import           Data.Either
import           System.Exit

import           Lambda                         ( parseTerm
                                                , Term(..)
                                                , Info(..)
                                                )

main :: IO ()
main = do
  cnt <- runTestTT tests
  let failCount = (errors cnt) + (failures cnt)
      exitCode  = if failCount == 0 then ExitSuccess else ExitFailure 1
  exitWith exitCode

test1 = TestCase
  (assertEqual "\\x.x" (Right $ Abs Info "x" (Var Info 0 1)) (parseTerm "\\x.x")
  )
test2 = TestCase
  (assertEqual " ( \\x.( x ) ) "
               (Right $ Abs Info "x" (Var Info 0 1))
               (parseTerm " ( \\x.( x ) ) ")
  )
test3 = TestCase
  (assertEqual "(\\x.(x))"
               (Right $ Abs Info "x" (Var Info 0 1))
               (parseTerm "(\\x.(x))")
  )

test4 = TestCase
  (assertEqual "\\x.x x"
               (Right $ Abs Info "x" $ App Info (Var Info 0 1) (Var Info 0 1))
               (parseTerm "\\x.x x")
  )

test5 = TestCase
  (assertEqual
    "\\x.x x x"
    (Right $ Abs Info "x" $ App Info
                                (App Info (Var Info 0 1) (Var Info 0 1))
                                (Var Info 0 1)
    )
    (parseTerm "\\x.x x x")
  )

test6 = TestCase
  (assertEqual
    "\\x.x x x x"
    (Right $ Abs Info "x" $ App
      Info
      (App Info (App Info (Var Info 0 1) (Var Info 0 1)) (Var Info 0 1))
      (Var Info 0 1)
    )
    (parseTerm "\\x.x x x x")
  )

tests = TestList
  [ TestLabel "parse simple abstraction"     test1
  , TestLabel "abstraction, paren + spaces"  test2
  , TestLabel "abstraction, paren"           test3
  , TestLabel "abstraction + apply"          test4
  , TestLabel "apply associated to the left" test5
  , TestLabel "apply associated to the left" test6
  ]
