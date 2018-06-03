import Test.HUnit
import Data.Either

import Lambda (parseTerm, Term(..), Info(..))

main :: IO ()
main = do
  runTestTT tests
  return ()

test1 = TestCase (assertEqual "\\x.x" (Right $ Abs Info "x" (Var Info 0 1)) (parseTerm "\\x.x"))

tests = TestList [TestLabel "test1" test1]
