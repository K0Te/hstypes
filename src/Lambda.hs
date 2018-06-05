module Lambda (Term(..), showTerm, Binding(..), Info(..), reval, termParser, parseApp, parseVar, parseTerm) where

import Data.List(elemIndex)
import Data.Maybe
import Debug.Trace
import Text.Parsec
-- import Text.ParserCombinators.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char


data Info = Info deriving (Show, Eq)
data Term = Var Info Int Int |
            Abs Info String Term |
            App Info Term Term deriving (Show, Eq)

data Binding = Binding deriving Show
type Context = [(String, Binding)]

type VarName = String
type TermParser = Parsec String [VarName] Term

parseTerm = runParser termParser []  "<input>"

termParser :: TermParser
termParser = parseApp <|> parseAbs <|> parseVar <|> parenTerm

parenTerm :: TermParser
parenTerm = try $ do
  skipMany space
  char '('
  skipMany space
  term <- termParser
  skipMany space
  char ')'
  skipMany space
  return term

parseApp :: TermParser
parseApp = try $ do
      (term:terms) <- (parseAbs <|> parseVar <|> parenTerm) `sepBy1` space
      return $ foldl (\acc el -> App Info acc el) term terms

parseAbs :: TermParser
parseAbs = do
  char '\\'
  var_name <- many alphaNum
  modifyState (\s -> var_name:s)
  char '.'
  term <- termParser
  modifyState (\(x:xs) -> xs)
  return $ Abs Info var_name term

parseVar :: TermParser
parseVar = do
  var_name <- many1 lower
  state <- getState
  let var_index = elemIndex var_name state
  maybe
    (unexpected $ "No var found: " ++ var_name)
    (\var_index -> return $ Var Info var_index (length state))
    var_index

index_to_name :: Info -> Context -> Int -> String
index_to_name _ ctx n = fst $ ctx !! n

ctx_len :: Context -> Int
ctx_len = length

is_name_bound :: String -> Context -> Bool
is_name_bound _ [] = False
is_name_bound x ((n, b):ns) = if n == x then True else is_name_bound x ns

pick_name :: Context -> String -> (Context, String)
pick_name ctx name = if is_name_bound name ctx
                     then pick_name ctx (name++"'")
                     else ((name, Binding):ctx, name)

showTerm :: Context -> Term -> String
showTerm context (Var info x n) = if ctx_len context == n
                                  then index_to_name info context x
                                  else "[bad index]"
showTerm context (Abs info name term) =
  let (context2, name2) = pick_name context name
  in "(labmda " ++ name2 ++ ". " ++ showTerm context2 term ++ ")"
showTerm context (App info t1 t2) =
  "(" ++ showTerm context t1 ++ " " ++ showTerm context t2 ++ ")"

termShift :: Int -> Term -> Term
termShift d term = walk 0 term where
  walk c (Var info x n) = if x >= c
                          then Var info (x+d) (n+d)
                          else Var info x (n+d)
  walk c (Abs info name term) = Abs info name (walk (c+1) term)
  walk c (App info t1 t2) = App info (walk c t1) (walk c t2)

termSubstitute :: Int -> Term -> Term -> Term
termSubstitute from to term = walk 0 term where
  walk c (Var info x n) = if x == from+c
                          then termShift c to
                          else Var info x n
  walk c (Abs info name term) = Abs info name (walk (c+1) term)
  walk c (App info t1 t2) = App info (walk c t1) (walk c t2)

termSubstituteTop :: Term -> Term -> Term
termSubstituteTop s term = termShift (-1) (termSubstitute 0 (termShift 1 s) term)

isval :: Term -> Bool
-- Why Var is not a value ???
isval (Abs _ _ _) = True
isval _ = False

eval :: Context -> Term -> Term
eval context (Var info x n) = Var info x n -- noop
eval context (Abs info name term) = Abs info name term -- noop ?
eval context (App info (Abs info2 name term) v2@(Abs _ _ _)) = termSubstituteTop v2 term
eval context (App info v1@(Abs info2 name term) t2) = let t2' = eval context t2 in App info v1 t2'
eval context (App info t1 t2) = let t1' = eval context t1 in App info t1' t2

reval :: Context -> Term -> IO Term
reval c t = let step = eval c t in if step == t
                                   then putStrLn (showTerm c t ++ " -> " ++ showTerm c step) >> return step
                                   else putStrLn (showTerm c  t ++ " -> " ++ showTerm c step) >> reval c step
-- let rec eval1 ctx t = match t with
    -- TmApp(fi,TmAbs(_,x,t12),v2) when isval ctx v2 ->
  --     termSubstTop v2 t12
  -- | TmApp(fi,v1,t2) when isval ctx v1 ->
  --     let t2' = eval1 ctx t2 in
  --     TmApp(fi, v1, t2')
  -- | TmApp(fi,t1,t2) ->
  --     let t1' = eval1 ctx t1 in
  --     TmApp(fi, t1', t2)
  -- | _ -> 
  --     raise NoRuleApplies






