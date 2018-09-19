module Main where

data Token = Num Int | Plus | X | Mult | Div | Expn | PO | PC | Exp | Minus deriving (Eq, Show)
data Expr = Val Int | XX | OP Token Expr Expr deriving (Eq, Show)

isNum c = filter (==c) ['0'..'9'] /= []
getNum s = (num, ret)
  where
  numS =  takeWhile isNum s
  num = (read numS) :: Int
  ret = snd $ splitAt (length numS) s

tokenize :: String -> [Token]
tokenize "" = []
tokenize (' ':cs) = tokenize cs
tokenize ('+':cs) = Plus:(tokenize cs)
tokenize ('-':cs) = Minus:(tokenize cs)
tokenize ('*':cs) = Mult:(tokenize cs)
tokenize ('/':cs) = Div:(tokenize cs)
tokenize ('^':cs) = Expn:(tokenize cs)
tokenize ('(':cs) = PO:(tokenize cs)
tokenize (')':cs) = PC:(tokenize cs)
tokenize ('x':cs) = X:(tokenize cs)
tokenize s@(c:cs) = if isNum c then let (n, cs) = getNum s in (Num n):(tokenize cs) else []

splitAtOp :: Token -> [Token] -> Maybe ([Token], [Token])
splitAtOp tk tokens = splint 0 0 tokens
  where
    splint :: Int -> Int -> [Token] -> Maybe ([Token], [Token])
    splint _ _ [] = Nothing
    splint n depth (t:ts) = if t == tk && depth == 0 && n>0 then (fmap.fmap) tail $ Just $ splitAt n tokens else
                            if t == PO then splint (n+1) (depth+1) ts else
                            if t == PC then splint (n+1) (depth-1) ts else
                            splint (n+1) depth ts

isJust Nothing = False
isJust _ = True

operations = [Plus, Minus, Mult, Div, Expn]
parse :: [Token] -> Expr
parse tokens = if results == [] then parseSimple tokens else let (op, Just (left, right)) = head results in OP op (parse left) (parse right)
  where
    splits = fmap (\op -> (op, splitAtOp op tokens)) operations
    results = filter (\(op, res) -> isJust res) splits

parseSimple :: [Token] -> Expr
parseSimple [Num x] = Val x
parseSimple [Num x, X] = OP Mult XX (Val x)
parseSimple [X, Num x] = OP Mult XX (Val x)
parseSimple [X] = XX
parseSimple (Minus:ts) = OP Minus (Val 0) (parse ts) -- unary minus
parseSimple (PO:ts) = parse $ reverse $ tail $ reverse ts -- assuming ) at end

eval :: Expr -> (Int, Int, Int, Int, Int, Int)
eval (Val n) = (0, 0, 0, 0, 0, n)
eval (XX) = (0, 0, 0, 0, 1, 0)
eval (OP Plus left right) = (x5+y5, x4+y4, x3+y3, x2+y2, x1+y1, x0+y0)
  where (x5, x4, x3, x2, x1, x0) = eval left
        (y5, y4, y3, y2, y1, y0) = eval right
eval (OP Minus left right) = (x5-y5, x4-y4, x3-y3, x2-y2, x1-y1, x0-y0)
  where (x5, x4, x3, x2, x1, x0) = eval left
        (y5, y4, y3, y2, y1, y0) = eval right
eval (OP Mult left right) = (0, 0, 0, 0, x1*y0+y1*x0, x0*y0)
  where (x5, x4, x3, x2, x1, x0) = eval left
        (y5, y4, y3, y2, y1, y0) = eval right

main = putStrLn $ show $ (parse . tokenize) "2+2"
