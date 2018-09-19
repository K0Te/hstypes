-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad (forM)
import Data.Vector (empty, Vector(..), (!), fromList, length, (++), replicate, (//))
import System.IO.Unsafe
import Control.DeepSeq

data Instruction = Inc | Dec | MoveLeft | MoveRight | Read | Print | RightIfZero | LeftIfNonZero deriving (Show, Eq)
safeInc :: Char -> Char
safeInc '\255' = '\0'
safeInc x = succ x
safeDec :: Char -> Char
safeDec '\0' = '\255'
safeDec x = pred x

isSource :: Char -> Bool
isSource c = elem c "+-<>.,[]"

parseSource :: String -> [Instruction]
parseSource = fmap parseChar
    where parseChar '+' = Inc
          parseChar '-' = Dec
          parseChar '<' = MoveLeft
          parseChar '>' = MoveRight
          parseChar ',' = Read
          parseChar '.' = Print
          parseChar '[' = RightIfZero
          parseChar ']' = LeftIfNonZero

data State = State {datum :: !(Vector Char), instrPtr:: !Int, datPtr:: !Int, iterCnt:: !Int, out:: !String, input:: !String} deriving Show

safeAccess :: Vector Char -> Int -> Vector Char
safeAccess v i = v `seq` if Data.Vector.length v >= (i+10) then v else v Data.Vector.++ (Data.Vector.replicate (100+i-Data.Vector.length v) '\0')

doAt :: (Char->Char) -> Vector Char -> Int -> Vector Char
doAt f v i =  v `seq` sv // [(i, f val)]
    where sv = safeAccess v i
          val = sv ! i

indexMatching :: (Int->Int) -> Instruction -> Int -> Vector Instruction -> Int -> Int
indexMatching op RightIfZero index instrutions skip = if skip == 0 && instrutions ! index == RightIfZero then index
    else if instrutions ! index == RightIfZero then indexMatching op RightIfZero (op index) instrutions (skip-1)
    else if instrutions ! index == LeftIfNonZero then indexMatching op RightIfZero (op index) instrutions (skip+1)
    else indexMatching op RightIfZero (op index) instrutions skip
indexMatching op LeftIfNonZero index instrutions skip = if skip == 0 && instrutions ! index == LeftIfNonZero then index
    else if instrutions ! index == LeftIfNonZero then indexMatching op LeftIfNonZero (op index) instrutions (skip-1)
    else if instrutions ! index == RightIfZero then indexMatching op LeftIfNonZero (op index) instrutions (skip+1)
    else indexMatching op LeftIfNonZero (op index) instrutions skip

run :: Vector Instruction -> State -> String
run instr (State dat instrPtr datPtr iterCnt out input) = dat `deepseq` (Data.Vector.length $ dat) `seq`
    if instrPtr > (Data.Vector.length instr) - 1 then out
    else if iterCnt >= 10^5 then out Prelude.++ "\nPROCESS TIME OUT. KILLED!!!"
    else  newState `seq` run instr newState
    where cmd = instr ! instrPtr
          -- cmd = unsafePerformIO $ do print $ Data.Vector.length dat
          --                            return $ instr ! instrPtr
          newState = case cmd of
            Inc -> State (doAt safeInc dat datPtr) (instrPtr+1) datPtr (iterCnt+1) out input
            Dec -> State (doAt safeDec dat datPtr) (instrPtr+1) datPtr (iterCnt+1) out input
            MoveLeft -> State dat (instrPtr+1) (datPtr-1) (iterCnt+1) out input
            MoveRight -> State dat (instrPtr+1) (datPtr+1) (iterCnt+1) out input
            Read -> State (doAt (\_ -> head input) dat datPtr) (instrPtr+1) datPtr (iterCnt+1) out (tail input)
            Print -> State dat (instrPtr+1) datPtr (iterCnt+1) (out Prelude.++ [(safeAccess dat datPtr) ! datPtr]) input
            RightIfZero -> if ((safeAccess dat datPtr) ! datPtr) == '\0' then
                                State dat (indexMatching succ LeftIfNonZero (instrPtr+1) instr 0) datPtr (iterCnt+1) out input
                           else State dat (instrPtr+1) datPtr (iterCnt+1) out input
            LeftIfNonZero -> if ((safeAccess dat datPtr) ! datPtr) == '\0' then
                                State dat (instrPtr+1) datPtr (iterCnt+1) out input
                             else State dat (indexMatching pred RightIfZero (instrPtr-1) instr 0) datPtr (iterCnt+1) out input

main :: IO ()
main = do
    n_m <- getLine
    let (n, m) = tail <$> span (/=' ') n_m
    let mi = read m :: Int
    input <- (takeWhile (/= '$')) <$> getLine
    code <- ((filter isSource).concat) <$> forM [1..mi] (\_ -> getLine)
    let result = run (fromList $ parseSource code) (State (Data.Vector.replicate 5000 '\0') 0 2500 0 "" input)
    putStrLn result
