module Main where

import Data.Char
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Error
import System.Environment

data Token = LeftParen | RightParen 
    | Comma 
    | Assign 
    | Plus 
    | Minus 
    | Asterisk 
    | Slash 
    | Colon 
    | Greater 
    | Less 
    | Name String 
    | StringLiteral String
    | NumberLiteral Int
    | Let
    | If
    | Then
    | Else
    | Goto
    deriving (Eq, Show)

punctuator :: String -> Maybe (String, Token)
punctuator (c:cs) = do 
    token  <- Map.lookup c signs
    return (cs, token)
        where signs = Map.fromList [ (',', Comma), ('=', Assign), ('+', Plus), ('-', Minus), 
                            ('*', Asterisk), ('/', Slash), (':', Colon), ('>', Greater), ('<', Less),
                            ('(', LeftParen), (')', RightParen)]

eatToken :: (Char -> Bool) -> String -> Maybe (String, String)
eatToken cond s = do
    (rest, rev) <- eatTokenAcc cond (Just (s, []))
    return (rest, (reverse rev))

eatTokenAcc :: (Char->Bool) -> Maybe(String, String) -> Maybe(String, String)
eatTokenAcc cond state = do
    (s, acc) <- state
    case s of 
        (c:cs) | cond c -> eatTokenAcc cond (Just (cs, c:acc))
        _ -> return (s, acc)

keywords = Map.fromList (map (\x -> ((map toLower $ show x), x)) [ If, Then, Else, Goto])

name :: String -> Maybe(String, Token)
name (c:cs) = 
    if isAlpha c 
        then do
            (rest, nm) <- eatToken (\c -> isDigit c || isAlpha c) (c:cs)
            case (Map.lookup nm keywords) of
                Just keyword -> return (rest, keyword)
                _ -> return (rest, (Name nm))
        else Nothing

number :: String -> Maybe (String, Token)
number (c:cs) = 
    if isDigit c 
        then do
             (rest, n) <- eatToken isDigit (c:cs) 
             return (rest, (NumberLiteral (read n)))
        else Nothing

string (c:cs) = 
    if c == '"' 
        then do 
            ((r:rs), s) <- eatToken (/= '"') (cs)
            return (rs, (StringLiteral s))
        else Nothing


lexer :: String -> (Either String [Token])
lexer s = do
        tokens <- lexAcc (lstrip s) (Right [])
        return (reverse tokens)

lexAcc [] r = r
lexAcc s r = do
    acc <- r
    (rest, token) <- (msum $ map (\f -> f s) [punctuator, number, name, string]) `orError`
                                    ("Unknown token " ++ (show $ head s))
    lexAcc (lstrip rest) (Right (token:acc))

lstrip = dropWhile isSpace
maybeToEither Nothing s = Left s
maybeToEither (Just r) _ = Right r
orError = maybeToEither

eitherToMaybe (Right r) = Just r
eitherToMaybe (Left _) = Nothing

data Expr = Var String | StrLit String | NumLit Int | Prefix Token Expr | Binary Token Expr Expr
                deriving (Eq, Show)

data Statement = LabelStmt String | AssignmentStmt String Expr 
                | CondStmt Expr Statement Statement | GotoStmt String | CallStmt String [Expr]
                deriving (Eq, Show)

precedence token = 
    case token of
        Plus -> 2
        Minus -> 2
        Asterisk -> 3
        Slash -> 3
        Greater -> 4
        Less -> 4
        Assign -> 4

labelStmt:: [Token]->Either String ([Token], Statement)
labelStmt tokens = do
--    tokens <- state
    (ts1, name) <- matchName tokens
    (ts2, _) <- match Colon ts1
    return (ts2, (LabelStmt name))

assgnStmt tokens  = do
--    tokens <- state
    (ts1, name) <- matchName tokens
    (ts2, _) <- match Assign ts1
    (ts3, e) <- matchExpression 0 ts2
    return (ts3, (AssignmentStmt name e))

condStmt tokens = do
--    tokens <- state
    (ts0, _) <- match If tokens
    (ts1, e) <- matchExpression 0 ts0
    (ts2, _) <- match Then ts1
    (ts3, onThen) <- statements ts2
    (ts4, _) <- match Else ts3
    (ts5, onElse) <- statements ts4
    return (ts5, (CondStmt e onThen onElse))

gotoStmt tokens = do
    (ts1, e) <- match Goto tokens
    (ts2, nm) <- matchName ts1
    return (ts2, GotoStmt nm)

callStmt tokens = do
    (ts, name) <- matchName tokens
    (ts, _) <- match LeftParen ts
    (ts, args) <- matchArgs [] ts
    (ts, _) <- match RightParen ts
    return (ts, CallStmt name args)

matchArgs :: [Expr] -> [Token] -> Either String ([Token], [Expr])            
matchArgs acc ts = do
        case (matchExpression 0 ts) of
            Left _ -> do
                        (ts, _) <- match RightParen ts
                        return (RightParen:ts, reverse acc)
            Right (ts, e) -> 
                case ts of
                    (Comma:rest) -> matchArgs (e:acc) rest
                    _ -> return (ts, reverse (e:acc))

    

matchExpression::Int ->[Token] ->  Either String ([Token], Expr)
matchExpression prec tokens = 
    let rightSide left p tokens =
            case (tokens) of
                (t:ts) | isSign t ->
                    if precedence t > p then do
                        (ts, right) <- matchExpression (precedence t) ts
                        rightSide (Binary t left right) p ts
                    else do
                        return (tokens, left)
                _ -> return (tokens, left) in
    do
        (tokens, left) <- leftSide tokens
        rightSide left prec tokens

isSign token = any (== token) [Plus, Minus, Asterisk, Slash, Greater, Less, Assign]

leftSide :: [Token] -> Either String ([Token], Expr)
leftSide tokens = do
    case tokens of 
        [] -> Left "Expected expression"
        (Name n):ts -> return (ts, Var n)
        (NumberLiteral n):ts -> return (ts, NumLit n)
        (StringLiteral s):ts -> return (ts, StrLit s)
        (LeftParen):ts -> do
            (ts2, subexpr) <- matchExpression 0 ts
            (ts3, _) <- match RightParen ts2
            return (ts3, subexpr)
        t:ts | isSign t -> do
            (ts2, e) <- leftSide ts
            return (ts2, Prefix t e) -- it's bad but will work for a while
        _ -> Left "Something wrong"
                


matchName :: [Token] -> Either String ([Token],String)
matchName tokens = do
    case tokens of
        ((Name n):ts) -> return (ts, n)
        _ -> Left "Expected name"

match :: Token -> [Token] -> Either String ([Token], Token)
match token tokens = do
    case tokens of
        (t:ts) | t == token -> return (ts, t)
        _ -> Left ("Expected token " ++ (show token) ++" not found")

statements :: [Token] -> Either String ([Token], Statement)
statements s = (msum $ map (\f -> f s) [labelStmt, assgnStmt, condStmt, gotoStmt, callStmt])

parser :: Either String [Token] -> Either String [Statement]
parser tokens = do
    ts <- tokens
    stmt <- parseAcc ts (Right [])
    return (reverse stmt)

parseAcc :: [Token] -> Either String [Statement] -> Either String [Statement]
parseAcc [] r = r
parseAcc ts r = do
    acc <- r
    (ts2, stmt) <- statements ts
    parseAcc ts2 (Right (stmt:acc))

labels stmts = Map.fromList $ catMaybes $ map labelName (zip stmts [0..])
    where labelName (s, n) = 
            case s of
                LabelStmt name -> Just (name, n)
                _ -> Nothing
interpreter :: [Statement] -> IO()
interpreter stmts = execute stmts (labels stmts) (Map.fromList[]) 0

execute :: [Statement] -> (Map.Map String Int) -> (Map.Map String Expr) -> Int -> IO()
execute statements labels vars pc = do
        if pc >= (length statements) then return ()
            else let handleError r = either putStrLn id r
                     handleNothing msg r = fromMaybe (do { putStrLn msg; return ()}) r
                     executeStatement stmt pc = case stmt of
                        GotoStmt name -> handleNothing  ("Unknown label :"++name) $
                                            do 
                                                newPc <- Map.lookup name labels
                                                return (execute statements labels vars newPc)
                        AssignmentStmt varName expr -> handleError (do
                                                                        e <- evalExpression vars expr
                                                                        return (execute statements labels (Map.insert varName e vars) (pc +1)))
                        CondStmt expr thenArm elseArm -> handleError $ do 
                                                                        NumLit(n) <- evalExpression vars expr
                                                                        
                                                                        let next = (if (n == 1) then thenArm
                                                                                                else elseArm) in
                                                                            return $executeStatement next (pc+1)
                        CallStmt name args -> handleError $ 
                                                    do 
                                                        args <- sequence $ map (\e -> evalExpression vars e) args
                                                        r <- evalCall name args
                                                        return $ do {r; execute statements labels vars (pc+1)}
                        _ -> execute statements labels vars (pc+1) in
                        executeStatement (statements!!pc) pc

impPrint :: [Expr] -> Either String (IO())
impPrint args = do
    argContents <- sequence $ map showContents args
    return (putStrLn (concat $ map id argContents))
        where showContents arg = do 
                case arg of 
                    NumLit n -> return (show n)
                    StrLit s -> return s
                    _ -> Left ("Argument evaluated to "++ (show arg))

procs :: Map.Map String ([Expr] -> Either String (IO()))
procs = Map.fromList [("print", impPrint)]

evalCall name args = do
    proc <- (Map.lookup name procs) `orError` ("Unknown procedure: "++name)
    proc args
    

liftIntIntInt :: (Int->Int->Int) -> (Expr->Expr-> Either String Expr)
liftIntIntInt f = 
    \a1 a2 -> do
        case a1 of
            NumLit n1 -> case a2 of 
                            NumLit n2 -> return $ NumLit(f n1 n2)
                            _ -> Left ("Expected Int but got "++(show a2))
            _ -> Left ("Expected Int but got "++(show a1))

evalExpression :: (Map.Map String Expr) -> Expr -> Either String Expr
evalExpression vars e = do
    case e of
        NumLit n -> return e
        StrLit s -> return e
        Var name -> (Map.lookup name vars) `orError` ("Unknown variable: "++ (show name))
        Binary t l r -> do
            left <- evalExpression vars l
            right <- evalExpression vars r
            let op = case t of
                        Plus -> liftIntIntInt (+)
                        Minus -> liftIntIntInt (-)
                        Asterisk -> liftIntIntInt (*)
                        Slash -> liftIntIntInt div
                        Greater -> liftIntIntInt (\a b -> if a > b then 1 else 0)
                        Less -> liftIntIntInt (\a b -> if a < b then 1 else 0)
                        Assign -> liftIntIntInt (\a b -> if a == b then 1 else 0)
                        _ -> \a b -> Left ("Unexpected operator: "++(show t)) in
                            op left right
        _ -> Left ("Unexpected expression " ++ (show e))


main = do
    args <- getArgs
    if length args /= 1 then putStrLn "Usage: bzik <filename>"
    else do
        src <- readFile (args !!0)
        let stmts = (parser . lexer) src in
            either putStrLn interpreter stmts
        return ()
