module Main where

import Data.Maybe
import Text.Read
import Debug.Trace

data Val = Op EOp | Integer Int deriving(Show)
data EOp = PLUS | MINUS | TIMES | DIVIDE deriving(Show)

-- String 2 Value functions

str2val "*" = Op TIMES
str2val "-" = Op MINUS
str2val "+" = Op PLUS
str2val "/" = Op DIVIDE
str2val s = case readMaybe s :: Maybe Int of 
        Just i -> Integer i

-- Get tokens from string

parseWords :: String -> String
parseWords s = show $ foldl evalStack [] (map str2val $ words s) 

evalStack :: [Val] -> Val -> [Val]
evalStack currStack nextTok = 
    -- Uncomment below to show a trace of the evaluation function.
    -- trace (show nextTok ++ " " ++ show currStack) $ 
    eval nextTok currStack

-- eval 
eval (Op PLUS) (Integer x : Integer y : rest) = Integer ( x + y ) : rest
eval (Op TIMES) (Integer x : Integer y : rest) = Integer ( x * y ) : rest
eval (Op MINUS) (Integer x : Integer y : rest) = Integer ( y - x ) : rest
eval (Op DIVIDE) (Integer x : Integer y : rest) = Integer ( quot y x ) : rest

-- Catch all
eval s x = s : x 

main = do
    interact parseWords