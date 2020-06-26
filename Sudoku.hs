{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Main where

import System.Environment
import Data.List
import Control.Monad.ST
import Data.Array.IO
import Data.Array
import Data.Array.ST
import Control.Monad
import Data.Either
import Data.STRef

type Value = Int
type Cell = (Int, Int)
type Options s = STArray s Cell (Either Value [Value])
type Solution = Array Cell Value

main :: IO ()
main = do str <- getArgs >>= readFile . head
          putStrLn "Solution:"
          printSolution (runST (createSol str))

createSol :: String -> ST s Solution
createSol str = do sud <- newArray ((0,0),(8,8)) (Right [1..9]) >>= initBoard str
                   ref <- newSTRef (array ((0,0),(8,8)) [((i,j) ,0) | i <- [0..8], j <- [0..8]])
                   solve sud ref
                   readSTRef ref


initBoard :: String -> Options s -> ST s (Options s)
initBoard str a = do sequence_ [putValue a (i,j) (read n) | (i,ls) <- zip [0..8] (lines str), (j,n) <- zip [0..8] (words ls)]
                     return a

printSolution :: Array Cell Value -> IO ()
printSolution a =
   let printLine x m n = concatMap (show . \y -> a !(x,y)) [m..n] in do
                     mapM_ (\x -> putStr (printLine x 0 2) >> putStr "|" >> putStr (printLine x 3 5) >> putStr "|" >> putStrLn (printLine x 6 8)) [0..2]
                     putStrLn "-----------"
                     mapM_ (\x -> putStr (printLine x 0 2) >> putStr "|" >> putStr (printLine x 3 5) >> putStr "|" >> putStrLn (printLine x 6 8))  [3..5]
                     putStrLn "-----------"
                     mapM_ (\x -> putStr (printLine x 0 2) >> putStr "|" >> putStr (printLine x 3 5) >> putStr "|" >> putStrLn (printLine x 6 8))  [6..8]

putValue :: Options s -> Cell -> Value -> ST s ()
putValue sud (i,j) val = when (val > 0) $ (do writeArray sud (i,j) (Left val)
                                              forM_ [0..8] (\y -> updateCell sud (i,y) val)
                                              forM_ [0..8] (\x -> updateCell sud (x,j) val)
                                              let i' = 3 * (i `quot` 3)
                                              let j' = 3 * (j `quot` 3)
                                              forM_ [0..8] (\x -> updateCell sud (i'+x`div`3 ,j'+x`mod`3) val))

updateCell :: Options s -> Cell -> Value -> ST s ()
updateCell sud cell val = readArray sud cell >>= either filled delOpt
                        where filled = const $ return ()
                              delOpt = writeArray sud cell . Right . delete val

findMin :: [(Cell,[Value])] -> Either Bool (Cell,[Value])
findMin [] = Left True
findMin ((_,[]):_) = Left False
findMin [(c,vs)] = Right (c, vs)
findMin ((c',vs'):(c,vs):xs) = if length vs' > length vs then findMin ((c,vs):xs) else findMin ((c',vs'):xs)

nextInput :: Options s -> ST s (Either Bool (Cell, [Value]))
nextInput = fmap (findMin . rights . map sequence) . getAssocs

-- Can this be improved?
-- At the moment:
-- - Try to find the next easiest position to try
-- - Try all options at that position, making copies of sudfor recursive calls
-- - If there aren't any positions left, write the current sud in ref and exit
solve :: Options s -> STRef s Solution -> ST s ()
solve sud ref = nextInput sud >>= either solved continue
               where solved b = when b (do reduce <- mapArray (fromLeft 0) sud
                                           convert <- freeze reduce
                                           writeSTRef ref convert)
                     continue (cell, vs) = forM_ vs (\v -> do sud' <- copySudoku sud
                                                              putValue sud' cell v
                                                              solve sud' ref)

copySudoku :: Options s -> ST s (Options s)
copySudoku a = do a' <- newArray ((0,0),(8,8)) (Right [1..9])
                  forM_ [0..80] (\n -> readArray a (n`div`9,n`mod`9) >>= writeArray a' (n`div`9,n`mod`9))
                  return a'
