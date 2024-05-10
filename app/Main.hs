module Main (main) where

import Lib
import System.IO 
import Data.Time

-- 画面をクリアする関数
cls :: IO ()
cls = putStr "\ESC[2J\ESC[H"

-- カーソルを指定の位置 (row, col) に移動するエスケープシーケンスを出力する
moveCursor :: Int -> Int -> IO ()
moveCursor row col = putStr $ "\ESC[" ++ show row ++ ";" ++ show col ++ "H"

-- 指定の位置に文字列を表示する
printAt :: Int -> Int -> String -> IO ()
printAt row col str = do
    moveCursor row col
    putStr str

-- 文字列の先頭から指定された数の文字を灰色にする
colorPrefixGray :: Int -> String -> String
colorPrefixGray n str =
    let gray = "\ESC[90m"   -- 灰色の開始エスケープシーケンス
        reset = "\ESC[0m"   -- 色をリセットするエスケープシーケンス
        (prefix, suffix) = splitAt n str
    in gray ++ prefix ++ reset ++ suffix

loop :: String -> IO Int
loop s = loop' s 1 0  
  where
    loop' :: String -> Int -> Int -> IO Int
    loop' text pos miss = do
      cls
      if pos == (1 + length text)
        then return miss
        else do
        
        printAt 2 2 $ colorPrefixGray (pos - 1) text
        printAt 3 (pos + 1) "^"
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False
     
        putStrLn ""
     
        char <- getChar
     
        if char == text !! (pos - 1)
          then loop' text (pos + 1) miss
          else loop' text pos (miss + 1)

main :: IO ()
main = do
  example <- lines <$> readFile "./example.txt"
  startTime <- getCurrentTime
  results <- mapM loop example
  endTime <- getCurrentTime
 
  let 
    time = realToFrac $ diffUTCTime endTime startTime
    missSum = sum results
  putStrLn $ "タイムは" ++ show time ++ "秒"
  putStrLn $ "ミスタイプは" ++ show (missSum) ++ "回"
