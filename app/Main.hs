{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brainhack.Evaluator
import Brainhack.Parser
import Brainhack.Parser.Items
import Control.Exception.Safe (SomeException)
import Control.Monad (void)
import Data.String (IsString)
import Data.Text (Text)
import System.Environment (getArgs)
import qualified Data.Text as T

newtype AiNoUta = AiNoUta
  { unAiNoUta :: Text
  } deriving (Eq, IsString)

instance BrainfuckToken AiNoUta where
  forwardToken   = "楽器を握るんじゃなくて、君の手を握りたいけど、"
  backwordToken  = "こうやって音楽を奏でて、君に言葉を伝えるその術しか持ってないから僕は君のために、"
  incrToken      = "でも、あの、"
  decrToken      = "その、だから、"
  outputToken    = "歌うもぼ、僕のために歌いたいんです！"
  inputToken     = "だけれども、僕はもう、"
  loopBeginToken = "僕はね、もう音楽なんかどうでも良くて、"
  loopEndToken   = "君のことが好きなんやけど、"
  toText         = unAiNoUta
  fromText       = AiNoUta
  toOperator     = flip lookup [ ("楽器を握るんじゃなくて、君の手を握りたいけど、", ForwardOp)
                               , ("こうやって音楽を奏でて、君に言葉を伝えるその術しか持ってないから僕は君のために、", BackwardOp)
                               , ("でも、あの、", IncrOp)
                               , ("その、だから、", DecrOp)
                               , ("歌うもぼ、僕のために歌いたいんです！", OutputOp)
                               , ("だけれども、僕はもう、", InputOp)
                               , ("僕はね、もう音楽なんかどうでも良くて、", LoopBeginOp)
                               , ("君のことが好きなんやけど、", LoopEndOp)
                               ]

main :: IO ()
main = do
  file <- head <$> getArgs
  code <- AiNoUta . T.pack <$> readFile file
  case parse code of
    Left  e -> error $ "GAME OVER: " ++ show (e :: SomeException)
    Right a -> void . flip runBrainState emptyMachine $ eval a
