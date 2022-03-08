module Main (main) where

import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just ":q" -> return ()
        Just input -> do
          res <- do
            let parsed = parse input
            case parsed of
              (Left e) -> return $ show e
              (Right hiExpr) -> do
                evaluated <- eval hiExpr
                return $ case evaluated of
                  (Left err) -> show err
                  (Right val) -> show $ prettyValue val
          outputStrLn res
          loop
