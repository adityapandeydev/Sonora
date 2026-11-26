module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import MusicSpec (musicTests)
import SignalSpec (signalTests)
import ParserSpec (parserTests)
import DeterminismSpec (determinismTests)
import AudioSpec (audioTests)
import ServiceSpec (serviceTests)
import ProjectSpec (projectTests)
import SessionSpec (sessionTests)
import LayoutSpec (layoutTests)
import GenerateSpec (generateTests)
import AuditionSpec (auditionTests)
import MixSpec (mixTests)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering  -- show how far a run got even if it dies
    putStrLn "========================================================"
    putStrLn "   SONORA FUNCTIONAL SYNTHESIS & DSP ENGINE TEST SUITE  "
    putStrLn "========================================================"
    p1 <- musicTests
    putStrLn ""
    p2 <- signalTests
    putStrLn ""
    p3 <- parserTests
    putStrLn ""
    p4 <- determinismTests
    putStrLn ""
    p5 <- audioTests
    putStrLn ""
    p6 <- serviceTests
    putStrLn ""
    p7 <- projectTests
    putStrLn ""
    p8 <- sessionTests
    putStrLn ""
    p9 <- layoutTests
    p10 <- generateTests
    p11 <- auditionTests
    p12 <- mixTests
    putStrLn "========================================================"
    if p1 && p2 && p3 && p4 && p5 && p6 && p7 && p8 && p9 && p10 && p11 && p12
        then do
            putStrLn "  ALL TEST SUITES PASSED SUCCESSFULLY! (100%)"
            putStrLn "========================================================"
            exitSuccess
        else do
            putStrLn "  SOME TESTS FAILED!"
            putStrLn "========================================================"
            exitFailure
