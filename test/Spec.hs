module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import MusicSpec (musicTests)
import SignalSpec (signalTests)
import ParserSpec (parserTests)
import DeterminismSpec (determinismTests)
import AudioSpec (audioTests)
import ServiceSpec (serviceTests)
import ProjectSpec (projectTests)

main :: IO ()
main = do
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
    putStrLn "========================================================"
    if p1 && p2 && p3 && p4 && p5 && p6 && p7
        then do
            putStrLn "  ALL TEST SUITES PASSED SUCCESSFULLY! (100%)"
            putStrLn "========================================================"
            exitSuccess
        else do
            putStrLn "  SOME TESTS FAILED!"
            putStrLn "========================================================"
            exitFailure
