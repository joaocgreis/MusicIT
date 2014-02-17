module Main where

import System.Random
import Graphics.UI.Gtk
import Sound.ALUT



fA4 :: Frequency
fA4 = 440

-- fn = fA4 * (a)^n
-- a = 2 ^ (1/12)
freq :: Integral b => b -> Frequency
freq n = fA4 * (1.059463094359 ^^ n)


playTone :: Duration -> Frequency -> IO ()
playTone dur hz = do
   buf <- createBuffer (Sine hz 0 dur)
   [source] <- genObjectNames 1
   buffer source $= Just buf
   play [source]
   sleep 1

rollNote :: IO Int
rollNote = getStdRandom (randomR (-9,2))

nameNote :: Integral a => a -> String
nameNote (-9) = "Dó"
nameNote (-8) = "Dó #"
nameNote (-7) = "Ré"
nameNote (-6) = "Ré #"
nameNote (-5) = "Mi"
nameNote (-4) = "Fá"
nameNote (-3) = "Fá #"
nameNote (-2) = "Sol"
nameNote (-1) = "Sol #"
nameNote 0 = "Lá"
nameNote 1 = "Lá #"
nameNote 2 = "Si"
nameNote n | n > 2 = nameNote $ n - 12 
           | otherwise = nameNote $ n + 12

octaveNote :: Integral a => a -> a
octaveNote n = (n+57) `div` 12

toNote :: (Integral a, Show a) => a -> String
toNote n = (nameNote n) ++ " " ++ (show (octaveNote n))


main :: IO ()
main = withProgNameAndArgs runALUT alutmain

alutmain :: String -> [String] -> IO ()
alutmain _progName _args = initGUI >> do
    loopy "Welcome to MusicIT! Guess the note ;)"

loopy :: String -> IO ()
loopy labeltext = do
    builder <- builderNew
    builderAddFromFile builder "interface.glade"
    window <- builderGetObject builder castToWindow "window1"
    _ <- onDestroy window mainQuit
    
    l <- builderGetObject builder castToLabel "label1"
    labelSetText l labeltext
    
    note <- rollNote
    
    b1 <- builderGetObject builder castToButton "button1"
    b2 <- builderGetObject builder castToButton "button2"
    b3 <- builderGetObject builder castToButton "button3"
    b4 <- builderGetObject builder castToButton "button4"
    b5 <- builderGetObject builder castToButton "button5"
    b6 <- builderGetObject builder castToButton "button6"
    b7 <- builderGetObject builder castToButton "button7"
    b8 <- builderGetObject builder castToButton "button8"
    b9 <- builderGetObject builder castToButton "button9"
    b10 <- builderGetObject builder castToButton "button10"
    b11 <- builderGetObject builder castToButton "button11"
    b12 <- builderGetObject builder castToButton "button12"
    buttonSetLabel b1 $ toNote (-9)
    buttonSetLabel b2 $ toNote (-8)
    buttonSetLabel b3 $ toNote (-7)
    buttonSetLabel b4 $ toNote (-6)
    buttonSetLabel b5 $ toNote (-5)
    buttonSetLabel b6 $ toNote (-4)
    buttonSetLabel b7 $ toNote (-3)
    buttonSetLabel b8 $ toNote (-2)
    buttonSetLabel b9 $ toNote (-1)
    buttonSetLabel b10 $ toNote 0
    buttonSetLabel b11 $ toNote 1
    buttonSetLabel b12 $ toNote 2
    _ <- onClicked b1 $ buttonClick window (-9) note 
    _ <- onClicked b2 $ buttonClick window (-8) note 
    _ <- onClicked b3 $ buttonClick window (-7) note 
    _ <- onClicked b4 $ buttonClick window (-6) note 
    _ <- onClicked b5 $ buttonClick window (-5) note 
    _ <- onClicked b6 $ buttonClick window (-4) note 
    _ <- onClicked b7 $ buttonClick window (-3) note 
    _ <- onClicked b8 $ buttonClick window (-2) note 
    _ <- onClicked b9 $ buttonClick window (-1) note 
    _ <- onClicked b10 $ buttonClick window 0 note 
    _ <- onClicked b11 $ buttonClick window 1 note 
    _ <- onClicked b12 $ buttonClick window 2 note 
    
    widgetShowAll window
    
    playTone 0.5 . freq $ note
    
    mainGUI

buttonClick :: (Integral a, Show a) => Window -> a -> a -> IO ()
buttonClick window note expected = do
    widgetDestroy window
    if note == expected then loopy $ "That is correct! It was "++(toNote expected)++". Now try this one ;)"
                        else loopy $ "Wrong... It was "++(toNote expected)++". Better luck with this one!" 

