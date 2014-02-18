module Main where

import System.Random
import Graphics.UI.Gtk
import Sound.ALUT



-- Expect range C0 - B8, -57 - 50
type Note = Int


fA4 :: Frequency
fA4 = 440

-- fn = fA4 * (a)^n
-- a = 2 ^ (1/12)
freq :: Note -> Frequency
freq n = fA4 * (1.059463094359 ^^ n)


playTone :: Duration -> Frequency -> IO ()
playTone dur hz = do
   buf <- createBuffer (Sine hz 0 dur)
   [source] <- genObjectNames 1
   buffer source $= Just buf
   play [source]
   sleep 1

roll :: Random a => (a, a) -> IO a
roll = getStdRandom .randomR

rangeToInterval :: Int -> (Note, Note)
rangeToInterval range = (base, base+range)
  where base = -(range `div` 2)

rollNote :: Int -> IO Note
rollNote = roll . rangeToInterval

nameNote :: Note -> String
nameNote (-9) = "Dó"
nameNote (-7) = "Ré"
nameNote (-5) = "Mi"
nameNote (-4) = "Fá"
nameNote (-2) = "Sol"
nameNote 0 = "Lá"
nameNote 2 = "Si"
nameNote n | n > 2 = nameNote $ n - 12 
           | n < (-9) = nameNote $ n + 12
           | otherwise = nameNote (n-1) ++ " #"

octaveNote :: Integral a => a -> a
octaveNote n = (n+57) `div` 12

toNote :: Note -> String
toNote n = nameNote n ++ " " ++ show (octaveNote n)



data MITS = MITS { labeltext :: String, notenum :: Note, noteoptions :: [Maybe Note], noterange :: Int }



genoptions :: Note -> Int -> IO [Maybe Note]
genoptions note range
  | range <= 11 = do let (f,l) = rangeToInterval range
                         list = map Just [f..l]
                     return $ until ((==12).length) (++[Nothing]) list
  | otherwise = do base <- curry roll (note - 11) note
                   return $ map Just [base .. base + 11]



main :: IO ()
main = withProgNameAndArgs runALUT $ \_progName _args -> initGUI >> do
  mapM_ (playTone 0.1 . freq) [-9, -7, -5, -4, -2, 0, 2, 3]
  options <- genoptions 0 0
  loopy (MITS (toNote 0) 0 options 0)

loopy :: MITS -> IO ()
loopy mits = do
    builder <- builderNew
    builderAddFromFile builder "interface.glade"
    window <- builderGetObject builder castToWindow "window1"
    _ <- onDestroy window mainQuit
    
    l <- builderGetObject builder castToLabel "label1"
    labelSetText l $ labeltext mits
    
    rb <- builderGetObject builder castToButton "replay_button"
    _ <- onClicked rb $ playTone 0.5 . freq $ notenum mits
    
    nnb <- builderGetObject builder castToButton "new_note_button"
    _ <- onClicked nnb $ newNoteClick window mits
    
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
    setButtonNoteLabel b1 0 mits
    setButtonNoteLabel b2 1 mits
    setButtonNoteLabel b3 2 mits
    setButtonNoteLabel b4 3 mits
    setButtonNoteLabel b5 4 mits
    setButtonNoteLabel b6 5 mits
    setButtonNoteLabel b7 6 mits
    setButtonNoteLabel b8 7 mits
    setButtonNoteLabel b9 8 mits
    setButtonNoteLabel b10 9 mits
    setButtonNoteLabel b11 10 mits
    setButtonNoteLabel b12 11 mits
    setButtonNoteClick window b1 0 mits
    setButtonNoteClick window b2 1 mits
    setButtonNoteClick window b3 2 mits
    setButtonNoteClick window b4 3 mits
    setButtonNoteClick window b5 4 mits
    setButtonNoteClick window b6 5 mits
    setButtonNoteClick window b7 6 mits
    setButtonNoteClick window b8 7 mits
    setButtonNoteClick window b9 8 mits
    setButtonNoteClick window b10 9 mits
    setButtonNoteClick window b11 10 mits
    setButtonNoteClick window b12 11 mits
    
    widgetShowAll window
    
    playTone 0.2 . freq $ notenum mits
    
    mainGUI


setButtonNoteLabel :: Button -> Int -> MITS -> IO ()
setButtonNoteLabel button number mits = buttonSetLabel button $ labelNoteDesc (noteoptions mits !! number)

labelNoteDesc :: Maybe Note -> String
labelNoteDesc (Just n) = toNote n
labelNoteDesc Nothing = "X"

setButtonNoteClick :: Window -> Button -> Int -> MITS -> IO ()
setButtonNoteClick window button number mits = do
  _ <- onClicked button $ if (noteoptions mits !! number) == Just (notenum mits)
    then buttonClick window mits
    else buttonSetLabel button "X_X" -- >> playTone 0.2 (freq (notenum mits))
  return ()

buttonClick :: Window -> MITS -> IO ()
buttonClick window mits = do
    widgetDestroy window
    let range = noterange mits
    newnote <- rollNote range
    options <- genoptions newnote range
    loopy $ MITS (labeltext mits) newnote options range

describeRange :: Int -> String
describeRange range = let (f,l) = rangeToInterval range
                     in toNote f ++ " <-> " ++ toNote l

newNoteClick :: Window -> MITS -> IO ()
newNoteClick window mits = do
    widgetDestroy window
    let range = 1 + noterange mits
        newnote = notenum mits
    options <- genoptions newnote range
    loopy $ MITS (describeRange range) newnote options range
    