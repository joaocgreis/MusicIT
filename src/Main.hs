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


getSource :: Duration -> Frequency -> IO Source
getSource dur hz = do
   buf <- createBuffer (Sine hz 0 dur)
   [source] <- genObjectNames 1
   buffer source $= Just buf
   return source

playTone :: Duration -> Frequency -> IO ()
playTone dur hz = do
   source <- getSource dur hz
   play [source]
   sleep dur

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
  mapM_ (playTone 0.2 . freq) [-9, -7, -5, -4, -2, 0, 2, 3]
  sleep 1
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
    
    mapM_ (\x -> do b <- builderGetObject builder castToButton ("button" ++ show (x + 1))
                    setButtonNoteLabel b x mits
                    setButtonNoteClick window b x mits) [0..11]
    
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
    