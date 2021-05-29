
{-# LANGUAGE  BlockArguments #-}

import System.MIDI
    ( close,
      enumerateDestinations,
      getName,
      openDestination,
      send,
      start,
      stop,
      Connection,
      Destination,
      MidiMessage(MidiMessage),
      MidiMessage'(NoteOn) )
import System.IO ( hSetBuffering, stdout, BufferMode(LineBuffering) )
import System.Environment ( getArgs )
import Safe (readMay)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  destinations <- enumerateDestinations
  names        <- mapM getName destinations
  let pairs = zip names destinations
  mapM_ print pairs
  getArgs >>= setup pairs

setup :: [(String, Destination)] -> [String] -> IO ()
setup [] _                     = putStrLn "No MIDI destinations found."
setup _  a | "--help" `elem` a = putStrLn "Provide no arguments to pick first MIDI source, or specify a name."
setup _  (_:_:_)               = putStrLn "Provide zero or one destination name as an argument."
setup destinations []          = run (snd $ head destinations)
setup destinations [n]         = do
  case lookup n destinations of
    Nothing -> putStrLn $ "Destination " <> n <> " couldn't be found"
    Just d -> run d

run :: Destination -> IO ()
run choice = do
  conn <- openDestination choice
  start conn
  mapM_ (event conn) =<< (lines <$> getContents)
  mapM_ (send conn . message 0) [0..64]
  stop conn
  close conn

event :: Connection -> String -> IO ()
event d l = do
  print [0..8]
  case map readMay (words l) of
    [Just x, Just y, Just v] -> change d x y v
    x -> putStrLn $ "Couldn't decode line: [" <> l <> "]. Expected format [x y v]. Example: [2 2 127]"

change :: Connection -> Int -> Int -> Int -> IO ()
change c x y v = do
  let z = y * 16 + x
  mapM_ (send c . message 127) [z]
  -- mapM_ (send c . message 127) [16..23]

message :: Int -> Int -> MidiMessage
message v p = MidiMessage 1 (NoteOn p v)
