
{-# LANGUAGE  BlockArguments #-}



{-


Remain alive:

select l1.x, l1.y, count(*) from lights l1, lights as l2
where
  (l2.x <= l1.x + 1 and l2.x >= l1.x - 1)
  and
  (l2.y <= l1.y + 1 and l2.y >= l1.y - 1)
  and
  (l2.x <> l1.x or l2.y <> l1.y)

group by l1.x, l1.y
having count(*) >= 2 and count(*) <= 3


Come Alive:

-- create type pair AS (a int, b int);

select c, (a::text::pair).a as x, (a::text::pair).b as y from
(
    select count(*) as c,
        unnest(ARRAY[
            (x-1, y-1), (x-1, y), (x-1, y+1),
            (x,   y-1),           (x,   y+1),
            (x+1, y-1), (x+1, y), (x+1, y+1)
        ]) as a
    from lights
    group by a
) as foo
where a not in (select (x,y) from lights)
and c = 3

Unioned:

-- create type pair AS (a int, b int);

select c, (a::text::pair).a as x, (a::text::pair).b as y from
(
    select count(*) as c,
        unnest(ARRAY[
            (x-1, y-1), (x-1, y), (x-1, y+1),
            (x,   y-1),           (x,   y+1),
            (x+1, y-1), (x+1, y), (x+1, y+1)
        ]) as a
    from lights
    group by a
) as foo
where a not in (select (x,y) from lights)
and c = 3

union

select l1.x, l1.y, count(*) from lights l1, lights as l2
where
  (l2.x <= l1.x + 1 and l2.x >= l1.x - 1)
  and
  (l2.y <= l1.y + 1 and l2.y >= l1.y - 1)
  and
  (l2.x <> l1.x or l2.y <> l1.y)

group by l1.x, l1.y
having count(*) >= 2 and count(*) <= 3


Other queries:


query MyQuery($scheduled_time: String, $payload: jsonb, $name: String, $id:String) {
  foo(where: {_and: [{test: {_neq: $scheduled_time}}, {test2: {_contains: $payload}}, {test: {_neq: $name}}, {test: {_neq: $id}}]}) {
    id
  }
}

{"scheduled_time": "asdf", "payload": {}, "name": "asdf", "id": "asdf"}



-}



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

help :: IO ()
help = putStrLn "Provide no arguments to pick first MIDI source, or specify a name."

setup :: [(String, Destination)] -> [String] -> IO ()
setup [] _                     = putStrLn "No MIDI destinations found."
setup _  a | "-h"     `elem` a = help
setup _  a | "--help" `elem` a = help
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
  sequence_ [change conn x y 0 | x <- [0..7], y <- [0..7]]
  mapM_ (event conn) =<< (lines <$> getContents)
  sequence_ [change conn x y 0 | x <- [0..7], y <- [0..7]]
  stop conn
  close conn

event :: Connection -> String -> IO ()
event d l = do
  case map readMay (words l) of
    [Just x, Just y, Just v] -> print l >> change d x y v
    x -> putStrLn $ "Couldn't decode line: [" <> l <> "]. Expected format [x y v]. Example: [2 2 127]"

change :: Connection -> Int -> Int -> Int -> IO ()
change c x y v = do
  let z = y * 16 + x
  mapM_ (send c . message v) [z]

message :: Int -> Int -> MidiMessage
message v p = MidiMessage 1 (NoteOn p v)
