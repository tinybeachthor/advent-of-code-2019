import Day03.Input

main = do
  input <- get "Day03/input"
  putStrLn . show $ input
  let positions = translations <$> input
  let wire1:wire2:_ = positions
  putStrLn . show $ wire1
  putStrLn . show $ wire2
  let allPairs = [(a,b) | a <- wire1, b <- wire2]
  let crossings = (\(a,b) -> crossing a b) <$> allPairs
  let validCrossings = dropInvalid crossings
  putStrLn . show $ validCrossings
  putStrLn . show . minimum $ (\(a,b,c) -> c) <$> validCrossings

type Position = (Int, Int, Int)
type Translation = (Position, Position)

translations :: [Segment] -> [Translation]
translations ss = go (0,0,0) ss
  where
    go p [] = []
    go p (s:ss) =
      let p' = nextPosition p s
       in (p,p'):(go p' ss)

nextPosition :: Position -> Segment -> Position
nextPosition (x,y,c) (U l) = (x,y+l,c+l)
nextPosition (x,y,c) (D l) = (x,y-l,c+l)
nextPosition (x,y,c) (R l) = (x+l,y,c+l)
nextPosition (x,y,c) (L l) = (x-l,y,c+l)

crossing :: Translation -> Translation -> Maybe Position
crossing ((ax,ay,ac),(ax',ay',ac')) ((bx,by,bc),(bx',by',bc'))
    -- horizontal <-> vertical
    | ay == ay' && bx == bx' =
      if between ax ax' bx && between by by' ay
         then Just (bx,ay,ac + abs (ax - bx) + bc + abs (by - ay))
         else Nothing
      -- vertical <-> horizontal
    | ax == ax' && by == by' =
      if between bx bx' ax && between ay ay' by
         then Just (ax,by,ac + abs (bx - ax) + bc + abs (ay - by))
         else Nothing
    | otherwise = Nothing

between :: Int -> Int -> Int -> Bool
between a b x
  | a <= b = if x < a || x > b then False else True
  | otherwise = between b a x

dropInvalid :: [Maybe Position] -> [Position]
dropInvalid [] = []
dropInvalid ((Nothing):rest) = dropInvalid rest
dropInvalid ((Just (0,0,0)):rest) = dropInvalid rest
dropInvalid ((Just p):rest) = p:(dropInvalid rest)
