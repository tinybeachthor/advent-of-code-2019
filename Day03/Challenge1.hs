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
  putStrLn . show $ crossings
  let validCrossings = dropInvalid crossings
  putStrLn . show $ validCrossings
  putStrLn . show . minimum $ (\(a,b) -> abs a + abs b) <$> validCrossings

type Position = (Int, Int)
type Translation = (Position, Position)

translations :: [Segment] -> [Translation]
translations ss = go (0,0) ss
  where
    go p [] = []
    go p (s:ss) =
      let p' = nextPosition p s
       in (p,p'):(go p' ss)

nextPosition :: Position -> Segment -> Position
nextPosition (x,y) (U l) = (x,y+l)
nextPosition (x,y) (D l) = (x,y-l)
nextPosition (x,y) (R l) = (x+l,y)
nextPosition (x,y) (L l) = (x-l,y)

crossing :: Translation -> Translation -> Maybe Position
crossing ((ax, ay),(ax',ay')) ((bx,by),(bx',by'))
    -- horizontal <-> vertical
    | ay == ay' && bx == bx' =
      if between ax ax' bx && between by by' ay
         then Just (bx, ay)
         else Nothing
      -- vertical <-> horizontal
    | ax == ax' && by == by' =
      if between bx bx' ax && between ay ay' by
         then Just (ax, by)
         else Nothing
    | otherwise = Nothing

between :: Int -> Int -> Int -> Bool
between a b x
  | a <= b = if x < a || x > b then False else True
  | otherwise = between b a x

dropInvalid :: [Maybe Position] -> [Position]
dropInvalid [] = []
dropInvalid ((Nothing):rest) = dropInvalid rest
dropInvalid ((Just (0,0)):rest) = dropInvalid rest
dropInvalid ((Just p):rest) = p:(dropInvalid rest)
