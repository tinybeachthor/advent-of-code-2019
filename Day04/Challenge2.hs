main :: IO ()
main = do
  let (low,high) = (172930,683082)
  let all = show <$> [x | x <- [low..high]]
  let valid = (filter $ \x -> (justTwoAdjacentSame x) && (nextNotLower x)) $ all
  putStrLn . show . length $ valid

justTwoAdjacentSame :: String -> Bool
justTwoAdjacentSame s = go s 0
  where
    go :: String -> Int -> Bool
    go (a:b:rest) c = if a == b
                         then go (b:rest) (c+1)
                         else if c == 1
                                 then True
                                 else go (b:rest) 0
    go (_:rest) c   = if c == 1
                         then True
                         else False

nextNotLower :: String -> Bool
nextNotLower (a:b:rest) = if a > b then False else nextNotLower (b:rest)
nextNotLower   (_:rest) = True
