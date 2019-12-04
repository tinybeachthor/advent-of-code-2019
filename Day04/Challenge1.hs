
main :: IO ()
main = do
  let (low,high) = (172930,683082)
  let all = show <$> [x | x <- [low..high]]
  let valid = (filter $ \x -> (twoAdjacentSame x) && (nextNotLower x)) $ all
  putStrLn . show . length $ valid

twoAdjacentSame :: String -> Bool
twoAdjacentSame (a:b:rest) = if a == b then True else twoAdjacentSame (b:rest)
twoAdjacentSame   (_:rest) = False

nextNotLower :: String -> Bool
nextNotLower (a:b:rest) = if a > b then False else nextNotLower (b:rest)
nextNotLower   (_:rest) = True
