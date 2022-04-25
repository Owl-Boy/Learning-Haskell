import Text.Read

interactiveSummation = do
  putStrLn "Enter 2 numbers:"
  sx <- getLine
  sy <- getLine
  let mx = readMaybe sx :: Maybe Double
      my = readMaybe sy
  case mx of
    Just x -> case my of
                Just y -> putStrLn ("Sum of the numbers is " ++ show (x +y))
                Nothing -> retry
    Nothing -> retry
  where
    retry = do
      putStrLn "Invalid number(s) entered. Retrying..."
      interactiveSummation

main = interactiveSummation
