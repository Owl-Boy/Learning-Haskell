interactiveConcatinating :: IO ()
interactiveConcatinating = do
  sz <- putStrLn "Enter 2 strings" *> ((++) <$> (take 3 <$> getLine) <*> getLine)
  putStrLn "Let's Concatinate them!" *> putStrLn sz

main = interactiveConcatinating
