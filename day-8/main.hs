
--Generalised version of 'words'
splitAt'     :: (Char -> Bool) -> String -> [String]
splitAt' f s =  case dropWhile f s of
                      "" -> []
                      s' -> w : splitAt' f s''
                            where (w, s'') = break f s'

main = do
  putStrLn "File:"
  userInput <- getLine
  rawFile <- readFile userInput
  let split = splitAt' (=='|') rawFile
      --sndPart = map (words . tail) split
  print split