module Main where
 import System.Environment
 import Text.ParserCombinators.Parsec hiding (spaces)
 import Control.Monad
 import Numeric


 symbol :: Parser Char
 symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

 spaces :: Parser ()
 spaces = skipMany1 space

 data LispVal = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool
              | Character Char deriving Show


 parseString :: Parser LispVal
 parseString = do
  _ <- char '"'
  x <- many( (noneOf "\"\\") <|> parseEscape  )
  _ <- char '"'
  return $ String x
 parseEscape :: Parser Char
 parseEscape = do
    _ <- char '\\'
    x <- anyChar --(oneOf "nr\"\\" <|> (unexpected "unknown escape code"))
    case x of
      '"' -> return '"'
      'r' -> return '\r'
      'n' -> return '\n'
      '\\' -> return '\\'
      _ -> fail "oops!"


 parseCharacter :: Parser LispVal
 parseCharacter = do
   _ <- try $ char '#' >> char '\\'
   x  <- anyChar
   xs <- many letter
   let res = case x:xs of
               "space" -> Just ' '
               "newline" -> Just '\n'
               [c] -> Just c
               _ -> Nothing
   case res of
     Just c -> return $ Character c
     _ -> fail $ "Unknown character lyteral: '" ++ x:xs ++ "'"

 parseAtom :: Parser LispVal
 parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
   "#t" -> Bool True
   "#f" -> Bool False
   _    -> Atom atom

 createNumber :: String -> LispVal
 createNumber = Number . read

 parseNumber :: Parser LispVal
 parseNumber = do
   prefix <- optionMaybe $ try (char '#' >> oneOf "bodx")
   digits <- many1 $ letter <|> digit
   return $ case prefix of
        Just 'x' -> Number (fst $ head $ readHex digits)
        Just 'o' -> Number (fst $ head $ readOct digits)
        Just 'b' -> Number (fst $ head $ readOct digits)
        _ -> createNumber digits
   --return $ createNumber digits

 parseNumber' :: Parser LispVal
 parseNumber' = (many1 digit) >>= (return  . createNumber)

 parseExpr :: Parser LispVal
 parseExpr = try parseNumber
  <|> parseCharacter
  <|> parseString
  <|> parseAtom

 readExpr :: String -> String
 readExpr input =
   let inputStr = " '" ++ input ++ "' " in
   case parse parseExpr "lisp" input of
    Left err -> inputStr ++ "No match: " ++ show err
    Right x -> inputStr ++ "Found value "  ++ show x

 main :: IO ()
 main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
