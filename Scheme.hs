import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

    
data SchemeVal = Char Char
               | String String
               | Integer Integer
               | Double Double
               | ProperList [SchemeVal]
               | ImproperList [SchemeVal] SchemeVal
               | False
   

instance Show SchemeVal where
    show = showSchemeVal

showSchemeVal              :: SchemeVal -> String
showSchemeVal (String str) = "String: " ++ str
showSchemeVal (Integer int)    = "Integer: " ++ show int
    

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<?=>@^_~#"


number :: Parser String
number = many1 digit


integer :: Parser String
integer = positive <|> negative
          where positive = do
                  option '+' $ char '+'
                  number
                negative = do 
                  char '-'
                  n <- number
                  return $ '-':n                           
         
scmString :: Parser SchemeVal
scmString = do
  char '"'
  str <- many1 . noneOf $ "\""
  char '"'
  return $ String str

scmInteger :: Parser SchemeVal
scmInteger = (Integer . read) `fmap` integer

scmChar :: Parser SchemeVal
scmChar = undefined
  


          
readExpr :: String -> String
readExpr = undefined 

run :: (Parser SchemeVal) -> String -> SchemeVal
run parser input = case parse parser "(stdin)" input of
                           Left err -> error . show $ err
                           Right scmVal -> scmVal

main :: IO ()
main = do
  [arg] <- getArgs
  print . (run scmInteger) $ arg
