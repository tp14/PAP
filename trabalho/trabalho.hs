import Text.ParserCombinators.Parsec
import Data.List 

data Type = TypeInt
          | TypeVar Name
          | TypeArrow Type Type

type Name = String
type Unifier = [(Name, Type)]

parseType :: Parser Type -- type: function | atom
parseType =
    try parseFun <|> parseAtom

parseAtom :: Parser Type -- atom: int | var | paren
parseAtom =
    try parseInt <|> parseVar <|> parseParen

parseInt :: Parser Type -- int: "Int"
parseInt = do
    name <- string "Int"
    return TypeInt

parseVar :: Parser Type -- var: lowercase+
parseVar = do
    name <- many1 lower
    return (TypeVar name)

parseFun :: Parser Type -- fun: atom "->" type
parseFun  = do
    first <- parseAtom
    whiteSpace
    arrow <- string "->"
    whiteSpace
    TypeArrow first <$> parseType

parseParen :: Parser Type -- paren: "(" type ")"
parseParen = do
    char '('
    name <- parseType
    char ')'
    return name 

whiteSpace :: Parser ()
whiteSpace = do
  many (char ' ')
  return ()

main :: IO ()
main = do
  putStrLn "Digite um termo:"
  str1 <- getLine
  let Right a = parse parseType "<stdin>" str1
  str2 <- getLine
  let Right b = parse parseType "<stdin>" str2
  --
  putStrLn "Unificacao:"
  case unify a b of
    Just s ->
      putStrLn $ showUnifier s
    Nothing ->
      putStrLn "Deu ruim!"

subst :: Unifier -> Type -> Type
subst s TypeInt =
    TypeInt
subst s (TypeVar x) =
    case lookup x s of
        Just e -> e
        Nothing -> TypeVar x
subst s (TypeArrow a b) =
    TypeArrow (subst s a) (subst s b)

occursCheck :: Name -> Type -> Bool
occursCheck x TypeInt =
    False
occursCheck x (TypeVar y) =
    x == y
occursCheck x (TypeArrow a b) =
    occursCheck x a || occursCheck x b

substUnifier :: Unifier -> Unifier -> Unifier
substUnifier s = map (\(x,e) -> (x, subst s e))

compose :: Unifier -> Unifier -> Unifier
compose s2 s1 =
    s2 ++ substUnifier s2 s1

unify :: Type -> Type -> Maybe Unifier
-- REFL
unify (TypeVar x) (TypeVar y) | x== y =
    Just []
-- LEFT
unify (TypeVar x) e | not (occursCheck x e) =
    Just [(x,e)]
-- RIGHT
unify e (TypeVar x) | not (occursCheck x e) =
    Just [(x,e)]
-- INT
unify TypeInt TypeInt =
    Just []
-- ARROW
unify (TypeArrow a b) (TypeArrow c d) = do
    s1 <- unify a c
    s2 <- unify (subst s1 b) (subst s1 d)
    return (compose s2 s1)
unify a b =
    Nothing

showUnifier [] =
  "{}"
showUnifier xs =
  "{ " ++ intercalate ", " (map showPair xs) ++ " }"
  where
    showPair (x, e) =
      x ++ " |-> " ++ show e

instance Show Type where
  show TypeInt =
    "Int"
  show (TypeVar x) =
    x
  show (TypeArrow a b) =
    "(" ++ show a ++ " -> " ++ show b ++ ")"