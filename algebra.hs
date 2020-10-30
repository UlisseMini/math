module Main where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Data.Functor
import Control.Applicative ((<|>), Applicative, Alternative)

import Test.Hspec
import Test.QuickCheck


-- lispy like because otherwise the communative prop will
-- drive me crazy (it still might)
data Expr
  = Const Float
  | Var Char
  | Mul [Expr] -- x[0] * x[1] * x[2]   ...
  | Add [Expr] -- x[0] + x[1] + x[2]   ...
  | Sub [Expr] -- x[0] - x[1] - x[2]   ...
  | Div [Expr] -- x[0]/(x[1]/x[2])     ...
  | Exp [Expr] -- x[0]^((x[1])^(x[2])) ...
  deriving (Eq, Show)


isVariable :: Char -> Bool
isVariable c = isLetter c

-- todo: maybe this should be String -> bool?
isConstant :: Char -> Bool
isConstant c = isDigit c

-- todo: there should be whitespace after var
varP :: ReadP Expr
varP = do
  a <- satisfy isVariable
  return $ Var a

constP :: ReadP Expr
constP = do 
  a <- many1 $ satisfy isConstant
  return $ Const (read a)


-- mulP :: ReadP Expr
-- mulP = do
--   a <- 


-- operatorP :: ReadP Expr
-- operatorP = do
--   choice
--   [ mulP
--   ]


readExprParser :: ReadP Expr
readExprParser = do

  a <- constP <|> varP
  b <- constP <|> varP

  return a


-- todo: finish this
readExpr :: String -> Expr
readExpr rawStr = fst $ last $ readP_to_S readExprParser str
  where str = filter (/= ' ') rawStr


showWithOP :: [Expr] -> Char -> String
showWithOP exp c = [c] ++ (intercalate " " $ map showPretty exp)
 
showPretty :: Expr -> String
showPretty exp = s
  where s = case exp of
             Mul xs -> "(" ++ (showWithOP xs '*') ++ ")"
             Add xs -> "(" ++ (showWithOP xs '+') ++ ")"
             Sub xs -> "(" ++ (showWithOP xs '-') ++ ")"
             Div xs -> "(" ++ (showWithOP xs '/') ++ ")"
             Exp xs -> "(" ++ (showWithOP xs '^') ++ ")"
             Var c -> [c]
             Const n -> show n


simplify :: Expr -> Expr
simplify exp = simplify_ 0 exp

simplify_ :: Int -> Expr -> Expr
simplify_ 10 exp = exp -- max simplify depth exceeded
simplify_ d exp =
  case exp of


    -- base case, no simplify possible
    _ -> exp


main :: IO ()
main = do
  l <- getLine
  putStrLn $ "Parsed: " ++ showPretty (readExpr l)
  putStrLn $ "Simplified: " ++ showPretty (simplify (readExpr l))
  main


test :: IO ()
test = hspec $ do
  -- todo: automatically make multiplication and addition tests
  -- communative
  describe "simplify" $ do
    it "simplifies x*0" $ do
      simplify (Mul [Var 'x', Const 0]) `shouldBe` (Const 0)

    it "simplifies 0*x" $ do
      simplify (Mul [Const 0, Var 'x']) `shouldBe` (Const 0)

    it "evaluates constants" $ do
      simplify (Mul [(Const 2), (Const 4)]) `shouldBe` (Const 8)
      simplify (Add [(Const 2), (Const 4)]) `shouldBe` (Const 6)
      simplify (Sub [(Const 2), (Const 4)]) `shouldBe` (Const (-2))
      simplify (Div [(Const 2), (Const 4)]) `shouldBe` (Const 0.5)
      simplify (Exp [(Const 2), (Const 4)]) `shouldBe` (Const 16)

    it "evaluates constants nested" $ do
      simplify (Mul [(Const 2), (Const 2), (Const 4)]) `shouldBe` (Const 16)
      simplify (Add [(Const 2), (Const 2), (Const 4)]) `shouldBe` (Const 8)
      simplify (Sub [(Const 2), (Const 2), (Const 4)]) `shouldBe` (Const 4)
      simplify (Div [(Const 2), (Const 2), (Const 4)]) `shouldBe` (Const 4)
      simplify (Exp [(Const 2), (Const 2), (Const 4)]) `shouldBe` (Const (2^16))

    it "simplifies (2*x)*4 = 8x" $ do
      simplify
        (Mul [(Const 2), (Var 'x'), (Const 4)])
        `shouldBe`
        (Mul [(Const 8), (Var 'x')])



    -- it "simplifies x+x to 2x" $ do
    --   simplify (Add (Var 'x') (Var 'x')) `shouldBe` (Mul (Var 'x') (Const 2))

    -- it "simplifies x+x+x to 3x" $ do
    --   simplify
    --     (Add (Var 'x') (Add (Var 'x') (Var 'x')))
    --     `shouldBe`
    --     (Mul (Var 'x') (Const 3))

    -- it "simplifies x+x+x+x to 4x" $ do
    --   simplify
    --     (Add (Var 'x') ((Add (Var 'x') (Add (Var 'x') (Var 'x')))))
    --     `shouldBe`
    --     (Mul (Var 'x') (Const 4))


  -- describe "readExpr" $ do
  --   it "reads constants" $ do
  --     readExpr "42" `shouldBe` Const 42
  --     readExpr "10" `shouldBe` Const 10

  --   it "reads variables" $ do
  --     readExpr "x" `shouldBe` Var 'x'
  --     readExpr "y" `shouldBe` Var 'y'

  --   -- NOTE: this is not scalable! I need to write code to gen
  --   -- tests otherwise I'm screwed.
  --   it "reads addition" $ do
  --     readExpr "x+y" `shouldBe` Add (Var 'x') (Var 'y')
  --     readExpr "x+5" `shouldBe` Add (Var 'x') (Const 5)
  --     readExpr "5+x" `shouldBe` Add (Const 5) (Var 'x')

  --   it "reads addition with whitespace" $ do
  --     readExpr " x + y" `shouldBe` Add (Var 'x') (Var 'y')
  --     readExpr " x + 5 " `shouldBe` Add (Var 'x') (Const 5)
  --     readExpr "5 + x " `shouldBe` Add (Const 5) (Var 'x')

  --   it "reads subtraction" $ do
  --     readExpr "x-y" `shouldBe` Sub (Var 'x') (Var 'y')
  --     readExpr "x-5" `shouldBe` Sub (Var 'x') (Const 5)
  --     readExpr "5-x" `shouldBe` Sub (Const 5) (Var 'x')

