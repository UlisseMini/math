module Main where

-- import Text.ParserCombinators.ReadP
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


isConst :: Expr -> Bool
isConst (Const x) = True
isConst x = False


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



count :: (a -> Bool) -> [a] -> Int
count p xs = length $ filter p xs


collectConstants :: (Float -> Float -> Float) -> Float -> [Expr] -> [Expr]
collectConstants op acc xs = [Const (foldr f acc constants)] ++ notConst
  where constants = filter isConst xs
        notConst = filter (not . isConst) xs
        f (Const a) acc = op acc a


simplify :: Expr -> Expr
simplify exp = simplify_ 0 exp

simplify_ :: Int -> Expr -> Expr
simplify_ 10 exp = exp -- max simplify depth exceeded
simplify_ d exp =
  case exp of
    Mul (x:[]) -> x
    Add (x:[]) -> x
    Sub ((Const x):[]) -> (Const (-x))

    -- multiplying by zero or one
    Mul xs | elem (Const 0) xs -> Const 0
    Mul xs | elem (Const 1) xs ->
      simplify_ (d+1) $ Mul $ filter (/= Const 1) xs

    -- collect constants in communative operators
    Mul xs | count isConst xs >= 2 ->
      simplify_ (d+1) $ Mul (collectConstants (*) 1 xs)

    Add xs | count isConst xs >= 2 ->
      simplify_ (d+1) $ Add (collectConstants (+) 0 xs)
  
    -- collect constants in non communative operators
    Sub xs | count isConst xs >= 2 ->
      simplify_ (d+1) $ Sub (collectConstants (-) 0 xs)



    -- base case, no simplify possible
    _ -> exp


main :: IO ()
main = test


test :: IO ()
test = hspec $ do
  -- todo: automatically make multiplication and addition tests
  -- communative
  describe "simplify" $ do
    it "simplifies x*0" $ do
      simplify (Mul [Var 'x', Const 0]) `shouldBe` (Const 0)

    it "simplifies 0*x" $ do
      simplify (Mul [Const 0, Var 'x']) `shouldBe` (Const 0)

    it "simplifies 1*x*1" $ do
      simplify (Mul [Const 1, Var 'x', Const 1]) `shouldBe` (Var 'x')

    it "does nothing to x + 1" $ do
      simplify (Add [Var 'x', Const 1]) `shouldBe` (Add [Var 'x', Const 1])

    it "evaluates mul" $ do
      simplify (Mul [(Const 2), (Const 4)]) `shouldBe` (Const 8)

    it "evaluates add" $ do
      simplify (Add [(Const 2), (Const 4)]) `shouldBe` (Const 6)

    it "evaluates sub" $ do
      simplify (Sub [(Const 2), (Const 4)]) `shouldBe` (Const (-2))

    it "evaluates div" $ do
      simplify (Div [(Const 2), (Const 4)]) `shouldBe` (Const 0.5)

    it "evaluates exp" $ do
      simplify (Exp [(Const 2), (Const 4)]) `shouldBe` (Const 16)

    it "evaluates constants nested (mul)" $ do
      simplify (Mul [(Const 2), (Const 2), (Const 4)]) `shouldBe` (Const 16)

    it "evaluates constants nested (add)" $ do
      simplify (Add [(Const 2), (Const 2), (Const 4)]) `shouldBe` (Const 8)

    it "evaluates constants nested (sub)" $ do
      simplify (Sub [(Const 2), (Const 2), (Const 4)]) `shouldBe` (Const (-4))

    it "evaluates constants nested (div)" $ do
      simplify (Div [(Const 2), (Const 2), (Const 4)]) `shouldBe` (Const (1/4))

    -- it "evaluates constants nested (exp)" $ do
    --   simplify (Exp [(Const 2), (Const 2), (Const 4)]) `shouldBe` (Const 256)

    -- it "simplifies (2*x)*4 = 8x" $ do
    --   simplify
    --     (Mul [(Const 2), (Var 'x'), (Const 4)])
    --     `shouldBe`
    --     (Mul [(Const 8), (Var 'x')])



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

