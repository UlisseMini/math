module Main where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.Functor
import Control.Applicative ((<|>), Applicative, Alternative)

import Test.Hspec
import Test.QuickCheck


data Expr
  = Const Float
  | Var Char
  | Mul Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
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
 
showPretty :: Expr -> String
showPretty exp = s
  where s = case exp of
             Mul e1 e2 -> "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
             Add e1 e2 -> "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
             Sub e1 e2 -> "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
             Div e1 e2 -> "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
             Exp e1 e2 -> "(" ++ show e1 ++  "^"  ++ show e2 ++ ")"
             Var c -> [c]
             Const n -> show n



-- make an expr commutative, following rules
-- 1. Variable always after Expr in Add/Sub
-- 2. Constants always before Expr in Add/Sub
makeComm :: Expr -> Expr
makeComm exp =
  case exp of
    -- vars comes last
    Mul (Var x) e -> Mul (makeComm e) (Var x)
    Add (Var x) e -> Add (makeComm e) (Var x)

    -- const comes first
    Mul e (Const x) -> Mul (Const x) (makeComm e)
    Add e (Const x) -> Add (Const x) (makeComm e)

    -- recurse on operators
    Mul a b -> Mul (makeComm a) (makeComm b)
    Add a b -> Add (makeComm a) (makeComm b)
    Sub a b -> Sub (makeComm a) (makeComm b)
    Div a b -> Div (makeComm a) (makeComm b)
    Exp a b -> Exp (makeComm a) (makeComm b)

    _ -> exp

simplify :: Expr -> Expr
simplify exp = simplify_ 0 exp

simplify_ :: Int -> Expr -> Expr
simplify_ 10 exp = exp -- max simplify depth exceeded
simplify_ d exp =
  case (makeComm exp) of
    Mul (Const 0) (Var c) -> Const 0 -- 0*x = 0
    Mul (Var c) (Const 0) -> Const 0 -- x*0 = 0
    Mul (Const 1) (Var c) -> Var c   -- 1*x = x
    Mul (Var c) (Const 1) -> Var c   -- x*1 = x

    -- thread constants downwards
    Mul (Const c1) (Mul (Const c2) e) ->
      simplify_ (d+1) (Mul (Const (c1*c2)) (simplify_ (d+1) e))

    Add (Const c1) (Add (Const c2) e) ->
      simplify_ (d+1) (Add (Const (c1+c2)) (simplify_ (d+1) e))


    -- Evaluate constants
    Mul (Const a) (Const b) -> Const (a*b)
    Add (Const a) (Const b) -> Const (a+b)
    Sub (Const a) (Const b) -> Const (a-b)
    Div (Const a) (Const b) -> Const (a/b)
    Exp (Const a) (Const b) -> Const (a**b)


    -- Recurse on operators
    Mul a b -> simplify_ (d+1) $ Mul (simplify_ (d+1) a) (simplify_ (d+1) b)
    Div a b -> simplify_ (d+1) $ Div (simplify_ (d+1) a) (simplify_ (d+1) b)
    Sub a b -> simplify_ (d+1) $ Sub (simplify_ (d+1) a) (simplify_ (d+1) b)
    Add a b -> simplify_ (d+1) $ Add (simplify_ (d+1) a) (simplify_ (d+1) b)
    Exp a b -> simplify_ (d+1) $ Exp (simplify_ (d+1) a) (simplify_ (d+1) b)

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
  describe "makeComm" $ do
    it "puts const first mult" $ do
      makeComm (Mul (Var 'x') (Const 1)) `shouldBe` (Mul (Const 1) (Var 'x'))

    it "puts const first add" $ do
      makeComm (Add (Var 'x') (Const 1)) `shouldBe` (Add (Const 1) (Var 'x'))


  -- todo: automatically make multiplication and addition tests
  -- communative
  describe "simplify" $ do
    it "simplifies x*0" $ do
      simplify (Mul (Var 'x') (Const 0)) `shouldBe` (Const 0)

    it "simplifies 0*x" $ do
      simplify (Mul (Const 0) (Var 'x')) `shouldBe` (Const 0)

    it "evaluates constants" $ do
      simplify (Mul (Const 2) (Const 4)) `shouldBe` (Const 8)
      simplify (Add (Const 2) (Const 4)) `shouldBe` (Const 6)
      simplify (Sub (Const 2) (Const 4)) `shouldBe` (Const (-2))
      simplify (Div (Const 2) (Const 4)) `shouldBe` (Const 0.5)
      simplify (Exp (Const 2) (Const 4)) `shouldBe` (Const 16)

    it "evaluates constants nested" $ do
      simplify (Mul (Const 2) (Mul (Const 2) (Const 4))) `shouldBe` (Const 16)
      simplify (Add (Const 2) (Add (Const 2) (Const 4))) `shouldBe` (Const 8)
      simplify (Sub (Const 2) (Sub (Const 2) (Const 4))) `shouldBe` (Const 4)
      simplify (Div (Const 2) (Div (Const 2) (Const 4))) `shouldBe` (Const 4)
      simplify (Exp (Const 2) (Exp (Const 2) (Const 4))) `shouldBe` (Const (2^16))

    it "simplifies (2*x)*4 = 8x" $ do
      simplify
        (Mul (Const 2) (Mul (Var 'x') (Const 4)))
        `shouldBe`
        (Mul (Const 8) (Var 'x'))

    it "simplifies (2*x)*(x*4) = 8x" $ do
      simplify
        (Mul (Mul (Const 2) (Var 'x')) (Mul (Var 'x') (Const 4)))
        `shouldBe`
        (Mul (Const 8) (Var 'x'))



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

