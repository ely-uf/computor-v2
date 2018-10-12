{-# LANGUAGE FlexibleInstances #-}

module Types
  ( ComputorCommand(..)
  , AExpr(..)
  , BinaryOperation(..)
  , ExpressionError
  , ComputorState(..)
  , Function(..)
  , VArg(..)
  , FunctionError
  , Value(..)
  , isValueTNum
  , isValueFunction
  , TNum(..)
  , VariableAssignment(..)
  , ComputorStateT
  , Matrix(..)
  , TSign(..)
  , ETNum(..)
  , Term(..)
  , Equation(..)
  , QuadraticEquation(..)
  , Solution(..)
  , termListToString
  , isNegativeTerm
  , isZeroTerm
  , getTermPower
  , getTermCoefficient
  , tnumToDouble
  , quadraticDiscriminant
  , quadraticCoefficients
  ) where

import Control.Monad.State.Strict
import Data.List (find, intercalate)
import qualified Data.Map as M

type ComputorStateT = StateT ComputorState

data ComputorCommand
  = CAssignment VariableAssignment
  | CEquation Equation
  | CAExpr AExpr
  | CVarQuery String
  | CBuiltin String
  | CNothing

{-- AExpr --}

type ExpressionError = String

data AExpr
  = Variable String
  | ConstVal TNum
  | Neg AExpr
  | BinaryExpr BinaryOperation AExpr AExpr
  | LambdaApplication Function [VArg]
  | FunctionCall AExpr [VArg]

data BinaryOperation
  = Add
  | Sub
  | Div
  | Mul
  | Pow 
  | Mod
  | MatrixMul

instance Show AExpr where
  show (Variable a) = a
  show (ConstVal a) = show a
  show (Neg a) = '-' : (show a)
  show (BinaryExpr op a1 a2) = '(' : (show a1) ++ " " ++ (show op) ++ " " ++ (show a2) ++ ")"
  show (FunctionCall name args) = (show name) ++ ('(': (intercalate ", " (map show args))) ++ ")"
  show (LambdaApplication fn args) = show fn ++ ('(': (intercalate ", " (map show args))) ++ ")"

instance Show BinaryOperation where
  show Add = "+"
  show Sub = "-"
  show Div = "/"
  show Mul = "*"
  show Pow = "^"
  show Mod = "%"
  show MatrixMul = "**"

{-- ComputorState --}

data ComputorState = ComputorState
  { variables :: M.Map String Value
  }

instance Show ComputorState where
  show (ComputorState variables) = showVariables
    where showVariables = "Variables:\n" ++ (show variables)

{-- Function --}

data Function
  = Function
  { args        :: [String]
  , appliedArgs :: [(String, Value)]
  , body        :: AExpr
  }
  | BuiltinFunction
  { bName        :: String
  , bArgs        :: [String]
  , bAppliedArgs :: [(String, Value)]
  , bHandler     :: [(String, Value)] -> ComputorState -> Either FunctionError Value
  }

data VArg
  = VArgAExpr AExpr
  | VArgTNum TNum
  | VArgFunc Function

type FunctionError = String

instance Show Function where
  show (BuiltinFunction _name _args [] _body) = _name ++ "(" ++ intercalate ", " _args ++ ") -> [native code]"
  show (BuiltinFunction _name _args _appliedArgs _body) = show (BuiltinFunction _name (substituteArgs _args _appliedArgs) [] _body)
  show (Function _args [] _body) = '(': intercalate ", " _args ++ ") -> " ++ show _body
  show (Function _args _appliedArgs _body) = show (Function (substituteArgs _args _appliedArgs) [] _body)

substituteArgs :: [String] -> [(String, Value)] -> [String]
substituteArgs args appliedArgs = map substitute args
  where substitute arg = case find ((== arg).fst) appliedArgs of
                            Nothing     -> arg
                            Just (_, v) -> arg ++ ": " ++ show v

instance Show VArg where
  show (VArgAExpr e) = show e
  show (VArgTNum n) = show n
  show (VArgFunc f) = show f

{-- Value --}

data Value
  = VFunc Function
  | VNum  TNum

isValueTNum :: Value -> Bool
isValueTNum (VNum _) = True
isValueTNum _ = False

isValueFunction :: Value -> Bool
isValueFunction = not . isValueTNum

instance Show Value where
  show (VFunc f) = show f
  show (VNum n ) = show n

{-- TNum --}

data TNum
  = TInteger Integer
  | TDouble Double
  | TComplex { real :: Double, imag :: Double}
  | TMatrix (Matrix TNum)

instance Show TNum where
  show (TInteger i) = show i
  show (TDouble d) = show d
  show (TComplex 0 1) = "i"
  show (TComplex 0 (-1)) = "-i"
  show (TComplex 0 0) = "0"
  show (TComplex r 0) = show r
  show (TComplex r 1) = '(': (show r) ++ " + i)"
  show (TComplex r (-1)) = '(': (show r) ++ " - i)"
  show (TComplex r i) | i < 0 = '(' : (show r) ++ " - " ++ (show $ negate i) ++ " * i)"
  show (TComplex r i) = '(' : (show r) ++ " + " ++ (show i) ++ " * i)"
  show (TMatrix m) = show m

instance Num TNum where
  (TInteger a)   + (TInteger b)   = TInteger (a + b)
  (TInteger a)   + (TDouble b)    = TDouble ((fromInteger a) + b)
  (TDouble a)    + (TInteger b)   = TDouble (a + (fromInteger b))
  (TDouble a)    + (TDouble b)    = TDouble (a + b)
  (TInteger a)   + (TComplex b c) = TComplex ((fromInteger a) + b) c
  (TComplex a b) + (TInteger c)   = TComplex (a + (fromInteger c)) b
  (TDouble a)    + (TComplex b c) = TComplex (a + b) c
  (TComplex a b) + (TDouble c)    = TComplex (a + c) b
  (TComplex a b) + (TComplex c d) = TComplex (a + c) (b + d)
  (TMatrix (Matrix nr nc el1)) + (TMatrix (Matrix _ _ el2)) = TMatrix $ Matrix nr nc (zipWith (+) el1 el2)
  (TInteger a)   - (TInteger b)   = TInteger (a - b)
  (TInteger a)   - (TDouble b)    = TDouble ((fromInteger a) - b)
  (TDouble a)    - (TInteger b)   = TDouble (a - (fromInteger b))
  (TDouble a)    - (TDouble b)    = TDouble (a - b)
  (TInteger a)   - (TComplex b c) = TComplex ((fromInteger a) - b) (-c)
  (TComplex a b) - (TInteger c)   = TComplex (a - (fromInteger c)) b
  (TDouble a)    - (TComplex b c) = TComplex (a - b) (-c)
  (TComplex a b) - (TDouble c)    = TComplex (a - c) b
  (TComplex a b) - (TComplex c d) = TComplex (a - c) (b - d)
  (TMatrix (Matrix nr nc el1)) - (TMatrix (Matrix _ _ el2)) = TMatrix $ Matrix nr nc (zipWith (-) el1 el2)
  (TInteger a)   * (TInteger b)   = TInteger (a * b)
  (TInteger a)   * (TDouble b)    = TDouble ((fromInteger a) * b)
  (TDouble a)    * (TInteger b)   = TDouble (a * (fromInteger b))
  (TDouble a)    * (TDouble b)    = TDouble (a * b)
  (TInteger a)   * (TComplex b c) = TComplex ((fromInteger a) * b) ((fromInteger a) * c)
  (TComplex a b) * (TInteger c)   = TComplex (a * (fromInteger c)) ((fromInteger c) * b)
  (TDouble a)    * (TComplex b c) = TComplex (a * b) (a * c)
  (TComplex a b) * (TDouble c)    = TComplex (a * c) (b * c)
  (TComplex a b) * (TComplex c d) = TComplex (a * c - b * d) (a * d + b * c)
  (TMatrix (Matrix nr nc el1)) * (TMatrix (Matrix _ _ el2)) = TMatrix $ Matrix nr nc (zipWith (*) el1 el2)
  (TMatrix m) * n = TMatrix $ fmap (* n) m
  n * (TMatrix m) = TMatrix $ fmap (* n) m
  negate (TInteger a)   = TInteger (negate a)
  negate (TDouble a)    = TDouble (negate a)
  negate (TComplex a b) = TComplex (negate a) (negate b)
  abs (TInteger a)      = TInteger (abs a)
  abs (TDouble a)       = TDouble (abs a)
  abs (TComplex a b)    = TComplex (abs a) (abs b)
  signum (TInteger a)   = TInteger (signum a)
  signum (TDouble a)    = TDouble (signum a)
  signum (TComplex a b) = TComplex (signum a) (signum b)
  fromInteger a         = TInteger a

instance Fractional TNum where
  (TInteger a) / (TInteger b) = TDouble (fromInteger a / fromInteger b)
  (TInteger a) / (TDouble  b) = TDouble (fromInteger a / b)
  (TDouble  a) / (TInteger b) = TDouble (a / fromInteger b)
  (TDouble  a) / (TDouble  b) = TDouble (a / b)
  (TInteger a) / c@(TComplex _ _)  = (TComplex (fromInteger a) 0) / c
  a@(TComplex _ _) / (TInteger b)  = a / (TComplex (fromInteger b) 0)
  (TDouble a)  / b@(TComplex _ _)  = (TComplex a 0) / b
  a@(TComplex _ _) / (TDouble b)   = a / (TComplex b 0)
  (TComplex a b) / (TComplex c d) =  TComplex ((a * c' + b * d') / den)  ((b * c' - a * d') / den)
                           where c' = scaleFloat k c
                                 d' = scaleFloat k d
                                 k   = - max (exponent c) (exponent d)
                                 den = c * c' + d * d'
  (TMatrix (Matrix nr nc el1)) / (TMatrix (Matrix _ _ el2)) = TMatrix $ Matrix nr nc $ zipWith (/) el1 el2
  fromRational a = TDouble $ fromRational a

{-- VariableAssignment --}

data VariableAssignment = VariableAssignment
  { key :: String
  , value :: VArg
  }

{-- Matrix --}

data Matrix a = Matrix {
    nrows :: !Int
  , ncols :: !Int
  , elems :: [a]
  }

instance Functor Matrix where
  fmap fn (Matrix nr nc elems) = Matrix nr nc (map fn elems)

instance Show (Matrix TNum) where
  show (Matrix nr nc elems) = "[" ++ (intercalate " "  . map show) (group nc elems) ++ "]"

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"

{- EQUATIONS.   -}
{- WARNING!     -}
{- LEGACY CODE! -}
{-              -}

data Equation = Equation
  { lhs :: [Term]
  , rhs :: [Term]
  } deriving (Eq)

data Solution = NoSolutions
  | AnySolution
  | SingleSolution ETNum
  | MultipleSolutions [ETNum]

data QuadraticEquation = QuadraticEquation Term Term Term

data Term = Term ETNum ETNum deriving (Eq)
data TSign = TPlus | TMinus deriving (Show, Eq)
data ETNum = ETDouble Double | ETInt Integer | ETComplex Double Double deriving (Eq)

instance Show (ETNum) where
  show (ETDouble d)    = show d
  show (ETInt i)       = show i
  show (ETComplex 0 0) = show 0
  show (ETComplex 0 1) = "i"
  show (ETComplex r 0) = (show r)
  show (ETComplex 0 i) = (show i) ++ "i"
  show (ETComplex r 1) = (show r) ++ " + i"
  show (ETComplex r (-1))      = (show r) ++ " - i"
  show (ETComplex r i) | i < 0 = (show r) ++ " - " ++ (show.negate $ i) ++ "i"
  show (ETComplex r i)         = (show r) ++ " + " ++ (show i) ++ "i"

instance Show (Term) where
  show (Term (ETInt 0) _)      = "0"
  show (Term (ETDouble 0.0) _) = "0"
  show (Term x (ETInt 0))      = show x
  show (Term x (ETDouble 0))   = show x
  show (Term (ETInt 1) (ETInt 1))       = "x"
  show (Term (ETDouble 1) (ETInt 1))    = "x"
  show (Term (ETInt 1) (ETDouble 1))    = "x"
  show (Term (ETDouble 1) (ETDouble 1)) = "x"
  show (Term (ETInt (-1)) (ETInt 1))       = "-x"
  show (Term (ETDouble (-1)) (ETInt 1))    = "-x"
  show (Term (ETInt (-1)) (ETDouble 1))    = "-x"
  show (Term (ETDouble (-1)) (ETDouble 1)) = "-x"
  show (Term x (ETInt 1))      = (show x) ++ "*x"
  show (Term x (ETDouble 1.0)) = (show x) ++ "*x"
  show (Term (ETInt 1) y)      = "x^" ++ (show y)
  show (Term (ETDouble 1.0) y) = "x^" ++ (show y)
  show (Term (ETInt (-1)) y)      = "-x^" ++ (show y)
  show (Term (ETDouble (-1.0)) y) = "-x^" ++ (show y)
  show (Term x y)             = (show x) ++ "*x^" ++ (show y)

instance Num (ETNum) where
  (ETInt x) + (ETInt y)       = ETInt (x + y)
  (ETInt x) + (ETDouble y)    = ETDouble ((fromIntegral x) + y)
  (ETDouble x) + (ETInt y)    = ETDouble (x + (fromIntegral y))
  (ETDouble x) + (ETDouble y) = ETDouble (x + y)
  (ETInt x) - (ETInt y)       = ETInt (x - y)
  (ETInt x) - (ETDouble y)    = ETDouble ((fromIntegral x) - y)
  (ETDouble x) - (ETInt y)    = ETDouble (x - (fromIntegral y))
  (ETDouble x) - (ETDouble y) = ETDouble (x - y)
  (ETInt x) * (ETInt y)       = ETInt (x * y)
  (ETInt x) * (ETDouble y)    = ETDouble ((fromIntegral x) * y)
  (ETDouble x) * (ETInt y)    = ETDouble (x * (fromIntegral y))
  (ETDouble x) * (ETDouble y) = ETDouble (x * y)
  negate (ETInt x)     = ETInt (negate x)
  negate (ETDouble x)  = ETDouble (negate x)
  abs (ETInt x)        = ETInt (abs x)
  abs (ETDouble x)     = ETDouble (abs x)
  signum (ETInt x)     = ETInt (signum x)
  signum (ETDouble x)  = ETDouble (signum x)
  fromInteger x       = ETInt (fromIntegral x)

instance Num (Term) where
  (Term x y) + (Term x1 y1) | y == y1 = (Term (x + x1) y)
  (Term x y) + (Term x1 y1) = error "Cannot add terms with different powers."
  (Term x y) - (Term x1 y1) | y == y1 = (Term (x - x1) y)
  (Term x y) - (Term x1 y1) = error "Cannot subtract terms with different powers."
  (Term x y) * (Term x1 y1) = (Term (x * x1) (y + y1))
  negate (Term x y) = (Term (negate x) y)
  abs (Term x y)    = (Term (abs x) y)
  signum (Term x y) = (Term (signum x) y)
  fromInteger x     = (Term (fromIntegral x) (ETInt 0))

instance Show (Equation) where
  show (Equation lhs rhs) = (termListToString lhs) ++ " = " ++ (termListToString rhs)

instance Show (QuadraticEquation) where
  show (QuadraticEquation a b c) = (termListToString [a, b, c]) ++ " = 0"

termListToString :: [Term] -> String
termListToString []         = ""
termListToString (x:xs:xss) = (show x) ++ " + " ++ (termListToString (xs:xss))
termListToString (x:[])     = show x

isNegativeTerm :: Term -> Bool
isNegativeTerm (Term (ETInt x) _)    = x < 0
isNegativeTerm (Term (ETDouble x) _) = x < 0.0

isZeroTerm :: Term -> Bool
isZeroTerm (Term (ETInt 0) _) = True
isZeroTerm (Term (ETDouble 0.0) _) = True
isZeroTerm _ = False

getTermPower :: Term -> Integer
getTermPower (Term _ (ETInt x)) = x

getTermCoefficient :: Term -> Double
getTermCoefficient (Term (ETInt x) _) = fromIntegral x
getTermCoefficient (Term (ETDouble x) _) = x

tnumToDouble :: ETNum -> Double
tnumToDouble (ETDouble x) = x
tnumToDouble (ETInt x)    = fromIntegral x

quadraticDiscriminant :: QuadraticEquation -> Double
quadraticDiscriminant (QuadraticEquation (Term a _) (Term b _) (Term c _)) = tnumToDouble $ (b * b) - 4 * a * c

quadraticCoefficients :: QuadraticEquation -> (Double, Double, Double)
quadraticCoefficients (QuadraticEquation a b c) = (
  getTermCoefficient a,
  getTermCoefficient b,
  getTermCoefficient c)

