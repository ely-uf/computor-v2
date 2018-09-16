{-# LANGUAGE ExistentialQuantification #-}

module TNum where

data TNum
  = TInteger Integer
  | TDouble Double
  | TComplex Double Double
  deriving Show

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
  (TInteger a)   - (TInteger b)   = TInteger (a - b)
  (TInteger a)   - (TDouble b)    = TDouble ((fromInteger a) - b)
  (TDouble a)    - (TInteger b)   = TDouble (a - (fromInteger b))
  (TDouble a)    - (TDouble b)    = TDouble (a - b)
  (TInteger a)   - (TComplex b c) = TComplex ((fromInteger a) - b) c
  (TComplex a b) - (TInteger c)   = TComplex (a - (fromInteger c)) b
  (TDouble a)    - (TComplex b c) = TComplex (a - b) c
  (TComplex a b) - (TDouble c)    = TComplex (a - c) b
  (TComplex a b) - (TComplex c d) = TComplex (a - c) (b - d)
  (TInteger a)   * (TInteger b)   = TInteger (a * b)
  (TInteger a)   * (TDouble b)    = TDouble ((fromInteger a) * b)
  (TDouble a)    * (TInteger b)   = TDouble (a * (fromInteger b))
  (TDouble a)    * (TDouble b)    = TDouble (a * b)
  (TInteger a)   * (TComplex b c) = TComplex ((fromInteger a) * b) c
  (TComplex a b) * (TInteger c)   = TComplex (a * (fromInteger c)) b
  (TDouble a)    * (TComplex b c) = TComplex (a * b) c
  (TComplex a b) * (TDouble c)    = TComplex (a * c) b
  (TComplex a b) * (TComplex c d) = TComplex (a * c - b * d) (a * d + b * c)
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
