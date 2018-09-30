{-# LANGUAGE ExistentialQuantification #-}

module TNum where

data TNum
  = TInteger Integer
  | TDouble Double
  | TComplex { real :: Double, imag :: Double}

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
  (TDouble a)    - (TComplex b c) = TComplex (a - b) (-c)
  (TComplex a b) - (TDouble c)    = TComplex (a - c) b
  (TComplex a b) - (TComplex c d) = TComplex (a - c) (b - d)
  (TInteger a)   * (TInteger b)   = TInteger (a * b)
  (TInteger a)   * (TDouble b)    = TDouble ((fromInteger a) * b)
  (TDouble a)    * (TInteger b)   = TDouble (a * (fromInteger b))
  (TDouble a)    * (TDouble b)    = TDouble (a * b)
  (TInteger a)   * (TComplex b c) = TComplex ((fromInteger a) * b) ((fromInteger a) * c)
  (TComplex a b) * (TInteger c)   = TComplex (a * (fromInteger c)) ((fromInteger c) * b)
  (TDouble a)    * (TComplex b c) = TComplex (a * b) (a * c)
  (TComplex a b) * (TDouble c)    = TComplex (a * c) (b * c)
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
  fromRational a = TDouble $ fromRational a
