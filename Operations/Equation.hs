module Operations.Equation
  ( solveEquation
  ) where

import Types

equateToZero :: Equation -> Equation
equateToZero (Equation x []) = (Equation x [0])
equateToZero (Equation x (y:ys)) | isZeroTerm y = equateToZero (Equation x ys)
equateToZero (Equation x (y:ys)) = equateToZero (Equation (x ++ ([negate y])) ys)

hasPowersGreaterThan :: Equation -> Double -> Bool
hasPowersGreaterThan (Equation lhs rhs) power = hasPowersGT power lhs || hasPowersGT power lhs
  where hasPowersGT power = any ((> power).fromIntegral.getTermPower)

hasDoublePower :: Term -> Bool
hasDoublePower (Term _ (ETDouble _)) = True
hasDoublePower _ = False

-- Hack for ease of use.
hasDoublePowers :: Equation -> Bool
hasDoublePowers (Equation lhs rhs) = any hasDoublePower lhs || any hasDoublePower rhs

sumAllTermsWithPowerOf :: Integer -> [Term] -> Term
sumAllTermsWithPowerOf power terms = foldl (+) (Term (ETInt 0) (ETInt power)) $ filteredTermsOfPower
  where filteredTermsOfPower = filter ((== power).getTermPower) terms

equationToQuadratic :: Equation -> QuadraticEquation
equationToQuadratic (Equation lhs [0]) = QuadraticEquation
  (sumAllTermsWithPowerOf 2 lhs)
  (sumAllTermsWithPowerOf 1 lhs)
  (sumAllTermsWithPowerOf 0 lhs)
equationToQuadratic eq = equationToQuadratic $ equateToZero eq

--- Just a hack before we have actual Double exponents
maxBetweenIntAndTermPower :: Integer -> Term -> Integer
maxBetweenIntAndTermPower x (Term _ (ETInt y)) = max x y

equationDegree :: Equation -> Integer
equationDegree (Equation lhs rhs) = foldl maxBetweenIntAndTermPower 0 nonZeroTerms
  where nonZeroTerms = filter ((/= 0).getTermCoefficient) (lhs ++ rhs)

reduceEquation :: Equation -> Equation
reduceEquation eq = reduceEquation' $ equateToZero eq
  where reduceEquation' (Equation lhs rhs) = Equation (reduceSide lhs) ([0])
        degree = equationDegree eq
        reduceSide side = reverse $ if null (nonZeroTerms side) then [0] else nonZeroTerms side
        reduceSide' side = map ((flip sumAllTermsWithPowerOf) side) [0 .. degree]
        nonZeroTerms termList = filter ((/= 0).getTermCoefficient) (reduceSide' termList)

solveSecondDegreePolynomial :: Equation -> IO Solution
solveSecondDegreePolynomial eq =
  let quadraticEquation = equationToQuadratic eq
      discriminant = quadraticDiscriminant quadraticEquation
      (a, b, c) = quadraticCoefficients quadraticEquation
      zeroDiscrSolution = ((negate b) / (2 * a))
      positiveDiscrSolutions = fmap ETDouble [positiveDiscrCalc (-), positiveDiscrCalc (+)]
      positiveDiscrCalc sign = ((negate b) `sign` (sqrt discriminant)) / (2 * a)
      negativeDiscrSolutions = [ETComplex realPart imaginaryPart, ETComplex realPart (negate imaginaryPart)]
      realPart      = (negate b) / (2 * a)
      imaginaryPart = (sqrt.negate $ discriminant) / (2 * a)
    in do
      putStrLn "==> Solving quadratic equation."
      putStrLn $ "==> QuadraticEquation -> " ++ (show quadraticEquation)
      putStrLn $ "==> D = " ++ (show discriminant)
      case compare discriminant 0 of
        LT -> putStrLn "==> No real solutions." >> return (MultipleSolutions negativeDiscrSolutions)
        EQ -> putStrLn "==> Only one solution exists." >> return (SingleSolution (ETDouble zeroDiscrSolution))
        GT -> putStrLn "==> Two solutions exist." >> return (MultipleSolutions positiveDiscrSolutions)

solveFirstDegreePolynomial :: Equation -> IO Solution
solveFirstDegreePolynomial (Equation lhs rhs) =
  let balancedEq = (Equation [leftSideTermsSum] [rightSideTermsSum])
      leftSideTermsSum   = sumDegreesOfPower 1
      rightSideTermsSum  = negate $ sumDegreesOfPower 0
      sumDegreesOfPower p = (sumAllTermsWithPowerOf p (fmap negate rhs)) + (sumAllTermsWithPowerOf p lhs)
      firstDegreeTermCoeff = getTermCoefficient leftSideTermsSum
      zeroDegreeTermCoeff = getTermCoefficient rightSideTermsSum
    in do
      putStrLn $ "==> Balanced first degree polynomial: " ++ (show balancedEq)
      return $ SingleSolution (ETDouble (zeroDegreeTermCoeff / firstDegreeTermCoeff))

-- Duh ;(
solveZeroDegreePolynomial :: Equation -> IO Solution
solveZeroDegreePolynomial (Equation terms [0]) | all ((== 0).getTermCoefficient) terms = return AnySolution
solveZeroDegreePolynomial (Equation _ [0]) = return NoSolutions
solveZeroDegreePolynomial eq = error $ "WTF: " ++ (show eq)

solvePolynomialOfDegree :: Equation -> Integer -> IO Solution
solvePolynomialOfDegree eq 0 = solveZeroDegreePolynomial eq
solvePolynomialOfDegree eq 1 = solveFirstDegreePolynomial eq
solvePolynomialOfDegree eq 2 = solveSecondDegreePolynomial eq
solvePolynomialOfDegree eq d = do
  putStrLn $ "==> The equation (" ++ (show eq) ++ ") of degree " ++ (show d) ++ " is not supported."
  return NoSolutions

solveEquation :: Equation -> IO ()
solveEquation equation
  | hasDoublePowers equation  = putStrLn "!> As for now, only integral powers are supported."
  | otherwise =
  let equatedToZero   = equateToZero equation
      reducedEquation = reduceEquation equatedToZero
      degree          = equationDegree reducedEquation
    in do
      putStrLn $ "==> Solving the equation: " ++ (show equation)
      if equation == equatedToZero then
        putStrLn "==> All terms are already on one side."
      else do
        putStrLn "==> Moved all the terms on a single side:"
        putStrLn $ "==> " ++ (show equatedToZero)
      putStrLn $ "==> Reduced equation: " ++ (show reducedEquation)
      putStrLn $ "==> Equation degree is " ++ (show degree)
      solution <- solvePolynomialOfDegree reducedEquation degree
      case solution of
        NoSolutions -> putStrLn "==> There are no solutions for this equation."
        AnySolution -> putStrLn "==> Any real number is a solution."
        SingleSolution a -> putStrLn $ "==> There is a single solution: x = " ++ (show (a :: ETNum))
        MultipleSolutions a -> putStrLn $ "==> There are multiple solutions: " ++ (show (a :: [ETNum]))
      putStrLn "---"
