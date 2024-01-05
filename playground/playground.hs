-- | A boolean expression (possibly unevaluated).
data BOOL
  = T -- ˆ Boolean TRUE.
  | F -- ˆ Boolean FALSE.
  | BOOL :&&: BOOL -- ˆ Logical AND.
  | BOOL :||: BOOL -- ˆ Logical OR.
  deriving (Show, Eq)

-- | Perform a single lazy evaluation step for a 'BOOL' expression.
--
-- >>> step (F :&&: T)
-- F
--
-- >>> step ((F :||: T) :&&: F)
-- T :&&: F
step :: BOOL -> BOOL
step (F :&&: _) = F
step (T :&&: b) = b
step (b1 :&&: b2) = (step b1) :&&: b2
step (T :||: _) = T
step (F :||: b) = b
step (b1 :||: b2) = (step b1) :||: b2
step b = b

-- | Perform evaluation of a 'BOOL' expression
-- until a nothing changes under 'step'.
-- Return all intermediate expressions.
--
-- >>> steps ((F :||: T) :&&: F)
-- [(F :||: T) :&&: F,T :&&: F,F]
steps :: BOOL -> [BOOL]
steps b
  | b == step (b) = [b]
  | otherwise = [b] ++ steps (step b)

-- | Perform evaluation of a 'BOOL' expression
-- until a nothing changes under 'step'.
-- Print all intermediate expressions.
--
-- >>> ppSteps ((F :||: T) :&&: F)
-- WAS WAS (F :||: T) :&&: F
-- WAS WAS T :&&: F
-- WAS WAS F
ppSteps :: BOOL -> IO ()
ppSteps b
  | b == (step b) = print b
  | otherwise = do
      print b
      ppSteps $ step b

--------------------------------------------
out1 = ppSteps (foldl (:&&:) T [T, F, T, F])

out2 = ppSteps (foldl (:||:) F [F, T, F, T])

out3 = ppSteps (foldr (:&&:) T [T, F, T, F])

out4 = ppSteps (foldr (:||:) F [F, T, F, T])

packMaybe :: a -> Maybe a
packMaybe v = return v