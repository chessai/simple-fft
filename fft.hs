{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Trans.Writer
import Data.Bifunctor
import Data.Complex
import Data.List
import Data.Monoid
import Data.Ratio
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype U = U Rational
  deriving (Show, Eq, Ord)

toComplex :: Floating a => U -> Complex a
toComplex (U q) = mkPolar 1 (2 * pi * realToFrac q)

mkU :: Rational -> U
mkU q = U (q - realToFrac (floor q))

u_pow :: U -> Integer -> U
u_pow (U q) p = mkU (fromIntegral p*q)

u_sqr :: U -> U
u_sqr x = u_pow x 2

split :: [a] -> ([a], [a])
split = foldr f ([], [])
  where
    f a (r1, r2) = (a : r2, r1)

ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s
                    then go s xs
                    else x : go (Set.insert x s) xs                  
evalFourier
  :: forall a. RealFloat a
  => [Complex a]
  -> [U]
  -> Writer (Sum Int) [Complex a]
evalFourier []  pts = pure $ 0 <$ pts
evalFourier [c] pts = pure $ c <$ pts
evalFourier coeffs pts = do
  let
    squares = ordNub $ u_sqr <$> pts
    (even_coeffs, odd_coeffs) = split coeffs
  even_values <- evalFourier even_coeffs squares
  odd_values  <- evalFourier odd_coeffs squares

  let
    square_map =
      Map.fromList
      . zip squares
      $ zip even_values odd_values
    
    eval1 :: U -> Writer (Sum Int) (Complex a)
    eval1 x = do
      let (ye, yo) = (square_map Map.! u_sqr x)
          r = ye + toComplex x * yo
      tell $ Sum 2
      pure r
  
  mapM eval1 pts

fft :: RealFloat a => [Complex a] -> ([Complex a], Int)
fft coeffs =
  second getSum 
  . runWriter
  . evalFourier coeffs
  . fmap (u_pow w)
  $ [0..n-1]
  where
    n = genericLength coeffs
    w = mkU (-1 % n)
