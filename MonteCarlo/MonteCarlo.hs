module MonteCarlo
    (
      areaMC
    , areaMCIO
    , extremeMC
    , extremeMCIO
    , getRands
    , getRandPairs
    , Extreme(Maximum, Minimum)
    ) where

import System.Random
import Data.List

{- Types and synonyms -}
data Extreme        = Maximum | Minimum deriving (Eq, Ord)
type Pair a         = (a, a)
type RandomBundle a = (Pair a, StdGen)

{- Helpers for random numbers -}
getRands :: (Num a, Random a) => Integer -> RandomBundle a -> [a]
getRands n (range, gen) = genericTake n $ randomRs range gen

getRandPairs :: (Num a, Random a) => Integer -> RandomBundle a -> RandomBundle a -> [Pair a]
getRandPairs n fstOptions sndOptions = zip (getRands n fstOptions)
                                           (getRands n sndOptions)

{- Estimating integrals -}
belowFunction :: (Real a) => (a -> a) -> Pair a -> Bool
belowFunction func point = snd point < func (fst point)

hits :: (Real a) => (a -> a) -> [Pair a] -> Int
hits func rands = length . filter (==True) $ map (belowFunction func) rands

areaMC :: (Double -> Double) -> Pair Double -> Pair Double -> [Pair Double] -> Integer -> Double
areaMC func (xi, xj) (yi, yj) rands num = fromIntegral (hits func rands)
                                        * rectArea / fromIntegral num
                                        where rectArea = (xj-xi) * (yj-yi)

areaMCIO :: (Double -> Double) -> Pair Double -> Pair Double -> Integer -> IO Double
areaMCIO func xRange yRange num = do
    g1 <- getStdGen
    g2 <- newStdGen

    let rands = getRandPairs num (xRange, g1) (yRange, g2)
    return $ areaMC func xRange yRange rands num

{- Estimating local extrema -}
extremeMC :: (Double -> Double) -> [Double] -> Extreme -> Double
extremeMC func rands option
    | option == Maximum = maximum ys
    | option == Minimum = minimum ys
        where ys = map func rands

extremeMCIO :: (Double -> Double) -> Pair Double -> Extreme -> Integer -> IO Double
extremeMCIO func xRange option num = do
    g1 <- getStdGen

    return $ extremeMC func (getRands num (xRange, g1)) option
