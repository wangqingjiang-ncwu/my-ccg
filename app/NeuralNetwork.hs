-- Copyright (c) 2019-2026 China University of Water Resources and Electric Power
-- All rights reserved.

module NeuralNetwork (
    newW,     -- (Int, Int) -> IO ()
    sigmoid,  -- (Floating e, Container c e) => c e -> c e
    loss,     -- Vector Double -> Vector Double -> Double
    linear',  -- Matrix Double -> Matrix Double -> Matrix Double
    sigmoid', -- Matrix Double -> Matrix Double -> Matrix Double
    loss',    -- Vector Double -> Vector Double -> Vector Double
    forward,  -- Matrix Double -> Matrix Double -> [Matrix Double]
--    descend,  -- (Matrix Double -> Matrix Double) -> Int -> Double -> Matrix Double -> [Matrix Double]
--    grad,     -- (Matrix Double, Vector Double) -> Matrix Double -> Matrix Double
--    test,     -- IO ()
    ) where

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra as LA

-- New weights
newW :: (Int, Int) -> IO (Matrix Double)
newW (nin, nout) = do
    let k = sqrt (1.0 / fromIntegral nin)
    w <- randn nin nout
    return (cmap (k *) w)

-- Forward transformations

sigmoid :: (Floating e, Container c e) => c e -> c e
sigmoid = cmap f
    where
      f x = recip $ 1.0 + exp (-x)

loss :: Matrix Double -> Matrix Double -> Double
loss y tgt = sumElements $ cmap (^2) diff
  where
    diff = y - tgt

-- Their gradients

linear' :: Matrix Double -> Matrix Double -> Matrix Double
linear' x dy = cmap (/ m) (tr' x LA.<> dy)
  where
    m = fromIntegral $ rows x    -- Number of rows

sigmoid' :: Matrix Double -> Matrix Double -> Matrix Double
sigmoid' x dY = dY * y * (ones - y)
  where
    y = sigmoid x
    ones = (rows y) >< (cols y) $ repeat 1.0

loss' :: Matrix Double -> Matrix Double -> Matrix Double
loss' y tgt =
  let diff = y - tgt
  in cmap (* 2) diff

-- Building NN
forward :: Matrix Double -> Matrix Double -> [Matrix Double]
forward x w =
  let h = x LA.<> w
      y = sigmoid h
  in [h, y]

{-

descend :: (Matrix Double -> Matrix Double) -> Int -> Double -> Matrix Double -> [Matrix Double]
descend gradF iterN gamma x0 = take iterN (iterate step x0)
  where
    step x = x - gamma * (gradF x)

grad :: (Matrix Double, Matrix Double) -> Matrix Double -> Matrix Double
grad (x, y) w1 = dW1
  where
    [h, y_pred] = forward x w1
    dE = loss' y_pred y
    dY = sigmoid' h dE
    dW1 = linear' x dY

test = do
  dta <- loadMatrix "iris_x.dat"
  tgt <- loadMatrix "iris_y.dat"

  let (nin, nout) = (4, 3)

  w1_rand <- newW (nin, nout)

  let epochs = 500
  let w1 = last $ descend (grad (dta, tgt)) epochs 0.01 w1_rand

  let [_, y_pred0] = forward dta w1_rand
  let [_, y_pred] = forward dta w1

  putStrLn $ "Initial loss " ++ show (loss y_pred0 tgt)
  putStrLn $ "Loss after training " ++ show (loss y_pred tgt)

  putStrLn "Some predictions by an untrained network:"
  print $ takeRows 5 y_pred0

  putStrLn "Some predictions by a trained network:"
  print $ takeRows 5 y_pred

  putStrLn "Targets"
  print $ takeRows 5 tgt
-}
