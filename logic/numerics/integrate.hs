-- File: logic/numerics/integrate.hs
module Logic.Numerics.Integrate (rk4) where

import Logic.Space.Vector (Vector, vectorAdd, scalarMul)  -- assumes existing vector ops

-- | Runge-Kutta 4th order integrator
-- f: derivative function (t, y) -> dy/dt
-- y0: initial value
-- t0: initial time
-- h: timestep
-- returns next value y1
rk4 :: (Double -> Vector -> Vector) -> Double -> Vector -> Double -> Vector
rk4 f t0 y0 h =
  let
    k1 = f t0 y0
    k2 = f (t0 + h/2) (y0 `vectorAdd` scalarMul (h/2) k1)
    k3 = f (t0 + h/2) (y0 `vectorAdd` scalarMul (h/2) k2)
    k4 = f (t0 + h)   (y0 `vectorAdd` scalarMul h k3)
  in
    y0 `vectorAdd` scalarMul (h/6) (k1 `vectorAdd` scalarMul 2 (k2 `vectorAdd` k3) `vectorAdd` k4)