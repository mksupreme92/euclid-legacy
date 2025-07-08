# Euclid Geometry Kernel

 euclid/
├── main.hs                    -- Entry point / demo / CLI tool
├── functions/
│   ├── space.hs              -- Definitions for Space, Metric, Vector, Matrix
│   ├── metric.hs             -- Metric typeclass and instances
│   ├── vector.hs             -- Vector operations (add, dot, norm, etc.)
│   ├── matrix.hs             -- Matrix operations (mult, transpose, etc.)
│   └── geometry/
│       ├── point.hs         -- Points as objects in a space
│       ├── line.hs          -- Lines defined by point + direction
│       ├── plane.hs         -- Planes defined by point + normal
│       └── curve.hs         -- (Future) Parametric curves and splines