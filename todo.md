# 🧱 Project Euclid TODO

## 🔧 Core Infrastructure

- [x] `vector.hs` — Vector math (add, sub, norm, dot, etc.)
- [x] `matrix.hs` — Matrix operations
- [x] `metric.hs` — Inner products, distance, etc.
- [x] `space.hs` — Space definitions with dimension + metric


## 📐 Geometry Primitives

- [x] `point.hs`
- [x] `line.hs`
- [x] `lineSegment.hs`
- [x] `plane.hs`
- [x] `face.hs` — Polygon face with convexity, winding, etc.
- [ ] `curve.hs` — Parametric curves, splines
    - [x] linearCurve tests for 2D–5D, negatives, irrationals, and extrapolation
    - Implemented and validated linearCurve over extended domain
    - Tests include negative coords, irrational points (π, e, √2), and extrapolated t
    - Verified results with ✅ status across all scenarios
    - [] Parametric Nonlinear Curves: Begin considering Bezier, Hermite, or B-spline support
        - testing produces error; need to refactor
- [ ] `surface.hs` — Surface logic (parametric + mesh support)
- [ ] `volume.hs` — Volumetric cells, bounded regions

## ⚙️ Higher-Level Operations

- [ ] `transform.hs` — Rotations, translations, projections, scaling
- [ ] `boolean.hs` (optional) — Boolean operations (union, subtract, intersect)
- [ ] `collision.hs` (optional) — Collision and intersection logic

## 🔢 Numerical Methods

- [x] `numerics/integrate.hs` — Runge-Kutta 4 (RK4) and general-purpose ODE integration
- [ ] `numerics/differentiate.hs` — Numerical differentiation (finite difference, etc.)
- [ ] `numerics/solve.hs` (future) — Linear/nonlinear equation solvers
- [ ] `numerics/interpolate.hs` (future) — Lagrange, spline, and parametric interpolation

## 🧩 Language Layer

- [ ] `elementa.hs` — Elementa DSL language layer (syntax + structure)
- [ ] `parser.hs` — Parser for Elementa DSL

## 🖼️ Rendering and Visualization

- [ ] `render.hs` — Rendering engine (ASCII, OpenGL/WebGL, SVG, etc.)

## 🏗️ CAD Layer: Higher-Level Constructs

_Not considered primitives — construction macros for modeling._

- [ ] `construct/cube.hs`
- [ ] `construct/sphere.hs`
- [ ] `construct/cylinder.hs`
- [ ] `construct/torus.hs`
- [ ] `construct/polyhedron.hs`
