# 🧱 Euclid Project TODO

## 🔧 Core Infrastructure

- [x] `vector.hs` — Vector math (add, sub, norm, dot, etc.)
- [x] `matrix.hs` — Matrix operations
- [x] `metric.hs` — Inner products, distance, etc.
- [x] `space.hs` — Space definitions with dimension + metric

## 🧠 Abstract Mathematical Structures

- [ ] `topology.hs` — Open sets, neighborhoods, continuity (abstract topology)
- [ ] `manifold.hs` — Charts, atlases, transition maps (geometry-aware)

## 📐 Geometry Primitives

- [x] `point.hs`
- [x] `line.hs`
- [x] `lineSegment.hs`
- [x] `plane.hs`
- [x] `face.hs` — Polygon face with convexity, winding, etc.
- [ ] `curve.hs` — Parametric curves, splines
- [ ] `geodesic.hs` — Metric-aware geodesics and shortest paths
- [ ] `surface.hs` — Surface logic (parametric + mesh support)
- [ ] `volume.hs` — Volumetric cells, bounded regions

## ⚙️ Higher-Level Operations

- [ ] `transform.hs` — Rotations, translations, projections, scaling
- [ ] `boolean.hs` (optional) — Boolean operations (union, subtract, intersect)
- [ ] `collision.hs` (optional) — Collision and intersection logic

## 🧩 Language, Parsing, and Rendering

- [ ] `elementa.hs` — Elementa DSL language layer (syntax + structure)
- [ ] `parser.hs` — Parser for Elementa DSL
- [ ] `render.hs` — Rendering engine (ASCII, OpenGL/WebGL, SVG, etc.)

## 🏗️ CAD Layer: Higher-Level Constructs

_Not considered primitives — construction macros for modeling._

- [ ] `construct/cube.hs`
- [ ] `construct/sphere.hs`
- [ ] `construct/cylinder.hs`
- [ ] `construct/torus.hs`
- [ ] `construct/polyhedron.hs`
