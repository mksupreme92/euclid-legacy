# Euclid Geometry Kernel — Legacy / Version 0

This repository contains the original prototype of **Euclid**, a modular, type-safe geometry kernel written in Haskell.

It served as the foundation for early development, providing:
- Core vector and matrix algebra
- Euclidean and flat non-Euclidean metric support
- Geometry primitives (points, lines, parametric curves, planes/faces, parametric surfaces, & parametric volumes)
- Surface and volume meshes
- Unit-tested logic for 2D–5D geometry

## Why "Legacy"?

This version was intentionally frozen to make way for a complete architectural refactor in **Euclid Version 2**, which introduces:
- A cleaner typeclass-driven design
- Rust FFI for performance meshing and rendering
- A future-facing path toward DSL integration and dimensional generalization

## Status

✅ Stable  
✅ All unit tests pass  
🛑 No longer actively developed  

## Successor

The actively maintained version of Euclid is located at:  
👉 [https://github.com/mksupreme92/euclid](https://github.com/mksupreme92/euclid)

---

*Legacy, but foundational.* This codebase proved the architecture, and now the design is evolving.

🦅

