Simple raytracer in Scheme

Implemented using Petite Chez Scheme 7.4b on a MacBook Pro.

To run:
> petite main.ss simple.ss

This will load 'simple.ss' for the scene. Output will go to 'simple.tga'.

---

Features:

  - Objects: sphere, plane, general quadrics, general polyhedra, cube, 
             tetrahedron, octahedron, icosahedron
  - CSG: union, intersect, difference
  - Shaders: constant, matte, metal, shiny-metal, plastic, stripes, checker,
             mirror, marble, granite, wood, glow
  - Lights: ambient, distant, point, spot

Less stable features:

  - Opacity and Volume shaders
  - Texture mapping
  - Normal mapping
  - Displacement shaders

Things to do:

- Take a look at performance. It can always be faster.

- More objects
  - cylinder
  - cone

- More lights
  - "uber" light

- More shaders
  - show-st
  - show-xyz
  - screen (wireframe)
  - shiny-plastic
  - painted-plastic
  - glass
  - depth-cue
  - fog
  - smoke
