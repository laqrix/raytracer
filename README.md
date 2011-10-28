Simple raytracer in Scheme

Implemented using Petite Chez Scheme 8.1 on a MacBook Pro.

To run:
> petite main.ss simple.ss

This will load 'simple.ss' for the scene. Output will go to 'simple.tga'.

---

Features:

  - Objects: sphere, plane, general quadrics, general polyhedra, cube, 
    tetrahedron, octahedron, icosahedron
  - CSG: union, intersect, difference
  - Shaders: constant, matte, metal, shiny-metal, plastic, stripes, checker,
    mirror, marble, granite, wood, glow, screen (wireframe)
  - Lights: ambient, distant, point, spot

Less stable features:

  - Opacity and Volume shaders
  - Texture mapping
  - Normal mapping
  - Displacement shaders
  - Glass shader
  - show-st, show-xyz
  - Antialiasing
  - Exposure (gain and gamma)

Things to do:

- Camera blur / depth of field

- Shadows thru transparent objects need work.

- Take a look at performance. It can always be faster.

- More objects
  - cylinder
  - cone

- More lights
  - "uber" light

- More shaders
  - shiny-plastic
  - painted-plastic
  - depth-cue
  - fog
  - smoke

- Build a physics engine to help generate raytracer scenes. Imagine
  modelling marbles dropped into a glass then rendering 1 or more
  frames in time.
