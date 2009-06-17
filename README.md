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
  - Shaders: matte, metal, shiny-metal, plastic, stripes, checker,
             mirror, marble, granite, wood
  - Lights: ambient, distant, point, spot

Less stable features:

  - Texture mapping
  - Normal mapping


Things to do:

- Take a look at performance. It can always be faster.

- More objects
  - cylinder
  - cone

- More lights
  - "uber" light

- More shaders
  - glass
