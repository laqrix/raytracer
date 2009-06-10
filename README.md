Simple raytracer in Scheme

Implemented using Petite Chez Scheme 7.4b on a MacBook Pro.

To run:
> petite main.ss simple.ss

This will load 'simple.ss' for the scene. Output will go to 'simple.tga'.

---

Things to do:

- Take a look at performance. It can always be faster.

- More objects
  - cylinder
  - cone
  - polyhedra - octahedron, icosahedron

- More lights
  - spot light
  - "uber" light

- Shader "library"
  - matte
  - metal
  - shiny metal
  - plastic
  - stripes
  - checker
  - marble
  - wood
  - mirror
  - glass
  - granite
  - texture

- Constructive Solid Geometry
  - union
  - intersection
  - difference
