Simple raytracer in Scheme

Implemented using Petite Chez Scheme 7.4b on a MacBook Pro.

To run:
> petite main.ss
>
> (x)

This will load 'simple.ss' for the scene. The camera is currently hardcoded.

Output will go to 'output.tga'.

---

Things to do:

- Take a look at performance. It can always be faster.

- More objects
  - plane
  - cylinder
  - cone
  - polyhedra - cube, tetrahedron, octahedron, icosahedron

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
