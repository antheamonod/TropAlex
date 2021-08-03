# TropAlex: Alexandrov Curvature in the Tropical Projective Torus with the Tropical Metric

This repository provides code to implement the numerical experiments and run the simulations in [Améndola and Monod (2021)](https://arxiv.org/abs/2105.07423), which studies classical Alexandrov curvature in the tropical projective torus with the tropical metric (generalized Hilbert projective metric).  Alexandrov curvature is a generalization of classical Riemannian sectional curvature to more general metric spaces; it is determined by a comparison of triangles in an arbitrary metric space to corresponding triangles in Euclidean space.  In the polyhedral setting of tropical geometry, triangles are a combinatorial object, which adds a combinatorial dimension to the study.  Here, the edges of tropical triangles are given by tropical line segments.

The code provided in this repository is for the numerical study of Alexandrov curvature in the plane and in higher dimensions.  In the plane, there are five combinatorially distinct tropical triangles.  This repository contains functions that randomly sample tropical triangles and returns their Alexandrov curvature and combinatorial type, and functions that allow for random sampling of tropical triangles by combinatorial type.

For further detail on Alexandrov curvature, the tropical metric and the tropical projective torus, please see [Améndola and Monod (2021)](https://arxiv.org/abs/2105.07423) and the references therein.

## The R Environment
R is a widely used, free, and open source software environment for statistical computing and graphics.  The most recent version of R can be downloaded from the [Comprehensive R Archive Network (CRAN)](http://cran.r-project.org/).  CRAN provides precompiled binary versions of R for Windows, MacOS, and select Linux distributions that are likely sufficient for many users' needs.  Users can also install R from source code; however, this may require a significant amount of effort.  For specific details on how to compile, install, and manage R and R-packages, refer to the manual [R Installation and Administration](http://cran.r-project.org/doc/manuals/r-release/R-admin.html).

### R Packages Required for Sampling Tropical Triangles
Randomly sampling tropical triangles by combinatorial type requires the installation of the following R library:

* [hitandrun](https://cran.r-project.org/web/packages/hitandrun/index.html): "Hit and Run" and "Shake and Bake'' for Sampling Uniformly from Convex Shapes

The easiest method to install this package is with the following example command entered in an R shell:

    install.packages("hitandrun", dependecies = TRUE)

Alternatively, it is also possible to [install R packages from the command line](http://cran.r-project.org/doc/manuals/r-release/R-admin.html#Installing-packages).

## Description of R Scripts

The `Computing_Curvature` directory contains the following set of R scripts:
* `Curvature2D.R` which computes the Alexandrov curvature of tropical triangles in the 3-dimensional tropical projective torus (plane).
* `CurvatureND.R` computes the Alexandrov curvature of tropical triangles in the n-dimensional tropical projective torus.
* `typeFunction.R` returns the combinatorial type of tropical triangle in the plane; there are five possibilities, please see the reference by Améndola and Monod (2021) for more details.
* `ultrametricTrees.R` simulates sets of equidistant phylogenetic trees, where the distance from the root to every leaf is constant, and creates triangles where every tree is a vertex and computes its Alexandrov curvature.

The `Sampling_Triangles` directory contains the following set of R scripts:
* `experiments_n.R` samples tropical triangles by type and returns the proportion of their curvature type as a percentage.

### Running Scripts and Reproducing Results

To replicate results similar to those presented in Améndola and Monod (2021):
1. Make sure that the current working directory contains all elements from this TropAlex repo;
2. Run each R script separately.

## Relevant Citations
Carlos Améndola and Anthea Monod. An Invitation to Tropical Alexandrov Curvature. arXiv:2105.07423.

## Credit

Beatrice Matteo contributed to this repository in partial fulfillment of requirements for the degree of MSc in Statistics at Imperial College London.

### Questions and Feedback
For questions or concerns with this repository, please contact [Beatrice Matteo](mailto:beatrice.matteo20@imperial.ac.uk) or [Anthea Monod](mailto:a.monod@imperial.ac.uk).

We appreciate any feedback you may have with our repository and instructions.
