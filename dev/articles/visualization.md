# Visualizing Euler diagrams with eulerr

Once we have ascertained that our Euler diagram fits well, we can turn
to visualizing the solution. For this purpose, **eulerr** relies on the
**grid** graphics system (R Core Team 2017) and offers intuitive and
granular control over the output.

Plotting the ellipses is straightforward using the parametrization of a
rotated ellipse,

$$\begin{bmatrix}
x \\
y
\end{bmatrix} = \begin{bmatrix}
{h + a\cos\theta} \\
{k + b\sin\theta}
\end{bmatrix},$$

where $\theta \in \lbrack 0,2\pi\rbrack,\quad a,b > 0$.

Most users will also prefer to label the ellipses and their
intersections with text and this, however, is considerably more
involved.

### Labeling

Labeling the ellipses is complicated since the shapes of the
intersections often are irregular, lacking well-defined centers; we know
of no analytical solution to this problem. Instead, **eulerr** relies on
the **polylabelr** package (Larsson 2018), which was created by the
author. It provides a simple wrapper for the **polylabel** (Mapbox 2018)
C++ library from *Mapbox*.

### Aesthetics

Euler diagrams display both quantitative and qualitative data. The
quantitative aspect is the quantities or sizes of the sets depicted in
the diagram and is visualized by the relative sizes, and possibly the
labels, of the areas of the shapes—this is the main focus of this paper.
The qualitative aspects, meanwhile, consist of the mapping of each set
to some quality or category, such as having a certain gene or not. In
the diagram, these qualities can be separated through any of the
following aesthetics:

- color,
- border type,
- text labeling,
- transparency,
- patterns,

or a combination of these. The main purpose of these aesthetics is to
separate out the different ellipses so that the audience may interpret
the diagram with ease and clarity.

Among these aesthetics, the best choice (from a viewer perspective)
appears to be color (Blake 2016), which provides useful information
without extraneous chart junk.

The issue with color, however, is that it cannot be perceived perfectly
by all. Eight percent of men and 0.4% of women in European Caucasian
countries, for instance, suffer the most common form, red–green color
deficiency. Moreover, color is often printed at a premium in scientific
publications and adds no utility to a diagram of two shapes.

For these reasons, **eulerr** defaults to distinguishing ellipses with
color using a manually tuned color palette.

``` r
set.seed(2)
library(eulerr)
con <- c(A = 1, B = 1, C = 1, D = 1, E = 1, F = 1, G = 1, H = 1,
         "A&B" = 0.2, "B&C" = 0.2, "C&D" = 0.2, "D&E" = 0.2, "E&F" = 0.2,
         "F&G" = 0.2, "G&H" = 0.2)
plot(euler(con), labels = as.character(1:8))
```

![The eight first colors of the default color
palette](visualization_files/figure-html/colorexamle-1.png)

The eight first colors of the default color palette

## Normalizing dispersed layouts

If there are disjoint clusters of ellipses, the optimizer will often
spread these out more than is necessary, wasting space in our diagram.
To tackle this, we use a SKYLINE-BL rectangle packing algorithm (Jylänki
2010) designed specifically for **eulerr**. In it, we surround each
ellipse cluster with a bounding box, pack these boxes into a bin of
appropriate size and aspect ratio, and adjust the coordinates of the
ellipses in the clusters to compact our diagram. As a bonus, this
increases the chance of having similar layouts for different function
calls.

## References

Blake, Andrew. 2016. “The Impact of Graphical Choices on the Perception
of Euler Diagrams.” {Ph.D.} dissertation, Brighton, UK: Brighton
University.

Jylänki, Jukka. 2010. “A Thousand Ways to Pack the Bin – a Practical
Approach to Two-Dimensional Rectangle Bin Packing.”

Larsson, Johan. 2018. “polylabelr: Find the Pole of Inaccessibility
(Visual Center) of a Polygon.”

Mapbox. 2018. “polylabel: A Fast Algorithm for Finding the Pole of
Inaccessibility of a Polygon (in JavaScript and C++).” Mapbox.

R Core Team. 2017. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.
