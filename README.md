A unified approach to understanding randomness in sport
================

A paper analyzing competitiveness across the four major sports.

To knit the paper:

1.  Use the **Compile PDF** button in RStudio, or
2.  Select **Tools -&gt; Shell...** from the menu and execute:

``` bash
make
```

Use with caution as these will overwrite the existing data!
-----------------------------------------------------------

<!--

To re-scrape the game results data:

```bash
make results
```

-->
To re-build the `bigfour` data:

``` bash
make bigfour
```

To re-do the simulations:

``` bash
make mcmc
```

Compile papers
--------------

To make the Sloan paper:

``` bash
# install dependencies for Ubuntu
# sudo apt get install texlive-xetex texlive-math-extra
make sloan2016.pdf
```

Note that this requires `xelatex` and the Cambria family of fonts installed.

To make the JASA paper:

``` bash
make jasa2016.pdf
```
