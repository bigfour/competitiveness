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

To re-scrape the game results data:

``` bash
make results
```

To re-wrangle the moneyline data:

``` bash
make mline
```

To re-build the `bigfour` data:

``` bash
make bigfour
```

To re-do the simulations:

``` bash
make mcmc
```
