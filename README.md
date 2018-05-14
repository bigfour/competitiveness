A unified approach to understanding randomness in sport
================

A paper analyzing competitiveness across the four major sports.

Slides
------

-   [NESSIS 2017](http://bigfour.github.io/competitiveness/NESSIS_2017.html)
-   [UMass 2017](http://bigfour.github.io/competitiveness/umass2017.html)
-   [CMU 2017](http://bigfour.github.io/competitiveness/cmu2017.html)
-   [Fields Institute 2018](http://bigfour.github.io/competitiveness/fields2018.html)

To knit the paper:
------------------

1.  Use the **Compile PDF** button in RStudio, or
2.  Select **Tools -&gt; Shell...** from the menu and execute:

``` bash
make
```

Data wrangling
--------------

To re-build the `bigfour` data:

``` bash
make bigfour
```

Note that this operation will look for a set of proprietary data files in a `data_raw` folder. This folder is not present in this repository due to licensing restrictions. However, our version of the resulting `bigfour.rda` file is present in the repository. This will enable you to run our simulations using the code below.

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

To make the journal paper:

``` bash
make aoas2017.pdf
```
