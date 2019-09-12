### About
`rmed` allows for efficient estimation of the Siegel repeated median regression line, in <img src="https://latex.codecogs.com/gif.latex?O(n\log^2&space;n)" title="O(n\log^2 n)" /> time via [randomized algorithm](http://cvs.cs.umd.edu/~mount/Papers/alg98-repeated-median.pdf). It also adds functions for counting inversions in <img src="https://latex.codecogs.com/gif.latex?O(n\log&space;n)" title="O(n\log n)" /> time that may be of independent interest for rank-distance calculations (e.g. Kendall's Tau).


### Installation
`rmed` can be installed via GitHub:

```r
if (!require(devtools)) {
    install.packages('devtools')
}
devtools::install_github('erocoar/rmed')
```

### Usage
```r
siegel_estimator(x, y)
```
