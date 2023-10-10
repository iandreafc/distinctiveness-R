# Distinctiveness Centrality - R
An R package to calculate Distinctiveness Centrality in social networks. 
For the [Python version please see here](https://github.com/iandreafc/distinctiveness).

Distinctiveness Centrality is a set of network metrics that attribute larger importance to distinctive connections. This provides an alternative view to that of some established metrics, which identify central nodes based on their brokerage power, the number and weight of their connections, and their ability to quickly reach all other nodes.

Formulas and full descriptions are provided in [this paper](https://doi.org/10.1371/journal.pone.0233276).

## Installation
```R
install.packages("distinctiveness")
```

## Quick Start and Tutorials
- A quick-start demo is in [this notebook](DCDemoR.ipynb).

## Please cite as
Fronzetti Colladon, A., & Naldi, M. (2020). Distinctiveness Centrality in Social Networks. *PLoS ONE*, *15*(5), e0233276. [https://doi.org/10.1371/journal.pone.0233276](https://doi.org/10.1371/journal.pone.0233276)

## Contributors
This package was originally developed by [lcmaas4](https://github.com/lcmaas4), who rewrote the pre-existing Python package into R.

## License MIT
Copyright 2023 Andrea Fronzetti Colladon.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
