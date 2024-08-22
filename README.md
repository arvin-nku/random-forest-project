
# Random Forest Project

## Overview
This repository contains the implementation of various algorithms related to Random Forests, including Greedy CART algorithms, Bagging, and Random Forest algorithms. Additionally, a Shiny app has been developed to enable interactive use of these algorithms.

## Project Structure
- **R/**: Source code for various algorithms, including:
  - `greedy_cart.R`: Implementation of the Greedy CART algorithm.
  - `random_forest.R`: Implementation of the Random Forest algorithm.
  - Additional R scripts for Bagging, Pruning, and related functions.
- **Shiny/**: The Shiny app files that allow interactive exploration of the algorithms.
- **tests/**: Test cases to ensure the correctness of the implementation.
- **vignettes/**: Extended documentation and tutorials in R Markdown format.
- **man/**: Documentation files for the package functions.
- **NAMESPACE**: The NAMESPACE file for the R package.
- **DESCRIPTION**: Metadata about the package including title, version, author, and dependencies.
- **LICENSE**: License information for the package.
- **cran-comments.md**: Comments and notes related to CRAN submission.
- **CRAN-SUBMISSION**: Information regarding the package submission to CRAN.

## Installation

### Installing the Package from GitHub
```r
# Install devtools if you haven't already
install.packages("devtools")

# Install the random-forest-project package
devtools::install_github("arvin-nku/random-forest-project")
```

### Loading the Package
```r
library(rforests)
```

### Installing Additional Dependencies
Ensure all suggested packages are installed for full functionality:

```r
install.packages(c("DiagrammeR", "data.tree", "shiny"))
```

## Usage

### Greedy CART for Regression
```r
X1 <- runif(100, 0, 1)
X2 <- runif(100, 0, 1)
e <- rnorm(100, 0, 0.1)
Y <- X1^2 + X2 + e
data <- list(x = matrix(c(X1, X2), nrow = 2), y = Y)

result <- greedy_cart(x = c(X1, X2), y = Y, data = data, type = "reg")
print(result$tree)
```

### Random Forest for Classification
```r
X1 <- runif(100, 0, 1)
X2 <- runif(100, 0, 1)
e <- rnorm(100, 0, 0.1)
Y <- ifelse(X1 + X2 + e > 1, 1, 2)
data <- list(x = matrix(c(X1, X2), nrow = 2), y = Y)

rf_model <- random_forest(x = c(X1, X2), y = Y, data = data, type = "cla", B = 10, m = 1)
print(rf_model)
```

### Shiny App

To use the Shiny app, you can run the following:

```r
shiny::runApp("Shiny/")
```

## Contributors

- Lorenz Kiesel - random-forets algorithms
- Mikhail Volkov - bagging algorithms
- Arvin Nikou - greedy-cart algorithms
