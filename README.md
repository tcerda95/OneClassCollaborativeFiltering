# One Class Collaborative Filtering

## Overview

This project presents 2 algorithms implemeting diverse recommendation models suited for a one class setting.

The algorithms presented are:

* Weighted Alternating Least Squares
* Fixed point solution

The baseline models presented are:

* ZAM
* ZAN

The one class models presented are:

* wZAN
* iZAN
* wiZAN
* wiZAN-Dual (only fixed point algorithm)

## Models and Algorithms

Two traits (interfaces) are defined for each algorithm. The methods defined in such traits represent the models. Hyper-parameters aside, a one-class recommending model receives the user-item relationship matrix (R) as input and returns the trained user (U) and item (V) latent feature matrices. 

R can be be approximated by U * V.t. In the RecommendationUtils inside the utility package an example for obtaining the recommendations for just a single user is provided.

## Evaluations

Inside the evaluation package are examples for measuring differente characteristics of the models and algorithms such as accuracy and efficiency. Moreover, examples of reading inputs and training the models are provided. The metrics provided for accuracy evaluation are MAP and HLU.

## Input file format

The input files are .csv files representing sparse matrices. That is, in the first line the number of rows and columns are provided and the following two lines contain the positions where a positive feedback exists. For example:

```
3,3
0,0,1,2
0,1,2,2
```

would represent the following R matrix:

```
1 1 0
0 0 1
0 0 1
```

A utility method is provided for reading such matrices as sparse matrices.
