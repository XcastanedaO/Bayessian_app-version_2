# Bayessian_app-version_2

This repository has the files needed to build conjugate models in a shiny application.

functions_aux_graph: It has some functions that allow to graph the posterior distribution depending on the prior parameters in the likelihood distributions.

This application has the graphic functions to plot the following scenarios:

- Normal model
  - Known mean and unknown variance
  - Unknown mean and known variance
  - Unknown variance and unknown mean with its prior distribution depending on variance
  - Mean’s prior distribution independent of variance’s prior distribution

- Poisson model

- Binomial model

It's allowed in this version to read data frames to graph the likelihood distribution.
