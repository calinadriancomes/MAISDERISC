# R/05_diagnostics.R
diagnose_count_model <- function(model) {
  sim <- DHARMa::simulateResiduals(model, n = 1000)
  list(
    sim = sim,
    dispersion_test = DHARMa::testDispersion(sim),
    zero_inflation_test = DHARMa::testZeroInflation(sim)
  )
}

plot_residuals_dharma <- function(sim) {
  DHARMa::plotSimulatedResiduals(sim)
}