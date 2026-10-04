# `ii` and `lamb` are foreach loop variables bound only inside %dopar%/%do%
# blocks; R CMD check's static analysis can't see that binding.
utils::globalVariables(c("ii", "lam", "lamb"))
