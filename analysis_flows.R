source("historical_data.R")

library(copula)
library(VineCopula)

vals <- list(fJan, fFeb, fMar, fApr, fMay, fJun, fJul, fAug, fSep, fOct, fNov, fDec)


cops <- c()


for (i in 1:(length(vals) - 1)) {
  u <- pobs(as.matrix(cbind(vals[[i]], vals[[i + 1]])))[, 1]
  v <- pobs(as.matrix(cbind(vals[[i]], vals[[i + 1]])))[, 2]
  selectedCopula <- BiCopSelect(u, v, familyset = NA)
  cops <- c(cops, list(selectedCopula))
  if (i == 11) {
    u <- pobs(as.matrix(cbind(vals[[12]], vals[[1]])))[, 1]
    v <- pobs(as.matrix(cbind(vals[[12]], vals[[1]])))[, 2]
    selectedCopula <- BiCopSelect(u, v, familyset = NA)
    cop <-
      copulaFromFamilyIndex(selectedCopula$family,
                            selectedCopula$par,
                            selectedCopula$par2)
    cops <- c(cops, list(selectedCopula))
  }
}

u1 <- runif(1)
probs <- c(u1)
for (i in c(2:600)) {
  if (i == 2) {
    u_i_prev <- u1
  }
  v <- runif(1)
  j <- if (i %% 12 != 0) i %% 12 else 12
  u_i <- BiCopCDF(u_i_prev, v, obj = cops[[j]])
  probs <- c(probs, u_i)
  u_i_prev <- u_i
}
