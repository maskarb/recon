source("historical_data.R")

library(copula)
library(VineCopula)

flows <- list(fJan, fFeb, fMar, fApr, fMay, fJun, fJul, fAug, fSep, fOct, fNov, fDec)
evaps <- list(eJan, eFeb, eMar, eApr, eMay, eJun, eJul, eAug, eSep, eOct, eNov, eDec)
precs <- list(pJan, pFeb, pMar, pApr, pMay, pJun, pJul, pAug, pSep, pOct, pNov, pDec)

lists <- c(list(flows), list(evaps), list(precs))

cops <- c()

for (j in 1:length(lists)) {
  vals <- lists[[j]]
  temp <- c()
for (i in 1:(length(vals)-1)) {
  u <- pobs(as.matrix(cbind(vals[[i]],vals[[i+1]])))[,1]
  v <- pobs(as.matrix(cbind(vals[[i]],vals[[i+1]])))[,2]
  selectedCopula <- BiCopSelect(u,v,familyset=NA)
  temp <- c(temp, list(selectedCopula))
  if (i == 11) {
    u <- pobs(as.matrix(cbind(vals[[12]],vals[[1]])))[,1]
    v <- pobs(as.matrix(cbind(vals[[12]],vals[[1]])))[,2]
    selectedCopula <- BiCopSelect(u,v,familyset=NA)
    cop <- copulaFromFamilyIndex(selectedCopula$family, selectedCopula$par, selectedCopula$par2)
    temp <- c(temp, list(selectedCopula))
  }
}
  cops <- c(cops, list(temp))
}

