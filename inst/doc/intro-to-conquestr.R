## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library("conquestr")

## -----------------------------------------------------------------------------
# set up
oldWd <- getwd()
myTmpDir <- tempdir()
setwd(myTmpDir)
file.copy(from = c(
  system.file("extdata", "ex1.cqc", package = "conquestr"),
  system.file("extdata", "ex1.dat", package = "conquestr"),
  system.file("extdata", "mysysfile.cqs", package = "conquestr")
  ), to = myTmpDir)

# run ConQuest
# ConQuestCall is not run here, as it requires a local install of ConQuest 
# ConQuestCall(cqc = file.path(myTmpDir, "ex1.cqc"), stdout = NULL) # searches for valid install of CQ
myExSys <- ConQuestSys(myCqs = file.path(myTmpDir, "mysysfile.cqs"))

# see the content of the sysfile
str(myExSys)

# tidy up
file.remove(
  list.files(myTmpDir, all.files = TRUE, full.names = TRUE, pattern = "^myi|^myS|ex1"),
  recursive=TRUE
)
setwd(oldWd)

## -----------------------------------------------------------------------------
# if no argument is provided to ConQuestSys, the example system file is read in by default.
myCqs <- ConQuestSys()

## -----------------------------------------------------------------------------
# search for objects named history in myCqs
searchConQuestSys("history", myCqs)

## ----pv-sim-------------------------------------------------------------------
set.seed(5309)
nStrata <- 32
nSchools <- nStrata * 2
nPerSchool <- 20
nPVs <- 10

# Sylvester construction of a 32 x 32 Hadamard matrix
hadamard <- matrix(1)
while (nrow(hadamard) < nStrata) hadamard <- rbind(cbind(hadamard, hadamard), cbind(hadamard, -hadamard))

school <- rep(seq_len(nSchools), each = nPerSchool)
stratum <- (school + 1) %/% 2
unit <- ifelse(school %% 2 == 1, 1, -1) # first or second school in the pair
n <- length(school)

pisaSim <- data.frame(
  school = school,
  sex = factor(sample(c("boy", "girl"), n, replace = TRUE)),
  escs = rnorm(nSchools, 0, 0.5)[school] + rnorm(n, 0, 0.8),
  W_FSTUWT = runif(n, 5, 50)
)
schoolEffect <- rnorm(nSchools, 0, 40)[school]
trueTheta <- 480 + 30 * pisaSim$escs + 12 * (pisaSim$sex == "girl") + schoolEffect + rnorm(n, 0, 70)
for (p in seq_len(nPVs)) {
  pisaSim[[paste0("PV", p, "READ")]] <- trueTheta + rnorm(n, 0, 25) # imputation noise
}

# Fay replicate weights: one school in each pair gets 1.5, the other 0.5
fayK <- 0.5
for (r in seq_len(nStrata)) {
  factorR <- ifelse(hadamard[stratum, r] * unit > 0, 2 - fayK, fayK)
  pisaSim[[paste0("W_FSTR", r)]] <- pisaSim$W_FSTUWT * factorR
}

pvs <- paste0("PV", seq_len(nPVs), "READ")
repWts <- paste0("W_FSTR", seq_len(nStrata))

## ----pv-describe--------------------------------------------------------------
pvDescribe(pisaSim, pvs, weight = "W_FSTUWT", repWeights = repWts,
           repMethod = "Fay", fayK = fayK, by = "sex")

## ----pv-se-check--------------------------------------------------------------
desc <- pvDescribe(pisaSim, pvs, weight = "W_FSTUWT", repWeights = repWts,
                   repMethod = "Fay", fayK = fayK)
all.equal(desc$se, sqrt(desc$samplingVar + (1 + 1 / desc$nPVs) * desc$imputationVar))

## ----pv-diff------------------------------------------------------------------
pvDiff(pisaSim, pvs, by = "sex", pairs = list(c("girl", "boy")),
       weight = "W_FSTUWT", repWeights = repWts, repMethod = "Fay", fayK = fayK)

## ----pv-cor-------------------------------------------------------------------
pvCor(pisaSim, pvs, "escs", weight = "W_FSTUWT", repWeights = repWts,
      repMethod = "Fay", fayK = fayK, combine = "fisherZ")

## ----pv-reg-------------------------------------------------------------------
pvRegression(pisaSim, pvs, ~ escs + sex, weight = "W_FSTUWT",
             repWeights = repWts, repMethod = "Fay", fayK = fayK)

## ----pv-sim-md----------------------------------------------------------------
trueMath <- 0.7 * (trueTheta - 480) + 470 + rnorm(n, 0, 60)
for (p in seq_len(nPVs)) {
  shared <- rnorm(n) # joint draw: reading and maths PV p share an error component
  pisaSim[[paste0("PV", p, "MATH")]] <- trueMath + 20 * shared + rnorm(n, 0, 15)
}
pisaSim$age <- round(rnorm(n, 189, 3.5))

measures <- list(
  read = paste0("PV", seq_len(nPVs), "READ"),
  math = paste0("PV", seq_len(nPVs), "MATH"),
  escs = "escs",
  age = "age"
)

## ----pv-cormat----------------------------------------------------------------
corMat <- pvCorMatrix(pisaSim, measures, weight = "W_FSTUWT", repWeights = repWts,
                      repMethod = "Fay", fayK = fayK, format = "matrix")
round(corMat$estimate, 3)
round(corMat$se, 3)

## ----pv-cormat-long-----------------------------------------------------------
pvCorMatrix(pisaSim, measures, weight = "W_FSTUWT", repWeights = repWts,
            repMethod = "Fay", fayK = fayK)[, c("dim1", "dim2", "estimate", "se",
                                                 "imputationVar", "nPVs")]

## ----pv-covmat----------------------------------------------------------------
covMat <- pvCovMatrix(pisaSim, measures, weight = "W_FSTUWT", repWeights = repWts,
                      repMethod = "Fay", fayK = fayK, format = "matrix")
round(covMat$estimate, 1)
pvVar(pisaSim, measures$read, weight = "W_FSTUWT", repWeights = repWts,
      repMethod = "Fay", fayK = fayK)$estimate

