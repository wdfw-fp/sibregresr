#  functions to build DLM models without environmental covariates ####

#   # Constant Intercept-only model
constIntOnly <- function(parm,x.mat){
  parm <- exp(parm)
  return( dlmModReg(X=x.mat, dV=parm[1], dW=0, addInt=FALSE))
}

# Random walk- time-varying intercept only
tvIntOnly <- function(parm,x.mat){
  parm <- exp(parm)
  return( dlmModReg(X=x.mat, dV=parm[1], dW=parm[2], addInt=FALSE))
}

# Time-varying Intercept and Slope
tvIntSlope <- function(parm, x.mat){
  parm <- exp(parm)
  return(dlmModReg(X=x.mat, dV=parm[1], dW=c(parm[2], parm[3] )))
}

# Time-varying slope
tvSlope <- function(parm, x.mat){
  parm <- exp(parm)
  return( dlmModReg(X=x.mat, dV=parm[1], dW=c(0, parm[2] )))
}

# Linear regression constant slope/intercept
constLM <- function(parm, x.mat){
  parm <- exp(parm)
  return( dlmModReg(X=x.mat, dV=parm[1], dW=c(0, 0)))
}

# Time-varying Cohort ratio (i.e., zero intercept model)
tvCRzeroInt <- function(parm, x.mat){
  parm <- exp(parm)
  return( dlmModReg(X=x.mat, dV=parm[1], dW=c(parm[2]), addInt=FALSE))
}

# Constant cohort ratio (i.e., zero intercept model)
constCRzeroInt <- function(parm, x.mat){
  parm <- exp(parm)
  return(dlmModReg(X=x.mat, dV=parm[1],dW=c(0), addInt=FALSE))
}

# Time-varying intercept, constant slope
tvInt <- function(parm, x.mat){
  parm <- exp(parm)
  return(dlmModReg(X=x.mat, dV=parm[1],dW=c(parm[2], 0)))
}


