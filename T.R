# private tool for asset allocation research
# by Jian Tang 2015
# all rights reserved
# packages needed to use this module:
# lpSolveAPI 

##################################################################################
# some convenient functions
##################################################################################

splstr <- function(s, delim = ',')
{
  # split a string and return a vector
  return(unlist(strsplit(s,delim)))
}

isnone <- function(var) {
  # if var is a non-valid object then return true, such as NA,NULL,FALSE
  if (len(var) == 0)
    return(TRUE)
  if (len(var) == 1) {
    if(is.na(var) || is.null(var) || var == FALSE || var == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

iif <- function (cond, truepart, falsepart) {
  # if cond is TRUE then return true part else return falsepart
  return(ifelse(isnone(cond), falsepart, truepart))
}
  


######## linear programming ######
optimize.LP <- function
(
  direction,           # "min" or "max"
  objective.vec,       # objective function
  const.mat,           # constraint matrix, each row is a constraint
  const.dir,           # direction, "=", ">=" "<=" etc
  const.rhs,           # rhs
  binary.vec  = NA,    # index of which vars are binary
  integer.vec = NA,    # index of which vars are integers
  lb     = 0,          # lower bound
  lb.vec = NA,         # what vars to set lower bound
  ub     = +Inf,       # upper bound
  ub.vec = NA          # what vars to set upper bound
)
{
  # linear programming tool with bounds and binary
  # a wrapper on lpSolveAPI, so requires package lpSolveAPI
  
  n <- len(objective.vec)
  iDim <- dim(const.mat) 
  m <- iDim[1]
  # n variables, m rows of constraints
  
  lprec <- make.lp(m, n)
  set.objfn(lprec, objective.vec)
  lp.control(lprec, sense = direction)
  # set constraints
  for (i in 1:ncol(const.mat)) {
    set.column(lprec, i, const.mat[, i])
  }
  
  # set constraint type
  set.constr.type(lprec, const.dir)
  
  # set rhs
  set.rhs(lprec, const.rhs)
  
  # set binary
  if(!isnone(binary.vec)) {
    for(i in binary.vec) {
      set.type(lprec, i, "binary")
    }
  }
  
  # set integer
  if(!isnone(integer.vec)) {
    for(i in integer.vec) {
      set.type(lprec, i, "integer")
    }
  }
  
  # set bounds  
  if(!isnone(lb.vec)) {
    if(len(lb) == 1) lb = rep(lb, len(lb.vec))
    set.bounds(lprec, lower = lb, columns = lb.vec)
  }
  
  if(!isnone(ub.vec)) {
    if(len(ub) == 1) ub = rep(ub, len(ub.vec))
    set.bounds(lprec, lower = ub, columns = ub.vec)
  }
 
  # ready to optimize
  print(lprec)
  
  status <- solve(lprec)
  if(status==0) {
    cat("LP Optimizing Successful.\n")
  } else {
    cat("LP Optimizing Failed.\n")
    rm(lprec)
    return(NULL)
  }
  
  # now get results
  rst     <- list()
  rst$Obj <- get.objective(lprec)
  rst$Sol <- get.variables(lprec)
  rst$Con <- get.constraints(lprec)
  
  # free memory
  rm(lprec)
  
  return(rst)
} #END solve.LP



