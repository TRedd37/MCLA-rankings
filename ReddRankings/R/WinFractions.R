#' @export
calculcateWinFraction <- function(df, method = "absolute"){
  winFraction <- switch(
    method,
    "absolute" = as.numeric(df$HomeGoals > df$AwayGoals),
    "relative" = df$HomeGoals / (df$AwayGoals + df$HomeGoals),
    "step"     = calculateStepWF(df),
    "logit"    = calculateLogitWF(df), 
    stop("Not a valid method")
  )
  return(winFraction)
}

#' @export
calculateStepWF <- function(df){
  wf <- bound(.5 + (df$HomeGoals - df$AwayGoals) *.1, 0, 1)
  return(wf)
}

#' @export
calculateLogitWF <- function(df, denominator = 2){
  differential <- df$HomeGoals - df$AwayGoals
  wf <- exp(differential / denominator) / (1 + exp(differential / denominator))
  return(wf)
}

#' @export
bound <- function(x, lb, ub){
  x[x < lb] <- lb
  x[x > ub] <- ub
  return(x)
}