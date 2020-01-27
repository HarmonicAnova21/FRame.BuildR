pv <- function(FV, r, n) {

  if(!is.atomic(FV)) {
    stop('FV must be an atomic vector')
  }

  present_value <- FV / (1 + r)^n
  round(present_value, 2)
}

formals(pv)
$FV
$r
$n

body(pv)
{
  present_value <- FV/(1 + r)^n
  round(present_value, 2)
}

environment(pv)
<environment: R_GlobalEnv>
