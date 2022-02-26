
get_e_tau <- function(tau, rho, lambda, mu) {
  return()
}

# E(\tau) = 
# 1 - \frac{e^{\int_0^\tau \lambda(u) - \mu(u) du}}
#          {\frac{1}{f} + \int_0^\tau e^{\int_0^s \lambda(u) - \mu(u) du} \lambda_s ds} 
  
get_m_tau <- function(tau, m0, lambda, mu) {
  growth_func <- function(u) return(lambda(u) * (e(u) - 1))
  growth_integral <- integrate(growth_func, lower = 0, upper = tau)$value
  return(m_0 * exp(growth_integral))
}

lambda <- function(tau) {
  return(rep(0.8, length(tau)))
}

mu <- function(tau) {
  return(rep(0.2, length(tau)))
}

r <- function(tau) {
  return(lambda(tau) - mu(tau))
}

#r(0:15)

#output <- integrate(r, lower = 0, upper = 2)
