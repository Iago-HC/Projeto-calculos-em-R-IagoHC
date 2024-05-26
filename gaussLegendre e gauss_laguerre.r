# gaussLegendre / gauss_laguerre
# install.packages("pracma")
library(pracma)

require(pracma)
gaussLegendre(n = 2, a = -1, b = 1)

gauss_legendre <- function(integrando, n_pontos, a, b, ...){
  pontos <- gaussLegendre(n_pontos, a = a, b = b)
  integral <- sum(pontos$w*integrando(pontos$x,...))
  return(integral)
}
gauss_laguerre <- function(integrando, n.pontos, ...){
  pontos <- gaussLaguerre(n.pontos)
  integral <- sum(pontos$w*integrando(pontos$x,...)
                  /exp(-pontos$x))
  return(integral)
}

fx <- function(x, lambda) {
  lambda * ((exp(-(sqrt(x)))) / (sqrt(x)))
}
                           
resultado = gauss_laguerre(integrando = fx, n.pontos = 20, lambda = 6)
resultado
round(resultado, digits = 3)
