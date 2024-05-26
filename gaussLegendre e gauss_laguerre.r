# Instalação e carregamento da biblioteca necessária
# install.packages("pracma")
library(pracma)

# Funções de integração utilizando a biblioteca pracma
gauss_legendre <- function(integrando, n_pontos, a, b, ...){
  # Uso da função gaussLegendre da biblioteca pracma
  pontos <- gaussLegendre(n_pontos, a = a, b = b)
  integral <- sum(pontos$w * integrando(pontos$x, ...))
  return(integral)
}

gauss_laguerre <- function(integrando, n.pontos, ...){
  # Uso da função gaussLaguerre da biblioteca pracma
  pontos <- gaussLaguerre(n.pontos)
  integral <- sum(pontos$w * integrando(pontos$x, ...) / exp(-pontos$x))
  return(integral)
}

# Função exemplo
fx <- function(x, lambda) {
  lambda * ((exp(-(sqrt(x)))) / (sqrt(x)))
}

# Parâmetros editáveis pelo usuário
n_pontos <- 20  # Número de pontos para a quadratura
lambda <- 6     # Parâmetro da função fx

# Execução principal
main <- function(){
  resultado <- gauss_laguerre(integrando = fx, n.pontos = n_pontos, lambda = lambda)
  resultado_arredondado <- round(resultado, digits = 3)
  return(resultado_arredondado)
}

# Chamada da função principal
main()
