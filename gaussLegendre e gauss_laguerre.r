# Instalação e carregamento da biblioteca necessária
# Se não tiver, use o comando do R: install.packages("pracma")
# Este script em R realiza a integração numérica utilizando os métodos de quadratura de Gauss-Legendre e Gauss-Laguerre, 
# São técnicas para calcular integrais definidas.

library(pracma)


# Função exemplo. Modificar aqui conforme o exercício =)
fx <- function(x, lambda) {
  lambda * ((exp(-(sqrt(x)))) / (sqrt(x)))
}

# Parâmetros editáveis pelo usuário. Colocar aqui os valores que deseja calcular =)
n_pontos <- 20  # Número de pontos para a quadratura
lambda <- 6     # Parâmetro da função fx


# Funções de integração
gauss_legendre <- function(integrando, n_pontos, a, b, ...){
  pontos <- gaussLegendre(n_pontos, a = a, b = b)
  integral <- sum(pontos$w * integrando(pontos$x, ...))
  return(integral)
}

gauss_laguerre <- function(integrando, n.pontos, ...){
  pontos <- gaussLaguerre(n.pontos)
  integral <- sum(pontos$w * integrando(pontos$x, ...) / exp(-pontos$x))
  return(integral)
}



# Execução principal
main <- function(){
  resultado <- gauss_laguerre(integrando = fx, n.pontos = n_pontos, lambda = lambda)
  resultado_arredondado <- round(resultado, digits = 3)
  return(resultado_arredondado)
}

# Chamada da função principal e veja se deu certo :D
main()
