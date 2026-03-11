# Aula 11/03 - UFSM00417

# Criando Dados

anos <- c(2,3,4,5,4,6,7,8,8,10)
clientes<-c(48,50,56,52,43,60,62,58,64,72)

# Diagrama de Dispersão

plot(anos, clientes, xlab = "Anos de Serviço", ylab = "Número de Clientes")

# Correlação

cor(anos, clientes)

# Criando e Verificando a Matriz
M <- cbind(anos, clientes, (anos*clientes))
print(M)

# Matriz de Correlação

cor(M)

# Modelo de Regressão Simples

ajuste1<- lm(clientes~anos)

summary(ajuste1)

# Função com base nos parametros estimados

clientesFuncao <- function(anos){
  Resultado = 39.6747 + 2.9518 * anos
  return (Resultado)
}


# Chamando-a com o argumento de anos = 9

clientesFuncao(9)

# Se der um ls(ajuste1) retornará diversas informações sobre o objeto

ls(ajuste1)

# Plot dos Resíduos

plot(ajuste1$residuals)

# Histograma dos Resíduos

hist(ajuste1$residuals)

# Verificando o ajuste do modelo

plot(anos, clientes) |> 
lines(anos, ajuste1$fitted.values)







# Regressão Linear Múltipla

estudo<-c(15,13,21,13,8,15,13,8,15,17)
sexo<-c(1,1,0,1,0,0,0,1,1,0)

# Diagrama de Dispersão 

plot(clientes, estudo)

# Boxplot

boxplot(clientes~sexo)

# Matriz de Correlação

matriz <- cbind(anos, clientes, estudo, sexo)

# Matriz de Correlação entre as variáveis

cor(matriz)

#Ajustando o modelo linear

ajuste2 <- lm(clientes~anos+estudo+sexo)

# Verificando-o

summary(ajuste2)
