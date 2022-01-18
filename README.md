# EVALUACION-2

library(haven)
base_85 <- read_sav("C:/Users/herre/Desktop/RSTUDIO/base_85.sav")
View(base_85)

## 2. Encuesta CEP
###2.1 Link: https://www.cepchile.cl/cep/encuestas-cep/encuestas-2010-2021/estudio-nacional-de-opinion-publica-n-85-septiembre-2021

## 7. Análisis de causalidad

### 7.1. Definir variables para regresión lineal ***** Ejemplo ya que 
### la variable dependiente tiene que ser continua

##### 7.1.1. Variable Dependiente Dummy: base_85$religion_1
#####Variable dummy religion_1 -- BINARIA

base_85$religion_1.Dummy<-ifelse(base_85$religion_1=="1",1,0)
table(base_85$religion_1.Dummy)

# Variable Independiente  1: Edad
# Variable Independiente  2: religion_1
# Variable Independiente  3: sexo

##### 7.1.2. Variables numéricas

######  VARIABLE 1: Edad: age_N-2

base_85["Edad_Re2"] = cut(as.numeric(base_85$Edad_Re2), c(0, 18, 30, 45, 55, Inf), c("0-17", "18-29", "30-44", "45-54", "55<"), include.lowest=TRUE)

base_85["age_N-2"] <- as.numeric(as.character(base_85$edad))

###### VARIABLE 2: Religion

base_85$religion_1

###### VARIABLE 3: sexo 

base_85$sexo


###### 7.1.3. Definir X, Y 

Y <- cbind(base_85$info_enc_58)
X1 <- cbind(base_85$`age_N-2`)

X <- cbind(base_85$`age_N-2`, base_85$religion_1, base_85$sexo)

###### 7.1.4. Análisis de correlación

cor(X,Y)
cor(X1,Y)

###### 7.1.5. Gráfico de relación
plot(Y ~ X1, data = base_85)

## 8. Regresión Lineal SImple


olsreg1 <- lm(Y ~ X1)
summary(olsreg1)
confint(olsreg1, level = 0.95)
anova(olsreg1)

### 8.1. Graficando la regresión
abline(olsreg1)

### 8.2. Regresión 2

olsreg2 <- lm(Y ~ X)
summary(olsreg2)
confint(olsreg2, level = 0.95)
anova(olsreg2)

### 8.3. Graficando la regresión 2
abline(olsreg2)

### 9. Modelos de regresion usando stargazer

stargazer(olsreg1, olsreg2, title = "Resultados", 
          align =TRUE, out = "resultados3.txt")

### 10. Correr Regresion Logistica Binaria ## 

### 10.1. Definir variables para regresión logistica

##### 10.1.1. Variable Dependiente: base_85$info_enc_58

# Variable Independiente  1: Edad
# Variable Independiente  2: Religión
# Variable Independiente  3: Sexo

table(base_85$info_enc_58)

m3 = glm(base_85$`age_N-2` ~ X, data = base_85,
         binomial())
summary(m3)


# 11. Instalar funcion de bondad de ajuste
install.packages("rsq")

library(rsq)

# 12. Calcular bondad de ajuste
rsq(m3)

# 13. Tabular Regresion Logistica Binaria
stargazer(m3, title = "Modelo 3", align =TRUE, out = "resultados2.txt")

