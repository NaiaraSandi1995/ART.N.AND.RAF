#Script final do artigo- Rafaela e Naiara, 2021
#Maio 2020

#Packages utilizados
library(haven)
library(memisc)
library(descr)
library(plyr)
library(sjPlot)

#1ª Parte do trabalho, análise longitudinal dos invíduos que toleram o aborto 
#em casos em que a mulher corre risco de vida, através da variável:

#V-w14a W14A. O(A) sr./sra acredita que se justifica 
#a interrupção da gravidez, ou seja, um aborto, 
#quando a saúde da mãe está em perigo?

##Brasil####
# Análise descritiva w14a LAPOPbr
# 2019
summary(Bra2019$w14a)
Bra2019$w14a <- as.factor(Bra2019$w14a)
Bra2019$Aborto <- recode(Bra2019$w14a, "Sim" <- 1, "Não" <- 2)
summary(Bra2019$Aborto)
#Sim  Não NA's 
#1033  415   50
Table(Bra2019$Aborto, percent= T)
#Counts Percent
#Sim   1033      71
#Não    415      29


options(scipen = 1000, digits = 2)

# 2017
summary(Bra2017$w14a)
Bra2017$w14a <- as.factor(Bra2017$w14a)
Bra2017$Aborto <- recode(Bra2017$w14a, "Sim" <- 1, "Não" <- 2)
summary(Bra2017$Aborto)
#Sim  Não NA's 
# 933  568   31 
Table(Bra2017$Aborto, percent= T)
#Counts Percent
#Sim    933      62
#Não    568      38

# 2014
summary(Bra2014$w14a)
Bra2014$w14a <- as.factor(Bra2014$w14a)
Bra2014$Aborto <- recode(Bra2014$w14a, "Sim" <- 1, "Não" <- 2)
summary(Bra2014$Aborto)
#Sim  Não NA's 
# 925  474  101 
Table(Bra2014$Aborto, percent= T)
#Counts Percent
#Sim     925      66
#Não    474      34



# 2012

Bra2012$w14a <-  as.factor(Bra2012$w14a)
Bra2012$Aborto <- recode(Bra2012$w14a, "Sim" <- 1, "Não" <- 2)
summary(Bra2012$Aborto)
#Sim  Não NA's 
# 963  462   75 
Table(Bra2012$Aborto, percent= T)
#Counts Percent
#Sim    963      68
#Não    462      32


##Argentina####
# Análise descritiva w14a LAPOParg
# 2019
summary(Arg2019$w14a)

Arg2019$w14a <- as.factor(Arg2019$w14a)
Arg2019$Aborto <- recode(Arg2019$w14a, "Sim" <- 1, "Não" <- 2)
summary(Arg2019$Aborto)
#Sim  Não NA's 
#1043  417   68 
Table(Arg_2019$Aborto, percent= T)
#Counts Percent
#Sim   1043      71
#Não    417      29



# 2017
summary(Arg2017$w14a)
Arg2017$w14a <- as.factor(Arg2017$w14a)
Arg2017$Aborto <- recode(Arg2017$w14a, "Sim" <- 1, "Não" <- 2)
summary(Arg2017$Aborto)
#Sim  Não NA's 
# 919  511   98 
Table(Arg2017$Aborto, percent= T)
#      Counts Percent
#Sim    919      64
#Não    511      36

# 2014
summary(Arg2014$w14a)
Arg2014$w14a <- as.factor(Arg2014$w14a)
Arg2014$Aborto <-
  recode(Arg2014$w14a, "Sim" <- 1, "Não" <- 2)
summary(Arg2014$Aborto)
#Sim  Não NA's 
# 864  477  171 
Table(Arg2014$Aborto, percent= T)
#   Counts Percent
#Sim    864      64
#Não    477      36

# 2012
summary(Arg2012$w14a)
Arg2012$w14a <- as.factor(Arg2012$w14a)
Arg2012$Aborto <- recode(Arg2012$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Arg2012$Aborto)
#Sim  Não NA's 
# 450  233  829 
Table(Arg2012$Aborto, percent= T)
# Counts Percent
#Sim    450      66
#Não    233      34

##Chile####

# 2012
summary(Chi2012$w14a)
Chi2012$w14a<- as.factor(Chi2012$w14a)
Chi2012$Aborto <- recode(Chi2012$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Chi2012$Aborto)
#Sim  Não NA's 
# 450  233  829 
Table(Chi2012$Aborto, percent= T)
# Counts   Percent
# Sim 371.00000  52.62411
# Não 334.00000  47.37589

# 2014
summary(Chi2014$w14a)
Chi2014$w14a<- as.factor(Chi2014$w14a)
Chi2014$Aborto <- recode(Chi2014$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Chi2014$Aborto)
#Sim  Não NA's 
#  943  492  136 
Table(Chi2014$Aborto, percent= T)
# Counts         Percent
# Sim 943.00000  65.71429
# Não 492.00000  34.28571


# 2016
summary(Chi2016$w14a)
Chi2016$w14a<- as.factor(Chi2016$w14a)
Chi2016$Aborto <- recode(Chi2016$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Chi2016$Aborto)
#Sim  Não NA's 
# 1122  429   74 
Table(Chi2016$Aborto, percent= T)
#         Counts    Percent
# Sim 1122.00000   72.34043
# Não  429.00000   27.65957

# 2018
summary(Chi2018$w14a)
Chi2018$w14a<- as.factor(Chi2018$w14a)
Chi2018$Aborto <- recode(Chi2018$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Chi2018$Aborto)
#  Sim   Não NA's 
# 1271  298   69  
Table(Chi2018$Aborto, percent= T)
#         Counts    Percent
# Sim 1122.00000   72.34043
# Não  429.00000   27.65957


##Honduras####
# 2012
summary(Hon2012$w14a)
Hon2012$w14a<- as.factor(Hon2012$w14a)
Hon2012$Aborto <- recode(Hon2012$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Hon2012$Aborto)
#  Sim   Não NA's 
# 261  538  929 
Table(Hon2012$Aborto, percent= T)
# Counts   Percent
# Sim 261.00000  32.66583
# Não 538.00000  67.33417

# 2014
summary(HonHon2014$w14a)
Hon2014$w14a<- as.factor(Hon2014$w14a)
Hon2014$Aborto <- recode(Hon2014$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Hon2014$Aborto)
#  Sim   Não NA's 
# 756  748   57 
Table(Hon2014$Aborto, percent= T)
# Counts   Percent
# Sim 756.00000  50.26596
# Não 748.00000  49.73404

# 2016
summary(HonHon2016$w14a)

Hon2016$w14a<- as.factor(Hon2016$w14a)
Hon2016$Aborto <- recode(Hon2016$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Hon2016$Aborto)
#  Sim   Não NA's 
# 870  635   55  
Table(Hon2016$Aborto, percent= T)
# Counts   Percent
# Sim 870.00000  57.80731
# Não 635.00000  42.19269


# 2018
summary(HonHon2018$w14a)

Hon2018$w14a<- as.factor(Hon2018$w14a)
Hon2018$Aborto <- recode(Hon2018$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Hon2018$Aborto)
#  Sim   Não NA's 
# 880  621   59   
Table(Hon2018$Aborto, percent= T)
# Counts   Percent
# Sim 880.00000  58.62758
# Não 621.00000  41.37242

##Nicaragua####
# 2012
summary(Nic2012$w14a)

Nic2012$w14a<- as.factor(Nic2012$w14a)
Nic2012$Aborto <- recode(Nic2012$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Nic2012$Aborto)
#  Sim   Não NA's 
#  365  445  876
Table(Nic2012$Aborto, percent= T)
# Counts   Percent
# Sim 365.00000  45.06173
# Não 445.00000  54.93827



options(scipen = 1000, digits = 2)

# 2014
summary(HonHon2018$w14a)

Nic2014$w14a<- as.factor(Nic2014$w14a)
Nic2014$Aborto <- recode(Nic2014$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Nic2014$Aborto)
#  Sim   Não NA's 
#  884  597   65 
Table(Nic2014$Aborto, percent= T)
# #  Counts Percent
# Sim    884      60
# Não    597      40

# 2016
summary(HonHon2018$w14a)

Nic2016$w14a<- as.factor(Nic2016$w14a)
Nic2016$Aborto <- recode(Nic2016$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Nic2016$Aborto)
#  Sim   Não NA's 
#  874  619   67  
Table(Nic2016$Aborto, percent= T)
# #  Counts Percent
# Sim    884      59
# Não    597      41

# 2018 ##Não tem a questão do aborto
summary(Nic2018$w14)

##Nic2018$ <- as.factor(Nic2018$w14a)
Nic2018$Aborto <- recode(Nic2018$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Nic2018$Aborto)
#  Sim   Não NA's 
#  874  619   67  
Table(Nic2018$Aborto, percent= T)
# #  Counts Percent
# Sim    884      59
# Não    597      41


##Peru####
# 2012
summary(Per2012$w14a)

Per2012$w14a<- as.factor(Per2012$w14a)
Per2012$Aborto <- recode(Per2012$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Per2012$Aborto)
#  Sim   Não NA's 
#  462  238  800  
Table(Per2012$Aborto, percent= T)
# #  Counts Percent
# Sim    462      66
# Não    238      34

# 2014
summary(Per2014$w14a)

Per2014$w14a<- as.factor(Per2014$w14a)
Per2014$Aborto <- recode(Per2014$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Per2014$Aborto)
#  Sim   Não NA's 
#  763  622  115  
Table(Per2014$Aborto, percent= T)
# #    Counts Percent
# Sim    763      55
# Não    622      45

# 2016
summary(Per2016$w14a)

Per2016$w14a<- as.factor(Per2016$w14a)
Per2016$Aborto <- recode(Per2016$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Per2016$Aborto)
#  Sim   Não NA's 
# 1688  862   97 
Table(Per2016$Aborto, percent= T)
# #    Counts Percent
# Sim     1688      66
# Não    862      34

# 2018
summary(Per2018$w14a)

Per2018$w14a<- as.factor(Per2018$w14a)
Per2018$Aborto <- recode(Per2018$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Per2018$Aborto)
#  Sim   Não NA's 
# 1010  451   60
Table(Per2018$Aborto, percent= T)
# #    Counts Percent
# Sim   1010      69
# Não    451      31

#Uruguai####
# 2012
summary(Uru2012$w14a)

Uru2012$w14a<- as.factor(Uru2012$w14a)
Uru2012$Aborto <- recode(Uru2012$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Uru2012$Aborto)
#  Sim   Não NA's 
# 590  130  792
Table(Uru2012$Aborto, percent= T)
# #     Counts Percent
# Sim    590      82
# Não    130      18

# 2014
summary(Uru2014$w14a)

Uru2014$w14a<- as.factor(Uru2014$w14a)
Uru2014$Aborto <- recode(Uru2014$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Uru2014$Aborto)
#  Sim   Não NA's 
# 1167  293   52 
Table(Uru2014$Aborto, percent= T)
# #     Counts Percent
# Sim   1167      80
# Não    293      20

# 2016
summary(Uru2016$w14a)

Uru2016$w14a<- as.factor(Uru2016$w14a)
Uru2016$Aborto <- recode(Uru2016$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Uru2016$Aborto)
#  Sim   Não NA's 
# 1130  321   63 
Table(Uru2016$Aborto, percent= T)
# #     Counts Percent
# Sim    1130      78
#Não    321      22


# 2018
summary(Uru2018$w14a)

Uru2018$w14a<- as.factor(Uru2018$w14a)
Uru2018$Aborto <- recode(Uru2018$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Uru2018$Aborto)
#  Sim   Não NA's 
# 1245  284   52 
Table(Uru2018$Aborto, percent= T)
# #     Counts Percent
# Sim   1245      81
# Não    284      19


##Bolívia####
#2012
summary(Bol2012$w14a)

Bol2012$w14a<- as.factor(Bol2012$w14a)
Bol2012$Aborto <- recode(Bol2012$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Bol2012$Aborto)
#  Sim   Não NA's 
#  820  573 1636 
Table(Bol2012$Aborto, percent= T)
# #     Counts Percent
# Sim   820      59
# Não    573      41


#2014
summary(Bol2014$w14a)

Bol2014$w14a<- as.factor(Bol2014$w14a)
Bol2014$Aborto <- recode(Bol2014$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Bol2014$Aborto)
#  Sim   Não NA's 
# 1571 1296  199 
Table(Bol2014$Aborto, percent= T)
# #     Counts Percent
# Sim   1571      55
# Não   1296      45

#2017
summary(Bol2017$w14a)

Bol2017$w14a<- as.factor(Bol2017$w14a)
Bol2017$Aborto <- recode(Bol2017$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Bol2017$Aborto)
#  Sim   Não NA's 
# 1033  597   61 
Table(Bol2017$Aborto, percent= T)
# #     Counts Percent
# Sim   1033      63
#Não    597      37

#2019
summary(Bol2019$w14a)

Bol2019$w14a<- as.factor(Bol2019$w14a)
Bol2019$Aborto <- recode(Bol2019$w14a,
                         "Sim" <- 1, "Não" <- 2)
summary(Bol2019$Aborto)
#  Sim   Não NA's 
# 1104  522   56 
Table(Bol2019$Aborto, percent= T)
# #     Counts Percent
# Sim    1104      68
# Não    522      32

#Para cumprir o primeiro objetivo do artigo criamos um gráfico de linhas 
#Com os dados de SIM, para Brasil e Argentina, em docx mesmo.

##Gráfico de linhas####
library(tidyr)
library(ggplot2)

Objc <- data.frame (Anos = c(2012,2014,2016,2018), 
                    Brasil=	c(68, 66, 62, 71),
                    Argentina	=	c(66, 64, 64, 71),
                    Chile =	c(53, 66, 72, 72),
                    Uruguai =	c(82,80, 78, 81)) 


Objc.1 <- Objc %>% gather(Países, Favoráveis, -Anos)

Gra1 <- ggplot(data = Objc.1, aes(x= Anos, y= Favoráveis, 
                                 group = Países))+
  geom_line(aes(colour = Países), size = 2) + 
  geom_point(aes(shape = Países)) +
  scale_color_manual(values = c(Argentina = '#677528', 
                               Brasil = '#E6F79E',
                               Chile = '#D7F553',
                               Uruguai= '#3A754B')) +
 guides(colour = 'none')


  
  
  library(RColorBrewer) # Tem um site 


União1 <- Gra1 + theme_classic() + 
  theme_classic()+ theme(legend.position = "bottom")


Objd <- data.frame (Anos = c(2012,2014,2016,2018), 
                    Peru	=	c(66, 55, 66, 69),
                    Honduras =	c(33, 50,57,59), 
                    Bolívia =	c(59, 55, 63, 68)) 

Obj <- Objd %>% gather(Países, Favoráveis, -Anos)

Gra2 <- ggplot(data = Obj, aes(x= Anos, y= Favoráveis, 
                                  group = Países))+
  geom_line(aes(colour = Países), size = 2) + 
  geom_point(aes(shape = Países)) +
scale_color_manual(values = c(Nicaragua = '#287555', 
                              Peru = '#2BB363',
                              Honduras = '#1F7F47',
                              Bolívia= '#67BF8C')) +
  guides(colour = 'none')


União2 <- Gra2 + theme_classic()+ theme(legend.position = "bottom")


#Local: "bottom", "top", "left", ou "right" 


## Criação do infográfico####

library(gridExtra)
grid.arrange(União1, União2, ncol = 2)

grid.arrange(uniao5, uniao1, 
             arrangeGrob(uniao2, uniao3, uniao4, ncol=1), nrow = 1)
#


#Condicionantes####
#Bra2019####
#Recodifição das variáveis:
#Denominação religiosa
Bra2019$q3cn <- as.factor(Bra2017$q3cn)
Bra2019$Denom <- recode(Bra2019$q3cn, "católico" <- 1, "evangélicos" <-
                          c(2,5), "outras" <- c(3,6,7,1501,10,12,77),
                        "ateu" <- c(4,11))
Bra2019$Denom <- as.numeric(Bra2019$Denom)

#Sexo
Bra2019$q1 <- as.factor(Bra2019$q1)
Bra2019$Sexo <- recode(Bra2019$q1, "Homem" <-1, "Mulher" <-2 )

#Tenho que renomear as variáveis que iremos utilizar

#Q2 - Idade
Bra2019$Idade <- as.numeric(Bra2019$q2)


#Ing4 - Dem
Bra2019$Dem <- as.numeric(Bra2019$ing4)


#l1- Ideologia
Bra2019$Ideol <- as.numeric(Bra2019$l1)


#exc7new- Políticos corruptos 
Bra2019$PCorru <- as.numeric(Bra2019$exc7new)


#cct1b - bolsa família
Bra2019$B.fam <- as.numeric(Bra2019$cct1b)


#CONSP2. Algumas pessoas tem mediunidade e podem ler a mente dos outros ou ver eventos
#antes que aconteçam.
Bra2019$Mediun <- as.numeric(Bra2019$consp2)


#q5a- Frequência religiosa
Bra2019$FreqRelig <- as.numeric(Bra2019$q5a)


#ls3- satisfação pessoal
Bra2019$SatPes <- as.numeric(Bra2019$ls3)


#prot3- participação em protesto
Bra2019$Prote <- as.numeric(Bra2019$prot3)


#b1- julgamento justo
Bra2019$Julgamento <- as.numeric(Bra2019$b1)


#b2- confiança nas instituições
Bra2019$ConfInst <- as.numeric(Bra2019$b2)

#


save(Bra2019, file = "Bra2019.RData")
#

#Arg2019####
#Denominação religiosa
Arg2019$q3cn <- as.factor(Arg2019$q3cn)
Arg2019$Denom <- recode(Arg2019$q3cn, "católico" <- 1, "evangélicos" <-
                          c(2,5), "outras" <- c(3,6,7,1501,10,12,77),
                        "ateu" <- c(4,11))
Arg2019$Denom <- as.numeric(Arg2019$Denom)

#Sexo
Arg2019$q1 <- as.factor(Arg2019$q1)
Arg2019$Sexo <- recode(Arg2019$q1, "Homem" <-1, "Mulher" <-2 )

#Tenho que renomear as variáveis que iremos utilizar

#Q2 - Idade
Arg2019$Idade <- as.numeric(Arg2019$q2)

#Ing4 - Dem
Arg2019$Dem <- as.numeric(Arg2019$ing4)

#l1- Ideologia
Arg2019$Ideol <- as.numeric(Arg2019$l1)

#exc7new- Políticos corruptos 
Arg2019$PCorru <- as.numeric(Arg2019$exc7new)

#cct1b - bolsa família
Arg2019$B.fam <- as.numeric(Arg2019$cct1b)

#CONSP2. Algumas pessoas tem mediunidade e podem ler a mente dos outros ou ver eventos
#antes que aconteçam.
Arg2019$Mediun <- as.numeric(Arg2019$consp2)

#q5a- Frequência religiosa
Arg2019$FreqRelig <- as.numeric(Arg2019$q5a)

#ls3- satisfação pessoal
Arg2019$SatPes <- as.numeric(Arg2019$ls3)

#prot3- participação em protesto
Arg2019$Prote <- as.numeric(Arg2019$prot3)

#b1- julgamento justo
Arg2019$Julgamento <- as.numeric(Arg2019$b1)

#b2- confiança nas instituições
Arg2019$ConfInst <- as.numeric(Arg2019$b2)
#
save(Arg2019, file = "Arg2019.RData")
#

#Chi2018####
#Denominação religiosa
Chi2018$q3cn <- as.factor(Chi2018$q3cn)
Chi2018$Denom <- recode(Chi2018$q3cn, "católico" <- 1, "evangélicos" <-
                          c(2,5), "outras" <- c(3,6,7,1501,10,12,77),
                        "ateu" <- c(4,11))
Chi2018$Denom <- as.numeric(Chi2018$Denom)

#Sexo
Chi2018$q1 <- as.factor(Chi2018$q1)
Chi2018$Sexo <- recode(Chi2018$q1, "Homem" <-1, "Mulher" <-2 )

#Tenho que renomear as variáveis que iremos utilizar

#Q2 - Idade
Chi2018$Idade <- as.numeric(Chi2018$q2)

#Ing4 - Dem
Chi2018$Dem <- as.numeric(Chi2018$ing4)

#l1- Ideologia
Chi2018$Ideol <- as.numeric(Chi2018$l1)

#exc7new- Políticos corruptos 
Chi2018$PCorru <- as.numeric(Chi2018$exc7new)

#cct1b - bolsa família
Chi2018$B.fam <- as.numeric(Chi2018$cct1b)

#CONSP2. Algumas pessoas tem mediunidade e podem ler a mente dos outros ou ver eventos
#antes que aconteçam.
Chi2018$Mediun <- as.numeric(Chi2018$consp2)

#q5a- Frequência religiosa
Chi2018$FreqRelig <- as.numeric(Chi2018$q5a)

#ls3- satisfação pessoal
Chi2018$SatPes <- as.numeric(Chi2018$ls3)

#prot3- participação em protesto
Chi2018$Prote <- as.numeric(Chi2018$prot3)

#b1- julgamento justo
Chi2018$Julgamento <- as.numeric(Chi2018$b1)

#b2- confiança nas instituições
Chi2018$ConfInst <- as.numeric(Chi2018$b2)
#
save(Chi2018, file = "Chi2018.RData")
#


##Hon2018####
#Denominação religiosa
Hon2018$q3cn <- as.factor(Hon2018$q3cn)
Hon2018$Denom <- recode(Hon2018$q3cn, "católico" <- 1, "evangélicos" <-
                          c(2,5), "outras" <- c(3,6,7,1501,10,12,77),
                        "ateu" <- c(4,11))
Hon2018$Denom <- as.numeric(Hon2018$Denom)

#Sexo
Hon2018$q1 <- as.factor(Hon2018$q1)
Hon2018$Sexo <- recode(Hon2018$q1, "Homem" <-1, "Mulher" <-2 )

#Tenho que renomear as variáveis que iremos utilizar

#Q2 - Idade
Hon2018$Idade <- as.numeric(Hon2018$q2)

#Ing4 - Dem
Hon2018$Dem <- as.numeric(Hon2018$ing4)

#l1- Ideologia
Hon2018$Ideol <- as.numeric(Hon2018$l1)

#exc7new- Políticos corruptos 
Hon2018$PCorru <- as.numeric(Hon2018$exc7new)

#cct1b - bolsa família
Hon2018$B.fam <- as.numeric(Hon2018$cct1b)

#CONSP2. Algumas pessoas tem mediunidade e podem ler a mente dos outros ou ver eventos
#antes que aconteçam.
Hon2018$Mediun <- as.numeric(Hon2018$consp2)

#q5a- Frequência religiosa
Hon2018$FreqRelig <- as.numeric(Hon2018$q5a)

#ls3- satisfação pessoal
Hon2018$SatPes <- as.numeric(Hon2018$ls3)

#prot3- participação em protesto
Hon2018$Prote <- as.numeric(Hon2018$prot3)

#b1- julgamento justo
Hon2018$Julgamento <- as.numeric(Hon2018$b1)

#b2- confiança nas instituições
Hon2018$ConfInst <- as.numeric(Hon2018$b2)
#
save(Hon2018, file = "Hon2018.RData")
#

##Peru2018####
#Denominação religiosa
Per2018$q3cn <- as.factor(Per2018$q3cn)
Per2018$Denom <- recode(Per2018$q3cn, "católico" <- 1, "evangélicos" <-
                          c(2,5), "outras" <- c(3,6,7,1501,10,12,77),
                        "ateu" <- c(4,11))
Per2018$Denom <- as.numeric(Per2018$Denom)

#Sexo
Per2018$q1 <- as.factor(Per2018$q1)
Per2018$Sexo <- recode(Per2018$q1, "Homem" <-1, "Mulher" <-2 )

#Tenho que renomear as variáveis que iremos utilizar

#Q2 - Idade
Per2018$Idade <- as.numeric(Per2018$q2)

#Ing4 - Dem
Per2018$Dem <- as.numeric(Per2018$ing4)

#l1- Ideologia
Per2018$Ideol <- as.numeric(Per2018$l1)

#exc7new- Políticos corruptos 
Per2018$PCorru <- as.numeric(Per2018$exc7new)

#cct1b - bolsa família
Per2018$B.fam <- as.numeric(Per2018$cct1b)

#CONSP2. Algumas pessoas tem mediunidade e podem ler a mente dos outros ou ver eventos
#antes que aconteçam.
Per2018$Mediun <- as.numeric(Per2018$consp2)

#q5a- Frequência religiosa
Per2018$FreqRelig <- as.numeric(Per2018$q5a)

#ls3- satisfação pessoal
Per2018$SatPes <- as.numeric(Per2018$ls3)

#prot3- participação em protesto
Per2018$Prote <- as.numeric(Per2018$prot3)

#b1- julgamento justo
Per2018$Julgamento <- as.numeric(Per2018$b1)

#b2- confiança nas instituições
Per2018$ConfInst <- as.numeric(Per2018$b2)
#
save(Per2018, file = "Per2018.RData")

##Uru2018####
#Denominação religiosa
Uru2018$q3cn <- as.factor(Uru2018$q3cn)
Uru2018$Denom <- recode(Uru2018$q3cn, "católico" <- 1, "evangélicos" <-
                          c(2,5), "outras" <- c(3,6,7,1501,10,12,77),
                        "ateu" <- c(4,11))
Uru2018$Denom <- as.numeric(Uru2018$Denom)

#Sexo
Uru2018$q1 <- as.factor(Uru2018$q1)
Uru2018$Sexo <- recode(Uru2018$q1, "Homem" <-1, "Mulher" <-2 )

#Tenho que renomear as variáveis que iremos utilizar

#Q2 - Idade
Uru2018$Idade <- as.numeric(Uru2018$q2)

#Ing4 - Dem
Uru2018$Dem <- as.numeric(Uru2018$ing4)

#l1- Ideologia
Uru2018$Ideol <- as.numeric(Uru2018$l1)

#exc7new- Políticos corruptos 
Uru2018$PCorru <- as.numeric(Uru2018$exc7new)

#cct1b - bolsa família
Uru2018$B.fam <- as.numeric(Uru2018$cct1b)

#CONSP2. Algumas pessoas tem mediunidade e podem ler a mente dos outros ou ver eventos
#antes que aconteçam.
Uru2018$Mediun <- as.numeric(Uru2018$consp2)

#q5a- Frequência religiosa
Uru2018$FreqRelig <- as.numeric(Uru2018$q5a)

#ls3- satisfação pessoal
Uru2018$SatPes <- as.numeric(Uru2018$ls3)

#prot3- participação em protesto
Uru2018$Prote <- as.numeric(Uru2018$prot3)

#b1- julgamento justo
Uru2018$Julgamento <- as.numeric(Uru2018$b1)

#b2- confiança nas instituições
Uru2018$ConfInst <- as.numeric(Uru2018$b2)
#
save(Uru2018, file = "Uru2018.RData")


#Bol2019####
#Denominação religiosa
Bol2019$q3cn <- as.factor(Bol2019$q3cn)
Bol2019$Denom <- recode(Bol2019$q3cn, "católico" <- 1, "evangélicos" <-
                          c(2,5), "outras" <- c(3,6,7,1501,10,12,77),
                        "ateu" <- c(4,11))
Bol2019$Denom <- as.numeric(Bol2019$Denom)

#Sexo
Bol2019$q1 <- as.factor(Bol2019$q1)
Bol2019$Sexo <- recode(Bol2019$q1, "Homem" <-1, "Mulher" <-2 )

#Tenho que renomear as variáveis que iremos utilizar

#Q2 - Idade
Bol2019$Idade <- as.numeric(Bol2019$q2)

#Ing4 - Dem
Bol2019$Dem <- as.numeric(Bol2019$ing4)

#l1- Ideologia
Bol2019$Ideol <- as.numeric(Bol2019$l1)

#exc7new- Políticos corruptos 
Bol2019$PCorru <- as.numeric(Bol2019$exc7new)

#cct1b - bolsa família
Bol2019$B.fam <- as.numeric(Bol2019$cct1b)

#CONSP2. Algumas pessoas tem mediunidade e podem ler a mente dos outros ou ver eventos
#antes que aconteçam.
Bol2019$Mediun <- as.numeric(Bol2019$consp2)

#q5a- Frequência religiosa
Bol2019$FreqRelig <- as.numeric(Bol2019$q5a)

#ls3- satisfação pessoal
Bol2019$SatPes <- as.numeric(Bol2019$ls3)

#prot3- participação em protesto
Bol2019$Prote <- as.numeric(Bol2019$prot3)

#b1- julgamento justo
Bol2019$Julgamento <- as.numeric(Bol2019$b1)

#b2- confiança nas instituições
Bol2019$ConfInst <- as.numeric(Bol2019$b2)
#
save(Bol2019, file = "Bol2019.RData")


#Merge####
#Ficaram apenas 7 países, pq Nicaragua não tem a questão do aborto 
#Rafa.

Banco1 <- merge(Arg2019, Bol2019, all = T)
Banco2 <- merge(Bra2019, Chi2018, all= T)
Banco3 <- merge(Hon2018, Per2018, all= T)

Banco4 <- merge(Banco1, Uru2018, all = T)
Banco5 <- merge(Banco2, Banco3, all= T)

Banco6 <- merge(Banco4, Banco5, all= T)

#Nível 2

BancoT <- merge(Banco6, zNivel2, all= T)
save(BancoT, file = "BancoT.RData")


BancoT$Escolaridade <- as.numeric(BancoT$ed)
BancoT$Denom <- as.factor(BancoT$Denom)


# Multinivel####

library(multilevel)#ativa o pacote para execução do modelo ANOVA
data(BancoLAPOPCompleto)#Indica o banco de dados integrado
library(lme4)


##ModeloNULO####
#TolerHomo
#MULTINIVEL
#Modelo 1: Participação em Manifestações e Protesto
#Modelo Nulo
Nulo <- glmer(Aborto ~ (1 | pais), family = binomial("logit"), data = BancoT)
summary(Nulo)
# 
# Generalized linear mixed model fit by maximum likelihood 
#(Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Aborto ~ (1 | pais)
# Data: BancoT
# 
# AIC      BIC   logLik deviance df.resid 
# 12394.7  12409.3  -6195.4  12390.7    10592 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.8349 -0.6671 -0.4879  1.1977  2.0765 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# pais   (Intercept) 0.1329   0.3645  
# Number of obs: 10594, groups:  pais, 7
# 
# Fixed effects:
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -0.9509     0.1398    -6.8 1.04e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > 

exp(-0.9509)
#[1] 0.3863931

Model.nulo <- glm(Aborto ~ 1, data = BancoT, family = binomial("logit"))
logLik(Model.nulo)-logLik(Nulo)
#'log Lik.' -125.3426 (df=1)
(-2*(-125.3426))
#[1] 250.6852

#Caterpillar para o Efeito-país
u0 <- ranef(Nulo, postVar = TRUE)
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ])
commid <- as.numeric(rownames(u0[[1]]))
u0tab <- cbind("commid" = commid, "u0" = u0[[1]], "u0se" = u0se)
colnames(u0tab)[2] <- "u0"
u0tab <- u0tab[order(u0tab$u0), ]
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
u0tab <- u0tab[order(u0tab$commid), ]
colnames(u0tab)[4] <- "u0rank"
plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", 
     ylab = "Modas Condicionais de Aborto por país",
     ylim = c(-2, 2))
segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 
           1.96*u0tab$u0se)
points(u0tab$u0rank, u0tab$u0, col = "blue")
abline(h = 0, col = "red")


##Modelo1*####
#Modelo de nível 1 
Mod1 <- glmer(Aborto ~ Denom + FreqRelig +  
                Escolaridade + Idade + Sexo + Dem + 
                (1 | pais), family = binomial("logit"), data = BancoT)

table(BancoT$Dem, BancoT$pais)


summary(Mod1)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Aborto ~ Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem +      (1 | pais)
# Data: BancoT
# 
# AIC      BIC   logLik deviance df.resid 
# 10824.8  10896.3  -5402.4  10804.8     9402 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.4131 -0.6546 -0.5097  1.0887  2.9577 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# pais   (Intercept) 0.04652  0.2157  
# Number of obs: 9412, groups:  pais, 7
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   0.677369   0.167712   4.039 5.37e-05 ***
#   Denom2        0.165794   0.060577   2.737   0.0062 ** 
#   Denom3       -0.067385   0.123251  -0.547   0.5846    
# Denom4       -0.003067   0.076719  -0.040   0.9681    
# FreqRelig    -0.180772   0.020168  -8.963  < 2e-16 ***
#   Escolaridade -0.059059   0.006299  -9.376  < 2e-16 ***
#   Idade        -0.001233   0.001524  -0.809   0.4184    
# SexoMulher    0.007843   0.047214   0.166   0.8681    
# Dem          -0.086362   0.013821  -6.248 4.15e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) Denom2 Denom3 Denom4 FrqRlg Esclrd Idade  SxMlhr
# Denom2      -0.324                                                 
# Denom3      -0.110  0.168                                          
# Denom4      -0.055  0.161  0.098                                   
# FreqRelig   -0.467  0.341  0.092 -0.262                            
# Escolaridad -0.465  0.078  0.001  0.032  0.014                     
# Idade       -0.537  0.122  0.046  0.104  0.127  0.341              
# SexoMulher  -0.217  0.019  0.022  0.046  0.089  0.012  0.038       
# Dem         -0.302 -0.002 -0.001  0.021 -0.013 -0.127 -0.108  0.037
# optimizer (Nelder_Mead) convergence code: 0 (OK)
# Model is nearly unidentifiable: very large eigenvalue
# - Rescale variables?

exp(fixef(Mod1 ))

#(Intercept)       Denom2       Denom3       Denom4    FreqRelig 
#  1.9686919    1.1803302    0.9348355    0.9969372    0.8346253 
#Escolaridade        Idade   SexoMulher
#0.9426508    0.9987675    1.0078734 
#Dem 
#0.9172623 

coef(Mod1 )
# $pais
# (Intercept)    Denom2      Denom3       Denom4  FreqRelig Escolaridade        Idade  SexoMulher
# 4    0.8644821 0.1657943 -0.06738469 -0.003067486 -0.1807724  -0.05905941 -0.001233295 0.007842545
# 10   0.7920983 0.1657943 -0.06738469 -0.003067486 -0.1807724  -0.05905941 -0.001233295 0.007842545
# 11   0.8144339 0.1657943 -0.06738469 -0.003067486 -0.1807724  -0.05905941 -0.001233295 0.007842545
# 13   0.3936729 0.1657943 -0.06738469 -0.003067486 -0.1807724  -0.05905941 -0.001233295 0.007842545
# 14   0.4202144 0.1657943 -0.06738469 -0.003067486 -0.1807724  -0.05905941 -0.001233295 0.007842545
# 15   0.5306436 0.1657943 -0.06738469 -0.003067486 -0.1807724  -0.05905941 -0.001233295 0.007842545
# 17   0.9309538 0.1657943 -0.06738469 -0.003067486 -0.1807724  -0.05905941 -0.001233295 0.007842545
# Dem
# 4  -0.08636183
# 10 -0.08636183
# 11 -0.08636183
# 13 -0.08636183
# 14 -0.08636183
# 15 -0.08636183
# 17 -0.08636183
# 
# attr(,"class")
# [1] "coef.mer"


library(sjPlot)
#Para visualizar a saída do modelo, com o sjPlot
tab_model(Mod1, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60,
          p.style ="stars")
0.84 - 1

#Modelo 2*####
#Modelo de inclinações variáveis
  
BancoT$Aborto  
Mod2 <- glmer(Aborto ~ Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem + 
                (1 + Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem|pais), 
              data = BancoT, family = binomial("logit"))
summary(Mod2)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Aborto ~ Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem +  
#   (1 + Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem |          pais)
# Data: BancoT
# 
# AIC      BIC   logLik deviance df.resid 
# 10855.2  11241.3  -5373.6  10747.2     9358 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.5546 -0.6706 -0.5027  1.0994  3.9099 
# 
# Random effects:
#   Groups Name         Variance  Std.Dev. Corr                                           
# pais   (Intercept)  6.565e-01 0.810241                                                
# Denom2       1.560e-02 0.124906  0.12                                          
# Denom3       6.273e-02 0.250457 -0.95 -0.02                                    
# Denom4       8.712e-03 0.093339  0.86 -0.36 -0.91                              
# FreqRelig    6.092e-03 0.078052 -0.72 -0.03  0.89 -0.75                        
# Escolaridade 6.973e-04 0.026407 -0.88 -0.55  0.85 -0.58  0.73                  
# Idade        4.166e-05 0.006454 -0.62  0.30  0.61 -0.67  0.45  0.38            
# SexoMulher   1.906e-02 0.138042 -0.07 -0.75 -0.09  0.31 -0.24  0.33 -0.60      
# Dem          2.383e-03 0.048811 -0.93 -0.20  0.94 -0.81  0.80  0.89  0.35  0.23
# Number of obs: 9412, groups:  pais, 7
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   0.7609876  0.3433987   2.216 0.026688 *  
#   Denom2        0.1777119  0.0807917   2.200 0.027833 *  
#   Denom3       -0.0663728  0.1607644  -0.413 0.679711    
# Denom4        0.0124049  0.0871240   0.142 0.886778    
# FreqRelig    -0.1857526  0.0363874  -5.105 3.31e-07 ***
#   Escolaridade -0.0654831  0.0120886  -5.417 6.06e-08 ***
#   Idade        -0.0008977  0.0029031  -0.309 0.757143    
# SexoMulher    0.0154683  0.0712816   0.217 0.828206    
# Dem          -0.0824098  0.0233699  -3.526 0.000421 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) Denom2 Denom3 Denom4 FrqRlg Esclrd Idade  SxMlhr
# Denom2      -0.084                                                 
# Denom3      -0.552  0.102                                          
# Denom4       0.290  0.029 -0.154                                   
# FreqRelig   -0.669  0.169  0.475 -0.385                            
# Escolaridad -0.782 -0.220  0.420 -0.181  0.505                     
# Idade       -0.615  0.205  0.325 -0.177  0.348  0.367              
# SexoMulher  -0.120 -0.309 -0.033  0.123 -0.106  0.196 -0.362       
# Dem         -0.745 -0.096  0.434 -0.248  0.506  0.528  0.189  0.151
# optimizer (Nelder_Mead) convergence code: 4 (failure to converge in 10000 evaluations)
# boundary (singular) fit: see ?isSingular
# failure to converge in 10000 evaluations


exp(fixef(Mod2))
VarCorr(Mod2)$pais

tab_model(Mod2, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60,
          p.style ="stars")

##Modelo3####
#modelo micro e macro
BancoT$Desemprego
BancoT$TNaH
BancoT$Dir_minorias

Mod3 <- glmer(Aborto ~ Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem + 
                 Desemprego + Dir_minorias +
              (1 | pais), family = binomial("logit"), data = BancoT)
summary(Mod3)



#Para visualizar a saída do modelo, com o sjPlot
tab_model(Mod3, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60,
          p.style ="stars")


##Modelo4####
#Modelo de interação micro-macro: Freq*Dir_minorias e Freq* Desemprego
Model4 <- glmer(Aborto ~ Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem + 
                             Desemprego + Dir_minorias + Dir_minorias*FreqRelig + 
                  Desemprego*FreqRelig +
                              (1 | pais), family = binomial("logit"), data = BancoT)
summary(Model4)

exp(fixef(Model4))

tab_model(Model4, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60,
          p.style ="stars")

##Modelo5####
#Modelo de interação micro-macro: Freq*Dir_minorias
Model5 <- glmer(Aborto ~ Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem +
                  Desemprego + Dir_minorias + Dir_minorias*FreqRelig +
                  (1 | pais), family = binomial("logit"), data = BancoT)
summary(Model5)

tab_model(Model5, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60,
          p.style ="stars")


##Modelo6####
#Modelo de interação micro-macro: Freq*Dir_minorias
Model6 <- glmer(Aborto ~ Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem + 
                  Desemprego + Dir_minorias + Desemprego*FreqRelig +
                  (1 | pais), family = binomial("logit"), data = BancoT)
summary(Model6)


tab_model(Model6, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60,
          p.style ="stars")

##Modelo8*####
BancoT$Liberdade
BancoT$Pod_Grupos

Mod8 <- glmer(Aborto ~ Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem + 
                Liberdade + Dir_minorias +
                (1 | pais), family = binomial("logit"), data = BancoT)
summary(Mod8)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#   glmerMod]
# Family: binomial  ( logit )
# Formula: Aborto ~ Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem +  
#   Liberdade + Dir_minorias + (1 | pais)
# Data: BancoT
# 
# AIC      BIC   logLik deviance df.resid 
# 10815.8  10901.6  -5395.9  10791.8     9400 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.4173 -0.6559 -0.5087  1.0904  2.9868 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# pais   (Intercept) 0.00355  0.05959 
# Number of obs: 9412, groups:  pais, 7
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   2.001754   0.263861   7.586 3.29e-14 ***
#   Denom2        0.171451   0.060038   2.856 0.004294 ** 
#   Denom3       -0.070988   0.123176  -0.576 0.564404    
# Denom4       -0.008470   0.076245  -0.111 0.911548    
# FreqRelig    -0.177189   0.020075  -8.826  < 2e-16 ***
#   Escolaridade -0.058994   0.006156  -9.583  < 2e-16 ***
#   Idade        -0.001181   0.001518  -0.778 0.436440    
# SexoMulher    0.009012   0.047209   0.191 0.848598    
# Dem          -0.083861   0.013803  -6.075 1.24e-09 ***
#   Liberdade    -0.157876   0.044168  -3.574 0.000351 ***
#   Dir_minorias -1.486554   0.245769  -6.049 1.46e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) Denom2 Denom3 Denom4 FrqRlg Esclrd Idade  SxMlhr Dem    Librdd
# Denom2      -0.146                                                               
# Denom3      -0.062  0.168                                                        
# Denom4       0.053  0.145  0.099                                                 
# FreqRelig   -0.243  0.337  0.098 -0.276                                          
# Escolaridad -0.328  0.099  0.012  0.056  0.011                                   
# Idade       -0.295  0.122  0.052  0.103  0.119  0.339                            
# SexoMulher  -0.122  0.017  0.023  0.044  0.087  0.014  0.037                     
# Dem         -0.133 -0.007 -0.004  0.007 -0.014 -0.118 -0.110  0.035              
# Liberdade   -0.751 -0.067  0.029 -0.039  0.082  0.102  0.052  0.020  0.006       
# Dir_minoris -0.741 -0.057 -0.034 -0.110 -0.117 -0.001 -0.089 -0.033 -0.095  0.557
# optimizer (Nelder_Mead) convergence code: 0 (OK)
# Model failed to converge with max|grad| = 0.00392027 (tol = 0.002, component 1)
# Model is nearly unidentifiable: very large eigenvalue
# - Rescale variables?

#Para visualizar a saída do modelo, com o sjPlot
tab_model(Mod8, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60,
          p.style ="stars")

##Modelo9*####
#Modelo de interação micro-macro
Model9 <- glmer(Aborto ~ Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem +
                 Liberdade + Dir_minorias + Dir_minorias*FreqRelig + 
                  Liberdade*FreqRelig +
                  (1 | pais), family = binomial("logit"), data = BancoT)
summary(Model9)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#   glmerMod]
# Family: binomial  ( logit )
# Formula: Aborto ~ Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem +  
#   Liberdade + Dir_minorias + Dir_minorias * FreqRelig + Liberdade *  
#   FreqRelig + (1 | pais)
# Data: BancoT
# 
# AIC      BIC   logLik deviance df.resid 
# 10800.2  10900.3  -5386.1  10772.2     9398 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.3821 -0.6682 -0.5038  1.1054  3.2207 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# pais   (Intercept) 0.00212  0.04604 
# Number of obs: 9412, groups:  pais, 7
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)             1.0039418  0.4480417   2.241  0.02504 *  
#   Denom2                  0.1706114  0.0601998   2.834  0.00460 ** 
#   Denom3                 -0.1007265  0.1235955  -0.815  0.41509    
# Denom4                  0.0268523  0.0764378   0.351  0.72537    
# FreqRelig               0.1343909  0.1206848   1.114  0.26546    
# Escolaridade           -0.0593145  0.0061306  -9.675  < 2e-16 ***
#   Idade                  -0.0009722  0.0015177  -0.641  0.52178    
# SexoMulher              0.0129759  0.0472450   0.275  0.78358    
# Dem                    -0.0827669  0.0138311  -5.984 2.18e-09 ***
#   Liberdade              -0.1456130  0.0829343  -1.756  0.07913 .  
# Dir_minorias           -0.0133398  0.4531568  -0.029  0.97652    
# FreqRelig:Dir_minorias -0.4533868  0.1237931  -3.662  0.00025 ***
#   FreqRelig:Liberdade    -0.0035344  0.0231934  -0.152  0.87888    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tab_model(Model9, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60,
          p.style ="stars")

print(Model9, correlation=TRUE) 

##Modelo10####
#Modelo de interação micro-macro
Model10 <- glmer(Aborto ~ Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem + 
                  Liberdade + Dir_minorias + 
                   Liberdade*FreqRelig +
                  (1 | pais), family = binomial("logit"), data = BancoT)
summary(Model10)

tab_model(Model10, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60,
          p.style ="stars")

#Model11####
#Modelo de interação micro-macro
Model11 <- glmer(Aborto ~ Denom + FreqRelig + Escolaridade + Idade + Sexo + Dem + 
                  Liberdade + Dir_minorias + Dir_minorias*FreqRelig + 
                  (1 | pais), family = binomial("logit"), data = BancoT)
summary(Model11)

tab_model(Model11, show.ci = F, auto.label =T, 
          show.se = F, collapse.se = F, wrap.labels = 60,
          p.style ="stars")



##Análise de valores preditos#####
#Criação do banco

#Interação entre denominação e tolerância nacional aos homossexuais 
PRED.Denom.TNaH <- data.frame(Denom=c("1", "1", "2", "2", "3", "3", "4", "4"),
                              AtRelig=c(0,0,2,2,2,2,2,2),
                              IntRelig=c(0,0,2,2,2,2,2,2),
                              Dem=c(4, 4, 4, 4, 4, 4, 4, 4),
                              ConfInt=c(2, 2, 2, 2, 2, 2, 2, 2),
                              Ed_sup=c(0, 0, 0, 0, 0, 0, 0, 0),
                              Idade=c(40, 40, 40, 40, 40, 40, 40, 40),
                              Sexo=c(0,0,0,0,0,0,0,0),
                              TNaH=c(0, 2,0, 2,0, 2,0, 2),
                              Desemprego= c(11,11,11,11,11,11,11,11))

predict(Model.Comp1,PRED.Denom.TNaH,level=0)

#[1] 4.392273 6.506973 3.596960 5.711660 2.571991 4.686691 2.937430 5.052130
#attr(,"label")
#[1] "Predicted values"


#3.4 é a medida de tolerância de um ateu que vive em um lugar com contexto sem direitos 
#5.5 contexto com tolerância
#Naiara cada dupla é uma categoria de Denom.


PRED.Denom.TNaH$TolerHomo <-predict(Model.Comp1,PRED.Denom.TNaH,level=0)
with(PRED.Denom.TNaH,interaction.plot(Denom,TNaH,TolerHomo, legend=F,xlab="Denominação", 
                                      ylab="Média de tolerância política", 
                                      main="Interação entre Denominações religiosas 
e Direitos dos Homossexuais (TNaH)"))
#

#

# Segunda opção de gráfico:
PRED.Denom.TNaH$TolerHomo <-predict(Model.Comp1,
                                    PRED.Denom.TNaH,level=0)
with(PRED.Denom.TNaH,
     interaction.plot(Denom,TNaH,TolerHomo,
                      xlab="Denominação", 
                      ylab="Média de tolerância política", 
                      legend=T, col = "steelblue",
                      main="Interação entre Denominações religiosas 
e Direitos dos Homossexuais (TNaH)")) 

#Linha tracejada é sem direitos, e a sólida é a média de tolerância 
#para todas as religiões nos contextos com direitos 


#Interação entre ativismo e tolerância aos homossexuais
PRED.At.TNaH <- data.frame(Denom=c("1", "1", "2", "2", "2", "2", "3", "3","3","3", "4", "4","4","4"),
                           AtRelig=c(0,0, 0,0,4,4, 0,0,4,4, 0,0,4,4),
                           IntRelig=c(2,2, 2,2,2,2, 2,2,2,2, 2,2,2,2),
                           Dem=c(4,4, 4,4,4,4, 4,4,4,4, 4,4,4,4),
                           ConfInt=c(2,2, 2,2,2,2, 2,2,2,2, 2,2,2,2),
                           Ed_sup=c(0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0),
                           Idade=c(40,40, 40,40,40,40, 40,40,40,40, 40,40,40,40),
                           Sexo=c(0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0),
                           TNaH=c(0,2, 0,2,0,2, 0,2,0,2, 0,2,0,2),
                           Desemprego= c(11,11, 11,11,11,11, 11,11,11,11, 11,11,11,11))

predict(Model.Comp1,PRED.At.TNaH,level=0)

# [1] 3.823698 5.938398 4.019819 6.134518 3.174102 5.288802 2.994850 5.109549 2.149133 4.263833
#[11] 3.360289 5.474988 2.514572 4.629272
#attr(,"label")
#[1] "Predicted values"

#Agora estamos analisando o ativismo religioso. 
#Tracejada contexto que não tem direito. E a sólida é o contexto onde tem direito

PRED.At.TNaH$TolerHomo <-predict(Model.Comp1,PRED.At.TNaH,level=0)
with(PRED.At.TNaH,interaction.plot(AtRelig,TNaH,TolerHomo, legend=T,xlab="Ativismo", 
                                   ylab="Média de tolerância política", 
                                   main="Interação entre Ativismo e 
  Direitos dos Homossexuais (TNaH)",
                                   col = "steelblue"))





#Interação entre intensidade e tolerância nacional aos homossexuais 
PRED.Int.TNaH <- data.frame(Denom=c("1", "1", "2", "2", "2", "2", "3", "3","3","3", "4", "4","4","4"),
                            AtRelig=c(0,0, 2,2,2,2, 2,2,2,2, 2,2,2,2),
                            IntRelig=c(0,0, 0,0,3,3, 0,0,3,3, 0,0,3,3),
                            Dem=c(4,4, 4,4,4,4, 4,4,4,4, 4,4,4,4),
                            ConfInt=c(2,2, 2,2,2,2, 2,2,2,2, 2,2,2,2),
                            Ed_sup=c(0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0),
                            Idade=c(40,40, 40,40,40,40, 40,40,40,40, 40,40,40,40),
                            Sexo=c(0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0),
                            TNaH=c(0,2, 0,2,0,2, 0,2,0,2, 0,2,0,2),
                            Desemprego= c(11,11, 11,11,11,11, 11,11,11,11, 11,11,11,11))

predict(Model.Comp1,PRED.Int.TNaH,level=0)

#[1] 4.392273 6.506973 4.165536 6.280236 3.312673 5.427372 3.140567 5.255267 2.287704 4.402404 3.506006
#[12] 5.620706 2.653143 4.767843
#attr(,"label")
#[1] "Predicted values"


PRED.Int.TNaH$TolerHomo <-predict(Model.Comp1,PRED.Int.TNaH,level=0)
with(PRED.Int.TNaH,interaction.plot(IntRelig,TNaH,TolerHomo, legend=T,xlab="Intensidade", 
                                    ylab="Média de tolerância política", 
                                    main="Interação entre intensidade religiosa e Direitos 
dos Homossexuais (TNaH)",
                                    col = "steelblue"))
