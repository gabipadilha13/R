# Graficos Maira

#install.packages("readxl")
library(readxl)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggpubr")
library(ggpubr)

#**********************GRAFICOS HOMENS:**************************
Homens_Coorte <- read_excel("C:/Users/e310034517/Desktop/Homens.xlsx",  sheet = "Coorte")
#View(Homens_Coorte)
Homens_Idade <- read_excel("C:/Users/e310034517/Desktop/Homens.xlsx",  sheet = "Idade")
#View(Homens_Idade)
Homens_Periodo <- read_excel("C:/Users/e310034517/Desktop/Homens.xlsx",  sheet = "Periodo")
#View(Homens_Periodo)

a<- ggplot(Homens_Coorte, aes(x=Coorte, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Ano de Nascimento") + scale_x_continuous(breaks = c(1970,1972,1974,1976,1978,1980,1982,1984,1986,1988,1990,1992,1994,1996,1998,2000)) + scale_y_continuous(breaks = c(-0.02 , -0.01, 0, 0.01, 0.02, 0.03, 0.04, 0.05))
a<- a + theme_bw()+ ggtitle("Efeito coorte") + theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept=0) 
a

b<- ggplot(Homens_Idade, aes(x=idade, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Idade")+ scale_x_continuous(breaks = c(16,17,18,19,20,21,22,23,24,25,26,27,28,29)) + scale_y_continuous(breaks=c(-0.01,0,0.01,0.02,0.03,0.04,0.05))
b<- b +  theme_bw() + ggtitle("Efeito idade")+ theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0)
b

c<- ggplot(Homens_Periodo, aes(x=Periodo, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Ano")+ scale_x_continuous(breaks = c(1997,1999,2001,2003,2005,2007,2009,2011,2013,2015)) + scale_y_continuous(breaks=c(-0.015,-0.01,-0.005,0,0.005,0.01,0.015))
c<- c +  theme_bw() + ggtitle("Efeito período")+ theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0)
c

graf_Homens <- ggarrange(a,ggarrange(b,c, ncol = 2),nrow = 2)
graf_Homens

#***********************GRAFICOS MULHERES:***************************************************
Mulheres_Coorte <- read_excel("C:/Users/e310034517/Desktop/Mulheres.xlsx",  sheet = "Coorte")
#View(Mulheres_Coorte)
Mulheres_Idade <- read_excel("C:/Users/e310034517/Desktop/Mulheres.xlsx",  sheet = "Idade")
#View(Mulheres_Idade)
Mulheres_Periodo <- read_excel("C:/Users/e310034517/Desktop/Mulheres.xlsx",  sheet = "Periodo")
#View(Mulheres_Periodo)

d<- ggplot(Mulheres_Coorte, aes(x=Coorte, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Ano de Nascimento") + scale_x_continuous(breaks = c(1970,1972,1974,1976,1978,1980,1982,1984,1986,1988,1990,1992,1994,1996,1998,2000)) + scale_y_continuous(breaks = c(-0.16,-0.14,-0.12,-0.10,-0.08,-0.06,-0.04,-0.02,0.00))
d<- d + theme_bw()+ ggtitle("Efeito coorte") + theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0) 
d

e<- ggplot(Mulheres_Idade, aes(x=idade, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Idade")+ scale_x_continuous(breaks = c(16,17,18,19,20,21,22,23,24,25,26,27,28,29)) + scale_y_continuous(breaks=c(-0.08,-0.04,0.00,0.04,0.08))
e<- e +  theme_bw() + ggtitle("Efeito idade")+ theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0)
e

f<- ggplot(Mulheres_Periodo, aes(x=Periodo, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Ano")+ scale_x_continuous(breaks = c(1997,1999,2001,2003,2005,2007,2009,2011,2013,2015)) + scale_y_continuous(breaks=c(-0.02,-0.01,0.00,0.01,0.02,0.03))
f<- f +  theme_bw() + ggtitle("Efeito período")+ theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0)
f

graf_Mulheres <- ggarrange(d,ggarrange(e,f, ncol = 2),nrow = 2)
graf_Mulheres

#************************GRAFICOS DIF MAES:********************
Dif_Maes_Coorte <- read_excel("C:/Users/e310034517/Desktop/Dif_Maes.xlsx",  sheet = "Coorte")
#View(Dif_Maes_Coorte)
Dif_Maes_Idade <- read_excel("C:/Users/e310034517/Desktop/Dif_Maes.xlsx",  sheet = "Idade")
#View(Dif_Maes_Idade)
Dif_Maes_Periodo <- read_excel("C:/Users/e310034517/Desktop/Dif_Maes.xlsx",  sheet = "Periodo")
#View(Dif_Maes_Periodo)

g<- ggplot(Dif_Maes_Coorte, aes(x=Coorte, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Ano de Nascimento") + scale_x_continuous(breaks = c(1970,1972,1974,1976,1978,1980,1982,1984,1986,1988,1990,1992,1994,1996,1998,2000)) + scale_y_continuous(breaks = c(-0.10,-0.05,0.00,0.05,0.10))
g<- g + theme_bw()+ ggtitle("Efeito coorte") + theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0) 
g

h<- ggplot(Dif_Maes_Idade, aes(x=idade, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Idade")+ scale_x_continuous(breaks = c(16,17,18,19,20,21,22,23,24,25,26,27,28,29)) + scale_y_continuous(breaks=c(-0.45,-0.4,-0.35,-0.3,-0.25,-0.2,-0.15,-0.1,-0.05,0,0.05))
h<- h +  theme_bw() + ggtitle("Efeito idade")+ theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0)
h

i<- ggplot(Dif_Maes_Periodo, aes(x=Periodo, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Ano")+ scale_x_continuous(breaks = c(1997,1999,2001,2003,2005,2007,2009,2011,2013,2015)) + scale_y_continuous(breaks=c(-0.03,-0.02,-0.01,0,0.01,0.02,0.03))
i<- i +  theme_bw() + ggtitle("Efeito período")+ theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0)
i

graf_Dif_Maes <- ggarrange(g,ggarrange(h,i, ncol = 2),nrow = 2)
graf_Dif_Maes

#******************GRAFICOS EDUC_HOMENS:**************************************
Educ_Homens_Coorte <- read_excel("C:/Users/e310034517/Desktop/Educ_Homens.xlsx",  sheet = "Coorte")
#View(Educ_Homens_Coorte)
Educ_Homens_Idade <- read_excel("C:/Users/e310034517/Desktop/Educ_Homens.xlsx",  sheet = "Idade")
#View(Educ_Homens_Idade)
Educ_Homens_Periodo <- read_excel("C:/Users/e310034517/Desktop/Educ_Homens.xlsx",  sheet = "Periodo")
#View(Educ_Homens_Periodo)

j<- ggplot(Educ_Homens_Coorte, aes(x=Coorte, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Ano de Nascimento") + scale_x_continuous(breaks = c(1970,1972,1974,1976,1978,1980,1982,1984,1986,1988,1990,1992,1994,1996,1998,2000)) + scale_y_continuous(breaks = c(0.00,0.02,0.04,0.06,0.08))
j<- j + theme_bw()+ ggtitle("Efeito coorte") + theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0) 
j

k<- ggplot(Educ_Homens_Idade, aes(x=idade, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Idade")+ scale_x_continuous(breaks = c(16,17,18,19,20,21,22,23,24,25,26,27,28,29)) + scale_y_continuous(breaks=c(0.00,0.01,0.02,0.03,0.04))
k<- k +  theme_bw() + ggtitle("Efeito idade")+ theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0)
k

l<- ggplot(Educ_Homens_Periodo, aes(x=Periodo, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Ano")+ scale_x_continuous(breaks = c(1997,1999,2001,2003,2005,2007,2009,2011,2013,2015)) + scale_y_continuous(breaks=c(-0.010,-0.005,0.000,0.005,0.010,0.015,0.020))
l<- l +  theme_bw() + ggtitle("Efeito período")+ theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0)
l

graf_Educ_Homens <- ggarrange(j,ggarrange(k,l, ncol = 2),nrow = 2)
graf_Educ_Homens

#******************GRAFICOS EDUC_MULHERES:********************************
Educ_Mulheres_Coorte <- read_excel("C:/Users/e310034517/Desktop/Educ_Mulheres.xlsx",  sheet = "Coorte")
#View(Educ_Mulheres_Coorte)
Educ_Mulheres_Idade <- read_excel("C:/Users/e310034517/Desktop/Educ_Mulheres.xlsx",  sheet = "Idade")
#View(Educ_Mulheres_Idade)
Educ_Mulheres_Periodo <- read_excel("C:/Users/e310034517/Desktop/Educ_Mulheres.xlsx",  sheet = "Periodo")
#View(Educ_Mulheres_Periodo)

m<- ggplot(Educ_Mulheres_Coorte, aes(x=Coorte, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Ano de Nascimento") + scale_x_continuous(breaks = c(1970,1972,1974,1976,1978,1980,1982,1984,1986,1988,1990,1992,1994,1996,1998,2000)) + scale_y_continuous(breaks = c(-0.04,-0.02,0.00,0.02,0.04,0.06,0.08,0.10))
m<- m + theme_bw()+ ggtitle("Efeito coorte") + theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0) 
m

n<- ggplot(Educ_Mulheres_Idade, aes(x=idade, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Idade")+ scale_x_continuous(breaks = c(16,17,18,19,20,21,22,23,24,25,26,27,28,29)) + scale_y_continuous(breaks=c(0.00,0.05,0.10,0.15,0.20,0.25))
n<- n +  theme_bw() + ggtitle("Efeito idade")+ theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0)
n

o<- ggplot(Educ_Mulheres_Periodo, aes(x=Periodo, y = Coeficiente, color = alfa), show.legend=FALSE) + geom_line(aes(linetype=alfa),size=1.1, show.legend=FALSE, color="darkblue") + ylab("") + xlab("Ano")+ scale_x_continuous(breaks = c(1997,1999,2001,2003,2005,2007,2009,2011,2013,2015)) + scale_y_continuous(breaks=c(-0.03,-0.02,-0.01,0.00,0.01,0.02,0.03,0.04))
o<- o +  theme_bw() + ggtitle("Efeito período")+ theme(axis.title.x = element_text(face = "bold"), plot.title = element_text(hjust = 0.5))+ geom_hline(yintercept=0)
o

graf_Educ_Mulheres <- ggarrange(m,ggarrange(n,o, ncol = 2),nrow = 2)
graf_Educ_Mulheres


















