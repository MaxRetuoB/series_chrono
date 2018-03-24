### Script travail sur les s?ries chronologiques

## Application 1 

load("euro_dol_hebdo.RData")

eudol<-ts(eudol1[,2],start=c(1990,48),freq=52)

summary(eudol)
plot(eudol,main="Cours Euro/Dollar \n du 26/11/1990 au 19/12/2011",xlab="Dates",ylab="Taux Euro/$")

monthplot(eudol)

eudol1[which(eudol==min(eudol)),]
eudol1[which(eudol==max(eudol)),]

acf(eudol)
lag.plot(eudol,6)

plot(diff(eudol))
acf(diff(eudol))
Box.test(diff(eudol))

## S?rie Gaz

load("gaz.RData")
str(gaz)
imp<-ts(gaz[,2],start=c(1981,1),freq=12)
cons<-ts(gaz[,3],start=c(1981,1),freq=12)

ts.plot(cons,imp,lty=c(1,2),lwd=c(1,2),xlab="Dates",main="Evolution de la consommation et des  importations de gaz \nen France 1981M1 ? 2011M12 en TWh",ylab="TWh")
legend(1980,370,legend=c("consommation", "Importations"),lty=c(1,2),lwd=c(1,2),bty="n")


monthplot(cons, main="Month-Plot de la consommation de Gaz", ylab="Mois",xlab="Consommation en TWh")
monthplot(imp,main="Month-Plot des importations de Gaz", ylab="Mois",xlab="Importations en TWh")

lag.plot(cons,24)
lag.plot(imp,12)

# Transformation en donn?es annuelles

par(mfrow=c(2,1))
plot(aggregate(cons),main="Evolution des donn?es agr?g?es au niveau annuel\n de la consommation de gaz en TWh",xlab="Dates",ylab="Consommation aggr?g?e")
plot(aggregate(imp),main="Evolution des donn?es agr?g?es au niveau annuel\n des importations de gaz en TWh",xlab="Dates",ylab="Importations aggr?g?es")
par(mfrow=c(1,1))

# Calcul des taux de croissance 

agrcons<-aggregate(cons)
txglob<-(agrcons[31]/agrcons[1])-1
txannmoy<-((txglob+1)^(1/30))-1
txglob
txannmoy

agrimp<-aggregate(imp)
txglob<-(agrimp[31]/agrimp[1])-1
txannmoy<-((txglob+1)^(1/30))-1
txglob
txannmoy

txcons<-numeric(0)
for(i in 1:30){
              tx<-agrcons[i+1]/agrcons[i]-1
              txcons<-append(txcons,tx)
              }
txcons



tximp<-numeric(0)
for(i in 1:30){
  tx<-agrimp[i+1]/agrimp[i]-1
  tximp<-append(tximp,tx)
}


## construction d'un mod?le de pr?vision sur la p?riode 01/81 12/2010

## Consommation ####
###################

cons1<-window(cons,start=c(1981,1),end=c(2010,12))

t<-1:length(cons1)   
tlin<-lm(cons1~t)       
summary(tlin) 
tquad<-lm(cons1~t+I(t^2))
summary(tquad)   

tcons1<-tlin$fitted

# Rapports ? la tendance et coefficient saisonniersx

eccons<-cons1/tcons1
plot(eccons)


cs<-numeric(0)
for(i in 1:12){
  x<-window(eccons,start=c(1981,i),deltat=1)
  m<-mean(x)
  cs<-append(cs,m)
}
cs_cor<-cs/mean(cs)
cs_cor

plot(cs_cor,type="l",main="Profil saisonnier de la consommation de Gaz",xlab="Mois",ylab="Coef. Saisonniers")
abline(h=1,lty=2)

# Utilisation de decompose
?decompose
dec_cons1<-decompose(cons1,"multiplicative")
dec_cons1
plot(dec_cons1)

## Comparaison des deux estimations des coef saisonniers

plot(cs_cor,type="l",ylab="coefficients de saisonnalité",xlab="mois",sub="sources : O Peron",main="coefficients de saisonnalités de la consommation de gaz\ncalculés et par la fonction decompose",lty=1,col=2)
lines(decompose(cons1,"multiplicative")$figure,lty=3,col=1,lwd=3)

lines(dec_cons1$figure, lty=2,col=2)
legend(locator(1),legend=c("coefficients calculés","coefficients decompose"),lty=c(1,3),col=c(2,1),lwd=c(1,3),bty="n")
abline(h=1,lty=2)

data.frame("CS1"=cs_cor,"CS2"=dec_cons1$figure)

#Serie CVS et serie ajust?e

cons_cvs<-cons1/cs_cor
ts.plot(cons1,cons_cvs,lwd=c(1,2),main="Consommation de Gaz : \n s?rie brute et s?rie CVS",xlab="Dates",ylab="Cons. Twh")


aj_cons<-ts(tcons1*cs_cor,start=start(cons1),freq=12)
r_cons<-cons1/aj_cons
plot(r_cons,main="Composante r?siduelle",xlab="Dates")
abline(h=1,lty=2,lwd=1.5)

ts.plot(cons1,cons_cvs,aj_cons,lwd=c(1,2,1),main="Consommation de gaz : \n s?rie brute, s?rie CVS et s?rie ajust?e",xlab="Dates",ylab="Cons. Twh",col=c(1,1,2))
legend(locator(1),legend=c("S?rie Observ?e", "S?rie CVS","S?rie ajust?e"),lty=c(1,1,1),col=c(1,1,2),lwd=c(1,2,1),bty="n")

sse_cons<-sum((cons-aj_cons)^2)
sse_cons
# Pr?visons

prev_tend<-predict(tlin,newdata=data.frame(t=361:372))
prev_cons<-ts(prev_tend*cs_cor,start=c(2011,1),freq=12)
prev_cons

x<-c(73.76800,61.10700,55.62900,27.95500,22.82000,19.96000,19.20200,17.23700,21.50200,35.25500,51.00900,60.84100)
x
obse<-ts(x,start=c(2011,1),freq=12)
obse
(obse-prev_cons)^2

sum((obse-prev_cons)^2)
ts.plot(cons1,prev_cons,lwd=c(1,2),lty=c(1,2),main="Consommation de Gaz : \n observations (1980-2010) et prévisions pour 2011",xlab="Années",sub="Sources: O. Peron",ylab="Consommation de gaz en Twh",col=c(1,2))


ts.plot(aj_cons,lwd=c(1,2),lty=c(1,2),main="Consommation de Gaz : \n série ajustée",xlab="Années",sub="Sources: O. Peron",ylab="Consommation de gaz en Twh",col=c(1,2))
ts.plot(aj_cons,prev_cons,lwd=c(1,2),lty=c(1,2),main="Consommation de Gaz : \n série ajustée et prévision",xlab="Années",sub="Sources: O. Peron",ylab="Consommation de gaz en Twh",col=c(1,2))


abline(v=2010+(11/12),lty=2,lwd=2)
legend(locator(1),legend=c("observation","Prévision"),lty=c(1,2),col=c(1,2),lwd=c(1,2),bty="n")


prev_cons
erreur_prec_carre<-sum((prev_cons-window(cons,start=c(2011,1)))^2)
erreur_prec_carre
window(cons,start=c(2011,1))
######Lissage exponentiel####
############################

#M?thode Holt-Winter application ? la s?rie cons


# Choix des constantes de lissage
cons_hw<-HoltWinters(cons1,alpha=0.3,beta=0.25,gamma=0.2,seasonal="multiplicative")
cons_hw

#Calcul des constantes de lissage
cons_hw_opt<-HoltWinters(cons1,seasonal="multiplicative")
cons_hw_opt


phw1<-predict(cons_hw,12)
phw1
phw2<-predict(cons_hw_opt,12)
phw2
(obse-phw1)^2

sum((obse-phw1)^2)
(obse-phw2)^2

sum((obse-phw2)^2)


cons_hw$SSE
cons_hw_opt$SSE
cbind(predict(cons_hw,12),predict(cons_hw_opt,12))

ts.plot(cons1,predict(cons_hw,12),predict(cons_hw_opt,12),lty=c(1,2,3),lwd=c(1,2,2),col=c(1,2,3),xlab="Dates",ylab="Gwh",main="Pr?vision Holt-Winter, consommation de gaz")

abline(v=2010+11/12)


erreur_prec_carreHw1<-sum((phw1-window(cons,start=c(2011,1)))^2)
erreur_prec_carreHw2<-sum((phw2-window(cons,start=c(2011,1)))^2)


round(data.frame("obs"=window(cons,start=c(2011,1)),"M1"=prev_cons,"M2"=predict(cons_hw,12),"M3"=predict(cons_hw_opt,12)))
data.frame(erreur_prec_carre,erreur_prec_carreHw1,erreur_prec_carreHw2)

ts.plot(cons1,predict(cons_hw,12),predict(cons_hw_opt,12),prev_cons,lty=c(1,2,3,4),lwd=c(1,1,1,1),col=c(1,2,3,4),xlab="Années",ylab="Consommation en Twh",main="Prévision par les différents modèles pour 2011",sub="Sources: O Peron")
legend(locator(1),legend=c("valeurs observées","holt winter-constantes fixées","holtwinter-constantes optimales","decompose"),lty=c(1,2,3,4),col=c(1,2,3,4),lwd=c(1,1,1,1),bty="n")


abline(v=2010+11/12)

ts.plot(window(cons,start=c(2011,1)),predict(cons_hw,12),predict(cons_hw_opt,12),prev_cons,lty=c(1,2,3,4),col=c(1,2,3,4),xlab="Mois",ylab="Consommation en TWh",main="Prévisions par les différents modèles pour 2011,\n et comparaison avec les valeurs observées",sub="Sources: O Peron")
legend(locator(1),legend=c("valeurs observées","holt winter-constantes fixées","holtwinter-constantes optimales","decompose"),lty=c(1,2,3,4),col=c(1,2,3,4),lwd=c(1,1,1,1),bty="n")


legend(locator(1),legend=c("valeurs observées","holt winter-constantes fixées","holtwinter-constantes optimales","decompose"),lty=c(1,2,3,4),col=c(1,2,3,4),lwd=c(1,2,3,4),bty="n")



## importations ######
######################

imp1<-window(imp,start=c(1981,1),end=c(2010,12))

t<-1:length(imp1)   
tlin<-lm(imp1~t)       
summary(tlin) 
tquad<-lm(imp1~t+I(t^2))
summary(tquad)   # La tendance lin?aire est plus adapt?e 

timp1<-tlin$fitted

# Rapports ? la tendance et coefficient saisonniersx

ecimp<-imp1-timp1
plot(ecimp)



cs<-numeric(0)
for(i in 1:12){
  x<-window(ecimp,start=c(1981,i),deltat=1)
  m<-mean(x)
  cs<-append(cs,m)
}
cs_cor<-cs-mean(cs)
cs_cor
plot(cs_cor,type="l",main="Profil saisonnier des importations de Gaz",xlab="Mois",ylab="Coef. Saisonniers")
abline(h=0,lty=2)

#Serie CVS et serie ajust?e

imp_cvs<-imp1-cs_cor
imp1
imp_cvs
ts.plot(imp1,imp_cvs,lty=c(1,2),main="importations de Gaz : \n série observée et série CVS",xlab="Années",ylab="importation de gaz en Twh",sub="Sources : O Peron")
legend(locator(1),legend=c("Série Observée", "Série CVS"),lty=c(1,2),col=c(1,1),lwd=c(1,1),bty="n")



aj_imp<-ts(timp1+cs_cor,start=start(imp1),freq=12)
ts.plot(aj_imp,main="importations de gaz: \n série ajustée",ylab="Twh",xlab="Années",sub="Sources: O Peron")
r_imp<-imp1-aj_imp
plot(r_imp,main="Composante residuelle",xlab="Années",ylab="composante residuelle",sub="Sources: O Peron")
abline(h=0,lty=2,lwd=1.5)

ts.plot(imp1,imp_cvs,aj_imp,lwd=c(1,2,1),main="importations de gaz : \n s?rie brute, s?rie CVS et s?rie ajust?e",xlab="Dates",ylab="imp. Twh",col=c(1,1,2))
legend(locator(1),legend=c("S?rie Observ?e", "S?rie CVS","S?rie ajust?e"),lty=c(1,1,1),col=c(1,1,2),lwd=c(1,2,1),bty="n")

sse_imp<-sum((imp1-aj_imp)^2)
sse_imp
# Pr?visons
##################
######################
###################
########################
prev_tend<-predict(tlin,newdata=data.frame(t=361:372))
prev_imp<-ts(prev_tend+cs_cor,start=c(2011,1),freq=12)
prev_imp
(prev_imp-window(imp,start=c(2011,1)))^2
sum((prev_imp-window(imp,start=c(2011,1)))^2)
ts.plot(imp1,prev_imp,lty=c(1,2),lwd=c(1,2),main="importations de Gaz : \n observations (1981-2010) et prévisions pour 2011",xlab="Années",ylab="importations en  Twh",col=c(1,2),sub="Sources: O. Peron")
legend(locator(1),legend=c("Observations","Prévisions"),lty=c(1,2),col=c(1,2),lwd=c(1,2),bty="n")
abline(v=2010+(11/12),lty=2,lwd=2)

ts.plot(aj_imp,prev_imp,lty=c(1,2),lwd=c(1,2),main="importations de Gaz : \n série ajustée et prévisions pour 2011",xlab="Années",ylab="importations en  Twh",col=c(1,2),sub="Sources: O. Peron")

legend(locator(1),legend=c("série ajustée","Prévisions"),lty=c(1,2),col=c(1,2),lwd=c(1,2),bty="n")


prev_imp
erreur_prec_carre<-sum((prev_imp-window(imp,start=c(2011,1)))^2)
erreur_prec_carre
window(imp,start=c(2011,1))
imp
######Lissage exponentiel####
############################

#M?thode Holt-Winter application ? la s?rie imp4



imp_hw<-HoltWinters(imp1,alpha=0.3,beta=0.25,gamma=0.2,seasonal="additive")
imp_hw
imp_hw_opt<-HoltWinters(imp1,seasonal="additive")
imp_hw_opt
phw1<-predict(imp_hw,12)
phw1
(phw1-window(imp,start=c(2011,1)))^2
sum((phw1-window(imp,start=c(2011,1)))^2)
phw2<-predict(imp_hw_opt,12)
phw2
(phw2-window(imp,start=c(2011,1)))^2
sum((phw2-window(imp,start=c(2011,1)))^2)

imp_hw$SSE
imp_hw_opt$SSE

ts.plot(imp1,predict(imp_hw,12),predict(imp_hw_opt,12),lty=c(1,2,3),lwd=c(1,2,2),col=c(1,2,3),xlab="Dates",ylab="Gwh",main="Pr?vision Holt-Winter, importations de gaz")
abline(v=2010+11/12)
ts.plot(window(imp,start=c(2011,1)),predict(imp_hw,12),predict(imp_hw_opt,12),prev_imp,lty=c(1,2,3,4),lwd=c(1,2,3,4),col=c(1,2,3,4),xlab="Mois",ylab="Importations en Twh",main="importations de gaz\n Prévisions Holt-Winter et decompose",sub="Sources:O Peron")
legend(locator(1),legend=c("valeurs observées","holt winter-constantes fixées","holtwinter-constantes optimales","decompose"),lty=c(1,2,3,4),col=c(1,2,3,4),lwd=c(1,2,3,4),bty="n")
abline(v=2010+11/12)

erreur_prec_carreHw1<-sum((phw1-window(imp,start=c(2011,1)))^2)
erreur_prec_carreHw2<-sum((phw2-window(imp,start=c(2011,1)))^2)


round(data.frame("obs"=window(imp,start=c(2011,1)),"M1"=prev_imp,"M2"=phw1,"M3"=phw2))
data.frame(erreur_prec_carre,erreur_prec_carreHw1,erreur_prec_carreHw2)

ts.plot(imp1,predict(imp_hw,12),predict(imp_hw_opt,12),prev_imp,lty=c(1,2,3,4),lwd=c(1,2,2,2),col=c(1,2,3,4),xlab="Années",ylab="Importations en Twh",main="Importations observées et Prévisions \n(decompose,Holt-Winter fixé et optimale)",sub="Sources:O Peron")
legend(locator(1),legend=c("importations observées","holt winter-constantes fixées","holtwinter-constantes optimales","decompose"),lty=c(1,2,3,4),col=c(1,2,3,4),lwd=c(1,2,2,2),bty="n")


abline(v=2010+11/12)

ts.plot(predict(imp_hw,12),predict(imp_hw_opt,12),prev_imp,lty=c(1,2,3),col=c(1,2,3),xlab="Dates",ylab="Gwh",main="Pr?visions Holt-Winter, importations de gaz")
legend(locator(1),legend=c("Prev_Hw","Prev_HWOpt","prev_Decomp."),lty=1:3,col=1:3,bty="n")

# Utilisation de decompose

dec_imp1<-decompose(imp1,"additive")
dec_imp1
plot(dec_imp1)

plot(cs_cor,type="l",col=2,main="coefficients de saisonnalité calculés et par decompose\n pour les importations de gaz",xlab="mois",ylab="coefficients de saisonnalité",sub="Sources: O Peron")
lines(dec_imp1$figure, lty=2,col=1,lwd=3)
legend(locator(1),legend=c("coefficients calculés","coefficients decompose"),lty=c(1,3),col=c(2,1),lwd=c(1,3),bty="n")
plot(dec_imp1$figure)
lines(cs_cor)
abline(h=0,lty=2)
data.frame("CS1"=cs_cor,"CS2"=dec_imp1$figure)


#### Application 2

### Stationnarit? et r?gressions fallacieuses

load("exo1_serchro.RData")
str(exo1)

exo1<-ts(exo1[,2:5],start=c(1990,6),freq=12)
x1<-exo1[,1]
x1
x2<-exo1[,2]
x2
x3<-exo1[,3]
x3
x4<-exo1[,4]
x4
plot(x1,main="graphique de x1",ylab="Années",sub="Sources: O Peron")
plot(x2,main="graphique de x2",ylab="Années",sub="Sources: O Peron")
plot(x3,main="graphique de x3",ylab="Années",sub="Sources: O Peron")
plot(x4,main="graphique de x4",ylab="Années",sub="Sources: O Peron")
plot(exo1, main="Trac? des 4 s?ries chronologiques \nde Juin 1990 ? Sept. 1998",xlab="Dates")

# ACF PACF et ljung box
par(mfrow=c(2,1))
acf(exo1[,1])
acf(exo1[,1],type="partial")
par(mfrow=c(1,1))


acf(x1,lag.max=100,main="ACF pour la série chronologique x1",xlab="retards",sub="Sources: O. Peron")
acf(x2,lag.max=100,main="ACF pour la série chronologique x2",xlab="retards",sub="Sources: O. Peron")
acf(x3,lag.max=100,main="ACF pour la série chronologique x3",xlab="retards",sub="Sources: O. Peron")
acf(x4,lag.max=100,main="ACF pour la série chronologique x4",xlab="retards",sub="Sources: O. Peron")
Box.test(exo1[,1],lag=1)

# Diff?rences premi?re de X2 et x3

dx2<-diff(exo1[,2])
dx3<-diff(exo1[,3])
plot(dx2,main="différence première de x2",xlab="Années",sub="Sources: O Peron")
plot(dx3,main="différence première de x3",xlab="Années",sub="Sources: O Peron")
acf(dx2,lag.max=100,main="ACF pour la différence première de la série chronologique x2",xlab="retards",sub="Sources: O. Peron")
acf(dx3,lag.max=100,main="ACF pour la différence première de la série chronologique x3",xlab="retards",sub="Sources: O. Peron")


ts.plot(dx2,dx3,main= "Trac? de la diff?rence premi?re\n des s?ries x2 zt x3",xlab="Dates")

par(mfrow=c(2,1))
acf(dx2)
acf(dx3)
par(mfrow=c(1,1))

Box.test(dx2)
Box.test(dx3)

# Regression sur le premier retard :recherche de racine unitaire

#x2

library(dynlm)

mx2<-dynlm(exo1[,2]~L(exo1[,2],1))
summary(mx2) # La constante est non significative
mx2<-dynlm(exo1[,2]~L(exo1[,2],1)-1)
summary(mx2)
confint(mx2)



#x3


mx3<-dynlm(exo1[,3]~L(exo1[,3],1))
summary(mx3) 
confint(mx3)

mxx4<-dynlm(exo1[,4]~L(exo1[,4],1))
summary(mxx4)
confint(mxx4)


## Diff?rences premi?re de X4

dx4<-diff(exo1[,4])
plot(dx4,main="différence première de la série x4",sub="sources: O Peron",ylab="Années")
acf(dx4,lag.max=99, main="ACF de la différence première de x4",ylab="retards",sub="Sources: O Peron")
dx4_2<-diff(exo1[,4],lag=2)
plot(dx4_2,main="différence seconde de la série x4",sub="sources: O Peron",xlab="Années")
acf(dx4_2,lag.max=98,main="ACF de la différence seconde de x4",ylab="retards",sub="Sources: O Peron")
Box.test(dx4,type="Ljung-Box")
Box.test(dx4_2,type="Ljung-Box")
# Tendance lin?aire

t<-1:100
tend<-lm(exo1[,4]~t)$fitted

det_x4<-exo1[,4]-tend

plot(det_x4,main="série x4 corrigé de sa tendance",xlab="Années",sub="Sources: O Peron")
acf(det_x4,lag.max=100,main="ACF de la série x4 corrigé de sa tendance",xlab="retards",sub="Sources: O Peron")
Box.test(det_x4)


# R?gression de x2 sur x3 : r?gression fallacieuse
ts.plot(x2,x3,lty=c(1,3),col=c(2,1),lwd=c(1,3),main="Chronogramme des séries x2 et x3",ylab="x2,x3",xlab="Années",sub="Sources: O Peron")
legend(locator(1),legend=c("série x2","série x 3"),lty=c(1,3),col=c(2,1),lwd=c(1,3),bty="n")


m1<-lm(exo1[,2]~exo1[,3]-1)
summary(m1)
m1res<-m1$resid
m1res<-ts(m1res,start=c(1990,6),freq=12)
ts.plot(m1res,main="résidus de la régression de x2 sur x3",ylab="résidus",xlab="Années",sub="Sources: O Peron") 
acf(m1res,main="ACF des résidus",xlab="retards",sub="Sources: O Peron",lag.max=100)
Box.test(m1res)
reg32<-lm(dx2~dx3-1)
summary(reg32)
plot(reg32$res)
res32<-reg32$res
acf(res32)

res_fin<-dynlm(m1res~L(m1res,1)-1)
summary(res_fin) 
confint(res_fin)

##### R?gression avec s?ries chronologiques  ###"
################################################

library("lmtest")
library("nlme")

## Application 3 : Bangla

load("bangla.RData")
str(bangla) 
summary(bangla)# Pas de valeurs manquantes
plot(log(bangla$prix),log(bangla$surface), main="Nuage de points entre la surface et le prix de vente des récoltes\n variables en log",xlab="log(prix)",ylab="log(surfaces)",sub="Sources: O Peron")


# Estimation de l'?lasticit?

m<-lm(log(surface)~log(prix),data=bangla)
summary(m) 
confint(m)

acf(m$resid,lag.max=32,xlab="retards",main="ACF des résidus de la régression entre prix et surface",sub="Sources: O Peron")

acf(m$resid,type="partial")
dwtest(m)



m2<-gls(log(surface)~log(prix),data=bangla,correlation=corAR1())
summary(m2)

confint(m2)

# Mod?le ARDL(1,1)

ls<-ts(log(bangla$surface),freq=1)
lp<-ts(log(bangla$prix),freq=1)

m3<-dynlm(ls~L(ls,1)+lp+L(lp,1))
summary(m3)
acf(m3$res,lag.max=29,main="ACF pour le modèle ARDL \nappliqué à la relation entre surface et prix",sub="Sources: O Peron",xlab="retards")
dwtest(m3) # Pas d'autocorr?lation des r?sidus

# Estimation des multiplicateurs

coef(m3)

bet0<-coef(m3)[3]
bet0
bet1<-coef(m3)[4]+bet0*coef(m3)[2]
bet1
bet2<-bet1*coef(m3)[2]
bet3<-bet2*coef(m3)[2]
bet4<-bet3*coef(m3)[2]
bet<-c(bet0,bet1,bet2,bet3,bet4)
bet
cumsum(bet)

mult_tot<-coef(m3)[3]+(coef(m3)[3]+coef(m3)[3]*coef(m3)[2])/(1-coef(m3)[2])
mult_tot


## Application 4 : Mod?le ARDL


load("conso.RData")
str(conso) 
summary(conso)# Pas de valeurs manquantes

conso<-ts(conso,start=c(1960,1),freq=4)
grev<-conso[,1]
gcons<-conso[,2]
plot(grev,main="chronogramme du revenu disponible des ménages aux Etats Unis",xlab="Années : 1960Q1 - 2009Q4",sub="Sources: O Peron",ylab="taux de croissance")
plot(gcons,main="chronogramme de la consommation  des ménages aux Etats Unis",xlab="Années : 1960Q1 - 2009Q4",sub="Sources: O Peron",ylab="taux de croissance")
monthplot(grev)

plot(conso,main="Evolution des taux de croissance de la consommation \n et du revenu disponible aux USA 1960Q1 ? 2009Q4",xlab="Dates")
acf(grev,lag.max=200,main="ACF de l'évolution du taux de croissance du revenu des ménages\n aux Etats Unis",xlab="retards",sub="Sources: O Peron")
acf(gcons,lag.max=200,main="ACF de l'évolution du taux de croissance de la consommation\n des ménages aux Etats Unis",xlab="retards",sub="Sources: O Peron")


### mod?le 1

mod1<-lm(gcons~grev,data=conso)
summary(mod1)
resid<-mod1$resid
acf(mod1$resid,lag.max=200,main="ACF des résidus de la régression \nde la consommation sur les revenus",xlab="retards",sub="Sources: O Peron")
acf(mod1$resid,type="partial")
dwtest(mod1)

resid<-ts(mod1$resid,start=c(1960,1),freq=4)
grev<-ts(conso[,1],start=c(1960,1),freq=4)
gcons<-ts(conso[,2],start=c(1960,1),freq=4)

### Test du multiplicateur de Lagrange d'une autocorr?lation d'ordre 2
library("zoo")
lm_t<-dynlm(resid~grev+L(resid,1:2))
summary(lm_t)
Stat_test_LM<-(200-2)*summary(lm_t)$r.squared
1-pchisq(Stat_test_LM,df=2) # la p-value est tr?s faible, on rejeete donc l'hypoth?se d'absence d'autocorr?lation

## Estimation avec correction de l'autocorr?lation d'ordre 2

mod12<-gls(gcons~grev,correlation=corARMA(p=2,q=0))
summary(mod12)
confint(mod12)

#Mod?le 2

mod2<-dynlm(gcons~L(gcons,1)+grev)
summary(mod2)
AIC(mod2)
AIC(mod1)
resid22<-mod2$resid
acf(resid22,main="ACFdes résidus pour la régression du modèle2",xlab="retards",sub=Sources: O Peron)
acf(resid22,lag.max=196,main="ACF des résidus pour la régression du modèle2",xlab="retards",sub="Sources: O Peron")
Box.test(mod2$resid,lag=1)

Box.test(mod2$resid,lag=2)


?Box.test
dwtest(mod12)

#Mod?le 3

mod3<-dynlm(gcons~L(gcons,1:2)+grev)
summary(mod3)
AIC(mod3)

resid33<-mod3$resid
plot(mod3$resid,type="l")
acf(resid33,lag.max=194,main="ACF des résidus du modèle3",xlab="retards",sub="Sources: O Peron")
Box.test(mod3$resid)
Box.test(resid33,lag=2)
#Mod?le 4

mod4<-dynlm(gcons~L(gcons,1:2)+grev+L(grev,1))
summary(mod4)
AIC(mod4)

resid44<-mod4$resid
acf(resid44,lag.max=194,main="ACF des résidus du modèle4",xlab="retards",sub="Sources: O Peron")
Box.test(mod4$resid,lag=1)
Box.test(mod4$resid,lag=2)
# Ajout de retard suppl?mentaires

#Ajout ordre 2 pour grev

mod42<-dynlm(gcons~L(gcons,1:2)+grev+L(grev,1:2))
summary(mod42)
AIC(mod42)

acf(mod42$resid)
Box.test(mod42$resid,type="Ljung-Box")

#Ajout ordre 2 et 3 

mod43<-dynlm(gcons~L(gcons,1:3)+grev+L(grev,1:3))
summary(mod43)
AIC(mod43)

acf(mod43$resid)
Box.test(mod43$resid,type="Ljung-Box")

# Mod?le 5

mod5<-dynlm(gcons~L(gcons,2)+grev+L(grev,1))
summary(mod5)
AIC(mod5)

acf(mod5$resid)
Box.test(mod5$resid,type="Ljung-Box")

confint(mod5)


