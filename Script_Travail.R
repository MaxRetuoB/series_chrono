### Script travail sur les séries chronologiques

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

## Série Gaz

load("gaz.RData")
str(gaz)
imp<-ts(gaz[,2],start=c(1981,1),freq=12)
cons<-ts(gaz[,3],start=c(1981,1),freq=12)

ts.plot(cons,imp,lty=c(1,2),lwd=c(1,2),xlab="Dates",main="Evolution de la consommation et des  importations de gaz \nen France 1981M1 à 2011M12 en TWh",ylab="TWh")
legend(1980,370,legend=c("consommation", "Importations"),lty=c(1,2),lwd=c(1,2),bty="n")


monthplot(cons, main="Month-Plot de la consommation de Gaz", ylab="Mois",xlab="Consommation en TWh")
monthplot(imp,main="Month-Plot des importations de Gaz", ylab="Mois",xlab="Importations en TWh")

lag.plot(cons,24)
lag.plot(imp,12)

# Transformation en données annuelles

par(mfrow=c(2,1))
plot(aggregate(cons),main="Evolution des données agrégées au niveau annuel\n de la consommation de gaz en TWh",xlab="Dates",ylab="Consommation aggrégée")
plot(aggregate(imp),main="Evolution des données agrégées au niveau annuel\n des importations de gaz en TWh",xlab="Dates",ylab="Importations aggrégées")
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


## construction d'un modèle de prévision sur la période 01/81 12/2010

## Consommation ####
###################

cons1<-window(cons,start=c(1981,1),end=c(2010,12))

t<-1:length(cons1)   
tlin<-lm(cons1~t)       
summary(tlin) 
tquad<-lm(cons1~t+I(t^2))
summary(tquad)   

tcons1<-tlin$fitted

# Rapports à la tendance et coefficient saisonniersx

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

dec_cons1<-decompose(cons1,"multiplicative")
plot(dec_cons1)

## Comparaison des deux estimations des coef saisonniers

plot(cs_cor,type="l")
lines(dec_cons1$figure, lty=2,col=2)
abline(h=0,lty=2)

data.frame("CS1"=cs_cor,"CS2"=dec_cons1$figure)

#Serie CVS et serie ajustée

cons_cvs<-cons1/cs_cor
ts.plot(cons1,cons_cvs,lwd=c(1,2),main="Consommation de Gaz : \n série brute et série CVS",xlab="Dates",ylab="Cons. Twh")


aj_cons<-ts(tcons1*cs_cor,start=start(cons1),freq=12)
r_cons<-cons1/aj_cons
plot(r_cons,main="Composante résiduelle",xlab="Dates")
abline(h=1,lty=2,lwd=1.5)

ts.plot(cons1,cons_cvs,aj_cons,lwd=c(1,2,1),main="Consommation de gaz : \n série brute, série CVS et série ajustée",xlab="Dates",ylab="Cons. Twh",col=c(1,1,2))
legend(locator(1),legend=c("Série Observée", "Série CVS","Série ajustée"),lty=c(1,1,1),col=c(1,1,2),lwd=c(1,2,1),bty="n")


# Prévisons

prev_tend<-predict(tlin,newdata=data.frame(t=361:372))
prev_cons<-ts(prev_tend*cs_cor,start=c(2011,1),freq=12)

ts.plot(cons1,cons_cvs,prev_cons,lwd=c(1,2,2),main="Consommation de Gaz : \n série brute et série CVS et prévisions",xlab="Dates",ylab="Cons. Twh",col=c(1,1,2))
abline(v=2010+(11/12),lty=2,lwd=2)
legend(locator(1),legend=c("Série Observée", "Série CVS","Prévisions"),lty=c(1,1,1),col=c(1,1,2),lwd=c(1,2,2),bty="n")


prev_cons
erreur_prec_carré<-sum((prev_cons-window(cons,start=c(2011,1)))^2)

######Lissage exponentiel####
############################

#Méthode Holt-Winter application à la série cons


# Choix des constantes de lissage
cons_hw<-HoltWinters(cons1,alpha=0.3,beta=0.25,gamma=0.25,seasonal="multiplicative")
cons_hw

#Calcul des constantes de lissage
cons_hw_opt<-HoltWinters(cons1,seasonal="multiplicative")
cons_hw_opt

phw1<-predict(cons_hw,12)
phw2<-predict(cons_hw_opt,12)

cons_hw$SSE
cons_hw_opt$SSE
cbind(predict(cons_hw,12),predict(cons_hw_opt,12))

ts.plot(cons1,predict(cons_hw,12),predict(cons_hw_opt,12),lty=c(1,2,3),lwd=c(1,2,2),col=c(1,2,3),xlab="Dates",ylab="Gwh",main="Prévision Holt-Winter, consommation de gaz")
abline(v=2010+11/12)


erreur_prec_carréHw1<-sum((phw1-window(cons,start=c(2011,1)))^2)
erreur_prec_carréHw2<-sum((phw2-window(cons,start=c(2011,1)))^2)


round(data.frame("obs"=window(cons,start=c(2011,1)),"M1"=prev_cons,"M2"=predict(cons_hw,12),"M3"=predict(cons_hw_opt,12)))
data.frame(erreur_prec_carré,erreur_prec_carréHw1,erreur_prec_carréHw2)

ts.plot(cons1,predict(cons_hw,12),predict(cons_hw_opt,12),prev_cons,lty=c(1,2,3,4),lwd=c(1,2,2,2),col=c(1,2,3,4),xlab="Dates",ylab="Gwh",main="Prévisions Holt-Winter, consommation de gaz")
abline(v=2010+11/12)

ts.plot(window(cons,start=c(2011,1)),predict(cons_hw,12),predict(cons_hw_opt,12),prev_cons,lty=c(1,2,3,4),col=c(1,2,3,4),xlab="Dates",ylab="Gwh",main="Prévisions Holt-Winter, consommation de gaz")
legend(locator(1),legend=c("Vraies valeurs","Prev_Hw","Prev_HWOpt","prev_Decomp."),lty=1:4,col=1:4,bty="n")



## importations ######
######################

imp1<-window(imp,start=c(1981,1),end=c(2010,12))

t<-1:length(imp1)   
tlin<-lm(imp1~t)       
summary(tlin) 
tquad<-lm(imp1~t+I(t^2))
summary(tquad)   # La tendance linéaire est plus adaptée 

timp1<-tlin$fitted

# Rapports à la tendance et coefficient saisonniersx

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
abline(h=1,lty=2)

#Serie CVS et serie ajustée

imp_cvs<-imp1-cs_cor
ts.plot(imp1,imp_cvs,lwd=c(1,2),main="importations de Gaz : \n série brute et série CVS",xlab="Dates",ylab="imp. Twh")


aj_imp<-ts(timp1+cs_cor,start=start(imp1),freq=12)
r_imp<-imp1-aj_imp
plot(r_imp,main="Composante résiduelle",xlab="Dates")
abline(h=0,lty=2,lwd=1.5)

ts.plot(imp1,imp_cvs,aj_imp,lwd=c(1,2,1),main="importations de gaz : \n série brute, série CVS et série ajustée",xlab="Dates",ylab="imp. Twh",col=c(1,1,2))
legend(locator(1),legend=c("Série Observée", "Série CVS","Série ajustée"),lty=c(1,1,1),col=c(1,1,2),lwd=c(1,2,1),bty="n")


# Prévisons

prev_tend<-predict(tlin,newdata=data.frame(t=361:372))
prev_imp<-ts(prev_tend+cs_cor,start=c(2011,1),freq=12)

ts.plot(imp1,imp_cvs,prev_imp,lwd=c(1,2,2),main="importations de Gaz : \n série brute et série CVS et prévisions",xlab="Dates",ylab="imp. Twh",col=c(1,1,2))
abline(v=2010+(11/12),lty=2,lwd=2)
legend(locator(1),legend=c("Série Observée", "Série CVS","Prévisions"),lty=c(1,1,1),col=c(1,1,2),lwd=c(1,2,2),bty="n")


prev_imp
erreur_prec_carré<-sum((prev_imp-window(imp,start=c(2011,1)))^2)

######Lissage exponentiel####
############################

#Méthode Holt-Winter application à la série imp4



imp_hw<-HoltWinters(imp1,alpha=0.3,beta=0.25,gamma=0.25,seasonal="additive")
imp_hw_opt<-HoltWinters(imp1,seasonal="additive")
phw1<-predict(imp_hw,12)
phw2<-predict(imp_hw_opt,12)

imp_hw$SSE
imp_hw_opt$SSE

ts.plot(imp1,predict(imp_hw,12),predict(imp_hw_opt,12),lty=c(1,2,3),lwd=c(1,2,2),col=c(1,2,3),xlab="Dates",ylab="Gwh",main="Prévision Holt-Winter, importations de gaz")
abline(v=2010+11/12)


erreur_prec_carréHw1<-sum((phw1-window(imp,start=c(2011,1)))^2)
erreur_prec_carréHw2<-sum((phw2-window(imp,start=c(2011,1)))^2)


round(data.frame("obs"=window(imp,start=c(2011,1)),"M1"=prev_imp,"M2"=phw1,"M3"=phw2))
data.frame(erreur_prec_carré,erreur_prec_carréHw1,erreur_prec_carréHw2)

ts.plot(imp1,predict(imp_hw,12),predict(imp_hw_opt,12),prev_imp,lty=c(1,2,3,4),lwd=c(1,2,2,2),col=c(1,2,3,4),xlab="Dates",ylab="Gwh",main="Prévisions Holt-Winter, importations de gaz")
abline(v=2010+11/12)

ts.plot(predict(imp_hw,12),predict(imp_hw_opt,12),prev_imp,lty=c(1,2,3),col=c(1,2,3),xlab="Dates",ylab="Gwh",main="Prévisions Holt-Winter, importations de gaz")
legend(locator(1),legend=c("Prev_Hw","Prev_HWOpt","prev_Decomp."),lty=1:3,col=1:3,bty="n")

# Utilisation de decompose

dec_imp1<-decompose(imp1,"additive")
plot(dec_imp1)

plot(cs_cor,type="l")
lines(dec_imp1$figure, lty=2,col=2)
abline(h=0,lty=2)
data.frame("CS1"=cs_cor,"CS2"=dec_imp1$figure)


#### Application 2

### Stationnarité et régressions fallacieuses

load("exo1_serchro.RData")
str(exo1)

exo1<-ts(exo1[,2:5],start=c(1990,6),freq=12)
plot(exo1, main="Tracé des 4 séries chronologiques \nde Juin 1990 à Sept. 1998",xlab="Dates")

# ACF PACF et ljung box
par(mfrow=c(2,1))
acf(exo1[,1])
acf(exo1[,1],type="partial")
par(mfrow=c(1,1))

Box.test(exo1[,1],type="Ljung-Box")

# Différences première de X2 et x3

dx2<-diff(exo1[,2])
dx3<-diff(exo1[,3])

ts.plot(dx2,dx3,main= "Tracé de la différence première\n des séries x2 zt x3",xlab="Dates")

par(mfrow=c(2,1))
acf(dx2)
acf(dx3)
par(mfrow=c(1,1))

Box.test(dx2,type="Ljung-Box")
Box.test(dx3,type="Ljung-Box")

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




## Différences première de X4

dx4<-diff(exo1[,4])
plot(dx4)

acf(dx4)
Box.test(dx4,type="Ljung-Box")

# Tendance linéaire

t<-1:100
tend<-lm(exo1[,4]~t)$fitted

det_x4<-exo1[,4]-tend

plot(det_x4)
acf(det_x4)
Box.test(det_x4,type="Ljung-Box")


# Régression de x2 sur x3 : régression fallacieuse

m1<-lm(exo1[,2]~exo1[,3]-1)
summary(m1)
plot(m1$resid,type="l") 
acf(m1$resid)


##### Régression avec séries chronologiques  ###"
################################################

library("lmtest")
library("nlme")

## Application 3 : Bangla

load("bangla.RData")
str(bangla) 
summary(bangla)# Pas de valeurs manquantes
plot(log(bangla$prix),log(bangla$surface), main="Relation entre la surface cultivée (en log) \n et le prix de vente des récoltes(en log)",xlab="log du prix",ylab="log des surfaces")


# Estimation de l'élasticité

m<-lm(log(surface)~log(prix),data=bangla)
summary(m) 
confint(m)

acf(m$resid)
acf(m$resid,type="partial")
dwtest(m)



m2<-gls(log(surface)~log(prix),data=bangla,correlation=corAR1())
summary(m2)

confint(m2)

# Modèle ARDL(1,1)

ls<-ts(log(bangla$surface),freq=1)
lp<-ts(log(bangla$prix),freq=1)

m3<-dynlm(ls~L(ls,1)+lp+L(lp,1))
summary(m3)
dwtest(m3) # Pas d'autocorrélation des résidus

# Estimation des multiplicateurs

coef(m3)

bet0<-coef(m3)[3]
bet1<-coef(m3)[4]+bet0*coef(m3)[2]
bet2<-bet1*coef(m3)[2]
bet3<-bet2*coef(m3)[2]
bet4<-bet3*coef(m3)[2]
bet<-c(bet0,bet1,bet2,bet3,bet4)
bet
cumsum(bet)

mult_tot<-coef(m3)[3]+(coef(m3)[3]+coef(m3)[3]*coef(m3)[2])/(1-coef(m3)[2])



## Application 4 : Modèle ARDL


load("conso.RData")
str(conso) 
summary(conso)# Pas de valeurs manquantes

conso<-ts(conso,start=c(1960,1),freq=4)
plot(conso, main="Evolution des taux de croissance de la consommation \n et du revenu disponible aux USA 1960Q1 à 2009Q4",xlab="Dates")

### modèle 1

mod1<-lm(gcons~grev,data=conso)
summary(mod1)
acf(mod1$resid)
acf(mod1$resid,type="partial")
dwtest(mod1)

resid<-ts(mod1$resid,start=c(1960,1),freq=4)
grev<-ts(conso[,1],start=c(1960,1),freq=4)
gcons<-ts(conso[,2],start=c(1960,1),freq=4)

### Test du multiplicateur de Lagrange d'une autocorrélation d'ordre 2

lm_t<-dynlm(resid~grev+L(resid,1:2))
summary(lm_t)
Stat_test_LM<-(200-2)*summary(lm_t)$r.squared
1-pchisq(Stat_test_LM,df=2) # la p-value est très faible, on rejeete donc l'hypothèse d'absence d'autocorrélation

## Estimation avec correction de l'autocorrélation d'ordre 2

mod12<-gls(gcons~grev,correlation=corARMA(p=2,q=0))
summary(mod12)
confint(mod12)

#Modèle 2

mod2<-dynlm(gcons~L(gcons,1)+grev)
summary(mod2)
AIC(mod2)
AIC(mod12)

acf(mod2$resid)
Box.test(mod2$resid,type="Ljung-Box")

#Modèle 3

mod3<-dynlm(gcons~L(gcons,1:2)+grev)
summary(mod3)
AIC(mod3)

plot(mod3$resid,type="l")
acf(mod3$resid)
Box.test(mod3$resid,type="Ljung-Box")

#Modèle 4

mod4<-dynlm(gcons~L(gcons,1:2)+grev+L(grev,1))
summary(mod4)
AIC(mod4)


acf(mod4$resid)
Box.test(mod4$resid,type="Ljung-Box")

# Ajout de retard supplémentaires

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

# Modèle 5

mod5<-dynlm(gcons~L(gcons,2)+grev+L(grev,1))
summary(mod5)
AIC(mod5)

acf(mod5$resid)
Box.test(mod5$resid,type="Ljung-Box")

confint(mod5)


