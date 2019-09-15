#___________________________________________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________________________________________
#_________________________________________Prediccion del riesgo default en acuerdos de ingreso compartido:__________________________________________
#_____________________Evidencia para la financiacion de educacion superior en Colombia usando metodos de machine learning___________________________
#__________________________________________________MSc.(c) Diana Carolina Lopez Becerra_____________________________________________________________
#___________________________________________________________Ph.D.Giovanni Munoz_____________________________________________________________________
#____________________________________________________________Ph.D.Hernando Diaz_____________________________________________________________________
#__________________________________________Universidad Nacional de Colombia, sede Bogota D.C._______________________________________________________
#_________________________________________________________________2019______________________________________________________________________________

#prueba de endogeneidad
phtest()
# x <- as.factor(x, ordered=FALSE) ##help Juan David

#manejar tildes#####################################################################################################################################
Sys.setlocale('LC_ALL','es_CO.iso88591')

#instalar paquetes##################################################################################################################################
install.packages("ROCR")
install.packages("ggplot2", dependencies = TRUE)
install.packages("arm")
install.packages("caret")

#cargar paquetes####################################################################################################################################
library(ROCR)
library(ggplot2)
#library(packagename)
library(arm) #no carga en amazon
library(caret)

#cargar las bases de datos############################################################################################################################
sample<-read.csv(paste('sample.csv',sep=""),header = TRUE, sep="," )
sampleO<-read.csv(paste('sampleO.csv',sep=""),header = TRUE, sep="," )
sampleR<-read.csv(paste('sampleR.csv',sep=""),header = TRUE, sep="," )
#cargar la base de datos de estudiantes para "experimiento en vivo"##################################################################################
experiment<-read.csv(paste('experiment.csv',sep=""),header = TRUE, sep="," )
experimentO<-read.csv(paste('experimentO.csv',sep=""),header = TRUE, sep="," )
experimentR<-read.csv(paste('experimentR.csv',sep=""),header = TRUE, sep="," )

#depurar bases######################################################################################################################################
#no se toman los predictores QSapiens y TipViv porque algunos registros no tienen valores
sampleDefault<-sample[,9:87]
sampleDefault<-sampleDefault[,-c(2,18,23)]
sampleDefaultO<-sampleO[,9:73]
sampleDefaultO<-sampleDefaultO[,-c(2,18,23)]
sampleDefaultR<-sampleR[,9:73]
sampleDefaultR<-sampleDefaultR[,-c(2,18,23)]

#reparticion de los datos en entrenamiento y prueba#################################################################################################
id_entrenamientoD<-createDataPartition(sample$Default, p=0.7, list=FALSE)

sampleDefault_tr<-sampleDefault[id_entrenamientoC,]
sampleDefault_ts<-sampleDefault[-id_entrenamientoC,]
sampleDefaultO_tr<-sampleDefaultO[id_entrenamientoD,]
#write.csv(sampleDefaultO_tr,paste('sampleDefaultO_tr.csv',sep=""))
sampleDefaultO_tS<-sampleDefaultO[-id_entrenamientoD,]
sampleDefaultR_tr<-sampleDefaultR[id_entrenamientoC,]
sampleDefaultR_ts<-sampleDefaultR[-id_entrenamientoC,]

###Logit para Default#################################################################################################################################
##variables originales##############################################################################################################################
#todos los predictores################################################################################################################################
set.seed(123)  
lm_D_O<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(61:62)], family=binomial,control = list(maxit = 100))
slm_D_O <- step(lm_D_O)
slm_D_O$anova
summary(slm_D_O)

#1sin los predictores financieros######################################################################################################################
set.seed(123)  
lm_D_O1<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(49:62)], family=binomial,control = list(maxit = 100))
slm_D_O1 <- step(lm_D_O1)
slm_D_O1$anova
summary(slm_D_O1)

#2sin las condiciones del contrato#####################################################################################################################
set.seed(123)  
lm_D_O2<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(25:28,61:62)], family=binomial,control = list(maxit = 100))
slm_D_O2 <- step(lm_D_O2)
slm_D_O2$anova
summary(slm_D_O2)

#3sin los  predictores con correlacion >0.9############################################################################################################
set.seed(123)  
lm_D_O3<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,54,61:62)], family=binomial,control = list(maxit = 100))
slm_D_O3 <- step(lm_D_O3)
slm_D_O3$anova
summary(slm_D_O3)#AIC: 174.8

#4**sin los  predictores con correlacion >0.9,Ing/Egr, pts financieros##############################################################################
set.seed(123)  
lm_D_O4<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,49:54,61:62)], family=binomial,control = list(maxit = 100))
slm_D_O4 <- step(lm_D_O4)
slm_D_O4$anova
summary(slm_D_O4)#AIC: 127.53 #13 sept 2da vez: AIC: 174.84

#datos de entrenamiento
y_glm_d_O4<- predict(slm_D_O4, type = "response")
a<-data.frame(y_glm_d_O4,DefaultReal=sampleDefaultO_tr$Default, scoring=sampleDefaultO_tr$scoring, PrbSel=sampleDefaultO_tr$PrbSel)
a<-a[order(a$y_glm_d_O4, decreasing=FALSE),]
a$rank<-1:nrow(a)
dev.off()
ggplot(data=a, aes(x=rank, y=y_glm_d_O4)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Registro") +ylab("Prediccion de la probabilidad de default")+ggtitle("Datos de entrenamiento")
ggsave("slm_D_O4Tt.pdf")

cor(y_glm_d_O4,sampleDefaultO_tr$scoring)
ggplot(data=a, aes(x=y_glm_d_O4, y=scoring)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Scoring")+ggtitle("Datos de entrenamiento")
ggsave("Scoring vs DefaultReal vs slm_D_O4Tt.pdf")

cor(y_glm_d_O4,sampleDefaultO_tr$PrbSel)
ggplot(data=a, aes(x=y_glm_d_O4, y=PrbSel)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Prueba de seleccion")+ggtitle("Datos de entrenamiento")
ggsave("Prueba de seleccion vs DefaultReal vs slm_D_O4Tt.pdf")

glm_d_O4 = rep("No", dim(sampleDefaultO_tr)[1])
glm_d_O4[y_glm_d_O4 > .5] = "Si"
glm_d_O4<-as.factor(glm_d_O4)
confusionMatrix(glm_d_O4,sampleDefaultO_tr$Default)

#datos de prueba
y_d_O4<- predict(slm_D_O4, sampleDefaultO_tS, type = "response")

a<-data.frame(y_d_O4,DefaultReal=sampleDefaultO_tS$Default,scoring=sampleDefaultO_tS$scoring,PrbSel=sampleDefaultO_tS$PrbSel)
a<-a[order(a$y_d_O4, decreasing=FALSE),]
a$rank<-1:nrow(a)
ggplot(data=a, aes(x=rank, y=y_d_O4)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Registro") +ylab("Prediccion de  la probabilidad de default")+ggtitle("Datos de prueba")
ggsave("slm_D_O4Ts.pdf")

cor(y_d_O4,sampleDefaultO_tS$scoring)
ggplot(data=a, aes(x=y_d_O4, y=scoring)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Scoring")+ggtitle("Datos de prueba")
ggsave("Scoring vs DefaultReal vs slm_D_O4Ts.pdf")

cor(y_d_O4,sampleDefaultO_tS$PrbSel)
ggplot(data=a, aes(x=y_d_O4, y=PrbSel)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Prueba de seleccion")+ggtitle("Datos de prueba")
ggsave("Prueba de seleccion vs DefaultReal vs slm_D_O4Ts.pdf")

ts_d_O4 = rep("No", dim(sampleDefaultO_tS)[1])
ts_d_O4[y_d_O4 > .5] = "Si"
ts_d_O4<-as.factor(ts_d_O4)
confusionMatrix(ts_d_O4,sampleDefaultO_tS$Default)

pred_ts_d_O4<- prediction(y_d_O4, sampleDefaultO_tS$Default)
perf_ts_d_O4 <- performance(pred_ts_d_O4,"tpr","fpr")
pdf("ROC_D_O4Ts.pdf")
plot(perf_ts_d_O4,colorize=TRUE)#ROC curve
dev.off()

#5sin los  predictores con correlacion >0.9, ni los 5 bienes##################################################################################################
set.seed(123)  
lm_D_O5<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,54:59,61:62)],family=binomial,control = list(maxit = 100))
slm_D_O5 <- step(lm_D_O5)
slm_D_O5$anova
summary(slm_D_O5)

#6sin los  predictores con correlacion >0.9, sin los predictores financieros##################################################################################
set.seed(123)  
lm_D_O6<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,49:62)],family=binomial,control = list(maxit = 100))
slm_D_O6 <- step(lm_D_O6)
slm_D_O6$anova
summary(slm_D_O6)

#7sin los  predictores con correlacion >0.9, sin los pts bancarios#######################################################################
set.seed(123)  
lm_D_O7<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,50:54,61:62)],family=binomial,control = list(maxit = 100))
slm_D_O7 <- step(lm_D_O7)
slm_D_O7$anova
summary(slm_D_O7)

#8sin los  predictores con correlacion >0.9, Ing/Egr,pts financieros,  B_pond########################################################################
lm_D_O8<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,49:54,60:62)],family=binomial,control = list(maxit = 100))
slm_D_O8 <- step(lm_D_O8)
slm_D_O8$anova
summary(slm_D_O8)

#9**sin los  predictores con correlacion >0.9,Ing/Egr, pts financieros, B_moto, B_pond##########################################################
set.seed(123)  
lm_D_O9<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,49:54,58,60:62)], 
                    family=binomial,control = list(maxit = 100),weights =floor(log(sampleDefaultO_tr$mntTotal)*1000))
slm_D_O9 <- step(lm_D_O9)
slm_D_O9$anova
summary(slm_D_O9)#131.58 #13 septiembre - AIC: 176.77

y_glm_d_O9<- predict(slm_D_O9, type = "response")
a<-data.frame(y_glm_d_O9,DefaultReal=sampleDefaultO_tr$Default, scoring=sampleDefaultO_tr$scoring, PrbSel=sampleDefaultO_tr$PrbSel)
a<-a[order(a$y_glm_d_O9, decreasing=FALSE),]
a$rank<-1:nrow(a)
ggplot(data=a, aes(x=rank, y=y_glm_d_O9)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Registro") +ylab("Prediccion de la probabilidad de default")+ggtitle("Datos de entrenamiento")
ggsave("slm_D_O9Tt.pdf")

cor(y_glm_d_O9,sampleDefaultO_tr$scoring)
ggplot(data=a, aes(x=y_glm_d_O9, y=scoring)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Scoring")+ggtitle("Datos de entrenamiento")
ggsave("Scoring vs DefaultReal vs slm_D_O9Tt.pdf")

cor(y_glm_d_O9,sampleDefaultO_tr$PrbSel)
ggplot(data=a, aes(x=y_glm_d_O9, y=PrbSel)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Prueba de seleccion")+ggtitle("Datos de entrenamiento")
ggsave("Prueba de seleccion vs DefaultReal vs slm_D_O9Tt.pdf")

glm_d_O9 = rep("No", dim(sampleDefaultO_tr)[1])
glm_d_O9[y_glm_d_O9 > .45] = "Si"
glm_d_O9<-as.factor(glm_d_O9)
confusionMatrix(glm_d_O9,sampleDefaultO_tr$Default)

#prueba
y_d_O9<- predict(slm_D_O9, sampleDefaultO_tS, type = "response")

a<-data.frame(y_d_O9,DefaultReal=sampleDefaultO_tS$Default,scoring=sampleDefaultO_tS$scoring,PrbSel=sampleDefaultO_tS$PrbSel)
a<-a[order(a$y_d_O9, decreasing=FALSE),]
a$rank<-1:nrow(a)
ggplot(data=a, aes(x=rank, y=y_d_O9)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Registro") +ylab("Prediccion de  la probabilidad de default")+ggtitle("Datos de prueba")
ggsave("slm_D_O9Ts.pdf")

cor(y_d_O9,sampleDefaultO_tS$scoring)
ggplot(data=a, aes(x=y_d_O9, y=scoring)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Scoring")+ggtitle("Datos de prueba")
ggsave("Scoring vs DefaultReal vs slm_D_O9Ts.pdf")

cor(y_d_O9,sampleDefaultO_tS$PrbSel)
ggplot(data=a, aes(x=y_d_O9, y=PrbSel)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Prueba de seleccion")+ggtitle("Datos de prueba")
ggsave("Prueba de seleccion vs DefaultReal vs slm_D_O9Ts.pdf")

ts_d_O9 = rep("No", dim(sampleDefaultO_tS)[1])
ts_d_O9[y_d_O9 > .5] = "Si"
ts_d_O9<-as.factor(ts_d_O9)
confusionMatrix(ts_d_O9,sampleDefaultO_tS$Default)

pred_ts_d_O9<- prediction(y_d_O9, sampleDefaultO_tS$Default)
perf_ts_d_O9 <- performance(pred_ts_d_O9,"tpr","fpr")
pdf("ROC_D_O9Ts.pdf")
plot(perf_ts_d_O9,colorize=TRUE)#ROC curve
dev.off()

#10sin los  predictores con correlacion >0.9, Ing/Egr,pts financieros, B_moto, B_carro, B_pond################################################
set.seed(123)  
lm_D_O10<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,49:54,58:62)],
                     family=binomial,control = list(maxit = 100))
slm_D_O10 <- step(lm_D_O10)
slm_D_O10$anova
summary(slm_D_O10)#AIC: 131.25

#11sin los  predictores con correlacion >0.9,Ing/Egr, pts financieros, B_moto, B_Viv, B_pond################################################
set.seed(123)  
lm_D_O11<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,49:55,58,60:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_O11 <- step(lm_D_O11)
slm_D_O11$anova
summary(slm_D_O11)

#12sin los  predictores con correlacion >0.9, pts financieros, Ing/Egr,  B_moto, B_Cel,B_pond################################################
set.seed(123)  
lm_D_O12<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,49:54,57:58,60:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_O12 <- step(lm_D_O12)
slm_D_O12$anova
summary(slm_D_O12)

#13sin los  predictores con correlacion >0.9, L_sinV, PrbSel, Ing/Egr, pts financieros con Pts_Banc, B_moto################################################
set.seed(123)  
lm_D_O13<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,32,48:53,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_O13 <- step(lm_D_O13)
slm_D_O13$anova
summary(slm_D_O13)

#14sin los  predictores con correlacion >0.9, pts financieros con Pts_Banc, Ing/Egr, B_moto, B_pond#################################################
set.seed(123)  
lm_D_O14<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,49:53,58,60:62)], 
                    family=binomial,control = list(maxit = 100))
slm_D_O14 <- step(lm_D_O14)
slm_D_O14$anova
summary(slm_D_O14)

#15sin los  predictores con correlacion >0.9, pts financieros, Ing/Egr, B_moto########################################################################
set.seed(123)  
lm_D_O15<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,49:54,58,61:62)], 
                    family=binomial,control = list(maxit = 100))
slm_D_O15 <- step(lm_D_O15)
slm_D_O15$anova
summary(slm_D_O15)

#16sin los  predictores con correlacion >0.9, categorias y PrbSel, pts financieros, Ing/Egr, B_moto###################################################
set.seed(123)  
lm_D_O16<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,29:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_O16 <- step(lm_D_O16)
slm_D_O16$anova
summary(slm_D_O16)

#17sin los  predictores con correlacion >0.9, categorias, pts financieros, Ing/Egr, B_moto###########################################################
set.seed(123)  
lm_D_O17<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,29:47,49:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_O17 <- step(lm_D_O17)
slm_D_O17$anova
summary(slm_D_O17)

#18sin los  predictores con correlacion >0.9, L_sinV, Logros, PrbSel, Ing/Egr,pts financieros, B_moto#####################################
set.seed(123)  
lm_D_O18<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,32,33,48:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_O18<- step(lm_D_O18)
slm_D_O18$anova
summary(slm_D_O18)#AIC: 139.62

#19**sin los  predictores con correlacion >0.9, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto##############################################
set.seed(123)  
lm_D_O19<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,32,48:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_O19<- step(lm_D_O19)
slm_D_O19$anova
summary(slm_D_O19)#AIC: 123.39

y_glm_d_O19<- predict(slm_D_O19,type = "response")
a<-data.frame(y_glm_d_O19,DefaultReal=sampleDefaultO_tr$Default, scoring=sampleDefaultO_tr$scoring, PrbSel=sampleDefaultO_tr$PrbSel)
a<-a[order(a$y_glm_d_O19, decreasing=FALSE),]
a$rank<-1:nrow(a)
ggplot(data=a, aes(x=rank, y=y_glm_d_O19)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Registro") +ylab("Prediccion de la probabilidad de default")+ggtitle("Datos de entrenamiento")
ggsave("slm_D_O19Tt.pdf")

cor(y_glm_d_O19,sampleDefaultO_tr$scoring)
ggplot(data=a, aes(x=y_glm_d_O19, y=scoring)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Scoring")+ggtitle("Datos de entrenamiento")
ggsave("Scoring vs DefaultReal vs slm_D_O19Tt.pdf")

cor(y_glm_d_O19,sampleDefaultO_tr$PrbSel)
ggplot(data=a, aes(x=y_glm_d_O19, y=PrbSel)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Prueba de seleccion")+ggtitle("Datos de entrenamiento")
ggsave("Prueba de seleccion vs DefaultReal vs slm_D_O19Tt.pdf")

glm_d_O19 = rep("No", dim(sampleDefaultO_tr)[1])
glm_d_O19[y_glm_d_O19 > .5] = "Si"
glm_d_O19<-as.factor(glm_d_O19)
confusionMatrix(glm_d_O19,sampleDefaultO_tr$Default)

#prueba
y_d_O19<- predict(slm_D_O19, sampleDefaultO_tS, type = "response")

a<-data.frame(y_d_O19,DefaultReal=sampleDefaultO_tS$Default,scoring=sampleDefaultO_tS$scoring,PrbSel=sampleDefaultO_tS$PrbSel)
a<-a[order(a$y_d_O19, decreasing=FALSE),]
a$rank<-1:nrow(a)
ggplot(data=a, aes(x=rank, y=y_d_O19)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Registro") +ylab("Prediccion de  la probabilidad de default")+ggtitle("Datos de prueba")
ggsave("slm_D_O19Ts.pdf")

cor(y_d_O19,sampleDefaultO_tS$scoring)
ggplot(data=a, aes(x=y_d_O19, y=scoring)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Scoring")+ggtitle("Datos de prueba")
ggsave("Scoring vs DefaultReal vs slm_D_O19Ts.pdf")

cor(y_d_O19,sampleDefaultO_tS$PrbSel)
ggplot(data=a, aes(x=y_d_O19, y=PrbSel)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Prueba de seleccion")+ggtitle("Datos de prueba")
ggsave("Prueba de seleccion vs DefaultReal vs slm_D_O19Ts.pdf")

ts_d_O19 = rep("No", dim(sampleDefaultO_tS)[1])
ts_d_O19[y_d_O19 > .5] = "Si"
ts_d_O19<-as.factor(ts_d_O19)
confusionMatrix(ts_d_O19,sampleDefaultO_tS$Default)

pred_ts_d_O19<- prediction(y_d_O19, sampleDefaultO_tS$Default)
perf_ts_d_O19 <- performance(pred_ts_d_O19,"tpr","fpr")
pdf("ROC_D_O19Ts.pdf")
plot(perf_ts_d_O19,colorize=TRUE)#ROC curve
dev.off()

#20sin los  predictores con correlacion >0.9, L_sinV, Pers, PrbSel, Ing/Egr, pts financieros, B_moto##################################
set.seed(123)  
lm_D_O20<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,32,39,48:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_O20<- step(lm_D_O20)
slm_D_O20$anova
summary(slm_D_O20)#AIC: 129.99

#21sin los  predictores con correlacion >0.9, L_sinV, Intereses, PrbSel, Ing/Egr, pts financieros, B_moto###################################
set.seed(123)  
lm_D_O21<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,32,47:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_O21<- step(lm_D_O21)
slm_D_O21$anova
summary(slm_D_O21)

#22sin los  predictores con correlacion >0.9, L_sinV, Logros, Intereses, PrbSel, Ing/Egr, pts financieros,B_moto############################
set.seed(123)  
lm_D_O22<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,32,33,47:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_O22<- step(lm_D_O22)
slm_D_O22$anova
summary(slm_D_O22)


#23sin los  predictores con correlacion >0.9, L_sinV, Ing/Egr,pts financieros,B_moto#####################################
set.seed(123)  
lm_D_O23<-  bayesglm(sampleDefaultO_tr$Default ~ ., data = sampleDefaultO_tr[,-c(20,23,24,32,49:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_O23<- step(lm_D_O23)
slm_D_O23$anova
summary(slm_D_O23)AIC: 145.19


#24sin los  predictores con correlacion >0.9, L_sinV, subcategorias, Ing/Egr, pts financieros,B_moto###################################
set.seed(123)
lm_D_O24<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100), 
                     data = sampleDefaultO_tr[,-c(20,23,24,29:32,34:38,40:46,49:54,58,61:62)])
slm_D_O24<- step(lm_D_O24)
slm_D_O24$anova
summary(slm_D_O24)#AIC: 136.36

#25sin los  predictores con correlacion >0.9, subcategorias, Ing/Egr, pts financieros,B_moto#########################################################
set.seed(123)
lm_D_O25<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),
                     data = sampleDefaultO_tr[,-c(20,23,24,29:31,34:38,40:46,49:54,58,61:62)])
slm_D_O25<- step(lm_D_O25)
slm_D_O25$anova
summary(slm_D_O25)#AIC: 136.49

#26sin los  predictores con correlacion >0.9, R_PromEdadH, Ing/Egr, pts financieros, B_moto#########################################################
set.seed(123)
lm_D_O26<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),
                     data = sampleDefaultO_tr[,-c(15,20,23,24,49:54,58,61:62)])
slm_D_O26<- step(lm_D_O26)
slm_D_O26$anova
summary(slm_D_O26)

#27sin predictores financieros,relacionados PrbSel#####################################################################################################
set.seed(123)
lm_D_O27<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(29:62)])
slm_D_O27<- step(lm_D_O27)
slm_D_O27$anova
summary(slm_D_O27)#AIC: 139.03

#28sin los  predictores con correlacion >0.9, predictores financieros,relacionados prbSel#############################################################
set.seed(123)
lm_D_O28<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(20,23,24,29:62)])
slm_D_O28<- step(lm_D_O28)
slm_D_O28$anova
summary(slm_D_O28)#AIC: 139.35

#29sin los  predictores relacionados PrbSel####################################################################################################
set.seed(123)
lm_D_O29<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(29:48,61:62)])
slm_D_O29<- step(lm_D_O29)
slm_D_O29$anova
summary(slm_D_O29)#AIC: 139.18
#30sin los  predictores con correlacion >0.9, predictores relacionados PrbSel####################################################################################################
set.seed(123)
lm_D_O30<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(20,23,24,29:48,54,61:62)])
slm_D_O30<- step(lm_D_O30)
slm_D_O30$anova
summary(slm_D_O30)#AIC: 138.3

#31sin los  predictores con correlacion >0.9, predictores relacionados PrbSel, predictores relacionados Acdm#############################################################################################
set.seed(123)
lm_D_O31<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(20:24,29:48,54,61:62)])
slm_D_O31<- step(lm_D_O31)
slm_D_O31$anova
summary(slm_D_O31)#AIC: 138.3

#32sin predictores relacionados PrbSel, predictores relacionados Acdm#############################################################################################
set.seed(123)
lm_D_O32<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(20:24,29:48,61:62)])
slm_D_O32<- step(lm_D_O32)
slm_D_O32$anova
summary(slm_D_O32)#AIC: 138.3

#33sin predictores relacionados PrbSel, predictores relacionados Familiar#############################################################################################
set.seed(123)
lm_D_O33<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(8:15,29:48,61:62)])
slm_D_O33<- step(lm_D_O33)
slm_D_O33$anova
summary(slm_D_O33)#AIC: 135.54

#34sin los  predictores con correlacion >0.9, predictores relacionados PrbSel, predictores relacionados Familiar#############################################################################################
set.seed(123)
lm_D_O34<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(8:15,20,23,24,29:48,54,61:62)])
slm_D_O34<- step(lm_D_O34)
slm_D_O34$anova
summary(slm_D_O34)#AIC: 138.3

#35sin los  predictores con correlacion >0.9, predictores relacionados PrbSel, predictores relacionados ecnm#############################################################################################
set.seed(123)
lm_D_O35<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(16:19,20,23,24,29:48,54,61:62)])
slm_D_O35<- step(lm_D_O35)
slm_D_O35$anova
summary(slm_D_O35)#AIC: 138.3

#36 sin predictores relacionados PrbSel, predictores relacionados ecnm#############################################################################################
set.seed(123)
lm_D_O36<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(16:19,29:48,61:62)])
slm_D_O36<- step(lm_D_O36)
slm_D_O36$anova
summary(slm_D_O36)#AIC: 136.18

#37sin predictores relacionados PrbSel, predictores relacionados socioD#############################################################################################
set.seed(123)
lm_D_O37<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(2:7,29:48,61:62)])
slm_D_O37<- step(lm_D_O37)
slm_D_O37$anova
summary(slm_D_O37)#AIC: 140.6

#38sin los  predictores con correlacion >0.9,predictores relacionados PrbSel, predictores relacionados socioD#############################################################################################
set.seed(123)
lm_D_O38<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(2:7,20, 23,24,29:48,54,61:62)])
slm_D_O38<- step(lm_D_O38)
slm_D_O38$anova
summary(slm_D_O38)#AIC: 140.56

#39sin los  predictores con correlacion >0.9,predictores relacionados PrbSel, predictores relacionados ISA#############################################################################################
set.seed(123)
lm_D_O39<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(20, 23,24:48,54,61:62)])
slm_D_O39<- step(lm_D_O39)
slm_D_O39$anova
summary(slm_D_O39)#AIC: 152.46

#40sin predictores relacionados PrbSel, predictores relacionados ISA#############################################################################################
set.seed(123)
lm_D_O40<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(25:48,61:62)])
slm_D_O40<- step(lm_D_O40)
slm_D_O40$anova
summary(slm_D_O40)#AIC: 140.64

#41sin predictores relacionados PrbSel, predictores relacionados Familiar, Ing/Egr#############################################################################################
set.seed(123)
lm_D_O41<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(25:49,61:62)])
slm_D_O41<- step(lm_D_O41)
slm_D_O41$anova
summary(slm_D_O41)#AIC: 140.62

#42sin los  predictores con correlacion >0.9, relacionados socioD, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_O42<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(2:7,20,23,24,32,48:54,58,61:62)])
slm_D_O42<- step(lm_D_O42)
slm_D_O42$anova
summary(slm_D_O42)#AIC: 159.31

#43sin los  predictores con correlacion >0.9, genero, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_O43<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(2,20,23,24,32,48:54,58,61:62)])
slm_D_O43<- step(lm_D_O43)
slm_D_O43$anovaX
summary(slm_D_O43)#AIC: 157

#44sin los  predictores con correlacion >0.9, EstCiv, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_O44<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(3,20,23,24,32,48:54,58,61:62)])
slm_D_O44<- step(lm_D_O44)
slm_D_O44$anova
summary(slm_D_O44)#AIC: 170.12

#45**sin los  predictores con correlacion >0.9, RegNac, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_O45<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(4,20,23,24,32,48:54,58,61:62)])
slm_D_O45<- step(lm_D_O45)
slm_D_O45$anova
summary(slm_D_O45)#AIC: 123.39

y_glm_d_O45<- predict(slm_D_O45,type = "response")
a<-data.frame(y_glm_d_O45,DefaultReal=sampleDefaultO_tr$Default, scoring=sampleDefaultO_tr$scoring, PrbSel=sampleDefaultO_tr$PrbSel)
a<-a[order(a$y_glm_d_O45, decreasing=FALSE),]
a$rank<-1:nrow(a)
ggplot(data=a, aes(x=rank, y=y_glm_d_O45)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Registro") +ylab("Prediccion de la probabilidad de default")+ggtitle("Datos de entrenamiento")
ggsave("slm_D_O45Tt.pdf")

cor(y_glm_d_O45,sampleDefaultO_tr$scoring)
ggplot(data=a, aes(x=y_glm_d_O45, y=scoring)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Scoring")+ggtitle("Datos de entrenamiento")
ggsave("Scoring vs DefaultReal vs slm_D_O45Tt.pdf")

cor(y_glm_d_O45,sampleDefaultO_tr$PrbSel)
ggplot(data=a, aes(x=y_glm_d_O45, y=PrbSel)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Prueba de seleccion")+ggtitle("Datos de entrenamiento")
ggsave("Prueba de seleccion vs DefaultReal vs slm_D_O45Tt.pdf")

glm_d_O45 = rep("No", dim(sampleDefaultO_tr)[1])
glm_d_O45[y_glm_d_O45 > .45] = "Si"
glm_d_O45<-as.factor(glm_d_O45)
confusionMatrix(glm_d_O45,sampleDefaultO_tr$Default)

#prueba
y_d_O45<- predict(slm_D_O45, sampleDefaultO_tS, type = "response")

a<-data.frame(y_d_O45,DefaultReal=sampleDefaultO_tS$Default,scoring=sampleDefaultO_tS$scoring,PrbSel=sampleDefaultO_tS$PrbSel)
a<-a[order(a$y_d_O45, decreasing=FALSE),]
a$rank<-1:nrow(a)
ggplot(data=a, aes(x=rank, y=y_d_O45)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Registro") +ylab("Prediccion de  la probabilidad de default")+ggtitle("Datos de prueba")
ggsave("slm_D_O45Ts.pdf")

cor(y_d_O45,sampleDefaultO_tS$scoring)
ggplot(data=a, aes(x=y_d_O45, y=scoring)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Scoring")+ggtitle("Datos de prueba")
ggsave("Scoring vs DefaultReal vs slm_D_O45Ts.pdf")

cor(y_d_O45,sampleDefaultO_tS$PrbSel)
ggplot(data=a, aes(x=y_d_O45, y=PrbSel)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Prueba de seleccion")+ggtitle("Datos de prueba")
ggsave("Prueba de seleccion vs DefaultReal vs slm_D_O45Ts.pdf")

ts_d_O45 = rep("No", dim(sampleDefaultO_tS)[1])
ts_d_O45[y_d_O45 > .5] = "Si"
ts_d_O45<-as.factor(ts_d_O45)
confusionMatrix(ts_d_O45,sampleDefaultO_tS$Default)

pred_ts_d_O45<- prediction(y_d_O45, sampleDefaultO_tS$Default)
perf_ts_d_O45 <- performance(pred_ts_d_O45,"tpr","fpr")
pdf("ROC_D_O45Ts.pdf")
plot(perf_ts_d_O45,colorize=TRUE)#ROC curve
dev.off()

#46sin los  predictores con correlacion >0.9, NacCap, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_O46<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(5,20,23,24,32,48:54,58,61:62)])
slm_D_O46<- step(lm_D_O46)
slm_D_O46$anova
summary(slm_D_O46)#AIC: 172.36

#47sin los  predictores con correlacion >0.9, ResEst, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_O47<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(6,20,23,24,32,48:54,58,61:62)])
slm_D_O47<- step(lm_D_O47)
slm_D_O47$anova
summary(slm_D_O47)#AIC: 172.36

#48sin los  predictores con correlacion >0.9, ResEst, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_O48<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(7,20,23,24,32,48:54,58,61:62)])
slm_D_O48<- step(lm_D_O48)
slm_D_O48$anova
summary(slm_D_O48)#AIC: 173.31

#49sin los  predictores con correlacion >0.9, Genero, RegNac, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_O49<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(2,4,20,23,24,32,48:54,58,61:62)])
slm_D_O49<- step(lm_D_O49)
slm_D_O49$anova
summary(slm_D_O49)#AIC: 157

#50sin los  predictores con correlacion >0.9, Herm,L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_O50<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(12,20,23,24,32,48:54,58,61:62)])
slm_D_O50<- step(lm_D_O50)
slm_D_O50$anova
summary(slm_D_O50)#AIC: 123.39

#51sin los  predictores con correlacion >0.9, RegNac,Herm,L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_O51<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(4,12,20,23,24,32,48:54,58,61:62)])
slm_D_O51<- step(lm_D_O51)
slm_D_O51$anova
summary(slm_D_O51)#AIC: 123.39

#52sin los  predictores con correlacion >0.9, RegNac,Herm,LugHerm,L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_O52<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(4,12,13,20,23,24,32,48:54,58,61:62)])
slm_D_O52<- step(lm_D_O52)
slm_D_O52$anova
summary(slm_D_O52)#AIC: 123.39

#53sin los  predictores con correlacion >0.9, RegNac,Herm,LugHerm,R_Estrato,L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_O53<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(4,12,13,16,20,23,24,32,48:54,58,61:62)])
slm_D_O53<- step(lm_D_O53)
slm_D_O53$anova
summary(slm_D_O53)#AIC: 123.39

#54sin los  predictores con correlacion >0.9, RegNac,Herm,LugHerm,R_Estrato,NivFrmc,L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_O54<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(4,12,13,16,20,21,23,24,32,48:54,58,61:62)])
slm_D_O54<- step(lm_D_O54)
slm_D_O54$anova
summary(slm_D_O54)#AIC: 123.39

#55sin los  predictores con correlacion >0.9, RegNac,Herm,LugHerm,R_Estrato,NivFrmc,RespEcnm,L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_O55<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(4,12,13,16,17,20,21,23,24,32,48:54,58,61:62)])
slm_D_O55<- step(lm_D_O55)
slm_D_O55$anova
summary(slm_D_O55)#AIC: 123.39

#56sin los  predictores con correlacion >0.9, RegNac,Herm,LugHerm,R_Estrato,NivFrmc,RespEcnm,L_sinV, PrbSel, Ing/Egr, pts financieros, B_cel,B_moto#############################################################################################
set.seed(123)
lm_D_O56<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(4,12,13,16,17,20,21,23,24,32,48:54,57,58,61:62)])
slm_D_O56<- step(lm_D_O56)
slm_D_O56$anova
summary(slm_D_O56)#AIC: 123.39

#57sin los  predictores con correlacion >0.9, L_sinV, PrbSel, Ing/Egr, pts financieros, B_viv,B_moto#############################################################################################
set.seed(123)
lm_D_O57<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(20,23,24,32,48:55,58,61:62)])
slm_D_O57<- step(lm_D_O57)
slm_D_O57$anova
summary(slm_D_O57)#AIC: 123.47

#58sin los  predictores con correlacion >0.9, L_sinV, PrbSel, Ing/Egr, pts financieros, B_carro,B_moto#############################################################################################
set.seed(123)
lm_D_O58<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(20,23,24,32,48:54,58,59,61:62)])
slm_D_O58<- step(lm_D_O58)
slm_D_O58$anova
summary(slm_D_O58)#AIC: 173.12

#59sin los  predictores con correlacion >0.9, L_sinV, PrbSel, Ing/Egr, CtaAh, B_moto#############################################################################################
set.seed(123)
lm_D_O59<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(20,23,24,32,48:50,54,58,61:62)])
slm_D_O59<- step(lm_D_O59)
slm_D_O59$anova
summary(slm_D_O59)#AIC: 160.19

#60sin los  predictores con correlacion >0.9, L_sinV, PrbSel, Ing/Egr, CtaAh, TarjCred, B_moto#############################################################################################
set.seed(123)
lm_D_O60<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(20,23,24,32,48:51,54,58,61:62)])
slm_D_O60<- step(lm_D_O60)
slm_D_O60$anova
summary(slm_D_O60)#AIC: 173.52

#61sin los  predictores con correlacion >0.9, L_sinV, PrbSel, Ing/Egr, CtaAh, TarjCred, Creditos,B_moto#############################################################################################
set.seed(123)
lm_D_O61<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(20,23,24,32,48:52,54,58,61:62)])
slm_D_O61<- step(lm_D_O61)
slm_D_O61$anova
summary(slm_D_O61)#AIC: 173.52

#62sin AreaCncm, L_sinV, PrbSel, Ing/Egr, pts financieros,B_moto#############################################################################################
set.seed(123)
lm_D_O62<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(23,32,48:54,58,61:62)])
slm_D_O62<- step(lm_D_O62)
slm_D_O62$anova
summary(slm_D_O62)#AIC: 154.53

#63sin PrdPA, L_sinV, PrbSel, Ing/Egr, pts financieros,B_moto#############################################################################################
set.seed(123)
lm_D_O63<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(24,32,48:54,58,61:62)])
slm_D_O63<- step(lm_D_O63)
slm_D_O63$anova
summary(slm_D_O63)#AIC: 142.21

#64sin AltaCalidad, L_sinV, PrbSel, Ing/Egr, pts financieros,B_moto#############################################################################################
set.seed(123)
lm_D_O64<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(20,32,48:54,58,61:62)])
slm_D_O64<- step(lm_D_O64)
slm_D_O64$anova
summary(slm_D_O64)#AIC: 148.35

#65sin AltaCalidad, PrdPA,L_sinV, PrbSel, Ing/Egr, pts financieros,B_moto#############################################################################################
set.seed(123)
lm_D_O65<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(20,24,32,48:54,58,61:62)])
slm_D_O65<- step(lm_D_O65)
slm_D_O65$anova
summary(slm_D_O65)#AIC: 123.39

#66sin AltaCalidad, AreaCncm,L_sinV, PrbSel, Ing/Egr, pts financieros,B_moto#############################################################################################
set.seed(123)
lm_D_O66<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(20,23,32,48:54,58,61:62)])
slm_D_O66<- step(lm_D_O66)
slm_D_O66$anova
summary(slm_D_O66)#AIC: 148.35

#67sin PrdPA, AreaCncm,L_sinV, PrbSel, Ing/Egr, pts financieros,B_moto#############################################################################################
set.seed(123)
lm_D_O67<-  bayesglm(sampleDefaultO_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultO_tr[,-c(24,23,32,48:54,58,61:62)])
slm_D_O67<- step(lm_D_O67)
slm_D_O67$anova
summary(slm_D_O67)#AIC: 142.21


##variables construidas (rangos)############################################################################################################################
#1sin los predictores financieros######################################################################################################################
set.seed(123)  
lm_D_R1<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(49:62)], family=binomial,control = list(maxit = 100))
slm_D_R1 <- step(lm_D_R1)
slm_D_R1$anova
summary(slm_D_R1)#AIC: 188.62

#2sin las condiciones del contrato#####################################################################################################################
set.seed(123)  
lm_D_R2<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(25:28,61:62)], family=binomial,control = list(maxit = 100))
slm_D_R2 <- step(lm_D_R2)
slm_D_R2$anova
summary(slm_D_R2)#AIC: 203.4

#3sin los  predictores con correlacion >0.9############################################################################################################
set.seed(123)  
lm_D_R3<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,54,61:62)], family=binomial,control = list(maxit = 100))
slm_D_R3 <- step(lm_D_R3)
slm_D_R3$anova
summary(slm_D_R3)#AIC: 202.96

#4sin los  predictores con correlacion >0.9,Ing/Egr, pts financieros##############################################################################
set.seed(123)  
lm_D_R4<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,49:54,61:62)], family=binomial,control = list(maxit = 100))
slm_D_R4 <- step(lm_D_R4)
slm_D_R4$anova
summary(slm_D_R4)#AIC: 197.7

#5sin los  predictores con correlacion >0.9, ni los 5 bienes##################################################################################################
set.seed(123)  
lm_D_R5<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,54:59,61:62)],family=binomial,control = list(maxit = 100))
slm_D_R5 <- step(lm_D_R5)
slm_D_R5$anova
summary(slm_D_R5)#AIC: 200.46

#6sin los  predictores con correlacion >0.9, sin los predictores financieros##################################################################################
set.seed(123)  
lm_D_R6<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,49:62)],family=binomial,control = list(maxit = 100))
slm_D_R6 <- step(lm_D_R6)
slm_D_R6$anova
summary(slm_D_R6)#AIC: 190.2

#7sin los  predictores con correlacion >0.9, sin los pts bancarios#######################################################################
set.seed(123)  
lm_D_R7<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,50:54,61:62)],family=binomial,control = list(maxit = 100))
slm_D_R7 <- step(lm_D_R7)
slm_D_R7$anova
summary(slm_D_R7)#AIC: 198

#8sin los  predictores con correlacion >0.9, Ing/Egr,pts financieros,  B_pond########################################################################
lm_D_R8<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,49:54,60:62)],family=binomial,control = list(maxit = 100))
slm_D_R8 <- step(lm_D_R8)
slm_D_R8$anova
summary(slm_D_R8)#AIC: 190.5

#9sin los  predictores con correlacion >0.9,Ing/Egr, pts financieros, B_moto, B_pond#################################################################
set.seed(123)  
lm_D_R9<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,49:54,58,60:62)], 
                    family=binomial,control = list(maxit = 100))#, weights =floor(log(sampleDefaultR_tr$mntTotal)*1000)
slm_D_R9 <- step(lm_D_R9)
slm_D_R9$anova
summary(slm_D_R9)#AIC: 188.55

#10sin los  predictores con correlacion >0.9, Ing/Egr,pts financieros, B_moto, B_carro, B_pond################################################
set.seed(123)  
lm_D_R10<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,49:54,58:62)],
                     family=binomial,control = list(maxit = 100))
slm_D_R10 <- step(lm_D_R10)
slm_D_R10$anova
summary(slm_D_R10)#AIC: 182.18

#11sin los  predictores con correlacion >0.9,Ing/Egr, pts financieros, B_moto, B_Viv, B_pond################################################
set.seed(123)  
lm_D_R11<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,49:55,58,60:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_R11 <- step(lm_D_R11)
slm_D_R11$anova
summary(slm_D_R11)#AIC: 182.65

#12sin los  predictores con correlacion >0.9, pts financieros, Ing/Egr,  B_moto, B_Cel,B_pond################################################
set.seed(123)  
lm_D_R12<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,49:54,57:58,60:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_R12 <- step(lm_D_R12)
slm_D_R12$anova
summary(slm_D_R12)#AIC: 179.33

#13sin los  predictores con correlacion >0.9, L_sinV, PrbSel, Ing/Egr, pts financieros con Pts_Banc, B_moto################################################
set.seed(123)  
lm_D_R13<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,32,48:53,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_R13 <- step(lm_D_R13)
slm_D_R13$anova
summary(slm_D_R13)#AIC: 193.99

#14sin los  predictores con correlacion >0.9, pts financieros con Pts_Banc, Ing/Egr, B_moto, B_pond#################################################
set.seed(123)  
lm_D_R14<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,49:53,58,60:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_R14 <- step(lm_D_R14)
slm_D_R14$anova
summary(slm_D_R14)#AIC: 189.36

#15sin los  predictores con correlacion >0.9, pts financieros, Ing/Egr, B_moto########################################################################
set.seed(123)  
lm_D_R15<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,49:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_R15 <- step(lm_D_R15)
slm_D_R15$anova
summary(slm_D_R15)#AIC: 195.73

#16sin los  predictores con correlacion >0.9, categorias y PrbSel, pts financieros, Ing/Egr, B_moto###################################################
set.seed(123)  
lm_D_R16<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,29:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_R16 <- step(lm_D_R16)
slm_D_R16$anova
summary(slm_D_R16)#AIC: 170.04

#17sin los  predictores con correlacion >0.9, categorias, pts financieros, Ing/Egr, B_moto###########################################################
set.seed(123)  
lm_D_R17<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,29:47,49:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_R17 <- step(lm_D_R17)
slm_D_R17$anova
summary(slm_D_R17)#AIC: 146.63

#18sin los  predictores con correlacion >0.9, L_sinV, Logros, PrbSel, Ing/Egr,pts financieros, B_moto#####################################
set.seed(123)  
lm_D_R18<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,32,33,48:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_R18<- step(lm_D_R18)
slm_D_R18$anova
summary(slm_D_R18)#AIC: 193.74

#19sin los  predictores con correlacion >0.9, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto##############################################
set.seed(123)  
lm_D_R19<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,32,48:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_R19<- step(lm_D_R19)
slm_D_R19$anova
summary(slm_D_R19)#AIC: 193.18

#20sin los  predictores con correlacion >0.9, L_sinV, Pers, PrbSel, Ing/Egr, pts financieros, B_moto######################################
set.seed(123)  
lm_D_R20<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,32,39,48:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_R20<- step(lm_D_R20)
slm_D_R20$anova
summary(slm_D_R20)#AIC: 191.46

#21sin los  predictores con correlacion >0.9, L_sinV, Intereses, PrbSel, Ing/Egr, pts financieros, B_moto###################################
set.seed(123)  
lm_D_R21<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,32,47:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_R21<- step(lm_D_R21)
slm_D_R21$anova
summary(slm_D_R21)#AIC:192.18

#22sin los  predictores con correlacion >0.9, L_sinV, Logros, Intereses, PrbSel, Ing/Egr, pts financieros,B_moto############################
set.seed(123)  
lm_D_R22<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,32,33,47:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_R22<- step(lm_D_R22)
slm_D_R22$anova
summary(slm_D_R22)#AIC: 187.12

#23sin los  predictores con correlacion >0.9, L_sinV, Ing/Egr,pts financieros,B_moto#####################################
set.seed(123)  
lm_D_R23<-  bayesglm(sampleDefaultR_tr$Default ~ ., data = sampleDefaultR_tr[,-c(20,23,24,32,49:54,58,61:62)], 
                     family=binomial,control = list(maxit = 100))
slm_D_R23<- step(lm_D_R23)
slm_D_R23$anova
summary(slm_D_R23)#AIC: 199.45

#24sin los  predictores con correlacion >0.9, L_sinV, subcategorias, Ing/Egr, pts financieros,B_moto###################################
set.seed(123)
lm_D_R24<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100), 
                     data = sampleDefaultR_tr[,-c(20,23,24,29:32,34:38,40:46,49:54,58,61:62)])
slm_D_R24<- step(lm_D_R24)
slm_D_R24$anova
summary(slm_D_R24)#AIC: 184

#25sin los  predictores con correlacion >0.9, subcategorias, Ing/Egr, pts financieros,B_moto#########################################################
set.seed(123)
lm_D_R25<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),
                     data = sampleDefaultR_tr[,-c(20,23,24,29:31,34:38,40:46,49:54,58,61:62)])
slm_D_R25<- step(lm_D_R25)
slm_D_R25$anova
summary(slm_D_R25)#AIC: 208.2

#26sin los  predictores con correlacion >0.9, R_PromEdadH, Ing/Egr, pts financieros, B_moto#########################################################
set.seed(123)
lm_D_R26<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),
                     data = sampleDefaultR_tr[,-c(15,20,23,24,49:54,58,61:62)])
slm_D_R26<- step(lm_D_R26)
slm_D_R26$anova
summary(slm_D_R26)#AIC: 192.15

#27sin predictores financieros,relacionados PrbSel#####################################################################################################
set.seed(123)
lm_D_R27<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(29:62)])
slm_D_R27<- step(lm_D_R27)
slm_D_R27$anova
summary(slm_D_R27)#AIC: 144.9

#28sin los  predictores con correlacion >0.9, predictores financieros,relacionados prbSel#############################################################
set.seed(123)
lm_D_R28<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(20,23,24,29:62)])
slm_D_R28<- step(lm_D_R28)
slm_D_R28$anova
summary(slm_D_R28)#AIC: 153.3

#29sin los  predictores relacionados PrbSel####################################################################################################
set.seed(123)
lm_D_R29<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(29:48,61:62)])
slm_D_R29<- step(lm_D_R29)
slm_D_R29$anova
summary(slm_D_R29)#AIC: 147.43

#30sin los  predictores con correlacion >0.9, predictores relacionados PrbSel####################################################################################################
set.seed(123)
lm_D_R30<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(20,23,24,29:48,54,61:62)])
slm_D_R30<- step(lm_D_R30)
slm_D_R30$anova
summary(slm_D_R30)#AIC: 146.23

#31sin los  predictores con correlacion >0.9, predictores relacionados PrbSel, predictores relacionados Acdm#############################################################################################
set.seed(123)
lm_D_R31<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(20:24,29:48,54,61:62)])
slm_D_R31<- step(lm_D_R31)
slm_D_R31$anova
summary(slm_D_R31)#AIC: 143.9

#32sin predictores relacionados PrbSel, predictores relacionados Acdm#############################################################################################
set.seed(123)
lm_D_R32<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(20:24,29:48,61:62)])
slm_D_R32<- step(lm_D_R32)
slm_D_R32$anova
summary(slm_D_R32)#AIC: 145.49

#33sin predictores relacionados PrbSel, predictores relacionados Familiar#############################################################################################
set.seed(123)
lm_D_R33<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(8:15,29:48,61:62)])
slm_D_R33<- step(lm_D_R33)
slm_D_R33$anova
summary(slm_D_R33)#AIC: 145.65

#34sin los  predictores con correlacion >0.9, predictores relacionados PrbSel, predictores relacionados Familiar#############################################################################################
set.seed(123)
lm_D_R34<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(8:15,20,23,24,29:48,54,61:62)])
slm_D_R34<- step(lm_D_R34)
slm_D_R34$anova
summary(slm_D_R34)#AIC: 188.48

#35sin los  predictores con correlacion >0.9, predictores relacionados PrbSel, predictores relacionados ecnm#############################################################################################
set.seed(123)
lm_D_R35<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(16:19,20,23,24,29:48,54,61:62)])
slm_D_R35<- step(lm_D_R35)
slm_D_R35$anova
summary(slm_D_R35)#AIC: 167.51

#36 sin predictores relacionados PrbSel, predictores relacionados ecnm#############################################################################################
set.seed(123)
lm_D_R36<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(16:19,29:48,61:62)])
slm_D_R36<- step(lm_D_R36)
slm_D_R36$anova
summary(slm_D_R36)#AIC: 152.24

#37sin predictores relacionados PrbSel, predictores relacionados socioD#############################################################################################
set.seed(123)
lm_D_R37<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(2:7,29:48,61:62)])
slm_D_R37<- step(lm_D_R37)
slm_D_R37$anova
summary(slm_D_R37)#AIC: 178.46

#38sin los  predictores con correlacion >0.9,predictores relacionados PrbSel, predictores relacionados socioD#############################################################################################
set.seed(123)
lm_D_R38<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(2:7,20, 23,24,29:48,54,61:62)])
slm_D_R38<- step(lm_D_R38)
slm_D_R38$anova
summary(slm_D_R38)#AIC: 161.3

#39sin los  predictores con correlacion >0.9,predictores relacionados PrbSel, predictores relacionados ISA#############################################################################################
set.seed(123)
lm_D_R39<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(20, 23,24:48,54,61:62)])
slm_D_R39<- step(lm_D_R39)
slm_D_R39$anova
summary(slm_D_R39)#AIC: 149.61

#40sin predictores relacionados PrbSel, predictores relacionados ISA#############################################################################################
set.seed(123)
lm_D_R40<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(25:48,61:62)])
slm_D_R40<- step(lm_D_R40)
slm_D_R40$anova
summary(slm_D_R40)#AIC: 175.94

#41sin predictores relacionados PrbSel, predictores relacionados Familiar, Ing/Egr#############################################################################################
set.seed(123)
lm_D_R41<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(25:49,61:62)])
slm_D_R41<- step(lm_D_R41)
slm_D_R41$anova
summary(slm_D_R41)#AIC: 175.94

#42sin los  predictores con correlacion >0.9, relacionados socioD, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_R42<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(2:7,20,23,24,32,48:54,58,61:62)])
slm_D_R42<- step(lm_D_R42)
slm_D_R42$anova
summary(slm_D_R42)#AIC: 214.69

#43sin los  predictores con correlacion >0.9, genero, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_R43<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(2,20,23,24,32,48:54,58,61:62)])
slm_D_R43<- step(lm_D_R43)
slm_D_R43$anova
summary(slm_D_R43)#AIC: 198.62

#44sin los  predictores con correlacion >0.9, EstCiv, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_R44<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(3,20,23,24,32,48:54,58,61:62)])
slm_D_R44<- step(lm_D_R44)
slm_D_R44$anova
summary(slm_D_R44)#AIC: 199.21

#45sin los  predictores con correlacion >0.9, RegNac, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_R45<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(4,20,23,24,32,48:54,58,61:62)])
slm_D_R45<- step(lm_D_R45)
slm_D_R45$anova
summary(slm_D_R45)#AIC: 194.26

#46sin los  predictores con correlacion >0.9, NacCap, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_R46<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(5,20,23,24,32,48:54,58,61:62)])
slm_D_R46<- step(lm_D_R46)
slm_D_R46$anova
summary(slm_D_R46)#AIC: 191.32

#47sin los  predictores con correlacion >0.9, ResEst, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_R47<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(6,20,23,24,32,48:54,58,61:62)])
slm_D_R47<- step(lm_D_R47)
slm_D_R47$anova
summary(slm_D_R47)#AIC: 192.25

#48sin los  predictores con correlacion >0.9, ResEst, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_R48<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(7,20,23,24,32,48:54,58,61:62)])
slm_D_R48<- step(lm_D_R48)
slm_D_R48$anova
summary(slm_D_R48)# AIC: 193.18

#49sin los  predictores con correlacion >0.9, Genero, RegNac, L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_R49<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(2,4,20,23,24,32,48:54,58,61:62)])
slm_D_R49<- step(lm_D_R49)
slm_D_R49$anova
summary(slm_D_R49)#AIC: 198.37

#50sin los  predictores con correlacion >0.9, Herm,L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_R50<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(12,20,23,24,32,48:54,58,61:62)])
slm_D_R50<- step(lm_D_R50)
slm_D_R50$anova
summary(slm_D_R50)#AIC: 189.71

#51sin los  predictores con correlacion >0.9, RegNac,Herm,L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_R51<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(4,12,20,23,24,32,48:54,58,61:62)])
slm_D_R51<- step(lm_D_R51)
slm_D_R51$anova
summary(slm_D_R51)#AIC: 202.59

#52sin los  predictores con correlacion >0.9, RegNac,Herm,LugHerm,L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_R52<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(4,12,13,20,23,24,32,48:54,58,61:62)])
slm_D_R52<- step(lm_D_R52)
slm_D_R52$anova
summary(slm_D_R52)#AIC: 196.58

#53sin los  predictores con correlacion >0.9, RegNac,Herm,LugHerm,R_Estrato,L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_R53<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(4,12,13,16,20,23,24,32,48:54,58,61:62)])
slm_D_R53<- step(lm_D_R53)
slm_D_R53$anova
summary(slm_D_R53)#AIC: 194.77

#54sin los  predictores con correlacion >0.9, RegNac,Herm,LugHerm,R_Estrato,NivFrmc,L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_R54<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(4,12,13,16,20,21,23,24,32,48:54,58,61:62)])
slm_D_R54<- step(lm_D_R54)
slm_D_R54$anova
summary(slm_D_R54)#AIC: 187.14

#55sin los  predictores con correlacion >0.9, RegNac,Herm,LugHerm,R_Estrato,NivFrmc,RespEcnm,L_sinV, PrbSel, Ing/Egr, pts financieros, B_moto#############################################################################################
set.seed(123)
lm_D_R55<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(4,12,13,16,17,20,21,23,24,32,48:54,58,61:62)])
slm_D_R55<- step(lm_D_R55)
slm_D_R55$anova
summary(slm_D_R55)#AIC: 171.21

#56sin los  predictores con correlacion >0.9, RegNac,Herm,LugHerm,R_Estrato,NivFrmc,RespEcnm,L_sinV, PrbSel, Ing/Egr, pts financieros, B_cel,B_moto#############################################################################################
set.seed(123)
lm_D_R56<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(4,12,13,16,17,20,21,23,24,32,48:54,57,58,61:62)])
slm_D_R56<- step(lm_D_R56)
slm_D_R56$anova
summary(slm_D_R56)#AIC: 184.98

#57sin los  predictores con correlacion >0.9, L_sinV, PrbSel, Ing/Egr, pts financieros, B_viv,B_moto#############################################################################################
set.seed(123)
lm_D_R57<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(20,23,24,32,48:55,58,61:62)])
slm_D_R57<- step(lm_D_R57)
slm_D_R57$anova
summary(slm_D_R57)#AIC: 192.66

#58sin los  predictores con correlacion >0.9, L_sinV, PrbSel, Ing/Egr, pts financieros, B_carro,B_moto#############################################################################################
set.seed(123)
lm_D_R58<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(20,23,24,32,48:54,58,59,61:62)])
slm_D_R58<- step(lm_D_R58)
slm_D_R58$anova
summary(slm_D_R58)#AIC: 195.26

#59sin los  predictores con correlacion >0.9, L_sinV, PrbSel, Ing/Egr, CtaAh, B_moto#############################################################################################
set.seed(123)
lm_D_R59<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(20,23,24,32,48:50,54,58,61:62)])
slm_D_R59<- step(lm_D_R59)
slm_D_R59$anova
summary(slm_D_R59)#AIC: 197.46

#60sin los  predictores con correlacion >0.9, L_sinV, PrbSel, Ing/Egr, CtaAh, TarjCred, B_moto#############################################################################################
set.seed(123)
lm_D_R60<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(20,23,24,32,48:51,54,58,61:62)])
slm_D_R60<- step(lm_D_R60)
slm_D_R60$anova
summary(slm_D_R60)#AIC: 196.18

#61**sin los  predictores con correlacion >0.9, L_sinV, PrbSel, Ing/Egr, CtaAh, TarjCred, Creditos,B_moto#############################################################################################
set.seed(123)
lm_D_R61<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(20,23,24,32,48:52,54,58,61:62)])
slm_D_R61<- step(lm_D_R61)
slm_D_R61$anova
summary(slm_D_R61)#AIC: 137.61

y_glm_d_R61<- predict(slm_D_R61,type = "response")
a<-data.frame(y_glm_d_R61,DefaultReal=sampleDefaultR_tr$Default, scoring=sampleDefaultR_tr$scoring, PrbSel=sampleDefaultR_tr$R_PrbSel)
a<-a[order(a$y_glm_d_R61, decreasing=FALSE),]
a$rank<-1:nrow(a)
ggplot(data=a, aes(x=rank, y=y_glm_d_R61)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Registro") +ylab("Prediccion de la probabilidad de default")+ggtitle("Datos de entrenamiento")
ggsave("slm_D_R61Tt.pdf")

cor(y_glm_d_R61,sampleDefaultR_tr$scoring)
ggplot(data=a, aes(x=y_glm_d_R61, y=scoring)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Scoring")+ggtitle("Datos de entrenamiento")
ggsave("Scoring vs DefaultReal vs slm_D_R61Tt.pdf")

cor(y_glm_d_R61,sampleDefaultR_tr$PrbSel)
ggplot(data=a, aes(x=y_glm_d_R61, y=PrbSel)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Prueba de seleccion")+ggtitle("Datos de entrenamiento")
ggsave("Prueba de seleccion vs DefaultReal vs slm_D_R61Tt.pdf")

glm_D_R61 = rep("No", dim(sampleDefaultR_tr)[1])
glm_D_R61[y_glm_d_R61 > .5] = "Si"
glm_D_R61<-as.factor(glm_D_R61)
confusionMatrix(glm_D_R61,sampleDefaultR_tr$Default)

#prueba
y_d_R61<- predict(slm_D_R61, sampleDefaultR_ts, type = "response")

a<-data.frame(y_d_R61,DefaultReal=sampleDefaultR_ts$Default,scoring=sampleDefaultR_ts$scoring,PrbSel=sampleDefaultR_ts$R_PrbSel)
a<-a[order(a$y_d_R61, decreasing=FALSE),]
a$rank<-1:nrow(a)
ggplot(data=a, aes(x=rank, y=y_d_R61)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Registro") +ylab("Prediccion de  la probabilidad de default")+ggtitle("Datos de prueba")
ggsave("slm_D_R61Ts.pdf")

cor(y_d_R61,sampleDefaultR_ts$scoring)
ggplot(data=a, aes(x=y_d_R61, y=scoring)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Scoring")+ggtitle("Datos de prueba")
ggsave("Scoring vs DefaultReal vs slm_D_R61Ts.pdf")

cor(y_d_R61,sampleDefaultR_ts$PrbSel)
ggplot(data=a, aes(x=y_d_R61, y=PrbSel)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Prediccion de  la probabilidad de default") +ylab("Prueba de seleccion")+ggtitle("Datos de prueba")
ggsave("Prueba de seleccion vs DefaultReal vs slm_D_R61Ts.pdf")

ts_d_R61 = rep("No", dim(sampleDefaultR_ts)[1])
ts_d_R61[y_d_R61 > .5] = "Si"
ts_d_R61<-as.factor(ts_d_R61)
confusionMatrix(ts_d_R61,sampleDefaultR_ts$Default)

pred_ts_d_R61<- prediction(y_d_R61, sampleDefaultR_ts$Default)
perf_ts_d_R61 <- performance(pred_ts_d_R61,"tpr","fpr")
pdf("ROC_D_R61Ts.pdf")
plot(perf_ts_d_R61,colorize=TRUE)#ROC curve
dev.off()

#62sin AreaCncm, L_sinV, PrbSel, Ing/Egr, pts financieros,B_moto#############################################################################################
set.seed(123)
lm_D_R62<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(23,32,48:54,58,61:62)])
slm_D_R62<- step(lm_D_R62)
slm_D_R62$anova
summary(slm_D_R62)#AIC: 194.38

#63sin PrdPA, L_sinV, PrbSel, Ing/Egr, pts financieros,B_moto#############################################################################################
set.seed(123)
lm_D_R63<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(24,32,48:54,58,61:62)])
slm_D_R63<- step(lm_D_R63)
slm_D_R63$anova
summary(slm_D_R63)#AIC: 189.55

#64sin AltaCalidad, L_sinV, PrbSel, Ing/Egr, pts financieros,B_moto#############################################################################################
set.seed(123)
lm_D_R64<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(20,32,48:54,58,61:62)])
slm_D_R64<- step(lm_D_R64)
slm_D_R64$anova
summary(slm_D_R64)#AIC: 176.6

#65sin AltaCalidad, PrdPA,L_sinV, PrbSel, Ing/Egr, pts financieros,B_moto#############################################################################################
set.seed(123)
lm_D_R65<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(20,24,32,48:54,58,61:62)])
slm_D_R65<- step(lm_D_R65)
slm_D_R65$anova
summary(slm_D_R65)#AIC: 176.6

#66sin AltaCalidad, AreaCncm,L_sinV, PrbSel, Ing/Egr, pts financieros,B_moto#############################################################################################
set.seed(123)
lm_D_R66<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(20,23,32,48:54,58,61:62)])
slm_D_R66<- step(lm_D_R66)
slm_D_R66$anova
summary(slm_D_R66)#

#67sin PrdPA, AreaCncm,L_sinV, PrbSel, Ing/Egr, pts financieros,B_moto#############################################################################################
set.seed(123)
lm_D_R67<-  bayesglm(sampleDefaultR_tr$Default ~ .,family=binomial,control = list(maxit = 100),data = sampleDefaultR_tr[,-c(24,23,32,48:54,58,61:62)])
slm_D_R67<- step(lm_D_R67)
slm_D_R67$anova
summary(slm_D_R67)#



