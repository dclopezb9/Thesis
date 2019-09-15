#___________________________________________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________________________________________
#_________________________________________Prediccion del riesgo default en acuerdos de ingreso compartido:__________________________________________
#_____________________Evidencia para la financiacion de educacion superior en Colombia usando metodos de machine learning___________________________
#__________________________________________________MSc.(c) Diana Carolina Lopez Becerra_____________________________________________________________
#___________________________________________________________Ph.D.Giovanni Munoz_____________________________________________________________________
#____________________________________________________________Ph.D.Hernando Diaz_____________________________________________________________________
#__________________________________________Universidad Nacional de Colombia, sede Bogota D.C._______________________________________________________
#_________________________________________________________________2019______________________________________________________________________________

#manejar tildes#####################################################################################################################################
Sys.setlocale('LC_ALL','es_CO.iso88591')

#instalar paquetes##################################################################################################################################
install.packages("vcd")
install.packages("hms")
install.packages("plyr")

#cargar paquetes####################################################################################################################################
library(vcd)
library(plyr)
library(hms)

#cargar la base de datos############################################################################################################################
base<-read.csv(paste('DM_DB_tobeDebugged.csv',sep=""),header = TRUE, sep=";")
scoring<-read.csv(paste('scoring.csv',sep=""),header = TRUE, sep=";")
mntTotal<-read.csv(paste('montoTotal.csv',sep=""),header = TRUE, sep=";")

#declarar clase de las variables####################################################################################################################
base$CuotMora<-as.factor(base$CuotMora)
base$RP_Aplic<-as.numeric(base$RP_Aplic)
base$AreaCncm<-as.factor(base$AreaCncm)
base$Mtrc<-as.numeric(base$Mtrc)
base$CultPag<-as.numeric(base$CultPag)
base$PrcntCuota<-as.numeric(base$PrcntCuota)
base$Pts_Banc<-as.numeric(base$Pts_Banc)
base$B_pond<-as.numeric(base$B_pond)

#crear variable default a partir de la variable CuotMora############################################################################################
base$Default<-as.character(base$Default)
for(i in 1:nrow(base)){
  if(base$CuotMora[i]=='0'||base$CuotMora[i]=="1"||base$CuotMora[i]=="2"||base$CuotMora[i]=="3"
     ||base$CuotMora[i]=="4"||base$CuotMora[i]=="5"){
    base$Default[i]<-as.character("No")}else{
      base$Default[i]<-as.character("Si")
    }
}
base$Default<-factor(base$Default)

#crear variable riesgo a partir de la variable CuotMora############################################################################################
base$Riesgo<-as.character(base$Riesgo)
for(i in 1:nrow(base)){
  if(base$CuotMora[i]=='0'||base$CuotMora[i]=="1"||base$CuotMora[i]=="2"){
    base$Riesgo[i]<-as.character("Aceptable")
  }else if(base$CuotMora[i]=="3"||base$CuotMora[i]=="4"){
    base$Riesgo[i]<-as.character("Apreciable")
  }else if(base$CuotMora[i]=="5"||base$CuotMora[i]=="6"){
    base$Riesgo[i]<-as.character("Significativo")
  }
}
base$Riesgo<-factor(base$Riesgo)

#crear la variable Region de nacimiento a partir de la variable Dpto de nacimiento##################################################################

#opcion 1-regiones: Bogota, C/marca, norte, suroeste, este y centro#################################################################################
#base$RegNac<-as.character("a")
#for(i in 1:nrow(base)){
  if(base$DptoNac[i]=='ATLANTICO'||base$DptoNac[i]=="BOLIVAR"||base$DptoNac[i]=="CORDOBA"||base$DptoNac[i]=="MAGDALENA"||base$DptoNac[i]=="SUCRE"){
    base$RegNac[i]<-as.character("N.")#Norte
  }else if(base$DptoNac[i]=='CAQUETA'||base$DptoNac[i]=="CAUCA"||base$DptoNac[i]=="VALLE DEL CAUCA"||base$DptoNac[i]=="NARINO"){
    base$RegNac[i]<-as.character("S.O.")#Suroeste
  }else if(base$DptoNac[i]=='CASANARE'||base$DptoNac[i]=="META"||base$DptoNac[i]=="BOYACA"
    ||base$DptoNac[i]=="SANTANDER"||base$DptoNac[i]=="NORTE DE SANTANDER"){
    base$RegNac[i]<-as.character("E.")#Este
  } else if(base$DptoNac[i]=='ANTIOQUIA'||base$DptoNac[i]=="CALDAS"
    ||base$DptoNac[i]=="HUILA"||base$DptoNac[i]=="TOLIMA"||base$DptoNac[i]=="QUINDIO"){
    base$RegNac[i]<-as.character("C.")#Centro
  }else if(base$DptoNac[i]=='CUNDINAMARCA'){
    base$RegNac[i]<-as.character("C/marca")#C/marca
  } else{
    base$RegNac[i]<-as.character(base$DptoNac[i])#Bogota D.C.
  }
}

#opcion 2-regiones: Bogota, centro, este, norte y suroeste##########################################################################################
#base$RegNac<-as.character("a")
#for(i in 1:nrow(base)){
  if(base$DptoNac[i]=='ATLANTICO'||base$DptoNac[i]=="BOLIVAR"||base$DptoNac[i]=="CORDOBA"
     ||base$DptoNac[i]=="MAGDALENA"||base$DptoNac[i]=="SUCRE"){
    base$RegNac[i]<-as.character("N.")#Norte
  }else if(base$DptoNac[i]=='CAQUETA'||base$DptoNac[i]=="CAUCA"||base$DptoNac[i]=="VALLE DEL CAUCA"||base$DptoNac[i]=="NARINO"){
    base$RegNac[i]<-as.character("S.O.")#Suroeste
  }else if(base$DptoNac[i]=='CASANARE'||base$DptoNac[i]=="META"||base$DptoNac[i]=="BOYACA"
           ||base$DptoNac[i]=="SANTANDER"||base$DptoNac[i]=="NORTE DE SANTANDER"){
    base$RegNac[i]<-as.character("E.")#Este
  } else if(base$DptoNac[i]=='ANTIOQUIA'||base$DptoNac[i]=="CALDAS"||base$DptoNac[i]=="CUNDINAMARCA"
            ||base$DptoNac[i]=="HUILA"||base$DptoNac[i]=="TOLIMA"||base$DptoNac[i]=="QUINDIO"){
    base$RegNac[i]<-as.character("C.")#Centro
  }else{
    base$RegNac[i]<-as.character(base$DptoNac[i])#Bogota D.C.
  }
}

#opcion 3-regiones: Bogota, centro, noreste y suroeste##############################################################################################
base$RegNac<-as.character("a")
for(i in 1:nrow(base)){
  if(base$DptoNac[i]=='ATLANTICO'||base$DptoNac[i]=="BOLIVAR"||base$DptoNac[i]=="CORDOBA"
     ||base$DptoNac[i]=="MAGDALENA"||base$DptoNac[i]=="SUCRE"||base$DptoNac[i]=='CASANARE'
     ||base$DptoNac[i]=="META"||base$DptoNac[i]=="BOYACA"||base$DptoNac[i]=="SANTANDER"||base$DptoNac[i]=="NORTE DE SANTANDER"){
    base$RegNac[i]<-as.character("N.E.")#Noreste
  }else if(base$DptoNac[i]=='CAQUETA'||base$DptoNac[i]=="CAUCA"||base$DptoNac[i]=="VALLE DEL CAUCA"||base$DptoNac[i]=="NARINO"){
    base$RegNac[i]<-as.character("S.O.")#Suroeste
  } else if(base$DptoNac[i]=='ANTIOQUIA'||base$DptoNac[i]=="CALDAS"||base$DptoNac[i]=="CUNDINAMARCA"
            ||base$DptoNac[i]=="HUILA"||base$DptoNac[i]=="TOLIMA"||base$DptoNac[i]=="QUINDIO"){
    base$RegNac[i]<-as.character("C.")#Centro
  }else{
    base$RegNac[i]<-as.character(base$DptoNac[i])#Bogota D.C.
  }
}

#convertir a factor la variable "Region de nacimiento"##############################################################################################
base$RegNac<-factor(base$RegNac)

#crear la variable Rango edad postulacion: a partir del analisis realizado con la muestra y la variable "edad postulacion"##########################
base$R_EdadPost<-cut(x = base$EdadPost, breaks = c(0,20,22,24,26,28,30,100), 
                        labels = c("[18,20]","(20,22]","(22,24]","(24,26]","(26,28]","(28,30]",">30"))
base$R_EdadPost<-factor(base$R_EdadPost,levels=c(">30","(28,30]","(26,28]","(24,26]","(22,24]","(20,22]","[18,20]"))

#editar la categoria "Entre 5 y 100" de la variable "FamDirViv" por "Mas de 5"######################################################################
levels(base$FamDirViv)[2] <-'5 o mas'
base$FamDirViv<-factor(base$FamDirViv,levels =c("Ninguno", "Entre 1 y 4","5 o mas"))
#editar las categorias de la variable "Herm" #######################################################################################################
levels(base$Herm)[3] <-'Menor o igual a uno'
levels(base$Herm)[2] <-'Mas de tres'
base$Herm<-factor(base$Herm,levels =c("Menor o igual a uno", "Dos", "Tres", "Mas de tres"))

#editar la variable PromEdadH#######################################################################################################################
levels(base$PromEdadH)[18] <-0
base$PromEdadH<-as.numeric(paste(base$PromEdadH))

#crear la variable Rango edad promedio de los hijos: a partir del analisis realizado con la muestra y la variable "PromEdadH"#######################
base$R_PromEdadH<-cut(x = base$PromEdadH, breaks = c(-1,0,6.5,100), labels = c("Sin hijos","0.5-6.5",">6.5"))

#crear la variable Rango estrato a partir del analisis realizado con la muestra y la variable "Estrato"#############################################
base$R_Estrato<-as.character("a")
for(i in 1:nrow(base)){
  if(base$Estrato[i]=='Uno'||base$Estrato[i]=="Dos"){
    base$R_Estrato[i]<-as.character("Uno y Dos")
  }else if(base$Estrato[i]=="Cinco"||base$Estrato[i]=="Seis"){
    base$R_Estrato[i]<-as.character("Cinco y Seis")
  }else{
    base$R_Estrato[i]<-as.character(base$Estrato[i])
  }
}
base$R_Estrato<-factor(base$R_Estrato)
base$R_Estrato<-factor(base$R_Estrato, levels =c("Uno y Dos","Tres","Cuatro","Cinco y Seis"))

#editar la categoria "Usted" de la variable "RespEcnm" por "El postulante"##########################################################################
levels(base$RespEcnm)[3] <-'El postulante'

#editar la variable IES: tabla que organiza de menor a mayor la frecuencia de la IES del postulante#################################################
#IES<-count(base$IES)
#IES[order(IES$freq),]

#editar la variable Nivel de formacion##############################################################################################################
levels(base$NivFrmc)[1] <-"Especializacion"
levels(base$NivFrmc)[2] <-"Maestria"
levels(base$NivFrmc)[3] <-"Universitario"
base$NivFrmc<-factor(base$NivFrmc,levels =c("Universitario", "Especializacion", "Maestria"))

#editar la variable Programa academico: tabla que organiza de menor a mayor la frecuencia del PA del postulante#####################################
#PA<-count(base$PA)
#PA[order(PA$freq),]
#editar la variable Metodologia#####################################################################################################################
levels(base$Metdlg)[1] <-"Distancia"
base$Metdlg<-factor(base$Metdlg,levels =c("Presencial", "Virtual","Distancia"))

#editar la variable Areas del conocimiento##########################################################################################################
levels(base$AreaCncm)[1] <-"B.A."
levels(base$AreaCncm)[2] <-"C.Ed."
levels(base$AreaCncm)[3] <-"C.S."
levels(base$AreaCncm)[4] <-"C.So."
levels(base$AreaCncm)[5] <-"C.Ec."
levels(base$AreaCncm)[6] <-"I.A."
levels(base$AreaCncm)[7] <-"M.Cn."

#crear la variable Rango periodos programa academico: a partir del analisis realizado con la muestra y la variable "PrdPA"##########################
base$R_PrdPA<-cut(x = base$PrdPA, breaks = c(0,2,4,7,8,9, 10, 12), labels = c("2","3-4","5-7", "8","9","10","11-12"))

#crear la variable Rango Valor matricula semestral: a partir del analisis realizado con la muestra y la variable "Mtrc"#############################
base$R_Mtrc<-cut(x = base$Mtrc, breaks = c(0,2000000,3000000,4000000,6000000,8000000,10000000,23000000),
                    labels = c("[100k,2M]","(2M,3M]","(3M,4M]","(4M,6M]","(6M,8M]","(8M,10M]","(10M,23M]"))
base$R_Mtrc<-factor(base$R_Mtrc,levels =c("(10M,23M]", "(8M,10M]","(6M,8M]","(4M,6M]","(3M,4M]","(2M,3M]","[100k,2M]"))

#crear la variable Rango CP: a partir del analisis realizado con la muestra y la variable "CultPag"#################################################
base$R_CP<-cut(x = base$CultPag, breaks = c(-1,22400,49999,50000,300000), labels = c("[0,22.4K]","(22.4K,50K)","50","(50K,300K]"))

#crear la variable Rango PC: a partir del analisis realizado con la muestra y la variable "PrcntCuota"####################################################
base$R_PC<-cut(x = base$PrcntCuota, breaks = c(0,10,14,15,16,19.98,19.99,20), 
                  labels = c("(4%,10%]","(10%,14%]","(14%,15%]","(15%,16%]","(16%,19.98%]","(19.98%,19.99%]","20%"))
base$R_PC<-factor(base$R_PC,levels =c("20%","(19.98%,19.99%]","(16%,19.98%]", "(15%,16%]","(14%,15%]","(10%,14%]","(4%,10%]"))

#crear la variable Rango RP: a partir del analisis realizado con la muestra y la variable "CuotPact"######################################################
base$R_RP<-cut(x = base$CuotPact, breaks = c(0,20,30,40,50,60,70,80,100), 
                  labels = c("[16,20]","(20,30]","(30,40]","(40,50]","(50,60]","(60,70]","(70,80]","(80,100]"))
base$R_RP<-factor(base$R_RP,levels =c("(80,100]","(70,80]","(60,70]","(50,60]","(40,50]","(30,40]","(20,30]","[16,20]"))

#crear la variable Rango Puntaje prueba Logros-verbal: a partir del analisis realizado con la muestra###############################################
base$R_LV<-cut(x = base$L_verb, breaks = c(-1,5,6,7,8,9,11),labels = c("[0,5]","6","7","8","9","10"))

#crear la variable Rango Puntaje prueba Logros-espacial: a partir del analisis realizado con la muestra#############################################
base$R_LE<-cut(x = base$L_esp, breaks = c(-1,1,2,3,4,5,6,8),labels = c("[0,1]","2","3","4","5","6","7"))

#crear la variable Rango Puntaje prueba Logros-numerico: a partir del analisis realizado con la muestra#############################################
base$R_LN<-cut(x = base$L_num, breaks = c(-1,5,6,7,8,9,10,12),labels = c("[0,5]","6","7","8","9","10","11"))

#crear la variable Rango Puntaje prueba Logros numerico y espacial: a partir del analisis realizado con la muestra ################################
base$R_LSin<-cut(x = base$L_sinV, breaks = c(-1,8,10,12,14,21),labels = c("[0,8]","(8,10]","(10,12]","(12,14]","(14,20]"))

#crear la variable Rango Puntaje prueba Logros: a partir del analisis realizado con la muestra#####################################################
base$R_L<-cut(x = base$Logros, breaks = c(-1,14,16,18,20,22,29),labels = c("[0,14]","(14,16]","(16,18]","(18,20]","(20,22]","(22,28]"))

#crear la variable Rango Puntaje prueba personalidad: a partir del analisis realizado con la muestra###############################################
base$R_P<-cut(x = base$Personalidad, breaks = c(-1,25,27,28,29,30,31,32,33,34,36),
                 labels = c("[0,25]","[26,27]","28","29","30","31","32","33","34","35"))

#crear la variable Rango Puntaje prueba intereses: a partir del analisis realizado con la muestra###################################################
base$R_I<-cut(x = base$Intereses, breaks = c(-1,19,22,24,26,29,31,40),
                 labels = c("[0,19]","[20,22]","[23,24]","[25,26]","[27,29]","[30,31]","[32,39]"))

#crear la variable Rango Puntaje prueba a partir del analisis realizado con la muestra##############################################################
base$R_PrbSel<-cut(x = base$PrbSel, breaks = c(-1,63,69,72,76,84,102),
                  labels = c("[0,63]","[64,69]","[70,72]","[73,76]","[77,84]","[85,102]"))

#crear la variable Rango Bienes ponderados a partir del analisis realizado con la muestra###########################################################
base$R_B_pond<-cut(x = base$B_pond, breaks = c(-1,1,2,3,4,6),labels = c("[0,1]","(1,2]","(2,3]","(3,4]","(4,5]"))

#ordenar valores de las variables###################################################################################################################
base$EstCiv<-factor(base$EstCiv,levels =c("Soltero", "Union libre","Casado", "Divorciado"))
base$LugHerm<-factor(base$LugHerm,levels =c("Hijo unico", "Menor","Intermedio", "Mayor"))

#construir base para "train" y "test"###############################################################################################################
muestra<-base[base$TipBase=="Muestra",]

#construir base estudiantes: para experiemento "en vivo"############################################################################################
experimento<-base[base$TipBase=="Experimiento",]

#crear objeto colores para las graficas#############################################################################################################
coloresCM<-c("lightcyan4","cadetblue4","cadetblue", "cadetblue3","lightblue","lightblue2","lightcyan2")

#construir tablas de contingencia: variable de interes vs posibles predictores #####################################################################
####################################################################################################################################################

#1Genero#############################################################################################################################################
pdf("t1Genero.pdf")
t1Genero<-table(muestra$Genero,muestra$CuotMora)
plot(t1Genero, col = coloresCM,  ylab = "Maximo cuotas en mora alcanzadas", main = "Genero")
dev.off()

pdf("t1_R_Gen.pdf")
t1_R_Gen<-table(muestra$Genero,muestra$Riesgo)
plot(t1_R_Gen, col = coloresCM, ylab ="Riesgo",  main = "Genero")
dev.off()

pdf("t1_D_Gen.pdf")
t1_D_Gen<-table(muestra$Genero,muestra$Default)
plot(t1_D_Gen, col = coloresCM, ylab ="Default", main = "Genero")
dev.off()

#2Estado civil#######################################################################################################################################
pdf("t2EstCiv.pdf")
t2EstCiv<-table(muestra$EstCiv,muestra$CuotMora)
plot(t2EstCiv, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas", main = "Estado civil")
dev.off()

pdf("t2_R_EstCiv.pdf")
t2_R_EstCiv<-table(muestra$EstCiv,muestra$Riesgo)
plot(t2_R_EstCiv, col = coloresCM,ylab ="Riesgo", main = "Estado civil")
dev.off()

pdf("t2_D_EstCiv.pdf")
t2_D_EstCiv<-table(muestra$EstCiv,muestra$Default)
plot(t2_D_EstCiv, col = coloresCM,ylab ="Default", main = "Estado civil")
dev.off()

#3Dpto y/o region de nacimiento######################################################################################################################
table(muestra$DptoNac) #puesto que hay 21 dptos se agruparon en regiones para probar cual es mas efectiva

pdf("t3RegNac.pdf")
t3RegNac<-table(muestra$RegNac,muestra$CuotMora)
plot(t3RegNac, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas", main = "Region de nacimiento")
dev.off()

pdf("t3_R_RegNac.pdf")
t3_R_RegNac<-table(muestra$RegNac,muestra$Riesgo)
plot(t3_R_RegNac, col = coloresCM, ylab ="Riesgo",main = "Region de nacimiento")
dev.off()

pdf("t3_D_RegNac.pdf")
t3_D_RegNac<-table(muestra$RegNac,muestra$Default)
plot(t3_D_RegNac, col = coloresCM, ylab ="Default",main = "Region de nacimiento")
dev.off()

#la variable municipio de nacimiento no se tomara en cuenta puesto que puede tomar 57 valores, un 18.33% de las observaciones, lo que complejizaria
#a los metodos de ML para realizar predicciones. A partir de esta variable se construyo la variable "NacCap" que hace referencia a si el municipio
#de nacimiento es capital o no del departamento en el que esta localizado
#table(muestra$MunNac) 

#4Nacido en capital#################################################################################################################################
pdf("t4NacCap.pdf")
t4NacCap<-table(muestra$NacCap,muestra$CuotMora)
plot(t4NacCap, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas", main = "El Municipio de nacimiento es capital de departamento?")
dev.off()

pdf("t4_R_NacCap.pdf")
t4_R_NacCap<-table(muestra$NacCap,muestra$Riesgo)
plot(t4_R_NacCap, col = coloresCM, ylab ="Riesgo", main = "El Municipio de nacimiento es capital de departamento?")
dev.off()

pdf("t4_D_NacCap.pdf")
t4_D_NacCap<-table(muestra$NacCap,muestra$Default)
plot(t4_D_NacCap, col = coloresCM, ylab ="Default",main = "El Municipio de nacimiento es capital de departamento?")
dev.off()

#5Estudia en lugar de residencia?##################################################################################################################
pdf("t5ResEst.pdf")
t5ResEst<-table(muestra$ResEst,muestra$CuotMora)
plot(t5ResEst, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Estudia en el lugar de residencia?")
dev.off()

pdf("t5_R_ResEst.pdf")
t5_R_ResEst<-table(muestra$ResEst,muestra$Riesgo)
plot(t5_R_ResEst, col = coloresCM,ylab ="Riesgo",main = "Estudia en el lugar de residencia?")
dev.off()

pdf("t5_D_ResEst.pdf")
t5_D_ResEst<-table(muestra$ResEst,muestra$Default)
plot(t5_D_ResEst, col = coloresCM,ylab ="Default",main = "Estudia en el lugar de residencia?")
dev.off()

#la variable "grupo etnico" no se tomara en cuenta porque los valores encontrados no son significativos, no hay claridad ############################
#entre la diferencia entre "No aplica" y "Otro". Ademas la mayoria (76.85%) de las observaciones estan en alguna de esas dos categorias.

#6Edad y Rango de edad de postulacion###############################################################################################################
table(muestra$EdadPost)
summary(muestra$EdadPost)
sd(muestra$EdadPost)
#la distribucion de la edad de postulacion se encuentra entre 18 y 45 anos, con una media y mediana de 25 anos y una desviacion estandar en 4.48.
#Razon por la cual se grafica un histograma acumulado y se divide  la muestra en 5 rangos
hEdadPost<-hist(muestra$EdadPost, main = "Histograma: Edad postulacion")
pdf("h6EdadPost.pdf")
plot(hEdadPost, main = "Histograma: Edad postulacion")
dev.off()
pdf("hA6EdadPost.pdf")
hEdadPost$counts<- cumsum(hEdadPost$counts)
plot(hEdadPost, main = "Histograma acumulado: Edad postulacion")
dev.off()
cumsum(hEdadPost$density)*200

pdf("t6EdadPost.pdf")
t6EdadPost<-table(muestra$EdadPost,muestra$CuotMora)
plot(t6EdadPost, col=coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Edad postulacion")
dev.off()

pdf("t6_R_EdadPost.pdf")
t6_R_EdadPost<-table(muestra$EdadPost,muestra$Riesgo)
plot(t6_R_EdadPost, col=coloresCM, ylab ="Riesgo",main = "Edad postulacion")
dev.off()

pdf("t6_D_EdadPost.pdf")
t6_D_EdadPost<-table(muestra$EdadPost,muestra$Default)
plot(t6_D_EdadPost, col=coloresCM, ylab ="Default",main = "Edad postulacion")
dev.off()

pdf("t6EdadPost_R.pdf")
t6EdadPostR<-table(muestra$R_EdadPost,muestra$CuotMora)
plot(t6EdadPostR, col=coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Edad postulacion")
dev.off()

pdf("t6_R_EdadPostR.pdf")
t6_R_EdadPostR<-table(muestra$R_EdadPost,muestra$Riesgo)
plot(t6_R_EdadPostR, col=coloresCM, ylab ="Riesgo",main = "Edad postulacion")
dev.off()

pdf("t6_D_EdadPostR.pdf")
t6_D_EdadPostR<-table(muestra$R_EdadPost,muestra$Default)
plot(t6_D_EdadPostR, col=coloresCM, ylab ="Default",main = "Edad postulacion")
dev.off()

#7Vive con familiares directos####################################################################################################################
pdf("t7FamDirViv.pdf")
t7FamDirViv<-table(muestra$FamDirViv,muestra$CuotMora)
plot(t7FamDirViv, col =coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Cantidad de familiares directos que viven con el postulante")
dev.off()

pdf("t7_R_FamDirViv.pdf")
t7_R_FamDirViv<-table(muestra$FamDirViv,muestra$Riesgo)
plot(t7_R_FamDirViv, col =coloresCM,ylab ="Riesgo",main = "Cantidad de familiares directos que viven con el postulante")
dev.off()

pdf("t7_D_FamDirViv.pdf")
t7_D_FamDirViv<-table(muestra$FamDirViv,muestra$Default)
plot(t7_D_FamDirViv, col =coloresCM,ylab ="Default",main = "Cantidad de familiares directos que viven con el postulante")
dev.off()

#8La madre aun vive###############################################################################################################################
pdf("t8MadViv.pdf")
t8MadViv<-table(muestra$MadViv,muestra$CuotMora)
plot(t8MadViv, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "La madre vive")
dev.off()

pdf("t8_R_MadViv.pdf")
t8_R_MadViv<-table(muestra$MadViv,muestra$Riesgo)
plot(t8_R_MadViv, col = coloresCM,ylab ="Riesgo",main = "La madre vive")
dev.off()

pdf("t8_D_MadViv.pdf")
t8_D_MadViv<-table(muestra$MadViv,muestra$Default)
plot(t8_D_MadViv, col = coloresCM,ylab ="Default",main = "La madre vive")
dev.off()

#9El padre aun vive###############################################################################################################################
pdf("t9PadViv.pdf")
t9PadViv<-table(muestra$PadViv,muestra$CuotMora)
plot(t9PadViv, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "El padre vive")
dev.off()

pdf("t9_R_PadViv.pdf")
t9_R_PadViv<-table(muestra$PadViv,muestra$Riesgo)
plot(t9_R_PadViv, col = coloresCM,ylab ="Riesgo",main = "El padre vive")
dev.off()

pdf("t9_D_PadViv.pdf")
t9_D_PadViv<-table(muestra$PadViv,muestra$Default)
plot(t9_D_PadViv, col = coloresCM,ylab ="Default",main = "El padre vive")
dev.off()

#10El postulante vive en casa de los padres########################################################################################################
pdf("t10VivPad.pdf")
t10VivPad<-table(muestra$VivPad,muestra$CuotMora)
plot(t10VivPad, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "El postulante vive en casa de los padres")
dev.off()

pdf("t10_R_VivPad.pdf")
t10_R_VivPad<-table(muestra$VivPad,muestra$Riesgo)
plot(t10_R_VivPad, col = coloresCM,ylab ="Riesgo",main = "El postulante vive en casa de los padres")
dev.off()

pdf("t10_D_VivPad.pdf")
t10_D_VivPad<-table(muestra$VivPad,muestra$Default)
plot(t10_D_VivPad, col = coloresCM,ylab ="Default",main = "El postulante vive en casa de los padres")
dev.off()

#11Cantidad de hermanos############################################################################################################################
pdf("t11Herm.pdf")
t11Herm<-table(muestra$Herm,muestra$CuotMora)
plot(t11Herm,col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Cantidad de hermanos")
dev.off()

pdf("t11_R_Herm.pdf")
t11_R_Herm<-table(muestra$Herm,muestra$Riesgo)
plot(t11_R_Herm,col = coloresCM, ylab ="Riesgo",main = "Cantidad de hermanos")
dev.off()

pdf("t11_D_Herm.pdf")
t11_D_Herm<-table(muestra$Herm,muestra$Default)
plot(t11_D_Herm,col = coloresCM, ylab ="Default",main = "Cantidad de hermanos")
dev.off()

#12Lugar entre los hermanos########################################################################################################################
pdf("t12LugHerm.pdf")
t12LugHerm<-table(muestra$LugHerm,muestra$CuotMora)
plot(t12LugHerm, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Lugar entre los hermanos")
dev.off()

pdf("t12_R_LugHerm.pdf")
t12_R_LugHerm<-table(muestra$LugHerm,muestra$Riesgo)
plot(t12_R_LugHerm, col = coloresCM,ylab ="Riesgo",main = "Lugar entre los hermanos")
dev.off()

pdf("t12_D_LugHerm.pdf")
t12_D_LugHerm<-table(muestra$LugHerm,muestra$Default)
plot(t12_D_LugHerm, col = coloresCM,ylab ="Default",main = "Lugar entre los hermanos")
dev.off()

#13Cantidad de hijos###############################################################################################################################
pdf("t13Hijos.pdf")
t13Hijos<-table(muestra$Hijos,muestra$CuotMora)
plot(t13Hijos, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Cantidad de hijos")
dev.off()

pdf("t13_R_Hijos.pdf")
t13_R_Hijos<-table(muestra$Hijos,muestra$Riesgo)
plot(t13_R_Hijos, col = coloresCM,ylab ="Riesgo",main = "Cantidad de hijos")
dev.off()

pdf("t13_D_Hijos.pdf")
t13_D_Hijos<-table(muestra$Hijos,muestra$Default)
plot(t13_D_Hijos, col = coloresCM,ylab ="Default",main = "Cantidad de hijos")
dev.off()

#14Promedio de edad de los hijos###################################################################################################################
table(muestra$PromEdadH)
summary(muestra$PromEdadH)
sd(muestra$PromEdadH)
pdf("h15PromEdadH.pdf")
hEdadH<-hist(muestra$PromEdadH, main = "Histograma: Edad promedio de los hijos")
dev.off()
#la distribucion de la edad de postulacion se encuentra entre 0 y 12.5 años, con una media de 0.76 añosy mediana de 0 años  y una
#desviacion estandar en 4.48  (equivalente a que el postulante no tiene hijos)
#Razon por la cual se grafica un histograma acumulado y se divide  la muestra en 3 rangos
pdf("hA15PromEdadH.pdf")
hEdadH$counts<- cumsum(hEdadH$counts)
plot(hEdadH, main = "Histograma acumulado: Edad promedio de los hijos")
dev.off()
cumsum(hEdadH$density)*100

pdf("t14PromEdadH.pdf")
t14PromEdadH<-table(muestra$R_PromEdadH,muestra$CuotMora)
plot(t14PromEdadH, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Promedio de edad de los hijos del postulante")
dev.off()

pdf("t14_R_PromEdadH.pdf")
t14_R_PromEdadH<-table(muestra$R_PromEdadH,muestra$Riesgo)
plot(t14_R_PromEdadH, col = coloresCM,ylab ="Riesgo",main = "Promedio de edad de los hijos del postulante")
dev.off()

pdf("t14_D_PromEdadH.pdf")
t14_D_PromEdadH<-table(muestra$R_PromEdadH,muestra$Default)
plot(t14_D_PromEdadH, col = coloresCM,ylab ="Default",main = "Promedio de edad de los hijos del postulante")
dev.off()

#15Estrato###########################################################################################################################################
pdf("t15Estrato.pdf")
t15Estrato<-table(muestra$R_Estrato,muestra$CuotMora)
plot(t15Estrato, col=coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Estrato")
dev.off()

pdf("t15_R_Estrato.pdf")
t15_R_Estrato<-table(muestra$R_Estrato,muestra$Riesgo)
plot(t15_R_Estrato, col=coloresCM, ylab ="Riesgo",main = "Estrato")
dev.off()

pdf("t15_D_Estrato.pdf")
t15_D_Estrato<-table(muestra$R_Estrato,muestra$Default)
plot(t15_D_Estrato, col=coloresCM, ylab ="Default",main = "Estrato")
dev.off()

#16Tipo de vivienda##################################################################################################################################
pdf("t16TipViv.pdf")
t16TipViv<-table(muestra$TipViv,muestra$CuotMora)
plot(t16TipViv, col=coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Tipo de Vivienda")
dev.off()

pdf("t16_R_TipViv.pdf")
t16_R_TipViv<-table(muestra$TipViv,muestra$Riesgo)
plot(t16_R_TipViv, col=coloresCM, ylab ="Riesgo",main = "Tipo de Vivienda")
dev.off()

pdf("t16_D_TipViv.pdf")
t16_D_TipViv<-table(muestra$TipViv,muestra$Default)
plot(t16_D_TipViv, col=coloresCM, ylab ="Default",main = "Tipo de Vivienda")
dev.off()

#17Quien responde economicamente por el postulante?################################################################################################
pdf("t17RespEcnm.pdf")
t17RespEcnm<-table(muestra$RespEcnm,muestra$CuotMora)
plot(t17RespEcnm, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Quien responde economicamente por el postulante?")
dev.off()

pdf("t17_R_RespEcnm.pdf")
t17_R_RespEcnm<-table(muestra$RespEcnm,muestra$Riesgo)
plot(t17_R_RespEcnm, col = coloresCM, ylab ="Riesgo",main = "Quien responde economicamente por el postulante?")
dev.off()

pdf("t17_D_RespEcnm.pdf")
t17_D_RespEcnm<-table(muestra$RespEcnm,muestra$Default)
plot(t17_D_RespEcnm, col = coloresCM,ylab ="Default" ,main = "Quien responde economicamente por el postulante?")
dev.off()

#18Cantidad de personas dependientes del postulante################################################################################################
pdf("t18PersDep.pdf")
t18PersDep<-table(muestra$PersDep,muestra$CuotMora)
plot(t18PersDep, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Cantidad de personas dependientes del postulante")
dev.off()

pdf("t18_R_PersDep.pdf")
t18_R_PersDep<-table(muestra$PersDep,muestra$Riesgo)
plot(t18_R_PersDep, col = coloresCM, ylab ="Riesgo",main = "Cantidad de personas dependientes del postulante")
dev.off()

pdf("t18_D_PersDep.pdf")
t18_D_PersDep<-table(muestra$PersDep,muestra$Default)
plot(t18_D_PersDep, col = coloresCM, ylab ="Default",main = "Cantidad de personas dependientes del postulante")
dev.off()

#19Estado laboral del postulante ##################################################################################################################
pdf("t19EstLabPost.pdf")
t19EstLabPost<-table(muestra$EstLabPost,muestra$CuotMora)
plot(t19EstLabPost, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Estado laboral del postulante")
dev.off()

pdf("t19_R_EstLabPost.pdf")
t19_R_EstLabPost<-table(muestra$EstLabPost,muestra$Riesgo)
plot(t19_R_EstLabPost, col = coloresCM,ylab ="Riesgo",main = "Estado laboral del postulante")
dev.off()

pdf("t19_D_EstLabPost.pdf")
t19_D_EstLabPost<-table(muestra$EstLabPost,muestra$Default)
plot(t19_D_EstLabPost, col = coloresCM,ylab ="Default",main = "Estado laboral del postulante")
dev.off()

#La variable Estado academico no se tomara en cuenta por falta de claridad en la deficion de cada categoria###########################################

#IES#############################################################################################################################################
#tIES<-table(muestra$IES,muestra$CuotMora)
#plot(tIES, col = coloresCM,main = "Maximo cuotas en mora alcanzadas vs IES")

#20IES certificada en calidad #####################################################################################################################
pdf("t20AltaCalidad.pdf")
t20AltaCalidad<-table(muestra$AltaCalidad,muestra$CuotMora)
plot(t20AltaCalidad, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "IES certificada en Alta Calidad")
dev.off()

pdf("t20_R_AltaCalidad.pdf")
t20_R_AltaCalidad<-table(muestra$AltaCalidad,muestra$Riesgo)
plot(t20_R_AltaCalidad, col = coloresCM, ylab ="Riesgo",main = "IES certificada en Alta Calidad")
dev.off()

pdf("t20_D_AltaCalidad.pdf")
t20_D_AltaCalidad<-table(muestra$AltaCalidad,muestra$Default)
plot(t20_D_AltaCalidad, col = coloresCM, ylab ="Default",main = "IES certificada en Alta Calidad")
dev.off()

#21QSapiens IES######################################################################################################################################
pdf("t21QSapiens.pdf")
t21QSapiens<-table(muestra$Qsapiens,muestra$CuotMora)
plot(t21QSapiens, col = coloresCM, ylab = "Maximo cuotas en mora alcanzadas",main = " Q Sapiens")
dev.off()

pdf("t21_R_QSapiens.pdf")
t21_R_QSapiens<-table(muestra$Qsapiens,muestra$Riesgo)
plot(t21_R_QSapiens, col = coloresCM,ylab =  "Riesgo", main = "Q Sapiens")
dev.off()

pdf("t21_D_QSapiens.pdf")
t21_D_QSapiens<-table(muestra$Qsapiens,muestra$Default)
plot(t21_D_QSapiens, col = coloresCM,ylab ="Default" ,main = "Q Sapiens")
dev.off()

#22Nivel academico#################################################################################################################################
pdf("t22NivAcdm.pdf")
t22NivAcdm<-table(muestra$NivAcdm, muestra$CuotMora)
plot(t22NivAcdm, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Nivel academico")
dev.off()

pdf("t22_R_NivAcdm.pdf")
t22_R_NivAcdm<-table(muestra$NivAcdm, muestra$Riesgo)
plot(t22_R_NivAcdm, col = coloresCM, ylab ="Riesgo",main = "Nivel academico")
dev.off()

pdf("t22_D_NivAcdm.pdf")
t22_D_NivAcdm<-table(muestra$NivAcdm, muestra$Default)
plot(t22_D_NivAcdm, col = coloresCM, ylab ="Default",main = "Nivel academico")
dev.off()

#22Nivel formacion#################################################################################################################################
pdf("t22NivFrmc.pdf")
t22NivFrmc<-table(muestra$NivFrmc,muestra$CuotMora)
plot(t22NivFrmc, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Nivel formacion")
dev.off()

pdf("t22_R_NivFrmc.pdf")
t22_R_NivFrmc<-table(muestra$NivFrmc,muestra$Riesgo)
plot(t22_R_NivFrmc, col = coloresCM, ylab ="Riesgo",main = "Nivel formacion")
dev.off()

pdf("t22_D_NivFrmc.pdf")
t22_D_NivFrmc<-table(muestra$NivFrmc,muestra$Default)
plot(t22_D_NivFrmc, col = coloresCM, ylab ="Default",main = "Nivel formacion")
dev.off()

#Programa academico##############################################################################################################################
#t25PA<-table(muestra$PA,muestra$CuotMora)
#plot(t25PA, col = coloresCM,main = "Maximo cuotas en mora alcanzadas vs Programa academico")

#23Metodologia Programa Academico##################################################################################################################
pdf("t23Metdlg.pdf")
t23Metdlg<-table(muestra$Metdlg,muestra$CuotMora)
plot(t23Metdlg, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Metodologia del programa academico")
dev.off()

pdf("t23_R_Metdlg.pdf")
t23_R_Metdlg<-table(muestra$Metdlg,muestra$Riesgo)
plot(t23_R_Metdlg, col = coloresCM,ylab ="Riesgo",main = "Metodologia del programa academico")
dev.off()

pdf("t23_D_Metdlg.pdf")
t23_D_Metdlg<-table(muestra$Metdlg,muestra$Default)
plot(t23_D_Metdlg, col = coloresCM,ylab ="Default",main = "Metodologia del programa academico")
dev.off()

#24Area conocimiento###############################################################################################################################
pdf("t24AreaCncm.pdf")
t24AreaCncm<-table(muestra$AreaCncm,muestra$CuotMora)
plot(t24AreaCncm, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Area de conocimiento")
dev.off()

pdf("t24_R_AreaCncm.pdf")
t24_R_AreaCncm<-table(muestra$AreaCncm,muestra$Riesgo)
plot(t24_R_AreaCncm, col = coloresCM,ylab ="Riesgo",main = "Area de conocimiento")
dev.off()

pdf("t24_D_AreaCncm.pdf")
t24_D_AreaCncm<-table(muestra$AreaCncm,muestra$Default)
plot(t24_D_AreaCncm, col = coloresCM,ylab ="Default",main = "Area de conocimiento")
dev.off()

#25Periodos academicos#############################################################################################################################
table(muestra$PrdPA)
summary(muestra$PrdPA)
sd(muestra$PrdPA)
pdf("h28PrdPA.pdf")
hPrdPA<-hist(muestra$PrdPA, main = "Histograma: Periodos programa academico")
dev.off()
#la distribucion de los periodos academicos se encuentra entre 2 y 12 semestres, con una media de 7.95, mediana de 9 semestres y una desviacion 
#estandar de 2.96. Razon por la cual se grafica un histograma acumulado y se divide la muestra en 6 rangos 
pdf("hA28PrdPA.pdf")
hPrdPA$counts<- cumsum(hPrdPA$counts)
plot(hPrdPA, main = "Histograma acumulado: Periodos programa academicos")
dev.off()
cumsum(hPrdPA$density)*100

pdf("t25PrdPA.pdf")
t25PrdPA<-table(muestra$R_PrdPA,muestra$CuotMora)
plot(t25PrdPA, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Cantidad de periodos del programa academico")
dev.off()

pdf("t25_R_PrdPA.pdf")
t25_R_PrdPA<-table(muestra$R_PrdPA,muestra$Riesgo)
plot(t25_R_PrdPA, col = coloresCM,ylab ="Riesgo",main = "Cantidad de periodos del programa academico")
dev.off()

pdf("t25_D_PrdPA.pdf")
t25_D_PrdPA<-table(muestra$R_PrdPA,muestra$Default)
plot(t25_D_PrdPA, col = coloresCM,ylab ="Default",main = "Cantidad de periodos del programa academico")
dev.off()

#26Valor matricula semestral#######################################################################################################################
table(muestra$Mtrc)
#hay dos valores atipicos: 26.000.000 y 22.400.000 COP, se sabe porque esas universidades no son tan caras; revision en SF: sea justo a 2.600.0000
#la otra postulacion parece estar correcta porque el desembolso fue por 17.000.000 COP, la persona aun es estudiante
summary(muestra$Mtrc)
sd(muestra$Mtrc)
#la distribucion del valor de la matricula se encuentra entre 100.000 COP (100k) y 14.600.000, con una media de 5.092.000, mediana de 3.798.000
#y una desviacion  estandar de 2957841 Razon por la cual se grafica un histograma acumulado y se divide la muestra en 6 rangos 
pdf("h26Mtrc.pdf")
hMtrc<-hist(muestra$Mtrc, main = "Histograma: Valor matricula semestral")
dev.off()
pdf("hA26Mtrc.pdf")
hMtrc$counts<- cumsum(hMtrc$counts)
plot(hMtrc, main = "Histograma acumulado: Valor matricula semestral")
dev.off()
cumsum(hMtrc$density)*200000000

pdf("t26Mtrc.pdf")
t26Mtrc<-table(muestra$R_Mtrc,muestra$CuotMora)
plot(t26Mtrc,col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Valor matricula semestral")
dev.off()

pdf("t26_R_Mtrc.pdf")
t26_R_Mtrc<-table(muestra$R_Mtrc,muestra$Riesgo)
plot(t26_R_Mtrc,col = coloresCM, ylab ="Riesgo",main = "Valor matricula semestral")
dev.off()

pdf("t26_D_Mtrc.pdf")
t26_D_Mtrc<-table(muestra$R_Mtrc,muestra$Default)
plot(t26_D_Mtrc,col = coloresCM, ylab ="Default",main = "Valor matricula semestral")
dev.off()

#27Cultura de pago#################################################################################################################################
table(muestra$CultPag)
summary(muestra$CultPag)
sd(muestra$CultPag)
#la persona que no paga CP, paga cuotas desde la época dae estudio
#la distribucion de las CP se encuentra entre 0 COP y 300.000 COP, con una media de 30.500 COP, 
#mediana de 22.400 COP y una desviacion estandar de 31 .772 COP
#Razon por la cual se grafica un histograma acumulado y se divide en 4 rangos la muestra
pdf("h27CP.pdf")
hCP<-hist(muestra$CultPag, breaks=12, main = "Histograma: Cultura de pago")
dev.off()

pdf("t27CP.pdf")
t27CP<-table(muestra$R_CP,muestra$CuotMora)
plot(t27CP, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Cultura de pago")
dev.off()

pdf("t27_R_CP.pdf")
t27_R_CP<-table(muestra$R_CP,muestra$Riesgo)
plot(t27_R_CP, col = coloresCM,ylab ="Riesgo",main = "Cultura de pago")
dev.off()

pdf("t27_D_CP.pdf")
t27_D_CP<-table(muestra$R_CP,muestra$Default)
plot(t27_D_CP, col = coloresCM,ylab ="Default",main = "Cultura de pago")
dev.off()

#28Porcentaje salario comprometido#################################################################################################################
table(muestra$PrcntCuota)
summary(muestra$PrcntCuota)
sd(muestra$PrcntCuota)
#la distribucion del % de salario compromentido se encuentra entre 4.42% y 20%, con una media de 16.6%, mediana de 15%
#y una desviacion estandar de 3.088%. Razon por la cual se grafica un histograma acumulado y se divide en 3 rangos la muestra
pdf("h28PrcntCuota.pdf")
hPC<-hist(muestra$PrcntCuota, main = "Histograma: Porcentaje salario comprometido")
dev.off()
hPC$counts<- cumsum(hPC$counts)
pdf("hA28PrcntCuota.pdf")
plot(hPC, main = "Histograma acumulado: Porcentaje salario comprometido")
dev.off()
cumsum(hPC$density)*200

pdf("t28PrcntCuota.pdf")
t28PrcntCuota<-table(muestra$R_PC,muestra$CuotMora)
plot(t28PrcntCuota,col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Porcentaje salario comprometido")
dev.off()

pdf("t28_R_PrcntCuota.pdf")
t28_R_PrcntCuota<-table(muestra$R_PC,muestra$Riesgo)
plot(t28_R_PrcntCuota,col = coloresCM, ylab ="Riesgo",main = "Porcentaje salario comprometido")
dev.off()

pdf("t28_D_PrcntCuota.pdf")
t28_D_PrcntCuota<-table(muestra$R_PC,muestra$Default)
plot(t28_D_PrcntCuota,col = coloresCM, ylab ="Default",main = "Porcentaje salario comprometido")
dev.off()

#29Cuotas pactadas#################################################################################################################################
table(muestra$CuotPact)
summary(muestra$CuotPact)
sd(muestra$CuotPact)
#la distribucion de las cuotas pactadas se encuentra entre 16 y 99, con una media de 57.6, mediana de 57
#y una desviacion estandar de 17.77. Razon por la cual se grafica un histograma acumulado y se divide en 3 rangos la muestra
pdf("h29CuotPact.pdf")
hRP<-hist(muestra$CuotPact, breaks=6, main = "Histograma Cuotas pactadas")
dev.off()
pdf("hA29CuotPact.pdf")
hRP$counts<- cumsum(hRP$counts)
plot(hRP, main = "Histograma acumulado: Cuotas pactadas")
dev.off()
cumsum(hRP$density)*1000

pdf("t29CuotPact.pdf")
t29CuotPact<-table(muestra$R_RP,muestra$CuotMora)
plot(t29CuotPact, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Cuotas pactadas")
dev.off()

pdf("t29_R_CuotPact.pdf")
t29_R_CuotPact<-table(muestra$R_RP,muestra$Riesgo)
plot(t29_R_CuotPact, col = coloresCM, ylab ="Riesgo",main = "Cuotas pactadas")
dev.off()

pdf("t29_D_CuotPact.pdf")
t29_D_CuotPact<-table(muestra$R_RP,muestra$Default)
plot(t29_D_CuotPact, col = coloresCM,ylab = "Default",main = "Cuotas pactadas")
dev.off()

#30Puntaje prueba Logros-verbal###################################################################################################################
pdf("t30L_verb.pdf")
t30L_verb<-table(muestra$L_verb,muestra$CuotMora)
plot(t30L_verb, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba Logros-verbal")
dev.off()

pdf("t30_R_L_verb.pdf")
t30_R_L_verb<-table(muestra$L_verb,muestra$Riesgo)
plot(t30_R_L_verb, col = coloresCM,ylab ="Riesgo",main = "Puntaje prueba Logros-verbal")
dev.off()

pdf("t30_D_L_verb.pdf")
t30_D_L_verb<-table(muestra$L_verb,muestra$Default)
plot(t30_D_L_verb, col = coloresCM,ylab ="Default",main = "Puntaje prueba Logros-verbal")
dev.off()

pdf("t30L_verbR.pdf")
t30L_verbR<-table(muestra$R_LV,muestra$CuotMora)
plot(t30L_verbR, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba Logros-verbal")
dev.off()

pdf("t30_R_L_verbR.pdf")
t30_R_L_verbR<-table(muestra$R_LV,muestra$Riesgo)
plot(t30_R_L_verbR, col = coloresCM,ylab ="Riesgo",main = "Puntaje prueba Logros-verbal")
dev.off()

pdf("t30_D_L_verbR.pdf")
t30_D_L_verbR<-table(muestra$R_LV,muestra$Default)
plot(t30_D_L_verbR, col = coloresCM,ylab ="Default",main = "Puntaje prueba Logros-verbal")
dev.off()

#31Puntaje prueba Logros-espacial#################################################################################################################
pdf("t31L_esp.pdf")
t31L_esp<-table(muestra$L_esp,muestra$CuotMora)
plot(t31L_esp,col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba Logros-espacial")
dev.off()

pdf("t31_R_L_esp.pdf")
t31_R_L_esp<-table(muestra$L_esp,muestra$Riesgo)
plot(t31_R_L_esp,col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba Logros-espacial")
dev.off()

pdf("t31_D_L_esp.pdf")
t31_D_L_esp<-table(muestra$L_esp,muestra$Default)
plot(t31_D_L_esp,col = coloresCM, ylab ="Default",main = "Puntaje prueba Logros-espacial")
dev.off()

pdf("t31L_espR.pdf")
t31L_espR<-table(muestra$R_LE,muestra$CuotMora)
plot(t31L_espR,col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba Logros-espacial")
dev.off()

pdf("t31_R_L_espR.pdf")
t31_R_L_espR<-table(muestra$R_LE,muestra$Riesgo)
plot(t31_R_L_espR,col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba Logros-espacial")
dev.off()

pdf("t31_D_L_espR.pdf")
t31_D_L_espR<-table(muestra$R_LE,muestra$Default)
plot(t31_D_L_espR,col = coloresCM, ylab ="Default",main = "Puntaje prueba Logros-espacial")
dev.off()

#32Puntaje prueba Logros-numerico#################################################################################################################
table(muestra$L_num)
summary(muestra$L_num)
sd(muestra$L_num)
#la distribucion del Puntaje prueba Logros: numerico se encuentra entre 2 y 11 , con una media de 7.79, mediana de 8
#y una desviacion estandar de 2.1. Razon por la cual se grafica un histograma acumulado y se divide la muestra en 7 rangos 
pdf("h32L_num.pdf")
hLNum<-hist(muestra$L_num, main = "Histograma: Puntaje prueba Logros-numerico")
dev.off()

pdf("t32L_num.pdf")
t32L_num<-table(muestra$L_num,muestra$CuotMora)
plot(t32L_num, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba Logros-numerico")
dev.off()

pdf("t32_R_L_num.pdf")
t32_R_L_num<-table(muestra$L_num,muestra$Riesgo)
plot(t32_R_L_num, col = coloresCM,ylab ="Riesgo",main = "Puntaje prueba Logros-numerico")
dev.off()

pdf("t32_D_L_num.pdf")
t32_D_L_num<-table(muestra$L_num,muestra$Default)
plot(t32_D_L_num, col = coloresCM,ylab ="Default",main = "Puntaje prueba Logros-numerico")
dev.off()

pdf("t32L_numR.pdf")
t32L_numR<-table(muestra$R_LN,muestra$CuotMora)
plot(t32L_numR, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba Logros-numerico")
dev.off()

pdf("t32_R_L_numR.pdf")
t32_R_L_numR<-table(muestra$R_LN,muestra$Riesgo)
plot(t32_R_L_numR, col = coloresCM,ylab ="Riesgo",main = "Puntaje prueba Logros-numerico")
dev.off()

pdf("t32_D_L_numR.pdf")
t32_D_L_numR<-table(muestra$R_LN,muestra$Default)
plot(t32_D_L_numR, col = coloresCM,ylab ="Default",main = "Puntaje prueba Logros-numerico")
dev.off()

#33Puntaje prueba Logros sin verbal#############################################################################################################
table(muestra$L_sinV)
summary(muestra$L_sinV)
sd(muestra$L_sinV)
#la distribucion de la prueba de Logros numerico y espacial se encuentra entre 3 y 18, con una media de 11.5, mediana de 12
#y una desviacion estandar de 2.94. Razon por la cual se grafica un histograma acumulado y se divide en 5 rangos la muestra
pdf("h33L_sinV.pdf")
hL_V<-hist(muestra$L_sinV, breaks=10, main = "Histograma Logros numerico y espacial")
dev.off()
pdf("hA33L_sinV.pdf")
hL_V$counts<- cumsum(hL_V$counts)
plot(hL_V, main = "Histograma acumulado: Logros numerico y espacial")
dev.off()
cumsum(hL_V$density)*2000

pdf("t33L_sinV.pdf")
t33L_sinV<-table(muestra$L_sinV,muestra$CuotMora)
plot(t33L_sinV,col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba Logros numerico y espacial")
dev.off()

pdf("t33_R_L_sinV.pdf")
t33_R_L_sinV<-table(muestra$L_sinV,muestra$Riesgo)
plot(t33_R_L_sinV,col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba Logros numerico y espacial")
dev.off()

pdf("t33_D_L_sinV.pdf")
t33_D_L_sinV<-table(muestra$L_sinV,muestra$Default)
plot(t33_D_L_sinV,col = coloresCM, ylab ="Default",main = "Puntaje prueba Logros numerico y espacial")
dev.off()

pdf("t33L_sinV_R.pdf")
t33L_sinV_R<-table(muestra$R_LSin,muestra$CuotMora)
plot(t33L_sinV_R,col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba Logros numerico y espacial")
dev.off()

pdf("t33_R_L_sinV_R.pdf")
t33_R_L_sinV_R<-table(muestra$R_LSin,muestra$Riesgo)
plot(t33_R_L_sinV_R,col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba Logros numerico y espacial")
dev.off()

pdf("t33_D_L_sinV_R.pdf")
t33_D_L_sinV_R<-table(muestra$R_LSin,muestra$Default)
plot(t33_D_L_sinV_R,col = coloresCM,ylab ="Default",main = "Puntaje prueba Logros numerico y espacial")
dev.off()

#34Puntaje prueba Logros###########################################################################################################################
table(muestra$Logros)
summary(muestra$Logros)
sd(muestra$Logros)
#la distribucion del Puntaje prueba Logros se encuentra entre 8 y 28 , con una media de 18.9, mediana de 19
#y una desviacion estandar de 3.7. Razon por la cual se grafica un histograma acumulado y se divide la muestra en 6 rangos 
pdf("h34Logros.pdf")
hL<-hist(muestra$Logros, main = "Histograma: Puntaje prueba-Logros")
dev.off()
pdf("hA34Logros.pdf")
hL$counts<- cumsum(hL$counts)
plot(hL, main = "Histograma acumulado: Puntaje prueba-Logros")
dev.off()
cumsum(hL$density)*200

pdf("t34Logros.pdf")
t34Logros<-table(muestra$Logros,muestra$CuotMora)
plot(t34Logros, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba-Logros")
dev.off()

pdf("t34_R_Logros.pdf")
t34_Logros<-table(muestra$Logros,muestra$Riesgo)
plot(t34Logros, col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba-Logros")
dev.off()

pdf("t34_D_Logros.pdf")
t34Logros<-table(muestra$Logros,muestra$Default)
plot(t34Logros, col = coloresCM, ylab ="Default",main = "Puntaje prueba-Logros")
dev.off()

pdf("t34LogrosR.pdf")
t34LogrosR<-table(muestra$R_L,muestra$CuotMora)
plot(t34LogrosR, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba-Logros")
dev.off()

pdf("t34_R_LogrosR.pdf")
t34_LogrosR<-table(muestra$R_L,muestra$Riesgo)
plot(t34LogrosR, col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba-Logros")
dev.off()

pdf("t34_D_LogrosR.pdf")
t34LogrosR<-table(muestra$R_L,muestra$Default)
plot(t34LogrosR, col = coloresCM, ylab ="Default",main = "Puntaje prueba-Logros")
dev.off()

#35Puntaje prueba Personalidad-agradabilidad#######################################################################################################
table(muestra$P_agrd)
pdf("h35P_agrd.pdf")
hist(muestra$P_agrd, main = "Histograma: Puntaje prueba personalidad-agradabilidad")
dev.off()

pdf("t35P_agrd.pdf")
t35P_agrd<-table(muestra$P_agrd,muestra$CuotMora)
plot(t35P_agrd, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba personalidad-agradabilidad")
dev.off()

pdf("t35_R_P_agrd.pdf")
t35_R_P_agrd<-table(muestra$P_agrd,muestra$Riesgo)
plot(t35_R_P_agrd, col = coloresCM,ylab ="Riesgo",main = "Puntaje prueba personalidad-agradabilidad")
dev.off()

pdf("t35_D_P_agrd.pdf")
t35_D_P_agrd<-table(muestra$P_agrd,muestra$Default)
plot(t35_D_P_agrd, col = coloresCM,ylab ="Default",main = "Puntaje prueba personalidad-agradabilidad")
dev.off()

#36Puntaje prueba Personalidad-apertura experiencia################################################################################################
table(muestra$P_apertExp)
pdf("h36P_apertExp.pdf")
hist(muestra$P_apertExp, main = "Histograma: Puntaje prueba personalidad-apertura a la experiencia")
dev.off()

pdf("t36P_apertExp.pdf")
t36P_apertExp<-table(muestra$P_apertExp,muestra$CuotMora)
plot(t36P_apertExp,col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",
     main = "Puntaje prueba personalidad-apertura a la experiencia")
dev.off()

pdf("t36_R_P_apertExp.pdf")
t36_R_P_apertExp<-table(muestra$P_apertExp,muestra$Riesgo)
plot(t36_R_P_apertExp,col = coloresCM, ylab ="Riesgo",
     main = "Puntaje prueba personalidad-apertura a la experiencia")
dev.off()

pdf("t36_D_P_apertExp.pdf")
t36_D_P_apertExp<-table(muestra$P_apertExp,muestra$Default)
plot(t36_D_P_apertExp,col = coloresCM, ylab ="Default",
     main = "Puntaje prueba personalidad-apertura a la experiencia")
dev.off()

#37Puntaje prueba Personalidad-conciencia##########################################################################################################
table(muestra$P_conc)
pdf("h37P_conc.pdf")
hist(muestra$P_conc, main = "Histograma: Puntaje prueba personalidad-conciencia")
dev.off()

pdf("t37P_conc.pdf")
t37P_conc<-table(muestra$P_conc,muestra$CuotMora)
plot(t37P_conc, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba Personalidad-conciencia")
dev.off()

pdf("t37_R_P_conc.pdf")
t37_R_P_conc<-table(muestra$P_conc,muestra$Riesgo)
plot(t37_R_P_conc, col = coloresCM,ylab ="Riesgo",main = "Puntaje prueba Personalidad-conciencia")
dev.off()

pdf("t37_D_P_conc.pdf")
t37_D_P_conc<-table(muestra$P_conc,muestra$Default)
plot(t37_D_P_conc, col = coloresCM,ylab ="Default",main = "Puntaje prueba Personalidad-conciencia")
dev.off()

#38Puntaje prueba Personalidad-estabilidad emocional###############################################################################################
table(muestra$P_estEmc)
pdf("h38P_estEmc.pdf")
hist(muestra$P_estEmc, main = "Histograma: Puntaje prueba personalidad-estabilidad emocional")
dev.off()

pdf("t38P_estEmc.pdf")
t38P_estEmc<-table(muestra$P_estEmc,muestra$CuotMora)
plot(t38P_estEmc, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",
     main = "Puntaje prueba Personalidad-estabilidad emocional")
dev.off()

pdf("t38_R_P_estEmc.pdf")
t38_R_P_estEmc<-table(muestra$P_estEmc,muestra$Riesgo)
plot(t38_R_P_estEmc, col = coloresCM,ylab ="Riesgo",
     main = "Puntaje prueba Personalidad-estabilidad emocional")
dev.off()

pdf("t38_D_P_estEmc.pdf")
t38_D_P_estEmc<-table(muestra$P_estEmc,muestra$Default)
plot(t38_D_P_estEmc, col = coloresCM,ylab ="Default",
     main = "Puntaje prueba Personalidad-estabilidad emocional")
dev.off()

#39Puntaje prueba Personalidad-extraversion########################################################################################################
table(muestra$P_extrv)
pdf("h39P_extrv.pdf")
hist(muestra$P_extrv, main = "Histograma: Puntaje prueba personalidad-extraversion")
dev.off()

pdf("t39P_extrv.pdf")
t39P_extrv<-table(muestra$P_extrv,muestra$CuotMora)
plot(t39P_extrv, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba personalidad-extraversion")
dev.off()

pdf("t39_R_P_extrv.pdf")
t39_R_P_extrv<-table(muestra$P_extrv,muestra$Riesgo)
plot(t39_R_P_extrv, col = coloresCM,ylab ="Riesgo vs",main = "Puntaje prueba personalidad-extraversion")
dev.off()

pdf("t39_D_P_extrv.pdf")
t39_D_P_extrv<-table(muestra$P_extrv,muestra$Default)
plot(t39_D_P_extrv, col = coloresCM,ylab ="Default",main = " Puntaje prueba personalidad-extraversion")
dev.off()

#40Puntaje prueba Personalidad####################################################################################################################
pdf("t40pers.pdf")
t40pers<-table(muestra$Pers,muestra$CuotMora)
plot(t40pers, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba Personalidad")
dev.off()

pdf("t40_R_pers.pdf")
t40_R_pers<-table(muestra$Pers,muestra$Riesgo)
plot(t40_R_pers, col = coloresCM,ylab ="Riesgo",main = "Puntaje prueba Personalidad")
dev.off()

pdf("t40_D_pers.pdf")
t40_D_pers<-table(muestra$Pers,muestra$Default)
plot(t40_D_pers, col = coloresCM,ylab ="Default",main = "Puntaje prueba Personalidad")
dev.off()

pdf("t40persR.pdf")
t40persR<-table(muestra$R_P,muestra$CuotMora)
plot(t40persR, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba Personalidad")
dev.off()

pdf("t40_R_persR.pdf")
t40_R_persR<-table(muestra$R_P,muestra$Riesgo)
plot(t40_R_persR, col = coloresCM,ylab ="Riesgo",main = "Puntaje prueba Personalidad")
dev.off()

pdf("t40_D_persR.pdf")
t40_D_persR<-table(muestra$R_P,muestra$Default)
plot(t40_D_persR, col = coloresCM,ylab ="Default",main = "Puntaje prueba Personalidad")
dev.off()

#41Puntaje prueba intereses-ciencias economicas####################################################################################################
pdf("t41I_cEcnm.pdf")
t41I_cEcnm<-table(muestra$I_cEcnm,muestra$CuotMora)
plot(t41I_cEcnm, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba intereses-ciencias economicas")
dev.off()

pdf("t41_R_I_cEcnm.pdf")
t41_R_I_cEcnm<-table(muestra$I_cEcnm,muestra$Riesgo)
plot(t41_R_I_cEcnm, col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba intereses-ciencias economicas")
dev.off()

pdf("t41_D_I_cEcnm.pdf")
t41_D_I_cEcnm<-table(muestra$I_cEcnm,muestra$Default)
plot(t41_D_I_cEcnm, col = coloresCM, ylab ="Default",main = "Puntaje prueba intereses-ciencias economicas")
dev.off()

#42Puntaje prueba intereses-ingenierias y arquitectura####################################################################################################
pdf("t42I_ingArq.pdf")
t42I_ingArq<-table(muestra$I_ingArq,muestra$CuotMora)
plot(t42I_ingArq, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba intereses-ingenierias y arquitectura")
dev.off()

pdf("t42_R_I_ingArq.pdf")
t42_R_I_ingArq<-table(muestra$I_ingArq,muestra$Riesgo)
plot(t42_R_I_ingArq, col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba intereses-ingenierias y arquitectura")
dev.off()

pdf("t42_D_I_ingArq.pdf")
t42_D_I_ingArq<-table(muestra$I_ingArq,muestra$Default)
plot(t42_D_I_ingArq, col = coloresCM, ylab ="Default",main = "Puntaje prueba intereses-ingenierias y arquitectura")
dev.off()

#43Puntaje prueba intereses-bellas artes###########################################################################################################
pdf("t43I_bellArt.pdf")
t43I_bellArt<-table(muestra$I_bellArt,muestra$CuotMora)
plot(t43I_bellArt, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba intereses-bellas artes")
dev.off()

pdf("t43_R_I_bellArt.pdf")
t43_R_I_bellArt<-table(muestra$I_bellArt,muestra$Riesgo)
plot(t43_R_I_bellArt, col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba intereses-bellas artes")
dev.off()

pdf("t43_D_I_bellArt.pdf")
t43_D_I_bellArt<-table(muestra$I_bellArt,muestra$Default)
plot(t43_D_I_bellArt, col = coloresCM, ylab ="Default",main = "Puntaje prueba intereses-bellas artes")
dev.off()

#44Puntaje prueba intereses:ciencias de la salud###################################################################################################
pdf("t44I_cSalud.pdf")
t44I_cSalud<-table(muestra$I_cSalud,muestra$CuotMora)
plot(t44I_cSalud, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba intereses-ciencias de la salud")
dev.off()

pdf("t44_R_I_cSalud.pdf")
t44_R_I_cSalud<-table(muestra$I_cSalud,muestra$Riesgo)
plot(t44_R_I_cSalud, col = coloresCM,ylab ="Riesgo" ,main = "Puntaje prueba intereses-ciencias de la salud")
dev.off()

pdf("t44_D_I_cSalud.pdf")
t44_D_I_cSalud.pdf<-table(muestra$I_cSalud,muestra$Default)
plot(t44_D_I_cSalud.pdf, col = coloresCM, ylab ="Default",main = "Puntaje prueba intereses-ciencias de la salud")
dev.off()

#45Puntaje prueba intereses-ciencias sociales######################################################################################################
pdf("t45I_cSoc.pdf")
t45I_cSoc.pdf<-table(muestra$I_cSoc,muestra$CuotMora)
plot(t45I_cSoc.pdf, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba intereses-ciencias sociales")
dev.off()

pdf("t45_R_I_cSoc.pdf")
t45_R_I_cSoc<-table(muestra$I_cSoc,muestra$Riesgo)
plot(t45_R_I_cSoc, col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba intereses-ciencias sociales")
dev.off()

pdf("t45_D_I_cSoc.pdf")
t45_D_I_cSoc<-table(muestra$I_cSoc,muestra$Default)
plot(t45_D_I_cSoc, col = coloresCM, ylab ="Default",main = "Puntaje prueba intereses-ciencias sociales")
dev.off()

#46Puntaje prueba intereses:tecnicas y tecnologicas################################################################################################
pdf("t46I_tyt.pdf")
t46I_tyt.pdf<-table(muestra$I_tyt,muestra$CuotMora)
plot(t46I_tyt.pdf, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba intereses-tecnicas y tecnologicas")
dev.off()

pdf("t46_R_I_tyt.pdf.pdf")
t46_R_I_tyt<-table(muestra$I_tyt,muestra$Riesgo)
plot(t46_R_I_tyt, col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba intereses-tecnicas y tecnologicas")
dev.off()

pdf("t46_D_I_tyt.pdf")
t46_D_I_tyt<-table(muestra$I_tyt,muestra$Default)
plot(t46_D_I_tyt, col = coloresCM, ylab ="Default",main = "Puntaje prueba intereses-tecnicas y tecnologicas")
dev.off()

#47Puntaje prueba intereses:ciencias naturales####################################################################################################
pdf("t47I_matCNat.pdf")
t47I_matCNat<-table(muestra$I_matCNat,muestra$CuotMora)
plot(t47I_matCNat, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba intereses-ciencias naturales")
dev.off()

pdf("t47_R_I_matCNat.pdf")
t47_R_I_matCNat<-table(muestra$I_matCNat,muestra$Riesgo)
plot(t47_R_I_matCNat, col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba intereses-ciencias naturales")
dev.off()

pdf("t47_D_I_matCNat.pdf")
t47_D_I_matCNat<-table(muestra$I_matCNat,muestra$Default)
plot(t47_D_I_matCNat, col = coloresCM,ylab = "Default",main = "Puntaje prueba intereses-ciencias naturales")
dev.off()

#48Puntaje prueba intereses########################################################################################################################
table(muestra$Intereses)
pdf("h48Intereses.pdf")
hist(muestra$Intereses,breaks = 20, main = "Histograma: Puntaje prueba intereses")
dev.off()
table(muestra$R_I)
#t54intereses<-table(muestra$Intereses,muestra$CuotMora)
#plot(t54intereses, col = coloresCM, main = "Maximo cuotas en mora alcanzadas vs Puntaje prueba intereses")

pdf("t48Intereses.pdf")
t48Intereses<-table(muestra$Intereses,muestra$CuotMora)
plot(t48Intereses, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba intereses")
dev.off()

pdf("t48_R_Intereses.pdf")
t48_R_Intereses<-table(muestra$Intereses,muestra$Riesgo)
plot(t48_R_Intereses, col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba intereses")
dev.off()

pdf("t48_D_Intereses.pdf")
t48_D_Intereses<-table(muestra$Intereses,muestra$Default)
plot(t48_D_Intereses, col = coloresCM, ylab ="Default",main = "Puntaje prueba intereses")
dev.off()

pdf("t48InteresesR.pdf")
t48Intereses_R<-table(muestra$R_I,muestra$CuotMora)
plot(t48Intereses_R, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba intereses")
dev.off()

pdf("t48_R_InteresesR.pdf")
t48_R_Intereses_R<-table(muestra$R_I,muestra$Riesgo)
plot(t48_R_Intereses_R, col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba intereses")
dev.off()

pdf("t48_D_InteresesR.pdf")
t48_D_Intereses_R<-table(muestra$R_I,muestra$Default)
plot(t48_D_Intereses_R, col = coloresCM, ylab ="Default",main = "Puntaje prueba intereses")
dev.off()

#49Puntaje prueba seleccion#########################################################################################################################
table(muestra$PrbSel)
pdf("h49prueba.pdf")
hist(muestra$PrbSel,breaks = 20, main = "Histograma: Puntaje prueba de seleccion")
dev.off()

pdf("t49PrbSel.pdf")
t49PrbSel<-table(muestra$PrbSel,muestra$CuotMora)
plot(t49PrbSel, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba de seleccion")
dev.off()

pdf("t49_R_PrbSel.pdf")
t49_R_PrbSel<-table(muestra$PrbSel,muestra$Riesgo)
plot(t49_R_PrbSel, col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba de seleccion")
dev.off()

pdf("t49_D_PrbSel.pdf")
t49_D_PrbSel<-table(muestra$PrbSel,muestra$Default)
plot(t49_D_PrbSel, col = coloresCM, ylab ="Default",main = "Puntaje prueba de seleccion")
dev.off()

pdf("h49PrbSelR.pdf")
h49PrbSelR<-table(muestra$R_PrbSel,muestra$CuotMora)
plot(h49PrbSelR, col = coloresCM, ylab="Maximo cuotas en mora alcanzadas",main = "Puntaje prueba de seleccion")
dev.off()

pdf("h49_R_PrbSelR.pdf")
h49_R_PrbSelR<-table(muestra$R_PrbSel,muestra$Riesgo)
plot(h49_R_PrbSelR, col = coloresCM, ylab ="Riesgo",main = "Puntaje prueba de seleccion")
dev.off()

pdf("h49_D_PrbSelR.pdf")
h49_D_PrbSelR<-table(muestra$R_PrbSel,muestra$Default)
plot(h49_D_PrbSelR, col = coloresCM, ylab ="Default",main = "Puntaje prueba de seleccion")
dev.off()

#50Ingresos/Egresos################################################################################################################################
pdf("t50Ing_Egr.pdf")
t50Ing_Egr<-table(muestra$Ing_Egr,muestra$CuotMora)
plot(t50Ing_Egr, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Ingresos/Egresos")
dev.off()

pdf("t50_R_Ing_Egr.pdf")
t50_R_Ing_Egr<-table(muestra$Ing_Egr,muestra$Riesgo)
plot(t50_R_Ing_Egr, col = coloresCM, ylab ="Riesgo",main = "Ingresos/Egresos")
dev.off()

pdf("t50_D_Ing_Egr.pdf")
t50_D_Ing_Egr<-table(muestra$Ing_Egr,muestra$Default)
plot(t50_D_Ing_Egr, col = coloresCM, ylab ="Default",main = "Ingresos/Egresos")
dev.off()

#51Cuenta ahorros##################################################################################################################################
pdf("t51CtaAh.pdf")
t51CtaAh<-table(muestra$CtaAh,muestra$CuotMora)
  plot(t51CtaAh, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Tener cuenta de ahorros")
dev.off()

pdf("t51_R_CtaAh.pdf")
t51_R_CtaAh<-table(muestra$CtaAh,muestra$Riesgo)
plot(t51_R_CtaAh, col = coloresCM, ylab ="Riesgo",main = "Tener cuenta de ahorros")
dev.off()

pdf("t51_D_CtaAh.pdf")
t51_D_CtaAh<-table(muestra$CtaAh,muestra$Default)
plot(t51_D_CtaAh, col = coloresCM, ylab ="Default",main = "Tener cuenta de ahorros")
dev.off()

#52Cuenta corriente################################################################################################################################
pdf("t52CtaCte.pdf")
t52CtaCte<-table(muestra$CtaCte,muestra$CuotMora)
plot(t52CtaCte, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Tener cuenta corriente")
dev.off()

pdf("t52_R_CtaCte.pdf")
t52_R_CtaCte<-table(muestra$CtaCte,muestra$Riesgo)
plot(t52_R_CtaCte, col = coloresCM, ylab ="Riesgo",main = "Tener cuenta corriente")
dev.off()

pdf("t52_D_CtaCte.pdf")
t52_D_CtaCte<-table(muestra$CtaCte,muestra$Default)
plot(t52_D_CtaCte, col = coloresCM, ylab ="Default",main = "Tener cuenta corriente")
dev.off()

#53Tarjeta de credito##############################################################################################################################
pdf("t53TarjCred.pdf")
t53TarjCred<-table(muestra$TarjCred,muestra$CuotMora)
plot(t53TarjCred, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Tener tarjeta de credito")
dev.off()

pdf("t53_R_TarjCred.pdf")
t53_R_TarjCred<-table(muestra$TarjCred,muestra$Riesgo)
plot(t53_R_TarjCred, col = coloresCM,ylab ="Riesgo" ,main = "Tener tarjeta de credito")
dev.off()

pdf("t53_D_TarjCred.pdf")
t53_D_TarjCred<-table(muestra$TarjCred,muestra$Default)
plot(t53_D_TarjCred, col = coloresCM,ylab ="Default",main = "Tener tarjeta de credito")
dev.off()

#54creditos#######################################################################################################################################
pdf("t54Cred.pdf")
t54Cred<-table(muestra$Creditos,muestra$CuotMora)
plot(t54Cred, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Tener creditos")
dev.off()

pdf("t54_R_Cred.pdf")
t54_R_Cred<-table(muestra$Creditos,muestra$Riesgo)
plot(t54_R_Cred, col = coloresCM, ylab ="Riesgo",main = "Tener creditos")
dev.off()

pdf("t54_D_Cred.pdf")
t54_D_Cred<-table(muestra$Creditos,muestra$Default)
plot(t54_D_Cred, col = coloresCM, ylab ="Default",main = "Tener creditos")
dev.off()

#55Productos Bancarios##############################################################################################################################
pdf("t55PtsBanc.pdf")
t55PtsBanc<-table(muestra$Pts_Banc,muestra$CuotMora)
plot(t55PtsBanc, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Productos financieros")
dev.off()

pdf("t55_R_PtsBanc.pdf")
t55_R_PtsBanc<-table(muestra$Pts_Banc,muestra$Riesgo)
plot(t55_R_PtsBanc, col = coloresCM, ylab ="Riesgo",main = "Productos financieros")
dev.off()

pdf("t55_D_PtsBanc.pdf")
t55_D_PtsBanc<-table(muestra$Pts_Banc,muestra$Default)
plot(t55_D_PtsBanc, col = coloresCM, ylab ="Default",main = "Productos financieros")
dev.off()

#56Vivienda########################################################################################################################################
pdf("t56B_viv.pdf")
t56B_viv<-table(muestra$B_viv,muestra$CuotMora)
plot(t56B_viv, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Poseer bienes: vivienda")
dev.off()

pdf("t56_R_B_viv.pdf")
t56_R_B_viv<-table(muestra$B_viv,muestra$Riesgo)
plot(t56_R_B_viv, col = coloresCM, ylab ="Riesgo",main = "Poseer bienes: vivienda")
dev.off()

pdf("t56_D_B_viv.pdf")
t56_D_B_viv<-table(muestra$B_viv,muestra$Default)
plot(t56_D_B_viv, col = coloresCM, ylab ="Default",main = "Poseer bienes: vivienda")
dev.off()

#57Computador#######################################################################################################################################
pdf("t57B_comp.pdf")
t57B_comp<-table(muestra$B_comp,muestra$CuotMora)
plot(t57B_comp, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Poseer bienes: computador")
dev.off()

pdf("t57_R_B_comp.pdf")
t57_R_B_comp<-table(muestra$B_comp,muestra$Riesgo)
plot(t657_R_B_comp, col = coloresCM, ylab ="Riesgo alcanzadas",main = "Poseer bienes: computador")
dev.off()

pdf("t57_D_B_comp.pdf")
t57_D_B_comp<-table(muestra$B_comp,muestra$Default)
plot(t57_D_B_comp, col = coloresCM,ylab ="Default" ,main = "Poseer bienes: computador")
dev.off()

#58Celular##########################################################################################################################################
pdf("t58B_cel.pdf")
t58B_cel<-table(muestra$B_cel,muestra$CuotMora)
plot(t58B_cel, col = coloresCM,ylab ="Maximo cuotas en mora alcanzadas",main = "Poseer bienes: celular")
dev.off()

pdf("t58_R_B_cel.pdf")
t58_R_B_cel<-table(muestra$B_cel,muestra$Riesgo)
plot(t58_R_B_cel, col = coloresCM, ylab ="Riesgo",main = "Poseer bienes: celular")
dev.off()

pdf("t58_D_B_cel.pdf")
t58_D_B_cel<-table(muestra$B_cel,muestra$Default)
plot(t58_D_B_cel, col = coloresCM, ylab ="Default",main = "Poseer bienes: celular")
dev.off()

#59Moto#############################################################################################################################################
pdf("t59B_moto.pdf")
t59B_moto<-table(muestra$B_moto,muestra$CuotMora)
plot(t59B_moto, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Poseer bienes: moto")
dev.off()

pdf("t59_R_B_moto.pdf")
t59_R_B_moto<-table(muestra$B_moto,muestra$Riesgo)
plot(t59_R_B_moto, col = coloresCM, ylab ="Riesgo",main = "Poseer bienes: moto")
dev.off()

pdf("t59_D_B_moto.pdf")
t59_D_B_moto<-table(muestra$B_moto,muestra$Default)
plot(t59_D_B_moto, col = coloresCM, ylab ="Default",main = "Poseer bienes: moto")
dev.off()

#60Carro############################################################################################################################################
pdf("t60B_carro.pdf")
t60B_carro<-table(muestra$B_carro,muestra$CuotMora)
plot(t60B_carro, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Poseer bienes: carro")
dev.off()

pdf("t60_R_B_carro.pdf")
t60_R_B_carro<-table(muestra$B_carro,muestra$Riesgo)
plot(t60_R_B_carro, col = coloresCM, ylab ="Riesgo",main = "Poseer bienes: carro")
dev.off()

pdf("t60_D_B_carro.pdf")
t60_D_B_carro<-table(muestra$B_carro,muestra$Default)
plot(t60_D_B_carro, col = coloresCM, ylab ="Default",main = "Poseer bienes: carro")
dev.off()

#61Bienes Ponderados################################################################################################################################
table(muestra$B_pond)
summary(muestra$B_pond)
sd(muestra$B_pond)
#la distribucion de la ponderacion de bienes se encuentra entre 0 y 5, con una media de 2.4, mediana de 2.36
#y una desviacion estandar de 1.28. Razon por la cual se grafica un histograma acumulado y se divide en 5 rangos la muestra
pdf("h61B_pond.pdf")
hB<-hist(muestra$B_pond, breaks=7, main = "Histograma: Bienes ponderados")
dev.off()
pdf("hA61B_pond.pdf")
hB$counts<- cumsum(hB$counts)
plot(hB, main = "Histograma acumulado: Bienes ponderados")
dev.off()
cumsum(hB$density)*100

pdf("t61B_pond.pdf")
t61B_pond<-table(muestra$B_pond,muestra$CuotMora)
plot(t61B_pond, col = coloresCM, ylab ="Maximo cuotas en mora alcanzadas",main = "Bienes ponderados")
dev.off()

pdf("t61_R_B_pond.pdf")
t61_R_B_pond<-table(muestra$B_pond,muestra$Riesgo)
plot(t61_R_B_pond, col = coloresCM, ylab ="Riesgo",main = "Bienes ponderados")
dev.off()

pdf("t61_D_B_pond.pdf")
t61_D_B_pond<-table(muestra$B_pond,muestra$Default)
plot(t61_D_B_pond, col = coloresCM, ylab ="Default",main = "Bienes ponderados")
dev.off()

pdf("t61B_pondR.pdf")
t61B_pondR<-table(muestra$R_B_pond,muestra$CuotMora)
plot(t61B_pondR, col = coloresCM, ylab =main = "Maximo cuotas en mora alcanzadas",main = "Bienes ponderados")
dev.off()

pdf("t61_R_B_pondR.pdf")
t61_R_B_pondR<-table(muestra$R_B_pond,muestra$Riesgo)
plot(t61_R_B_pondR, col = coloresCM, ylab ="Riesgo",main = "Bienes ponderados")
dev.off()

pdf("t61_D_B_pondR.pdf")
t61_D_B_pondR<-table(muestra$R_B_pond,muestra$Default)
plot(t61_D_B_pondR, col = coloresCM, ylab  ="Default",main = "Bienes ponderados")
dev.off()

#depurar base final para la construccion del modelo###################################################################################################
names(base)
DM_DB_Debugged<-base[,-c(12,14:16,26:27,32,35,37)]
names(DM_DB_Debugged)
DM_DB_Debugged<-DM_DB_Debugged[,c(1:11, 68, 12:21,70,71,22:67,69,72:84)]
write.csv(DM_DB_Debugged,paste('DM_DB_Debugged.csv',sep=""))

#matriz correlacion"########################################################################################################################
vars<-names(DM_DB_Debugged[,10:84])
dat<-DM_DB_Debugged
catcorrm <- function(vars, dat) sapply(vars, function(y) sapply(vars, function(x) assocstats(table(dat[,x], dat[,y]))$cramer))
matCor<-catcorrm(vars,dat)
View(matCor)
write.csv(matCor,paste('matCor.csv',sep=""))

#grafica matriz de correlacion entre predictores
predictors<-DM_DB_Debugged[,10:86]
gp = ggpairs(predictors, lower = list(continuous = "smooth")) 

#variables correlacionadas >=0.6############################################################################################################
altCor6<-data.frame()
for (i in 1:nrow(matCor)) {
  a<-data.frame("variable"=colnames(matCor)[i],"cor"=matCor[i,])
  a<-a[a$cor>0.6,]
  a$xVariable<-rownames(a)
  altCor6<-rbind(altCor6,a)
}
freCor6<-count(altCor6$variable)
freCor6<-freCor6[order(freCor6$freq,decreasing = TRUE),]
freVar6<-count(freCor6$freq)
freVar6<-freVar6[order(freVar6$x,decreasing = TRUE),]
View(altCor6)
View(freCor6)
View(freVar6)

#variables correlacionadas >=0.7############################################################################################################
altCor7<-data.frame()
for (i in 1:nrow(matCor)) {
  a<-data.frame("variable"=colnames(matCor)[i],"cor"=matCor[i,])
  a<-a[a$cor>0.7,]
  a$xVariable<-rownames(a)
  altCor7<-rbind(altCor7,a)
}
freCor7<-count(altCor7$variable)
freCor7<-freCor7[order(freCor7$freq,decreasing = TRUE),]
freVar7<-count(freCor7$freq)
freVar7<-freVar7[order(freVar7$x,decreasing = TRUE),]
View(altCor7)
View(freCor7)
View(freVar7)

#variables correlacionadas >=0.8############################################################################################################
altCor8<-data.frame()
for (i in 1:nrow(matCor)) {
  a<-data.frame("variable"=colnames(matCor)[i],"cor"=matCor[i,])
  a<-a[a$cor>0.8,]
  a$xVariable<-rownames(a)
  altCor8<-rbind(altCor8,a)
}
freCor8<-count(altCor8$variable)
freCor8<-freCor8[order(freCor8$freq,decreasing = TRUE),]
freVar8<-count(freCor8$freq)
freVar8<-freVar8[order(freVar8$x,decreasing = TRUE),]
View(altCor8)
View(freCor8)
View(freVar8)

#variables correlacionadas >=0.9############################################################################################################
altCor9<-data.frame()
for (i in 1:nrow(matCor)) {
  a<-data.frame("variable"=colnames(matCor)[i],"cor"=matCor[i,])
  a<-a[a$cor>0.9,]
  a$xVariable<-rownames(a)
  altCor9<-rbind(altCor9,a)
}
freCor9<-count(altCor9$variable)
freCor9<-freCor9[order(freCor9$freq,decreasing = TRUE),]
freVar9<-count(freCor9$freq)
freVar9<-freVar9[order(freVar9$x,decreasing = TRUE),]
View(altCor9)
View(freCor9)
View(freVar9)


#construir y guardar base depurada para "train" y "test"####################################################################################
sample<-DM_DB_Debugged[DM_DB_Debugged$TipBase=="Muestra",]
#sample$CuotMora<-as.numeric(sample$CuotMora)
sample$scoring<-scoring$Scoring[match(sample$Id,scoring$Id)]
sample$mntTotal<-mntTotal$mntTotal[match(sample$Id,mntTotal$Id)]
names(sample)
nrow(sample)
ncol(sample)
write.csv(sample,paste('sample.csv',sep=""))

#base con variables numericas originales
sampleO<-sample[,1:70]
names(sampleO)
#sampleO$CuotMora<-as.numeric(sampleO$CuotMora)
sampleO$scoring<-scoring$Scoring[match(sampleO$Id,scoring$Id)]
sampleO$mntTotal<-mntTotal$mntTotal[match(sampleO$Id,mntTotal$Id)]
write.csv(sampleO,paste('sampleO.csv',sep=""))

#base con variables numericas en rangos
sampleR<-sample[,-c(15,34:43,57,58,70)];
names(sampleR)
sampleR<-sampleR[,c(1:14,57,15:32,58:63,65,64,66,67,33:45,68,69,46:56,70)];
#sampleR$CuotMora<-as.numeric(sampleR$CuotMora)
sampleR$scoring<-scoring$Scoring[match(sampleR$Id,scoring$Id)]
sampleR$mntTotal<-mntTotal$mntTotal[match(sampleR$Id,mntTotal$Id)]
write.csv(sampleR,paste('sampleR.csv',sep=""))

#construir y guardar base depurada para experiemento "en vivo"##############################################################################
experiment<-DM_DB_Debugged[DM_DB_Debugged$TipBase=="Experimiento",]
write.csv(experiment,paste('experiment.csv',sep=""))

#base con variables numericas originales
experimentO<-experiment[,1:70]
write.csv(experimentO,paste('experimentO.csv',sep=""))

#base con variables numericas en rangos
experimentR<-experiment[,-c(15,34:43,57,58,70)];
names(experimentR)
experimentR<-sampleR[,c(1:14,57,15:32,58:63,65,64,66,67,33:45,68,69,46:56,70)];
write.csv(experimentR,paste('experimentR.csv',sep=""))
