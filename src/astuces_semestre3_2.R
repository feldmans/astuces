#astuces_semestre3
library(stringr)

idpw <- readRDS("data/idpw.rds")

#drop columns

iris %>% select(-one_of(drop.cols))
df <- subset(df, select = -c(a,c) )
drops <- c("x","z")
DF[ , !(names(DF) %in% drops)]


#data mining dans des fichiers 

v<-"spo2" #expression recherchée

#fichier dans lesquels on recherche l'expression
fic<-"données/pat/PATIENT.sas"
x<-scan(fic, what=as.character(), sep="\n")
i<-grep(v, tolower(x));x[i]

fic<-"données/pre/PATIENT.sas"
x<-scan(fic, what=as.character(), sep="\n")
i<-grep(v, tolower(x));x[i]

fic<-"données/trt/PATIENT.sas"
x<-scan(fic, what=as.character(), sep="\n")
i<-grep(v, tolower(x));x[i]



#delete non used level
mydf <- droplevels(mydf)




#Recherche de doublons: attention aux problèmes de facteurs

table(tabidpw<-table(idpw$PATIENT))
#non!
idpw[tabidpw>1,] #oui = tabidpwb[tabidpwb>1]
#non!
idpwb <- droplevels(idpw)
table(tabidpwb<-table(idpwb$PATIENT))
idpwb[tabidpwb>1,] # c("ALARZA_DANIELLE","GAGNEUX_NATHALIE","GALLARD_JEAN CLAUDE","RANTY_CLAUDE") 

#oui!
names(tabidpwb[tabidpwb>1])
#ou
idpw[duplicated(idpw$PATIENT),] #quls sont les patients dupliqués
names_dbl <- c("ALARZA_DANIELLE", "GAGNIERE_MARIE CLAUDE", "GAMBIER_JANINE", "RATTINASSAMY_MARIE")
idpwb[idpwb$PATIENT %in% names_dbl,] #ce sont les vrais doublons!




#pas besoin de réécrire les éléments d'un vecteur séparé par des virgules...
names_dbl <- names(tabidpw[tabidpw>1])
names_dbl <- c("ALARZA_DANIELLE","GAGNEUX_NATHALIE","GALLARD_JEAN CLAUDE","RANTY_CLAUDE") 
#tester ici:
idpwb[idpwb$PATIENT %in% names_dbl,]




#Gérer les dates ND en partie
 
DATE_DCD <- readRDS("F:/to push/astuces/data/manage_date_DC.rds")
dcd <- data.frame(DATE_DCD)

manage_date_ND <- function(vec){ #vec doit être un vecteur avec éléments de la forme 04/04/1989 ou "04/04/1989"
  vec <- as.character(vec)
  exist_year <-!is.na(str_sub(vec, 7, 10))
  vec[exist_year] <- gsub("ND/ND", "01/07",vec,fixed=T)
  vec[exist_year] <- gsub("ND", "15",vec,fixed=T)
  vec_d <- as.Date(vec,"%d/%m/%Y")
  return(vec_d)
}

# Je remplace ND jour et mois par 01/07 (pas de cas ici):
dcd$date_dcd <- as.character(dcd$DATE_DCD)
exist_year <-!is.na(str_sub(dcd$date_dcd, 7, 10))
dcd$date_dcd[exist_year] <- gsub("ND/ND", "01/07",dcd$date_dcd,fixed=T)
# Puis je remplace ND jour uniquement par 15 :
dcd$date_dcd[exist_year] <- gsub("ND", "15",dcd$date_dcd,fixed=T)
#vérif:
dcd$date_dcd[str_sub(dcd$date_dcd, 1, 2)=="ND"]
dcd$date_dcd <- as.Date(dcd$date_dcd,"%d/%m/%Y")
table(!is.na(dcd$date_dcd[dcd$FORM != "ID"])) #NB : nous indique 0 NA tant que ce n'est pas passé en date car "" est compté comme une valeure

dcd$date_dcdb <- dcd$date_dcd

dcd$date_dcd <- manage_date_ND(dcd$DATE_DCD)

table(!is.na(dcd$date_dcd[dcd$FORM != "ID"]))
table(!is.na(dcd$date_dcdb[dcd$FORM != "ID"]))
identical(dcd$date_dcd,dcd$date_dcdb)


# # Je remplace ND pour jour uniquement par 15
# dcd$date_dcd <- as.character(dcd$DATE_DCD)
# dates <- dcd$date_dcd[str_sub(dcd$date_dcd, 1, 2)=="ND" & str_sub(dcd$date_dcd, 4, 5)!="ND"]
# dcd$date_dcd <- ifelse(dcd$date_dcd %in% dates, gsub("ND", "15",dcd$date_dcd), dcd$date_dcd)
# #dcd$date_dcd[dcd$date_dcd %in% dates] <- gsub("ND", "15",dcd$date_dcd[dcd$date_dcd %in% dates]) #plus rapide?
# dcd$date_dcd <- as.Date(dcd$date_dcd,"%d/%m/%Y")
# 
# # Je remplace ND jour et mois par 01/07 (pas de cas ici)
# dcd$date_dcd <- as.character(dcd$date_dcd)
# dates <- dcd$date_dcd[str_sub(dcd$date_dcd, 1, 2)=="ND" & str_sub(dcd$date_dcd, 4, 5)=="ND"]
# dcd$date_dcd <- ifelse(dcd$date_dcd %in% dates, gsub("ND/ND", "01/07",dcd$date_dcd), dcd$date_dcd)
# dcd$date_dcd <- as.Date(dcd$date_dcd,"%d/%m/%Y")



#Quelles dates sont ND
dcd$PATIENT <- 1:length(dcd$DATE_DCD)
who_is_date_ND <- function(vec_name,vec_date) {
  vec_date <- as.character(vec_date)
  exist_year <-!is.na(str_sub(vec_date, 7, 10))
  exist_month <- !is.na(str_sub(vec_date,4,5))
  name_ND <- vec_name[grep("ND",vec_date[exist_year])]
  date_ND <- vec_date[grep("ND",vec_date[exist_year])]
  df_ND <- data.frame(name = name_ND, date = date_ND,
                      N_ND = ifelse ( date_ND %in% vec_date[exist_month], "day", "day_month"))
  return(df_ND)
}

who_is_date_ND (dcd$PATIENT,dcd$DATE_DCD)

#row NA creating a dataframe:
df <- data.frame(name = LETTERS[1:10], number1 = 1:10, number2 = c(10:3, NA, NA))
df[df$number1 < df$number2, ] #introduce rows NA
df[df$number1 < df$number2 & !is.na(df$number2), ] #its because number2 had NA

#select colnames 
ALLDIAG <- visite[,colnames(visite)[grep("DIAG",colnames(visite))]] #Pas bon, sélectionne aussi les dates
ALLDIAG <-visite[,colnames(visite)[str_sub(colnames(visite),1,8)=="DIAG_V_M"]] #plus précis

#sort dataframe by a column
dd <- data.frame(b = factor(c("Hi", "Med", "Hi", "Low"), 
                            levels = c("Low", "Med", "Hi"), ordered = TRUE),
                 x = c("A", "D", "A", "C"), y = c(8, 3, 9, 9),
                 z = c(1, 1, 1, 2))

dd[with(dd, order(-z, b)), ] #Median time: 778
dd[order(-dd$z, dd$b),] #Median time: 788
arrange(dd,desc(z),b)#Median time: 862 (library(plyr))


#proxy pour github: (set avant de faire un push ou un pull)
Sys.setenv(http_proxy = "http://adresse du proxy automatique:8080/")
Sys.setenv(https_proxy = "http://adresse du proxy automatique:8080/")

#Diagramme (qualitatif)
ggplot (data, aes(x=data$var)) + geom_bar()
#ou
histogram(factor(data$var),xlab = c(class1,class2,etc...))

#Diagramme (quantitatif)
ggplot (data, aes(x=data$var1,y=data$var2)) + geom_histogram()


#installer un package qui n'est pas à jour:
install.packages("devtools")
#set_prox si c'est un ordi de l'aphp : sinon on ne peut pas se connecter à github depuis R
Sys.setenv(http_proxy="http://adresse du proxy automatique:8080/")
Sys.setenv(https_proxy="http://adresse du proxy automatique:8080/")
#installer le package depuis github:
devtools::install_github("kassambara/r2excel") #adresse github du package r2excel
library(r2excel)

#pour exporter dans excel sur des feuilles differentes chaque base: 
library(r2excel)

xlsx.writeMultipleData("myworkbook.xlsx", 
                       mtcars, Titanic, AirPassengers, state.x77)
xlsx.openFile("myworkbook.xlsx")# view the file


#pour exporter dans excel plusieurs objets R sur une meme feuille
wb <- createWorkbook(type="xlsx")
sheet1 <- createSheet(wb, sheetName = "doublons base trt_PATIENT")

xlsx.addHeader(wb,sheet1, value="Doublons trt : seuls les colonnes avec une différence sont affichées")
xlsx.addLineBreak(sheet1, 2)
for (i in names2ttt) print.report(i,ttt,file=wb, sheet=sheet1)

saveWorkbook(wb, "examples_add_table.xlsx")
xlsx.openFile("examples_add_table.xlsx")

#Attention 2 fonctions nécessaires : print.report et print_double_unique:
#print_double_unique imprime les colonnes qui different pour 2 (ou plus) doublons
print_double_unique <- function(data,name_pat){ #name_pat entre guillemet!
  wt1 <-data[data$PATIENT==name_pat, ]
  wt <- wt1[1:(nrow(wt1)-1), ] == wt1[2:nrow(wt1),] #compare ligne du dessus à ligne du dessous
  st <- apply(wt,2,all)
  diff_coll <- c("PATIENT",names(st[st==F & !is.na(st)]))
  if ( all(diff_coll=="PATIENT") ) res <- "no difference"
  else {
    res <- wt1[,diff_coll]
  }
  write.table(print(res),file="clipboard",sep="\t",dec=",",row.names=FALSE) 
  return (res)
}
#Impression des doublons sur excel
print.report <- function(.name,data,file,sheet) {
  xlsx.addHeader(file, sheet,.name)
  res <- print_double_unique (data,.name)
  if (is.null(ncol(res))) {
    xlsx.addParagraph (file, sheet, res)
    xlsx.addLineBreak(sheet, 1)
  } else {
    xlsx.addTable(file,sheet,res)
    xlsx.addLineBreak(sheet, 1)
  } 
} 


#Idem mais en écrivant sur 2 feuilles excel d'un meme fichier
wb <- createWorkbook(type="xlsx")
sheet1 <- createSheet(wb, sheetName = "doublons base trt_PATIENT")
sheet2 <- createSheet(wb, sheetName = "doublons base visite_PATIENT2")

xlsx.addHeader(wb,sheet1, value="Doublons trt : seuls les colonnes avec une différence sont affichées")
xlsx.addLineBreak(sheet1, 2)
for (i in names2ttt) print.report(i,ttt,file=wb, sheet=sheet1)

xlsx.addHeader(wb,sheet2, value="Doublons visite : seuls les colonnes avec une différence sont affichées")
xlsx.addLineBreak(sheet2, 2)
for (i in names2visite) print.report(i,visite,file=wb, sheet=sheet2)

saveWorkbook(wb, "examples_add_table.xlsx")
xlsx.openFile("examples_add_table.xlsx")


#proxy ? indiquer pour avoir internet ? SA:

ourvir le navigateur internet, personnaliser chrome (les 3 points), param?tres,
param?tres avanc?s tout en bas de la page, section r?seau : modifier les param?tres du proxy.
connexion, param?tres de r?seau local, param?tres r?seau : 
  ? la maison, cocher d?tecter automatiquement les param?tres de connexion
  ? SA, cocher "utiliser un serveur proxy pour votre r?seau local (...)" et rentrer ces param?tres:
    adresse :xxx.xxx.xxx.xx port: 8080


#date
length(unique(ttvhc [ttvhc$DDEBUT <= as_date("2014-06-12") & ttvhc$ID_DCI==14, "ID_PAT"]))
#different de
length(unique(ttvhc [ttvhc$DDEBUT <= "2014-06-12" & ttvhc$ID_DCI==14, "ID_PAT"])) #ici r ne reconnait pas que c'est une date donc n'enregistre pas la condition


#changer format de multiples colonnes : facteur en numeric
sla1<- sla
sla1[,var_quanti] <- apply(sla1[,var_quanti], 2, function(x) as.numeric(as.character(x)))#ok : idem que boucle

sla2 <- sla
sla2[,var_quanti] <- as.numeric(as.character(unlist(sla2[,var_quanti]))) #pas très précis : des différences de moyenne avec les 3 autres méthodes

sla3 <- sla
sla3[,var_quanti] <- apply(sla3[,var_quanti], 2, function(x) as.numeric(x)) # ok : idem que sla 1 et boucle : apply converti déjà automatiquement les facteurs en charactère

sla4 <- sla
for (i in var_quanti){
  sla4[,i] <- as.numeric(as.character(sla4[,i]))
}
# choix : sla3

#changer format de multiples colonnes : numeric en facteur
for (i in qual){
  d[,i] <- factor(d[,i])
}

#Changer l'échelle de l'abscisse
http://stackoverflow.com/questions/31198144/formatting-a-scale-x-continuous-axis-with-quarterly-data

#Belles courbes de survie en ggplot
https://rdrr.io/cran/GGally/man/ggsurv.html

for (i in var_quali){ # exemple : i="sex"
  var <- sla[,i]
  var <- as.factor(var)
  a <- survfit(Surv(sla$time.vni,sla$censor)~var, conf.int=.95)
  g <- ggsurv(a)
  g <- g +
    guides(linetype = FALSE)+ #avec la ligne du dessous, permet de changer le titre de la légende
    scale_colour_discrete( name = i, breaks = c(0,1), labels = levels(var))+ #warning qui est normal : Scale for 'colour' is already present. Adding another scale for 'colour', which will replace the existing scale.
    labs(title = paste0("Survie en fonction de ",i), x = "Time, years", y = "Survival, %") + #change le tritre des axes et le titre du plot
    scale_x_continuous(breaks=seq(0,max(sla$time.vni),365.25), labels=0:(length(seq(0,max(sla$time.vni),365.25))-1)) + #change les jours en années
    scale_y_continuous(labels=percent) #change les proba en pourcentage
  print(g)
}

#--------------------------
#rajout de table de survie et d'intervalle de confiance a la main : version base

#VERSION MODELE VIDE
s<-sla
s$tps<-s$time.vni/365.25
km <- survfit(Surv(tps,censor)~1, data=s, conf.int=.95)
skmi<-summary(km, time=c(3, 6)) #pour IC95%
skm<-summary(km, time=seq(0, 10, by=1)) #pour table survie
skm

#preparation de la fenetre de plot : conserve de l'espace autour du plot
par(mai=c(2,1.5,1.5,.5)) 
#plot sans l'intervalle de confiance
plot(km, conf.int=F, col="blue")
#intervalle de confiance a 3 et 6 ans
#barre vertical
segments(skmi$time, skmi$lower, skmi$time, skmi$upper, col="blue")
#lower
segments(skmi$time-0.1, skmi$lower, skmi$time+0.1, skmi$lower, col="blue") #-0.1 et +0.1 pour tracer traits horizontaux
#upper
segments(skmi$time-0.1, skmi$upper, skmi$time+0.1, skmi$upper, col="blue") #-0.1 et +0.1 pour tracer traits horizontaux
#table de survie
mtext(side=1, at=skm$time, skm$n.risk, adj=.5, col="blue", line=2, cex=0.75)


#VERSION 1 COVARIABLE
s<-sla
s$tps<-s$time.vni/365.25
#courbe de KM
km <- survfit(Surv(tps,censor)~sex, data=s, conf.int=.95)
km0 <- survfit(Surv(tps,censor)~sex, data=s[s$sex==0,], conf.int=.95)
km1 <- survfit(Surv(tps,censor)~sex, data=s[s$sex==1,], conf.int=.95)
km
#pour IC95%
skmi0<-summary(km0, time=c(3, 6)-0.1)
skmi1<-summary(km1, time=c(3, 6)+0.1)
#pour table de survie
skm0<-summary(km0, time=seq(0, 10, by=1))
skm1<-summary(km1, time=seq(0, 10, by=1))
skm

#preparation legende
leg<-names(km$strata)
coul<-c("blue", "red")
#prepapration fenetre
par(mai=c(2,1.5,1.5,.5))

#plot
plot(km, conf.int = F, col = coul)
#legende
legend("topright", adj=0, col=coul, leg=leg, lty=1)
#trait vertical pour sex=0
segments(skmi0$time, skmi0$lower, skmi0$time, skmi0$upp, col=coul[1])
segments(skmi0$time-0.1, skmi0$lower, skmi0$time+0.1, skmi0$lower, col="blue")
segments(skmi0$time-0.1, skmi0$upper, skmi0$time+0.1, skmi0$upper, col="blue")
#trait vertical pour sex=1
segments(skmi1$time, skmi1$lower, skmi1$time, skmi1$upp, col=coul[2])
segments(skmi1$time-0.1, skmi1$lower, skmi1$time+0.1, skmi1$lower, col="red") #-0.1 et +0.1 pour tracer traits horizontaux
segments(skmi1$time-0.1, skmi1$upper, skmi1$time+0.1, skmi1$upper, col="red") #-0.1 et +0.1 pour tracer traits horizontaux
#table de survie
#sex=0
mtext(side=1, at=-0.7, leg[1], col=coul[1], line=2)
mtext(side=1, at=skm0$time, skm0$n.risk, adj=.5, col=coul[1], line=2, cex=0.75)
#sex=1
mtext(side=1, at=-0.7, leg[2], col=coul[2], line=2)
mtext(side=1, at=skm1$time, skm1$n.risk, adj=.5, col=coul[2], line=3, cex=0.75)

#-----------------
#rajout de table de survie et d'intervalle de confiance a la main : version ggplot2 (+library survminer)

#VERSION MODELE VIDE
g <- ggsurv(km, CI=FALSE) +
  scale_x_continuous(breaks=seq(0,max(s$time.vni),1), labels=0:(length(seq(0,max(s$time.vni),1))-1)) +
  scale_y_continuous(labels=percent) +
  theme(legend.position="bottom", legend.title=element_blank()) +
  theme(plot.margin = unit(c(1,1,3,1), "cm")) +
  labs(x="Time of follow-up, year")+
for (i in 1:2) {
  g <- g + geom_segment(x = skmi$time[i], y = skmi$lower[i], xend = skmi$time[i], yend = skmi$upper[i])
  g <- g + geom_segment(x = skmi$time[i] - 0.1, y = skmi$lower[i], xend = skmi$time[i] + 0.1, yend = skmi$lower[i])
  g <- g + geom_segment(x = skmi$time[i] - 0.1, y = skmi$upper[i], xend = skmi$time[i] + 0.1, yend = skmi$upper[i])
}

for (ii in 1:nrow(skm)) {
  g <- g + annotation_custom(grob = textGrob(skm$n.risk[ii]), xmin = skm$time[ii], xmax = skm$time[ii], ymin= - 1.3 )
}
gt <- ggplotGrob(g)
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)



#VERSION 1 COVARIABLE

var <- paste0("Survival by age at vni superior to ", round(median(s$a),0), " ans")
km <- survfit(Surv(tps,censor)~a_recode, data=s, conf.int=.95)
#km <- survfit(Surv(start, stop, censor) ~ a_recode + at, data=slat)
km0 <- survfit(Surv(tps,censor)~a_recode, data=s[s$a_recode==0,], conf.int=.95)
km1 <- survfit(Surv(tps,censor)~a_recode, data=s[s$a_recode==1,], conf.int=.95)

#pour IC95%
skmi0<-summary(km0, time=c(3, 6)-0.1)
#skmi0<-summary(km0, time=c(0.5, 1)-0.1)
skmi1<-summary(km1, time=c(3, 6)+0.1)
#skmi1<-summary(km1, time=c(0.5, 1)+0.1) #plus d'évènement apres 1.94 ans

#pour table de survie
skm0 <- summary(km0, time=seq(0, 10, by=1))
skm0 <- data.frame(time=skm0$time, n.risk=skm0$n.risk)
skm1<-summary(km1, time=seq(0, 10, by=1))
skm1 <- data.frame(time=skm1$time, n.risk=skm1$n.risk)

#preparation legende
leg<-str_sub(names(km$strata),-1,-1)
col <- hue_pal()(length(leg))

#courbe de survie
g <- ggsurv(km, CI=FALSE, order.legend=FALSE, surv.col=col, cens.col=col) +
  #changement des axes
  scale_x_continuous(breaks=seq(0,max(s$tps),1), labels=0:(length(seq(0,max(s$tps),1))-1)) +
  scale_y_continuous(labels=percent) +
  labs(x="Time of follow-up, year", title=var) +
  #changement legende
  guides (linetype = FALSE) +
  scale_colour_discrete( labels = leg) +
  theme(legend.position="right", legend.title=element_blank()) +
  #espace autour du schéma
  theme(plot.margin = unit(c(1,1,3,2), "cm")) #top, right, bottom, left
#intervalle de confiance
for (i in 1:2) {
  g <- g + geom_segment(x = skmi0$time[i], y = skmi0$lower[i], xend = skmi0$time[i], yend = skmi0$upper[i], colour = col[1])
  g <- g + geom_segment(x = skmi0$time[i] - 0.1, y = skmi0$lower[i], xend = skmi0$time[i] + 0.1, yend = skmi0$lower[i], colour = col[1])
  g <- g + geom_segment(x = skmi0$time[i] - 0.1, y = skmi0$upper[i], xend = skmi0$time[i] + 0.1, yend = skmi0$upper[i], colour = col[1])
}
for (i in 1:2) {
  g <- g + geom_segment(x = skmi1$time[i], y = skmi1$lower[i], xend = skmi1$time[i], yend = skmi1$upper[i], colour = col[2])
  g <- g + geom_segment(x = skmi1$time[i] - 0.1, y = skmi1$lower[i], xend = skmi1$time[i] + 0.1, yend = skmi1$lower[i], colour = col[2])
  g <- g + geom_segment(x = skmi1$time[i] - 0.1, y = skmi1$upper[i], xend = skmi1$time[i] + 0.1, yend = skmi1$upper[i], colour = col[2])
}
#risk table
for (ii in 1:nrow(skm0)) {
  g <- g + annotation_custom(grob = textGrob(skm0$n.risk[ii]), xmin = skm0$time[ii], xmax = skm0$time[ii], ymin= - 1.5 )
}  
for (ii in 1:nrow(skm1)) {
  g <- g + annotation_custom(grob = textGrob(skm1$n.risk[ii]), xmin = skm1$time[ii], xmax = skm1$time[ii], ymin= - 1.7 )
} 
#display group text
g <- g + annotation_custom(grob = textGrob(leg[1]), xmin = -1.7, xmax = -1.7, ymin= - 1.5 )
g <- g + annotation_custom(grob = textGrob(leg[2]), xmin = -1.7, xmax = -1.7, ymin= - 1.7 )

gt <- ggplotGrob(g)
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)


#VERSION 1 COVARIABLE QUALITATIVE NON ORDONNEE

var <- "Lieu deb"
km <- survfit(Surv(tps,censor)~LIEUDEB_recode, data=s, conf.int=.95)

for (i in 1:length(km$strata)){
  .km <- survfit(Surv(tps,censor)~LIEUDEB_recode, data=s[s$LIEUDEB_recode==levels(s$LIEUDEB_recode)[i], ], conf.int=.95)
  assign(paste0("km",i), .km)
  #pour IC95%
  am <- i/10
  .skmi <- summary(.km, time=c(1, 3)-am)
  assign(paste0("skmi",i), .skmi)
  #pour table de survie
  .skm <- summary(.km, time=seq(0, 10, by=1))
  .skm <- data.frame(time=.skm$time, n.risk=.skm$n.risk)
  assign(paste0("skm",i), .skm)
}

#preparation legende
leg<-names(km$strata)

#courbe de survie
g <- ggsurv(km, CI=FALSE, order.legend = FALSE) +
  #changement des axes
  scale_x_continuous(breaks=seq(0,max(s$tps),1), labels=0:(length(seq(0,max(s$tps),1))-1)) +
  scale_y_continuous(labels=percent) +
  labs(x="Time of follow-up, year") +
  #changement legende
  guides (linetype = FALSE) +
  scale_colour_discrete( labels = leg) +
  theme(legend.position="right", legend.title=element_blank()) +
  #espace autour du schéma
  theme(plot.margin = unit(c(0,1,4,2), "cm")) #top, right, bottom, left

#intervalle de confiance
for (j in 1:5){
  .skmi <- get(paste0(paste0("skmi",j)))
  for (i in 1:2) {
    g <- g + geom_segment(x = .skmi$time[i], y = .skmi$lower[i], xend = .skmi$time[i], yend = .skmi$upper[i])
    g <- g + geom_segment(x = .skmi$time[i] - 0.1, y = .skmi$lower[i], xend = .skmi$time[i] + 0.1, yend = .skmi$lower[i])
    g <- g + geom_segment(x = .skmi$time[i] - 0.1, y = .skmi$upper[i], xend = .skmi$time[i] + 0.1, yend = .skmi$upper[i])
  }
}


#risk table
#position_y <- c(-1.2, -1.5, -1.7, -1.9, -2.1)
position_y <- c(-1.4, -1.5, -1.6, -1.7, -1.8)
for (j in 1:5){
  .skm <- get(paste0(paste0("skm",j)))
  .pos <- position_y[j]
  for (ii in 1:nrow(.skm)) {
    g <- g + annotation_custom(grob = textGrob(.skm$n.risk[ii]), xmin = .skm$time[ii], xmax = .skm$time[ii], ymin= .pos )
  } 
}

#display group text
for (j in 1:5){
  g <- g + annotation_custom(grob = textGrob(str_sub(leg,16,-1)[j]), xmin = -2.1, xmax = -2.1, ymin = position_y[j])
}

gt <- ggplotGrob(g)
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)

#---------------
#SOURCE dans rnotebook
#dirname permet d'aller là où se trouve le projet, plutôt que là où se trouve le script 
.dir <- getwd() 
#"C:/Users/4051268/Desktop/Kingston 20170123/to push/sla_git/src"
.dir <- dirname(getwd()) 
#"C:/Users/4051268/Desktop/Kingston 20170123/to push/sla_git"
source(paste0(.dir,"/src/libraries_SLA.R"))

#-----------------

#imprimer message d'une fonction uniquement si elle n'est pas assigné à un object

fun <- function(x) {
  class(x) <- 'beep'
  comment(x) <- "hello world"
  return(x)
}

print.beep<- function(beep) {
  cat(paste0(comment(beep), "\n"))
  print(beep[1:length(beep)])
}


# > fun(1:10)
# hello world
# [1]  1  2  3  4  5  6  7  8  9 10


#If the reader didn't want the little [1] index to print either they could cat the output int he print statement as:

print.beep<- function(beep) {
    cat(paste0(comment(beep), "\n"))
    cat(beep[1:length(beep)], "\n")
}

#-----------------
#changer en date
#as_date (lubridate)
a <- -9649
a <- as_date(a)
#class
a <- -9649
class(a) <- "Date"
a

#♦--------------
#pour intercaler les colonnes (les rownames des colonnes à assembler doivent finir par le même numéro)
df <- data.frame(t(c(1,2,3,4)))
df <- cbind(df, data.frame(t(c("A","B","C","D"))))
df[,order(str_sub(colnames(df),-1,-1))]

#--------------
#split vector
transf <- "6-18-20"
as.numeric(unlist(strsplit(transf, "-")))

#--------------
#transformer une variable quali en k-1 binaires

varps_quali <- c("var_quali1", "var_quali2")
for (j in varps_quali){
  num <- which(varps_quali==j)
  a <- model.matrix( ~ dbis[ ,j])
  #pour avoir un nom de variable reconnaissable dans les schéma (si on laisse tel quel ça donne dbis[ ,j] CHF par exemple)
  colnames(a) <- gsub("dbis",j,  colnames(a))
  colnames(a) <- gsub("\\[", "",  colnames(a))
  colnames(a) <- gsub("\\]", "",  colnames(a))
  colnames(a) <- gsub("\\,", "",  colnames(a))
  colnames(a) <- gsub("j", "",  colnames(a))
  colnames(a) <- gsub(" ", "_",  colnames(a))
  #créer les  variables binaires
  for (i in 1:(length(colnames(a))-1)){
    dbis[ ,colnames(a)[i+1]] <- a[ ,i+1]
  }
  #créer un vecteur avec les noms de variables bianires crées
  vec_tmp <- colnames(a)[-1]
  vec_var <- if(num==1) vec_tmp else c(vec_tmp, vec_var) 
}

#------------------
#remove space between axis and plot
scale_x_date(.........., expand=c(0,0))
scale_x_continuous(.........., expand=c(0,0))


#-----------------
#afficher cellule vide dans excel avec write.table
#la valeur doit être codée "" dans R et non NA

