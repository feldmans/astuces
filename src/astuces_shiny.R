#Creer un projet shiny relier à guithub
#file> new project>version control>github>copier l'url du site internet ou se trouve le fichier git

#Quand on n'arrive pas a cloner un fichier git hub :
# Message d'erreur : 
  #Cloning into '2016-07-hep-AE-shiny'...
  #fatal: unable to access 'https://github.com/nlapidus/2016-07-hep-AE-shiny/': Failed to connect to github.com port 443: Timed out
#redemarrer R studio
#taper dans la console (dans n'importe quel projet): 
Sys.setenv('http_proxy'="http://134.157.220.19:8080/")
Sys.setenv('https_proxy'="http://134.157.220.19:8080/")
#Reessayer de creer le projet


Truc qui sert à rien :
> c(1, 3, 4, 2)
[1] 1 3 4 2
> x <- c(1, 3, 4, 2)
> sort(x)
[1] 1 2 3 4
> order(x)
[1] 1 4 2 3
> x[order(x)]
[1] 1 2 3 4

#explication mapply
AESOCmax<-data.frame(t(mapply(
  IC_sd,
  rep(1:3, each = 3),
  rep(1:3, 3)
)))


#comparer 2 listes
pt1 <- (sort(unique(EVT[EVT$SOC == "Endocrine disorders", "PT"])))
pt2 <- (sort(unique(EVT[EVT$SOC == "Metabolism and nutrition disorders", "PT"])))
pt1[pt1 %in% pt2]
pt2[pt2 %in% pt1]

#Supprimer la derniere colonne
#  df_ttt[, -ncol(df_ttt)],
#  df_ttt[, 1:(ncol(df_ttt)-1)],
#  df_ttt[, names(df_ttt)[names(df_ttt) != "date"]],
subset(df_ttt, select = -date),
#  subset(df_ttt, select = DCI_1:DCI_63),
#  df_ttt %>% select(-date),


#sortir une liste avec nom de la table et contenu entre parenthèse séparé par virgule
essai<-table(EVT_DCI1 %>% filter (SOC=="Surgical and medical procedures" & DCI5==TRUE) %>% select (PT))

cat(paste(names(essai),"(",as.numeric(essai),"), "))


#IF ET ELSE DANS UNE BOUCLE
if (.i=="TTVHBSUB") {
  saveRDS(df_ttt, file="data/sum_ttt_VHB.RData")
} else {
  saveRDS(df_ttt, file="data/sum_ttt_VHC.RData")
}
#Et pas:
# ifelse(i=="TTVHBSUB",saveRDS(df_ttt, file="data/sum_ttt_VHB.RData"),saveRDS(df_ttt, file="data/sum_ttt_VHC.RData")) # caca
#ifelse ne sert que pour la création d'un vecteur!!!!

#ENLEVER DSE GUILLEMETS
vec <- LETTERS
vec<-noquote(vec)


#EFFACER UNE PARTIE DE LA LISTE ENVIRONNEMENT:
a<-str_sub(ls(),1,2) %in% "PT"
b<-str_sub(ls(),1,3) %in% "SOC" & str_sub(ls(),-2,-1)%in% "_T" #commence par SOC et termine par _T
#voir lesquels c'est
ls()[a]
#les effacer
rm(list=ls()[a])


#ERREUR : "argument is missing with no default"
#signifie qu'on a oublié un argument de la fonction
#ex: ifelse(a==T,1) : il manque un 3e argument 


#PRINT THE LAST ERROR MESSAGE
geterrmessage()
