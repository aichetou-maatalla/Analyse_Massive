########################## Chargement des Bibiliotheques ########################################################################################

library("hemp")
library(psych)
library(FactoMineR)
library(psychTools)

############################################ chargement du DonneÃ©s ##############################################################################
Aichetou_Dim<- read.csv("C:/Users/hpi7/Desktop/PR_M2/Analyse complexe et massive/PROJET DE ANALYSE MASSIVE ET COMPLEXE/Aichetou-Matala-dim", stringsAsFactors=TRUE)
View(Aichetou_Dim)
summary(Aichetou_Dim)

#======================== Analyses numÃÂ©riques Univariee ===============

describe(Aichetou_Dim)

#===================================== Analyses graphiques univariee =============================================================

boxplot(Aichetou_Dim,data=NULL,subset,na.action=NULL,add=FALSE,drop=FALSE,sep=".",lex.order=FALSE)
#=======================================================================================================================

multi.hist(Aichetou_Dim,bcol="azure",dcol=c("red","purple"))
#========================================================================================================================
multi.hist(Aichetou_Dim,density=TRUE,freq=F,bcol="grey", dcol=c("black","red"),dlty=c("solid","solid"),main="",mar = c(4.5, 1, 4, 1),breaks=21,global=F,las=0,yaxt="n")
#========================================================================================================================

#===================================== Analyses des corrÃÂ©lations =====================================================
pairs(Aichetou_Dim,pch=20,lower.panel = NULL,col="black")

pairs.panels(Aichetou_Dim, smooth = TRUE, scale = FALSE, density=F, ellipses=TRUE, digits = 2, method="pearson", lm=FALSE, cor=TRUE, hist.col="grey", show.points=TRUE, rug=F, breaks = "FD")
#==========================================================================================================================
#=============== Analyse dimentionnelles =====================================
#============== Analyse avec FactomineR
#-------------------------------------#

resultat.pca<-PCA(Aichetou_Dim[1:12],scale.unit = TRUE) 
#========== projection des individus sur les dims ====================
plot(resultat.pca,cex=0.8,label="none",invisible="quali",title="Graphe des Individus")

#fviz_pca_ind(resultat.pca,cex=0.8,habillage="variables",repel=TRUE,title="Graphe des individus")


# Choisir le nombre de composantes Ã  analyser
#-------------------------------------#

barplot(resultat.pca$eig[,2],main="",las=1,ylab="Pourcentage de varicance expliquée",names.arg=paste("Comp",1:nrow(resultat.pca$eig))) 
#---------- les individus -------------
summary(resultat.pca, ncp=2, nbelements=Inf)
#
#================= Choix du nombres de composantes ===============================

scree(Aichetou_Dim,factors=FALSE,pc=TRUE,hline=-1,main="")
fa.parallel(Aichetou_Dim,fa="pc",cor="cor")

# Choix du nombre de facteurs
#-------------------------------------#
scree(Aichetou_Dim,factors=TRUE,pc=FALSE,hline=-1,main="")
fa.parallel(Aichetou_Dim,fa="fa",cor="cor")

#scree(Aichetou_Dim,factors=FALSE,hline=-1,main="")
#fa.parallel(Aichetou_Dim,cor="cor")

#------------------------------------------------------------------------------------------------------------------------------------
#================= Analyse dimensionnelle exploratoires -----------------------------------------------------
# Pc
pc11<-principal(Aichetou_Dim,nfactors=1,rotate="none",cor="cor")
pc21<-principal(Aichetou_Dim,nfactors=2,rotate="none",cor="cor")
pc31<-principal(Aichetou_Dim,nfactors=3,rotate="none",cor="cor")

pc11
pc21
pc31

# graghique PC

fa.diagram(pc11)
fa.diagram(pc21)
fa.diagram(pc31)

#-----------------------------------------------------------------------------------------------------------------------------------
#============== Analyse factorielles exploratoires =============================
# FA
fa11<-fa(Aichetou_Dim,1)
fa21<-fa(Aichetou_Dim,2)
fa31<-fa(Aichetou_Dim,3)

fa11
fa21
fa31
#= Graphiques FA 
fa.diagram(fa(Aichetou_Dim,1))
fa.diagram(fa(Aichetou_Dim,2))
fa.diagram(fa(Aichetou_Dim,3))
#fa.diagram(fa(Aichetou_Dim,4))

#=================== Analyses Numeriques et Graphiques bivariees====================================================
Correlations<-cor(Aichetou_Dim,method = "pearson")
Correlations
round(Correlations, 2)

#-----------les relation entres les variables ---------------------------------------------------------------------------
#------------- V1 avec les Auters variables --------------------------------------------------------------------------
#---------- V1, V2--------------------------------------
#par(mfrow=c(3,1))
#plot(Aichetou_Dim$V1, Aichetou_Dim$V2, xlab="V1", ylab="V2", main="Nuge de points")
#d1=lm(Aichetou_Dim$V2 ~ Aichetou_Dim$V1)
#abline(d1,col="red")

# FIN ANALYSE 










