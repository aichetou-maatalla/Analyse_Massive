library("hemp")
library(psych)
library(psychTools)
library(corrplot)

Aichetou <- read.csv("C:/Users/hpi7/Desktop/PR_M2/Analyse complexe et massive/PROJET DE ANALYSE MASSIVE ET COMPLEXE/Aichetou-Matala-perf", header=TRUE, stringsAsFactors=TRUE)
#===========================Preparation du Donneés===============================
View(Aichetou)
summary(Aichetou)
#===========================Analyse Univarieé======================================
describe(Aichetou)
#===============les graphiques =====================================
multi.hist(Aichetou,bcol="red",dcol=c("green","brown"))
boxplot(Aichetou)
#===============================
quantile ( Aichetou ,na.rm=TRUE)
quantile ( Aichetou$V1 ,na.rm=TRUE)

#================== calcule de la difficulte des items(Pourcentage de reussis P) ==========================
diff_items<-colMeans(Aichetou, na.rm = TRUE)
round(diff_items, 2)
#=======================Representation du score de reussite =================================
plot(diff_items,las=1,type="h",ylim=(0:1),xlab="Items",ylab="Pourcentage de réussite")
text(diff_items,labels=names(Aichetou),adj=.5,pos=3,offset = .1)
grid()
#================ La discriminisation des Items ================================
totale_score <- rowSums(Aichetou, na.rm = TRUE)
items_discr <- cor(Aichetou, totale_score,use = "pairwise.complete.obs")
items_discr
#================== La fonction alpha ==========================
alpha(Aichetou)

# PrÃ©sentation graphique des discriminations des items

discrim<-data.frame(Aichetou, totale_score)
cor.plot(discrim,upper=F,diag=F)
#=================== l'indice de discriminisatin =============================
idi(Aichetou, Aichetou$V10, perc_cut=.25)
#================= Corrologramme ============================
corrplot(discrim,method="circle",zlim=c(0,1))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5555

corPlot(discrim,numbers=TRUE,colors=TRUE,n=51,main=NULL,zlim=c(-1,1),
        show.legend=TRUE, labels=NULL,n.legend=10,keep.par=TRUE,select=NULL, pval=NULL,  
        cuts=c(.001,.01),scale=TRUE,upper=TRUE,diag=TRUE, symmetric=TRUE,stars=FALSE,
        adjust="holm",xaxis=1, xlas=0,ylas=2,alpha=.75,min.length=NULL)

#=================================================================================
#table(Aichetou$V1)





