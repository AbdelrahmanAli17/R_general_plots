library(psych)
library(FactoMineR)
library(factoextra) 
library(RColorBrewer)

attach(USJudgeRatings)
head(USJudgeRatings)
str(USJudgeRatings)


fa.parallel(USJudgeRatings[,-1],n.iter=100,show.legend = TRUE,main="Scree plot with parallel analysis") 

pc <- principal(USJudgeRatings[,-1],nfactors=1)# if u decided more than 1 pc, type it in nfactors.
pc 

pc <- principal(USJudgeRatings[,-1],nfactors=1,scores=TRUE)
# nfactors based on earlier testing and set scores to true to retrieve the data

head(pc$scores)

cor(USJudgeRatings$CONT,pc$scores)# correlate the number of contact column "CONT" with scores of PC

head(iris)
str(iris) 

pca_facto <- PCA(iris[,-5], graph = FALSE)# call PCA
pca_facto

eigenvalues <- pca_facto$eig # from above u can call any function
eigenvalues# 1st col is eigenvalues, 2nd is percentage (eigen*100),3rd is cummulative%


fviz_screeplot(pca_facto, ncp=10)
pca_facto$var$coord

pca_facto$var$cos2

fviz_pca_var(pca_facto, col.var="coord")+scale_color_gradient2(low="white", mid="blue", high="red")


fviz_pca_ind(pca_facto, col.var="cos2",pointsize=4,jitter = list(width = 0.3, height = 0.3))


#------------------------------------------------------------
#data show be transbosed and col name is the features and row names is the samples  
# 
pls_func <- function(data , label , number_of_comp =5 ){
  library("pls")
  data <- as.matrix(dd)
  #for y in the model i use virsual model to make dammy matrix from 
  #the label in the original data frame  
  model <-plsr(model.matrix(~label -1) ~ data ,method = "oscorespls", 
               ncomp =number_of_comp )
  model[["scores"]]
  model[["loadings"]]
  write.csv(signif(model[["scores"]], 5), row.names = rownames(dd), 
                 file = "score.csv")
  write.csv(signif(model[["loadings"]], 5),file = "loadings.csv")
  
}




pls::validationplot(model)

plot(model, "validation", val.type = "MSEP", legendpos = "top") 
  


summary(model)
install.packages("mdatools")
library("mdatools")


data(iris)

cal.ind = c(1:25, 51:75, 101:125)
val.ind = c(26:50, 76:100, 126:150)

Xc = iris[cal.ind, 1:4]
Xv = iris[val.ind, 1:4]

cc.all = iris[cal.ind, 5]
cv.all = iris[val.ind, 5]
show(cc.all)


###################################################################
#plsda analysis for dynamic databases 


df<- read.csv("D:/Work57/Azza project/processedDf_CD_7D.csv")
rownames(df) <- df[,1]
df<- df[,-c(1)]
df <- as.data.frame(t(df))

label <- df[,1]
df <- df[-c(1),]
df <- mutate_all(df,function(x) as.numeric(as.character(x)))
m <-  pls(x = data , c = model.matrix(~label - 1), ncomp = 5 , scale = T)
m <- selectCompNum(m, 3)


m$calres$r2
write.csv(m$calres$r2 , "plsda_cc.csv" , row.names = T)
 

PCA_data <- read.csv("plsda_cc.csv" )

vars <- mSet$analSet$plsr$Xtotvar
PC1_var <- round( ( mSet$analSet$plsr$Xvar[1] / vars ) * 100, 1)
PC2_var <- round( ( mSet$analSet$plsr$Xvar[2] / vars ) * 100, 1)
PC3_var <- round( ( mSet$analSet$plsr$Xvar[3] / vars ) * 100, 1)

PCA_data$sample <- rownames(PCA_data)
PCA_data$color <- ifelse(grepl("A", PCA_data$sample,fixed = TRUE), "Autism", "Control")
PCA_data$sample <- NULL

x <- ggplot(PCA_data , aes( x = PC1, y = PC2, colour = color)) + 
  
  theme(  panel.background = element_rect(fill = "white",
                                          colour = "white") , 
          panel.border = element_rect(colour = "black", fill=NA, size=1, 
                                      linetype = "solid") , 
          
          panel.grid = element_line(colour = "grey", size = 0.1)) +
  
  stat_ellipse(aes(x=PC1, y=PC2, fill=color) ,level=0.95,geom="polygon",alpha=0.1) +
  
  geom_point(aes(colour=color, fill=color) ,alpha = 0.7, 
             shape = 21,size = 4,colour = "black" , stroke = 1.5)+
  
  scale_fill_manual(values=c( "#ff0000" ,"#00cc00")) +
  
  scale_color_manual(values=c( "#ff0000" ,"#00cc00")) +
  
  theme( axis.line = element_line(colour = "black", 
                                  size = 1, linetype = "solid")) + 
  
  theme(legend.background = element_rect(size=0.5, linetype="solid", 
                                         colour ="black")) + 
  
  theme(legend.position="top" , legend.title=element_blank()) +
  
  theme(text=element_text(size=16, face = "bold")) + 
  
  ylab(paste0("PC2 ", "(", PC2_var , "%", ")")) + 
  xlab(paste0("PC1 ", "(", PC1_var , "%" ,")"))

ggsave("PCA.jpeg",x, dpi = 300, width = 8, height = 7)


mt <- plsda(df , rownames(df) , ncomp = 5)
