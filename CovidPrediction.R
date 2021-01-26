###########################################################################
###########################################################################
###                                                                     ###
###             INPUT DATASET  E INSTALLAZIONE LIBRERIE                 ###
###                                                                     ###
###########################################################################
###########################################################################





# Librerie utilizzate
install.packages("readxl")
install.packages("rpart")
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("corrplot")
install.packages("gplots")
install.packages("lattice")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("Amelia")
install.packages("caret")
install.packages("pROC ")
install.packages("ROCR")
install.packages("nnet")
install.packages("devtools")
install.packages("reshape")
install.packages("klaR")
install.packages("e1071")
install.packages("psych")
install.packages("gridExtra")
install.packages("mlbench")
install.packages('Boruta')
install.packages('here')



library(readxl) 
library(rpart)  
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(corrplot)
library(gplots)
library(lattice)
library(FactoMineR)
library(factoextra)
library(Amelia)
library(caret)
library(ROCR)
library(pROC)
library(nnet)
library(klaR)
library(e1071)
library(corrplot)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
library(reshape)
library(psych)
library(gridExtra)
library(mlbench)
library(Boruta)
library(here)





# IMPORT DATASET RAW

script.path <- here("covid_data.xlsx")
dataset_raw <- read_excel(script.path)






# SELEZIONE DELLE COLONNE CHE RIENTRANO NEL DOMINIO DELLO STUDIO, OVVERO I VALORI OTTENIBILI DA ESAME DEL SANGUE

df <- dataset_raw[, c(2,3,7:20)]







###########################################################################
###########################################################################
###                                                                     ###
###                      ANALISI ESPLORATIVA                            ###
###                                                                     ###
###########################################################################
###########################################################################







# ELENCO FEATURE DI DOMINIO

colnames = colnames(df)  
colnames

# CAMBIO NOMI PER MIGLIORE INDICIZZAZIONE

colnames(df)[1] <- "ageQuantile"
colnames(df)[2] <- "examResult"
colnames(df)[9] <- "MCHC"
colnames(df)[12] <- "MCH"
colnames(df)[14] <- "MCV"
colnames(df)[16] <- "RDW"



# STATISTICHE DELLE FEATURE DEL DOMINIO

head(df)
tail(df)
str(df)
summary(df)
colMeans(is.na(df))


# STUDIO DI VALORI MANCANTI IN OGNI COLONNA

missing = colSums(is.na(df))
missing

# NA IN PERCENTUALE
colMeans(is.na(df))


# PLOT DEI VALORI MANCANTI


missmap(df, main="Missing Map")
par(mfrow=c(1,1))

table(df$examResult)


# CONVERSIONE DA STRINGA A FOTTERE DELLA COLONNA TARGET

df$examResult <- as.factor(df$examResult)




# PULIZIA DATASET DA VALORI MANCANTI

df <- df[!is.na(df$Hematocrit),]
df <- df[!is.na(df$Hemoglobin),]
df <- df[!is.na(df$Platelets),]
df <- df[!is.na(df$`Mean platelet volume`),]
df <- df[!is.na(df$`Red blood Cells`),]
df <- df[!is.na(df$Lymphocytes),]
df <- df[!is.na(df$MCHC),]
df <- df[!is.na(df$Leukocytes),]
df <- df[!is.na(df$Basophils),]
df <- df[!is.na(df$MCH),]
df <- df[!is.na(df$Eosinophils),]
df <- df[!is.na(df$MCV),]
df <- df[!is.na(df$Monocytes),]
df <- df[!is.na(df$RDW),]



# CAMBIO ORDINE PER AVERE LA COLONNA TARGET COME PRIMA

df <- df[, c(2,1,3:16)]


# TABELLA PER STUDIARE IL BILANCIAMENTO DEL DATASET

table(df$examResult)

# PLOT DELLO SBILANCIAMENTO DEL DATASET 
pie(table(df$examResult))

# SCATTERPLOT MATRIX
pairs(df[,3:16], pch = 19)

pairs.panels(df[,3:16], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# BOXPLOT

par(mfrow=c(3,2), mai = c(0.5, 0.5, 0.5, 0.5))

boxplot(df$Hematocrit ~ df$examResult, col=c("#8080ff", "#b30000"), main="Hematocrit")
boxplot(df$Hemoglobin ~ df$examResult, col=c("#8080ff", "#b30000"), main="Hemoglobin")
boxplot(df$Platelets ~ df$examResult, col=c("#8080ff", "#b30000"), main="Platelets")
boxplot(df$`Mean platelet volume` ~ df$examResult, col=c("#8080ff", "#b30000"), main="Mean platelet volume")
boxplot(df$`Red blood Cells` ~ df$examResult, col=c("#8080ff", "#b30000"), main="Red blood Cells")
boxplot(df$Lymphocytes ~ df$examResult, col=c("#8080ff", "#b30000"), main="Lymphocytes")

par(mfrow=c(3,2), mai = c(0.5, 0.5, 0.5, 0.5))
boxplot(df$MCHC ~ df$examResult, col=c("#8080ff", "#b30000"), main="MCHC")



boxplot(df$Leukocytes ~ df$examResult, col=c("#8080ff", "#b30000"), main="Leukocytes")
boxplot(df$Basophils ~ df$examResult, col=c("#8080ff", "#b30000"), main="Basophils")
boxplot(df$MCH ~ df$examResult, col=c("#8080ff", "#b30000"), main="MCH")
boxplot(df$Eosinophils ~ df$examResult, col=c("#8080ff", "#b30000"), main="Eosinophils")
boxplot(df$MCV ~ df$examResult, col=c("#8080ff", "#b30000"), main="MCV")

par(mfrow=c(2,1), mai = c(0.5, 0.5, 0.5, 0.5))
boxplot(df$Monocytes ~ df$examResult, col=c("#8080ff", "#b30000"), main="Monocytes")
boxplot(df$RDW ~ df$examResult, col=c("#8080ff", "#b30000"), main="RDW")



# DENSITY PLOT

p1 <- ggplot() + 
  geom_density(data=df, aes(x=Hematocrit, group=examResult, fill=examResult),alpha=0.5, adjust=2) + 
  xlab("Hematocrit") +
  geom_vline(aes(xintercept=mean(df$Hematocrit)),color="black", linetype="dashed", size=1) +
  ylab("Density")

p2 <- ggplot() + 
  geom_density(data=df, aes(x=Hemoglobin, group=examResult, fill=examResult),alpha=0.5, adjust=2) + 
  xlab("Hemoglobin") +
  geom_vline(aes(xintercept=mean(df$Hemoglobin)),color="black", linetype="dashed", size=1) +
  ylab("Density")

p3 <- ggplot() + 
  geom_density(data=df, aes(x=Platelets, group=examResult, fill=examResult),alpha=0.5, adjust=2) + 
  xlab("Platelets") +
  geom_vline(aes(xintercept=mean(df$Platelets)),color="black", linetype="dashed", size=1) +
  ylab("Density")

p4 <- ggplot() + 
  geom_density(data=df, aes(x=`Mean platelet volume`, group=examResult, fill=examResult),alpha=0.5, adjust=2) + 
  xlab("Mean platelet volume") +
  geom_vline(aes(xintercept=mean(df$`Mean platelet volume`)),color="black", linetype="dashed", size=1) +
  ylab("Density")
p5 <- ggplot() + 
  geom_density(data=df, aes(x=`Red blood Cells`, group=examResult, fill=examResult),alpha=0.5, adjust=2) + 
  xlab("Red blood Cells") +
  geom_vline(aes(xintercept=mean(df$`Red blood Cells`)),color="black", linetype="dashed", size=1) +
  ylab("Density")

p6 <- ggplot() + 
  geom_density(data=df, aes(x=Lymphocytes, group=examResult, fill=examResult),alpha=0.5, adjust=2) + 
  xlab("Lymphocytes") +
  geom_vline(aes(xintercept=mean(df$Lymphocytes)),color="black", linetype="dashed", size=1) +
  ylab("Density")

p7 <- ggplot() + 
  geom_density(data=df, aes(x=MCHC, group=examResult, fill=examResult),alpha=0.5, adjust=2) + 
  xlab("MCHC") +
  geom_vline(aes(xintercept=mean(df$MCHC)),color="black", linetype="dashed", size=1) +
  ylab("Density")



p8 <- ggplot() + 
  geom_density(data=df, aes(x=Leukocytes, group=examResult, fill=examResult),alpha=0.5, adjust=2) + 
  xlab("Leukocytes") +
  geom_vline(aes(xintercept=mean(df$Leukocytes)),color="black", linetype="dashed", size=1) +
  ylab("Density")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8,  ncol=2, nrow = 4)


p9 <- ggplot() + 
  geom_density(data=df, aes(x=Basophils, group=examResult, fill=examResult),alpha=0.5, adjust=2) + 
  xlab("Basophils") +
  geom_vline(aes(xintercept=mean(df$Basophils)),color="black", linetype="dashed", size=1) +
  ylab("Density")
p10 <- ggplot() + 
  geom_density(data=df, aes(x=MCH, group=examResult, fill=examResult),alpha=0.5, adjust=2) + 
  xlab("MCH") +
  geom_vline(aes(xintercept=mean(df$MCH)),color="black", linetype="dashed", size=1) +
  ylab("Density")

p11 <- ggplot() + 
  geom_density(data=df, aes(x=Eosinophils, group=examResult, fill=examResult),alpha=0.5, adjust=2) + 
  xlab("Eosinophils") +
  geom_vline(aes(xintercept=mean(df$Eosinophils)),color="black", linetype="dashed", size=1) +
  ylab("Density")


p12 <- ggplot() + 
  geom_density(data=df, aes(x=MCV, group=examResult, fill=examResult),alpha=0.5, adjust=2) + 
  xlab("MCV") +
  geom_vline(aes(xintercept=mean(df$MCV)),color="black", linetype="dashed", size=1) +
  ylab("Density")

p13 <- ggplot() + 
  geom_density(data=df, aes(x=Monocytes, group=examResult, fill=examResult),alpha=0.5, adjust=2) + 
  xlab("Monocytes") +
  geom_vline(aes(xintercept=mean(df$Monocytes)),color="black", linetype="dashed", size=1) +
  ylab("Density")

p14 <- ggplot() + 
  geom_density(data=df, aes(x=RDW, group=examResult, fill=examResult),alpha=0.5, adjust=2) + 
  xlab("RDW") +
  geom_vline(aes(xintercept=mean(df$RDW)),color="black", linetype="dashed", size=1) +
  ylab("Density")
grid.arrange(p9, p10, p11,p12, p12, p14, ncol=2, nrow = 3)
par(mfrow=c(1,1))




# applico la funzione qqnorm, che confronta la distribuzione della variabile con la distribuzione normale, ad alcune delle feature
# se i punti si dispongono su una retta di 45 gradi, allora sono coincidenti
# vedo poi se farlo anche per le altre o meno, penso sia più utile questo grafico per le time series

qqnorm(df$Hematocrit)
qqline(df$Hematocrit)

par(mfrow=c(1,1))


colnames(df)


# LIVELLO CORRELAZIONE VARIABILI

corrplot.mixed(corr=cor(df[ , c(3:16)], use="complete.obs"),upper="ellipse", tl.pos="lt",lower.col= colorpanel(50, "red", "gray60", "blue4"), upper.col= colorpanel(50, "red", "gray60", "blue4"))



# BARCHART DI OGNI VARIABILE NUMERICA RISPETTO AL TARGET

df.mean<-aggregate(Hematocrit~examResult, data=df, mean)
df.mean
barchart(Hematocrit~examResult, data=df.mean, col=c("blue", "red"), main="Hematocrit mean")


df.mean<-aggregate(Hemoglobin~examResult, data=df, mean)
df.mean
barchart(Hemoglobin~examResult, data=df.mean, col=c("blue", "red"), main="Hemoglobin mean")


df.mean<-aggregate(Platelets~examResult, data=df, mean)
df.mean
barchart(Platelets~examResult, data=df.mean, col=c("blue", "red"), main="Platelets mean")


df.mean<-aggregate(`Mean platelet volume`~examResult, data=df, mean)
df.mean
barchart(`Mean platelet volume`~examResult, data=df.mean, col=c("blue", "red"), main="Mean platelet volume mean")


df.mean<-aggregate(`Red blood Cells`~examResult, data=df, mean)
df.mean
barchart(`Red blood Cells`~examResult, data=df.mean, col=c("blue", "red"), main="Red blood Cells mean")


df.mean<-aggregate(Lymphocytes~examResult, data=df, mean)
df.mean
barchart(Lymphocytes~examResult, data=df.mean, col=c("blue", "red"), main="Lymphocytes mean")


df.mean<-aggregate(MCHC~examResult, data=df, mean)
df.mean
barchart(MCHC~examResult, data=df.mean, col=c("blue", "red"), main="MCHC mean")


df.mean<-aggregate(Leukocytes~examResult, data=df, mean)
df.mean
barchart(Leukocytes~examResult, data=df.mean, col=c("blue", "red"), main="Leukocytes mean")


df.mean<-aggregate(Basophils~examResult, data=df, mean)
df.mean
barchart(Basophils~examResult, data=df.mean, col=c("blue", "red"), main="Basophils mean")


df.mean<-aggregate(MCH~examResult, data=df, mean)
df.mean
barchart(MCH~examResult, data=df.mean, col=c("blue", "red"), main="MCH mean")


df.mean<-aggregate(Eosinophils~examResult, data=df, mean)
df.mean
barchart(Eosinophils~examResult, data=df.mean, col=c("blue", "red"), main="Eosinophils mean")


df.mean<-aggregate(MCV~examResult, data=df, mean)
df.mean
barchart(MCV~examResult, data=df.mean, col=c("blue", "red"), main="MCV mean")


df.mean<-aggregate(Monocytes~examResult, data=df, mean)
df.mean
barchart(Monocytes~examResult, data=df.mean, col=c("blue", "red"), main="Monocytes mean")


df.mean<-aggregate(RDW~examResult, data=df, mean)
df.mean
barchart(RDW~examResult, data=df.mean, col=c("blue", "red"), main="RDW mean")






###########################################################################
###########################################################################
###                                                                     ###
###                               PCA                                   ###
###                                                                     ###
###########################################################################
###########################################################################




#SELEZIONE VARIABILI NUMERICHE

df_active = df[, c(2:16)]
res.pca <- PCA(df_active, graph=FALSE)
eig.val <- get_eigenvalue(res.pca)
eig.val


# PLOT VARIANZE SPIEGATE

fviz_eig(res.pca, addlabels = TRUE, ylim=c(0,50))
var <- get_pca_var(res.pca)
var 
head(var$coord, 6)
fviz_pca_var(res.pca, col.var="black") 


# STUDIO SUGLI INDIVIDUI

ind <- get_pca_ind(res.pca) 
ind 
fviz_pca_ind(res.pca)



# PLOT INDIVIDUI


fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = df$examResult, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             )




corrplot(var$contrib, is.corr=FALSE)    







###########################################################################
###########################################################################
###                                                                     ###
###       MODELLO  OTTENUTO DA  DATASET TRAFORMATO TRAMITE PCA          ###
###                                                                     ###
###########################################################################
###########################################################################

# NUOVO DATASET OTTENUTO DA MOLTIPLICAZIONE DELLA MATRICE DI PARTENZA CON PROIEZIONE OTTENUTA DA PCA

ds <- cbind(as.matrix(df[, c(2:16)]) %*% as.matrix(var$contrib), 
            df$examResult)

ds <- ds[, c(6,1:5)]

ds <- data.frame(ds)

colnames(ds)[1] = "result"

ds$result[ds$result == 1] <- "negative"
ds$result[ds$result == 2] <- "positive"
ds$result <- as.factor(ds$result)




# SEED CASUALE

set.seed(2)



# SPLIT DATASET IN TRAINING E TEST

ind = sample(2, nrow(ds), replace = TRUE, prob=c(0.7, 0.3))
trainset = ds[ind == 1,]
testset = ds[ind == 2,]



# UTILIZZO DI 10-FOLD CROSS VALIDATION

control = trainControl(method = "repeatedcv", number = 10,repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)




# DECISION TREE 


decisionTree.PCA = train(result ~ .,
                      data = trainset,
                      method = "rpart",
                      metric = "ROC",
                      trControl = control)




# STUDIO ALBERO OTTENUTO E COMPLESSITA'

summary(decisionTree.PCA)
plot(decisionTree.PCA)


# STUDIO DEL TUNING DEL MODELLO

decisionTree.PCA$bestTune



# PLOT TREE TUNED

fancyRpartPlot(decisionTree.PCA$finalModel)


# PREDIZIONE

tree.pred <- predict(decisionTree.PCA, testset[,! names(testset) %in% c("result")], type = "prob")


# STUDIO ROC

pred.rocr.tree.PCA = prediction(tree.pred[,2], testset$result) 
perf.rocr.tree.PCA = performance(pred.rocr.tree.PCA, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr.tree.PCA = performance(pred.rocr.tree.PCA, "tpr","fpr")
plot(perf.tpr.rocr.tree.PCA, colorize=T,main=paste("DECISION TREE (PCA) AUC:",(perf.rocr.tree.PCA@y.values)))
abline(a=0, b=1)


# CONFUSION MATRIX

tree.pred <- predict(decisionTree.PCA, testset)
table(tree.pred , testset[,c("result")])
result.tree.pca = confusionMatrix(tree.pred, testset[,c("result")], positive = "positive")
result.tree.pca_recall = confusionMatrix(tree.pred, testset[,c("result")], mode = "prec_recall", positive = "positive")


# ---------------------------------------------------------------------------------------------------------------------------------------------


# RETE NEURALE


Network.PCA <- train(result ~ .,
                 data = trainset,
                 method = "nnet",
                 linout = FALSE,
                 metric = "ROC",
                 trControl = control)


# PLOT RETE NEURALE TUNED

plot.nnet(Network.PCA$finalModel)


# PREDIZIONE

nnet.pred <- predict(Network.PCA, testset[,! names(testset) %in% c("result")], type = "prob")



# STUDIO ROC

pred.rocr.nnet.PCA = prediction(nnet.pred[,2], testset$result) 
perf.rocr.nnet.PCA = performance(pred.rocr.nnet.PCA, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr.nnet.PCA = performance(pred.rocr.nnet.PCA, "tpr","fpr")
plot(perf.tpr.rocr.nnet.PCA, colorize=T,main=paste("NEURAL NET (PCA) AUC:",(perf.rocr.nnet.PCA@y.values)))
abline(a=0, b=1)


# CONFUSION MATRIX

nnet.pred <- predict(Network.PCA, testset)
table(nnet.pred , testset[,c("result")])
result.nnet.pca = confusionMatrix(nnet.pred, testset[,c("result")], positive = "positive")
result.nnet.pca_recall = confusionMatrix(nnet.pred, testset[,c("result")], mode = "prec_recall", positive = "positive")


# ---------------------------------------------------------------------------------------------------------------------------------------------



# NAIVE BAYES



nb.model.PCA <- train(result ~., 
                  data = trainset, method = "nb",
                  trControl = control)


# PREDIZIONE

nb.pred = predict(nb.model.PCA, testset[,! names(testset) %in% c("result")], type = "prob")


# STUDIO ROC


pred.rocr.nb.PCA = prediction(nb.pred[,2], testset$result) 
perf.rocr.nb.PCA = performance(pred.rocr.nb.PCA, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr.nb.PCA = performance(pred.rocr.nb.PCA, "tpr","fpr")
plot(perf.tpr.rocr.nb.PCA, colorize=T,main=paste("NAIVE BAYES (PCA) AUC:",(perf.rocr.nb.PCA@y.values)))
abline(a=0, b=1)


# CONFUSION MATRIX

nb.pred <- predict(nb.model.PCA, testset)
table(nb.pred , testset[,c("result")])
result.nb.pca = confusionMatrix(nb.pred, testset[,c("result")], positive = "positive")
result.nb.pca_recall = confusionMatrix(nb.pred, testset[,c("result")], mode = "prec_recall", positive = "positive")







# # LEAVE ONE OUT - RIMOSSA PER PORBLEMI DI PERFORMANCE - NESSUN MIGLIORAMENTO SIGNIFICATIVO RISPETTO 10-FOLD
# # 
# # UTILIZZO DI LEAVE ONE OUT
# 
# control = trainControl(method = "LOOCV", classProbs = TRUE, summaryFunction = twoClassSummary)
# 
# 
# 
# # DECISION TREE 
# 
# 
# decisionTree.PCA = train(result ~ .,
#                          data = trainset,
#                          method = "rpart",
#                          metric = "ROC",
#                          trControl = control)
# 
# 
# 
# 
# # STUDIO ALBERO OTTENUTO E COMPLESSITA'
# 
# summary(decisionTree.PCA)
# plot(decisionTree.PCA)
# 
# 
# # STUDIO DEL TUNING DEL MODELLO
# 
# decisionTree.PCA$bestTune
# 
# 
# 
# # PLOT TREE TUNED
# 
# fancyRpartPlot(decisionTree.PCA$finalModel)
# 
# 
# # PREDIZIONE
# 
# tree.pred <- predict(decisionTree.PCA, testset[,! names(testset) %in% c("result")], type = "prob")
# 
# 
# # STUDIO ROC
# 
# pred.rocr = prediction(tree.pred[,2], testset$result) 
# perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")
# perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")
# plot(perf.tpr.rocr, colorize=T,main=paste("DECISION TREE (PCA) AUC:",(perf.rocr@y.values)))
# abline(a=0, b=1)
# 
# 
# # CONFUSION MATRIX
# 
# tree.pred <- predict(decisionTree.PCA, testset)
# table(tree.pred , testset[,c("result")])
# result.tree.pca = confusionMatrix(tree.pred, testset[,c("result")], positive = "positive")
# result.tree.pca_recall = confusionMatrix(tree.pred, testset[,c("result")], mode = "prec_recall", positive = "positive")
# 
# 
# # ---------------------------------------------------------------------------------------------------------------------------------------------
# 
# 
# # RETE NEURALE
# 
# 
# Network.PCA <- train(result ~ .,
#                      data = trainset,
#                      method = "nnet",
#                      linout = FALSE,
#                      metric = "ROC",
#                      trControl = control)
# 
# 
# # PLOT RETE NEURALE TUNED
# 
# plot.nnet(Network.PCA$finalModel)
# 
# 
# # PREDIZIONE
# 
# nnet.pred <- predict(Network.PCA, testset[,! names(testset) %in% c("result")], type = "prob")
# 
# 
# 
# # STUDIO ROC
# 
# pred.rocr = prediction(nnet.pred[,2], testset$result) 
# perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")
# perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")
# plot(perf.tpr.rocr, colorize=T,main=paste("NEURAL NET (PCA) AUC:",(perf.rocr@y.values)))
# abline(a=0, b=1)
# 
# 
# # CONFUSION MATRIX
# 
# nnet.pred <- predict(Network.PCA, testset)
# table(nnet.pred , testset[,c("result")])
# result.nnet.pca = confusionMatrix(nnet.pred, testset[,c("result")], positive = "positive")
# result.nnet.pca_recall = confusionMatrix(nnet.pred, testset[,c("result")], mode = "prec_recall", positive = "positive")
# 
# 
# # ---------------------------------------------------------------------------------------------------------------------------------------------
# 
# 
# 
# # NAIVE BAYES
# 
# 
# 
# nb.model.PCA <- train(result ~., 
#                       data = trainset, method = "nb",
#                       trControl = control)
# 
# 
# # PREDIZIONE
# 
# nb.pred = predict(nb.model.PCA, testset[,! names(testset) %in% c("result")], type = "prob")
# 
# 
# # STUDIO ROC
# 
# 
# pred.rocr = prediction(nb.pred[,2], testset$result) 
# perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")
# perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")
# plot(perf.tpr.rocr, colorize=T,main=paste("NAIVE BAYES (PCA) AUC:",(perf.rocr@y.values)))
# abline(a=0, b=1)
# 
# 
# # CONFUSION MATRIX
# 
# nb.pred <- predict(nb.model.PCA, testset)
# table(nb.pred , testset[,c("result")])
# result.nb.pca = confusionMatrix(nb.pred, testset[,c("result")], positive = "positive")
# result.nb.pca_recall = confusionMatrix(nb.pred, testset[,c("result")], mode = "prec_recall", positive = "positive")
# 
# 
# 
# 
# # ---------------------------------------------------------------------------------------------------------------------------------------------



###########################################################################
###########################################################################
###                                                                     ###
###             MODELLO OTTENUTO TRAMITE FEATURE SELECTION              ###
###                                                                     ###
###########################################################################
###########################################################################




# UTILIZZO DI 10-FOLD CROSS VALIDATION


control = trainControl(method = "repeatedcv", number = 10,repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)


#DATASET OTTENUTO TRAMITE SELEZIONE DI TUTTE LE FEATURE INERENTI AL DOMINIO DI INTERESSE

ds <- cbind(df[, c(1:5, 8:16)])
colnames(ds)[1] = "result"
ds$result <- as.factor(ds$result)



# SPLIT FUNCTION

set.seed(25)

ind = sample(2, nrow(ds), replace = TRUE, prob=c(0.7, 0.3))
trainset = ds[ind == 1,]
testset = ds[ind == 2,]



# FEATURE SELECTION

boruta_output <- Boruta(result ~ ., data=trainset, doTrace=0)  
plot(boruta_output, cex.axis=.55, las=2, xlab="", main="Variable Importance") 


trainset <- trainset[, c(1:5,8,9,11,13)]
testset <- testset[, c(1:5,8,9,11,13)]


# DECISION TREE


decisionTree = train(result ~ .,
                      data = trainset,
                      method = "rpart",
                      metric = "ROC",
                      trControl = control)




# STUDIO ALBERO OTTENUTO E COMPLESSITA'

summary(decisionTree)
plot(decisionTree)

decisionTree$bestTune


# PLOT TREE TUNED

fancyRpartPlot(decisionTree$finalModel)


# PREDIZIONE

tree.pred <- predict(decisionTree, testset[,! names(testset) %in% c("result")], type = "prob")


# STUDIO ROC

pred.rocr.tree = prediction(tree.pred[,2], testset$result) 
perf.rocr.tree = performance(pred.rocr.tree, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr.tree = performance(pred.rocr.tree, "tpr","fpr")
plot(perf.tpr.rocr.tree, colorize=T,main=paste("DECISION TREE (FEATURE SELECTION) AUC:",(perf.rocr.tree@y.values)))
abline(a=0, b=1)

#STUDIO CONFUSION MATRIX

tree.pred <- predict(decisionTree, testset)
table(tree.pred , testset[,c("result")])
result.tree.ds  = confusionMatrix(tree.pred, testset[,c("result")], positive = "positive")
result.tree.ds_recall  = confusionMatrix(tree.pred, testset[,c("result")], mode = "prec_recall", positive = "positive")


# ---------------------------------------------------------------------------------------------------------------------------------------------



# RETE NEURALE

network <- train(result ~ .,
                 data = trainset,
                 method = "nnet",
                 linout = FALSE,
                 metric = "ROC",
                 trControl = control)

# PLOT RETE NEURALE TUNED


plot.nnet(network$finalModel)


# PREDIZIONE

nnet.pred <- predict(network, testset[,! names(testset) %in% c("result")], type = "prob")


# STUDIO ROC

pred.rocr.nnet = prediction(nnet.pred[,2], testset$result) 
perf.rocr.nnet = performance(pred.rocr.nnet, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr.nnet = performance(pred.rocr.nnet, "tpr","fpr")
plot(perf.tpr.rocr.nnet, colorize=T,main=paste("NEURAL NET (FEATURE SELECTION) AUC:",(perf.rocr.nnet@y.values)))
abline(a=0, b=1)


# STUDIO CONFUSION MATRIX

nnet.pred <- predict(network, testset)
table(nnet.pred , testset[,c("result")])
result.nnet.ds = confusionMatrix(nnet.pred, testset[,c("result")], positive = "positive")
result.nnet.ds_recall = confusionMatrix(nnet.pred, testset[,c("result")], mode = "prec_recall", positive = "positive")


# ---------------------------------------------------------------------------------------------------------------------------------------------


# NAIVE BAYES


nb.model <- train(result ~., 
                  data = trainset, method = "nb",
                  trControl = control)


# PREDIZIONE

nb.pred = predict(nb.model, testset[,! names(testset) %in% c("result")], type = "prob")



# STUDIO ROC


pred.rocr.nb = prediction(nb.pred[,2], testset$result) 
perf.rocr.nb = performance(pred.rocr.nb, measure = "auc", x.measure = "cutoff")
perf.tpr.rocr.nb = performance(pred.rocr.nb, "tpr","fpr")
plot(perf.tpr.rocr.nb, colorize=T,main=paste("NAIVE BAYES (FEATURE SELECTION) AUC:",(perf.rocr.nb@y.values)))
abline(a=0, b=1)


#STUDIO CONFUSION MATRIX

nb.pred <- predict(nb.model, testset)
table(nb.pred , testset[,c("result")])
result.nb.ds = confusionMatrix(nb.pred, testset[,c("result")], positive = "positive")
result.nb.ds_recall = confusionMatrix(nb.pred, testset[,c("result")], mode = "prec_recall", positive = "positive")



# # ---------------------------------------------------------------------------------------------------------------------------------------------
# # 
# # # MODELLO SCARTATO - MIGLIORA SPECIFICITA' A DISCAPITO DI SENSIBILITA' E ACCURATEZZA
# # 
# # ###########################################################################
# # ###########################################################################
# # ###                                                                     ###
# # ###       MODELLO OTTENUTO TRAMITE FEATURE SELECTION E UPSAMPLING       ###
# # ###                                                                     ###
# # ###########################################################################
# # ###########################################################################
# 
# 
# # IMPORT DATASET
# 
# ds <- cbind(df[,])
# colnames(ds)[1] = "result"
# ds$result <- as.factor(ds$result)
# 
# 
# # SPLIT FUNCTION
# 
# set.seed(25)
# 
# ind = sample(2, nrow(ds), replace = TRUE, prob=c(0.7, 0.3))
# trainset = ds[ind == 1,]
# testset = ds[ind == 2,]
# 
# # SELEZIONE FEATURE
# 
# trainset <- trainset[, c(1:5,8,9,11,13)]
# testset <- testset[, c(1:5,8,9,11,13)]
# 
# 
# # UPSAMPLING
# 
# 
# trainset = upSample(x = trainset,
#                     y = trainset$result)
# 
# 
# trainset <- trainset[, 1:9]
# 
# 
# 
# #UTILIZZO 10-FOLD CROSS
# 
# 
# control = trainControl(method = "repeatedcv", number = 10,repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)
# 
# 
# 
# # DECISION TREE
# 
# 
# decisionTree.up = train(result ~ .,
#                      data = trainset,
#                      method = "rpart",
#                      metric = "ROC",
#                      trControl = control)
# 
# 
# 
# 
# # PLOT TREE
# fancyRpartPlot(decisionTree.up$finalModel, cex = 0.55)
# 
# 
# 
# # PREDIZIONE
# 
# tree.pred <- predict(decisionTree.up, testset[,! names(testset) %in% c("result")], type = "prob")
# 
# 
# 
# # STUDIO ROC
# 
# pred.rocr = prediction(tree.pred[,2], testset$result)
# perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")
# perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")
# plot(perf.tpr.rocr, colorize=T,main=paste("TREE (FEATURE SELECTION + UPSAMPLING) AUC:",(perf.rocr@y.values)))
# abline(a=0, b=1)
# 
# #STUDIO CONFUSION MATRIX
# 
# tree.pred <- predict(decisionTree.up, testset)
# table(tree.pred , testset[,c("result")])
# result.tree.ds.up  = confusionMatrix(tree.pred, testset[,c("result")], positive = "positive")
# result.tree.ds.up_recall  = confusionMatrix(tree.pred, testset[,c("result")], mode = "prec_recall", positive = "positive")
# 
# 
# # ---------------------------------------------------------------------------------------------------------------------------------------------
# 
# # NEURAL NETWORK
# 
# network.up <- train(result ~ .,
#                  data = trainset,
#                  method = "nnet",
#                  linout = FALSE,
#                  metric = "ROC",
#                  trControl = control)
# 
# 
# #PLOT RETE NEURALE
# 
# plot.nnet(network.up)
# 
# # PREDIZIONE
# 
# nnet.pred <- predict(network.up, testset[,! names(testset) %in% c("result")], type = "prob")
# 
# 
# # STUDIO ROC
# 
# 
# pred.rocr = prediction(nnet.pred[,2], testset$result)
# perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")
# perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")
# plot(perf.tpr.rocr, colorize=T,main=paste("NEURAL NETWORK (FEATURE SELECTION + UPSAMPLING) AUC:",(perf.rocr@y.values)))
# abline(a=0, b=1)
# 
# # STUDIO CONFUSION MATRIX
# 
# nnet.pred <- predict(network.up, testset)
# table(nnet.pred , testset[,c("result")])
# result.nnet.ds.up = confusionMatrix(nnet.pred, testset[,c("result")], positive = "positive")
# result.nnet.ds.up_recall = confusionMatrix(nnet.pred, testset[,c("result")], mode = "prec_recall", positive = "positive")
# 
# 
# # ---------------------------------------------------------------------------------------------------------------------------------------------
# 
# # NAIVE BAYSE
# 
# 
# nb.model.up <- train(result ~.,
#                   data = trainset, method = "nb",
#                   trControl = control)
# 
# # PREDIZIONE
# 
# nb.pred = predict(nb.model.up, testset[,! names(testset) %in% c("result")], type = "prob")
# 
# 
# # STUDIO ROC
# 
# 
# pred.rocr = prediction(nb.pred[,2], testset$result)
# perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")
# perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")
# plot(perf.tpr.rocr, colorize=T,main=paste("NAIVE BAYES (FEATURE SELECTION + UPSAMPLING) AUC:",(perf.rocr@y.values)))
# abline(a=0, b=1)
# 
# 
# # STUDIO CONFUSION MATRIX
# 
# nb.pred <- predict(nb.model.up, testset)
# table(nb.pred , testset[,c("result")])
# result.nb.ds.up = confusionMatrix(nb.pred, testset[,c("result")], positive = "positive")
# result.nb.ds.up_recall = confusionMatrix(nb.pred, testset[,c("result")], mode = "prec_recall", positive = "positive")
# 
# # ---------------------------------------------------------------------------------------------------------------------------------------------
# 









###########################################################################
###########################################################################
###                                                                     ###
###                         COMPARAZIONE MODELLI                        ###
###                                                                     ###
###########################################################################
###########################################################################



# COMPARAZIONE MODELLI PCA

cv.values.PCA = resamples(list(tree = decisionTree.PCA, nnet = Network.PCA, nb = nb.model.PCA))
summary(cv.values.PCA)

dotplot(cv.values.PCA, metric = "ROC") 
bwplot(cv.values.PCA, layout = c(3, 1)) 
splom(cv.values.PCA,metric="ROC")




# COMPARAZIONE MODELLI FEATURE SELECTION

cv.values = resamples(list(tree = decisionTree, nnet = network, nb = nb.model))
summary(cv.values)

dotplot(cv.values, metric = "ROC") 
bwplot(cv.values, layout = c(3, 1)) 
splom(cv.values,metric="ROC")




# COMPARAZIONE GENERALE


cv.values.all = resamples(list(tree.PCA = decisionTree.PCA, nnet.PCA = Network.PCA, nb.PCA = nb.model.PCA,
                               tree = decisionTree, nnet = network, nb = nb.model))
summary(cv.values.all)
cv.values.all$timings

dotplot(cv.values.all, metric = "ROC") 
bwplot(cv.values.all, layout = c(3, 1)) 




# COMPARAZIONE AUC

par(mfrow=c(2,3))

# pca

plot(perf.tpr.rocr.tree.PCA, colorize=T,main=paste("DECISION TREE (PCA):",(perf.rocr.tree.PCA@y.values)))
abline(a=0, b=1)

plot(perf.tpr.rocr.nnet.PCA, colorize=T,main=paste("NEURAL NET (PCA):",(perf.rocr.nnet.PCA@y.values)))
abline(a=0, b=1)

plot(perf.tpr.rocr.nb.PCA, colorize=T,main=paste("NAIVE BAYES (PCA):",(perf.rocr.nb.PCA@y.values)))
abline(a=0, b=1)


# feature selection

plot(perf.tpr.rocr.tree, colorize=T,main=paste("DECISION TREE (FS):",(perf.rocr.tree@y.values)))
abline(a=0, b=1)

plot(perf.tpr.rocr.nnet, colorize=T,main=paste("NEURAL NET (FS):",(perf.rocr.nnet@y.values)))
abline(a=0, b=1)

plot(perf.tpr.rocr.nb, colorize=T,main=paste("NAIVE BAYES (FS):",(perf.rocr.nb@y.values)))
abline(a=0, b=1)



# CONFUSION MATRIX


par(mfrow=c(1,1))

# pca


fourfoldplot(result.tree.pca$table, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main =paste("TREE (PCA) ACCURACY:",(result.tree.pca$overall[1])))


fourfoldplot(result.nnet.pca$table, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main =paste("NEURAL NETWORK (PCA) ACCURACY:",(result.nnet.pca$overall[1])))


fourfoldplot(result.nb.pca$table, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main =paste("NAIVE BAYES (PCA) ACCURACY:",(result.nb.pca$overall[1])))



# feature selection



fourfoldplot(result.tree.ds$table, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main =paste("TREE (FS) ACCURACY:",(result.tree.ds$overall[1])))


fourfoldplot(result.nnet.ds$table, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main =paste("NEURAL NETWORK (FS) ACCURACY:",(result.nnet.ds$overall[1])))


fourfoldplot(result.nb.ds$table, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main =paste("NAIVE BAYES (FS) ACCURACY:",(result.nb.ds$overall[1])))




models <- c("TREE (PCA)", "NEURAL NETWORK (PCA)", "NAIVE BAYES (PCA)",
            "TREE (FS)", "NEURAL NETWORK (FS)", "NAIVE BAYES (FS)")

models.accuracy <- c(result.tree.pca$overall[1], result.nnet.pca$overall[1], result.nb.pca$overall[1],
                     result.tree.ds$overall[1], result.nnet.ds$overall[1], result.nb.ds$overall[1])

models.kappa <- c(result.tree.pca$overall[2], result.nnet.pca$overall[2], result.nb.pca$overall[2],
                     result.tree.ds$overall[2], result.nnet.ds$overall[2], result.nb.ds$overall[2])

models.accuracyLow <- c(result.tree.pca$overall[3], result.nnet.pca$overall[3], result.nb.pca$overall[3],
                     result.tree.ds$overall[3], result.nnet.ds$overall[3], result.nb.ds$overall[3])

models.accuracyUpp <- c(result.tree.pca$overall[4], result.nnet.pca$overall[4], result.nb.pca$overall[4],
                     result.tree.ds$overall[4], result.nnet.ds$overall[4], result.nb.ds$overall[4])

models.accuracyPvalue <- c(result.tree.pca$overall[6], result.nnet.pca$overall[6], result.nb.pca$overall[6],
                     result.tree.ds$overall[6], result.nnet.ds$overall[6], result.nb.ds$overall[6])


models.stat <- c(models.accuracy, models.kappa, models.accuracyLow, models.accuracyUpp, models.accuracyPvalue)


overall_confusionMatrix <- matrix(models.stat, ncol = 5, 
                                  dimnames = list(models, c("Accuracy", "Kappa", "AccuracyLower", "AccuracyUpper", "AccuracyPValue")))


print(overall_confusionMatrix)
