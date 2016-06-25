# set wd to main path of project
setwd("C:\\Users\\brian\\Documents\\UVA\\Capstone\\Git\\Git")

# split up list of file names into accuracy and raw model output
library(stringr)
outputs <- list.files("./data_dsicap/ref/Model Results")
accuracy_files <- outputs[str_detect(outputs,"accuracy")]
model_files <- outputs[str_detect(outputs,"output")]

# create table for accuracies
accuracy_table <- NULL

for(i in 1:length(accuracy_files))
{
  temp_csv <- read.csv(paste0("./data_dsicap/ref/Model Results/",accuracy_files[i]))
  accuracy_table <- rbind(accuracy_table,temp_csv[,2])
}

rownames(accuracy_table) <- accuracy_files
colnames(accuracy_table) <- as.character(temp_csv[,3])
accuracy_table <- as.data.frame(accuracy_table)

accuracy_table$average <- rowMeans(accuracy_table[,c("ANN","RF","SVM")])


# create table for MSE
MSE_table <- NULL

for(i in 1:length(model_files))
{
  temp_csv <- read.csv(paste0("./data_dsicap/ref/Model Results/",model_files[i]))
  MSEs <- c(mean((temp_csv$Actual - temp_csv$ANN)^2),mean((temp_csv$Actual - temp_csv$OL)^2),
            mean((temp_csv$Actual - temp_csv$RF)^2),mean((temp_csv$Actual - temp_csv$SVM)^2))
  MSE_table <- rbind(MSE_table,MSEs)
}

rownames(MSE_table) <- model_files
colnames(MSE_table) <- colnames(accuracy_table[1:4]) 
MSE_table <- as.data.frame(MSE_table)

MSE_table$average <- rowMeans(MSE_table[,c("ANN","RF","SVM")])


# create confusion matricies for models
for(i in 1:length(model_files))
{
  temp_csv <- read.csv(paste0("./data_dsicap/ref/Model Results/",model_files[i]))

  for(j in 1:nrow(temp_csv))
    { 
    temp_csv[j,c(3)] <- ifelse((abs(temp_csv[j,c(2)]-temp_csv[j,c(3)]))<=1,temp_csv[j,c(2)],round(temp_csv[j,c(3)]))
    temp_csv[j,c(5)] <- ifelse((abs(temp_csv[j,c(2)]-temp_csv[j,c(5)]))<=1,temp_csv[j,c(2)],round(temp_csv[j,c(5)]))
    temp_csv[j,c(6)] <- ifelse((abs(temp_csv[j,c(2)]-temp_csv[j,c(6)]))<=1,temp_csv[j,c(2)],round(temp_csv[j,c(6)]))
  }
  
  for(k in c(colnames(temp_csv)[c(3,5,6)]))
  {
    temp_table <- as.data.frame.matrix(table(temp_csv$Actual, temp_csv[,c(k)]))

    for(l in 1:8)
    {
      if(!l %in% colnames(temp_table))
      {
        temp_table <- cbind(temp_table,l=rep(0,nrow(temp_table)))
        colnames(temp_table)[length(colnames(temp_table))] <- as.character(l)
       
      }
    }
    
    temp_table <- temp_table[,order(names(temp_table))]
    temp_table <- rbind(temp_table,"5"=rep(0,ncol(temp_table)))
    temp_table <- temp_table[order(rownames(temp_table)),]
    write.csv(temp_table,paste0("./data_dsicap/ref/Model Results/ConfusionMatrix/",str_split(model_files[i],".csv")[[1]][1]," - ", k,".csv"))
  }
  
}


# create heat maps
library(ggplot2)
library(reshape2)
library(scales)
temp_table <-  read.csv(paste0("./data_dsicap/ref/Model Results/ConfusionMatrix/model_output-optimal model - RF.csv"))
temp_table <- temp_table[,-1]
plot_table <- as.data.frame(lapply(temp_table, function(x) x/rowSums(temp_table)))
colnames(plot_table) <- seq(1:8)
plot_table[5,] <- rep(0,ncol(plot_table))
plot_table <- plot_table[rev(order(rownames(plot_table))),]
plot_table_melt <- melt(as.matrix(plot_table))
colnames(plot_table_melt) <- c("Actual","Predicted","Density")
ggplot(data = plot_table_melt, aes(x=Actual, y=Predicted, fill=Density)) + geom_tile() + scale_fill_gradient(low="#0066CC", high="#FF6633", labels = percent) + labs(title = "Prediction Density by Class", x="Ground Truth Linguistic Flexibility", y="Predicted Linguistic Flexibility")+ theme(axis.text=element_text(size=16),axis.title=element_text(size=16),plot.title=element_text(size=16,face="bold"),axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)),legend.text = element_text(size = 12),legend.key.size = unit(2.5, "lines"),legend.title=element_text(size=14))

temp_table <-  read.csv(paste0("./data_dsicap/ref/Model Results/ConfusionMatrix/model_output-Full model-Semantic - RF.csv"))
temp_table <- temp_table[,-1]
plot_table <- as.data.frame(lapply(temp_table, function(x) x/rowSums(temp_table)))
colnames(plot_table) <- seq(1:8)
plot_table[5,] <- rep(0,ncol(plot_table))
plot_table <- plot_table[rev(order(rownames(plot_table))),]
plot_table_melt <- melt(as.matrix(plot_table))
colnames(plot_table_melt) <- c("Actual","Predicted","Density")
ggplot(data = plot_table_melt, aes(x=Actual, y=Predicted, fill=Density)) + geom_tile() + scale_fill_gradient(low="#0066CC", high="#FF6633", labels = percent) + labs(x="Ground Truth Linguistic Flexibility", y="Predicted Linguistic Flexibility")+ theme(axis.text=element_text(size=30),axis.title=element_text(size=27),plot.title=element_text(size=24,face="bold"),axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)),legend.text = element_text(size = 24),legend.key.size = unit(3.5, "lines"),legend.title=element_text(size=27))
#title = "Prediction Density by Class",

temp_table <-  read.csv(paste0("./data_dsicap/ref/Model Results/ConfusionMatrix/model_output-Full model-Performative - RF.csv"))
temp_table <- temp_table[,-1]
plot_table <- as.data.frame(lapply(temp_table, function(x) x/rowSums(temp_table)))
colnames(plot_table) <- seq(1:8)
plot_table[5,] <- rep(0,ncol(plot_table))
plot_table <- plot_table[rev(order(rownames(plot_table))),]
plot_table_melt <- melt(as.matrix(plot_table))
colnames(plot_table_melt) <- c("Actual","Predicted","Density")
ggplot(data = plot_table_melt, aes(x=Actual, y=Predicted, fill=Density)) + geom_tile() + scale_fill_gradient(low="#0066CC", high="#FF6633", labels = percent) + labs(x="Ground Truth Linguistic Flexibility", y="Predicted Linguistic Flexibility")+ theme(axis.text=element_text(size=30),axis.title=element_text(size=27),plot.title=element_text(size=24,face="bold"),axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)),legend.text = element_text(size = 24),legend.key.size = unit(3.5, "lines"),legend.title=element_text(size=27))
