# A script of helper functions for PCA
library("biclust") 
library("fmsb")
library("viridis")
library("ggplot2")
library("purrr")
library("plyr")
library("randomForest")
library('heatmap3')


TwoWayclust <- function(Data1, group_thresh = 1.5, variable_thresh = 1.5, outputdir) {
  #takes in a csv dataset, cleans it up, standardizes it, and calculates the twowayclust on the data at the threshold defined by the user
  #saves the heatmap, and outputs the mycl for the group differences function
  
  #change so that if variable is already loaded into workspace the can call it
  #if variable is a path then do the following, else just set Clusterset equal to the dataset_path variable
  
  # 3.1 Euclidean + Ward Clustering of Subjects
  r_dist <- dist(Data1, method = "euclidean")
  hr <- hclust(r_dist, method = "ward.D2");
  #r_dist <- dist(Data1, method = "binary")
  #hr <- hclust(r_dist, method = "complete");
  
  # 3.2 Spearman + Complete Clustering of Variables
  c_dist <- dist(t(Data1), method = "euclidean")
  hc <- hclust(c_dist, method = "ward.D2")
  #c_dist <- dist(t(Data1), method = "binary")
  #hc <- hclust(c_dist, method = "complete")
  
  # 4 Subject Group Assignment
  Sub_Group <- cutree(hr, h = max(hr$height)/group_thresh)
  mycolhr <- rainbow(length(unique(Sub_Group)), start = 0.1, end = 0.9); 
  mycolhr <- mycolhr[as.vector(Sub_Group)]
  
  # 5 Variable Group Assignment
  Var_Group <- cutree(hc, h = max(hc$height)/variable_thresh)
  mycolhc <- rainbow(length(unique(Var_Group)), start = 0.1, end = 0.9); 
  mycolhc <- mycolhc[as.vector(Var_Group)]
  
  # 6 Visualization
  h<-heatmap3(Data1, Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = inferno(256), scale = "none", RowSideColors = mycolhr, 
              ColSideColors = mycolhc, cexRow = 1, file = 'heatmap3.pdf',
              ColSideLabs = '', RowSideLabs = '')
  
  #Action- Figure out how to export the visualization to disk-
  #Action- Figure out how to save all relevant variables- Var-Group, Sub_group, mycolhr, hr, hc
  #Returns a list with the heatmap object + variable group assignment
  return(list(h, mycolhc, mycolhr))
}

####################################
#One hot encode qualitative factors#
####################################
#qual <- [SOME DATA FRAME OF ***ONLY*** FACTOR VARIABLES]


#Define function to one-hot encode a raw factor
OneHotEnc <- function(varname, data) {
  levels = levels(data[,varname])
  recode_df = list()
  for (i in levels) { #rename the new variable: [ITEM]_is_[VALUE], and code so that 1 is true, 0 is false.
    recode_df[[paste0(varname, "_is_", i)]] <- ifelse(data[,varname] == i, 1, 0)
  }
  return_df <- as.data.frame(recode_df)
  return(return_df)
}



