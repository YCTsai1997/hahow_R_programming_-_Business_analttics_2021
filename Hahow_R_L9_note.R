
setwd("~/My files/001_Programming Related/R")

library(tidyverse)
library(knitr)
library(reshape2)
library(ggfortify)

# Import file ----------------------------------------
GameLog <- read_csv("L9_Dataset/Game_Log.csv")
UserTable <- read_csv("L9_Dataset/User_Table.csv")

#Check datatype, and change type if needed ----------------------------------------
str(GameLog)

GameLog <- GameLog %>% 
    mutate(User_Id = as.factor(User_Id))

## Need to use lapply! (Check laaply meaning!)
str(UserTable)
UserTable <- as.data.frame(lapply(UserTable, as.factor))


kable(GameLog[1:6,])
kable(UserTable[1:6,])

# Merge table ----------------------------------------

#Check distinct valeus and understand the data structure 
print(paste("distinct count:", n_distinct(UserTable$User_Id), " ,total rows: ", length(UserTable$User_Id)))
print(paste("distinct count:", n_distinct(GameLog$User_Id), " ,total rows: ", length(GameLog$User_Id)))

#Due to Gamelog is one to many, so we take mean for each user
GameTable <- GameLog %>% 
    group_by(User_Id) %>%
    summarise(
        Min_Aft = mean(Min_Aft),
        Min_Eve = mean(Min_Eve),
        Min_Mid = mean(Min_Mid),
        Buy_Coin = mean(Buy_Coin),
        Buy_Dia = mean(Buy_Dia),
        Buy_Car = mean(Buy_Car)
    ) %>%
    inner_join(UserTable, by="User_Id")

# Pre data for modeling - numeric ----------------------------------------

# Check the data range 
summary(GameTable)

# Normalize variable for clustering
min_max_norm <- function (x){
    return ((x-min(x))/(max(x)-min(x)))
}


GameTable_norm <- GameTable %>%
    mutate(
        Aft = min_max_norm(Min_Aft),
        Eve = min_max_norm(Min_Eve),
        Mid = min_max_norm(Min_Mid),
        Coin = min_max_norm(Buy_Coin),
        Dia = min_max_norm(Buy_Dia),
        Car = min_max_norm(Buy_Car)
    ) %>%
    relocate(c("Identity","Telecom"), .after = last_col())

kable(GameTable_norm[1:5,])


# Pre data for modeling - categorical variables ----------------------------------------

# check the combinations of two categorical variable
GameTable_norm %>% group_by(Identity, Telecom) %>% count()

# Dummy coding
DummyTable <- model.matrix( ~ Identity + Telecom, data = GameTable_norm)
kable(DummyTable[1:5,])

# combined all the data prep back to a final table ----------------------------------------

GameTable_F <- cbind(
    GameTable_norm[, -c(1,2:7,14,15)],
    DummyTable[,-1]
)

kable(GameTable_F[1:5,])


# DEA - Correlation matrix ----------------------------------------
CorMatrix <- GameTable_F %>%
    cor() %>%
    melt()

kable(CorMatrix[1:5,])

ggplot(data = CorMatrix) +
    geom_tile(aes(Var1, Var2, fill=value), colour="White") +
    scale_fill_gradient2(low="firebrick4", high = "steelblue") +
    guides(fill = guide_legend(title="Correlation")) +
    theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1),
          axis.title = element_blank())


# Hierarchy clustering: Help to identified the nu,ber of groups ----------------------------------------

set.seed(500)
Distance <- dist(GameTable_F, method = "euclidean")
hclust(Distance, method = "complete") %>%
    plot()
# Based on the plot result, we know that we can clustert data into 3 or 4 groups


# Grouping - Kmeans() in 3 groups----------------------------------------

set.seed(500) # Remove random effect 
K3 <- kmeans(GameTable_F,3) 

ClusterResult3 <- cbind(
    GameTable_F, 
    Cluster = K3$cluster
) %>% as.data.frame()

kable(ClusterResult3[1:5,])

table(ClusterResult3$Cluster)


# Kmeans(): continuous variable ----------------------------------------
ClusterResultForPlot3 <- ClusterResult3 %>%
    gather(
        key = "Continuous_Variable",
        value = "Normalized_Value",
        1:6
    ) %>%
    mutate(Continuous_Variable = factor(Continuous_Variable, levels = c('Aft','Eve','Mid','Coin','Dia','Car')))


ggplot(data = ClusterResultForPlot3, 
       aes(x=Continuous_Variable, y= Normalized_Value),
       size = 0.7) +
    geom_boxplot() +
    facet_wrap(~Cluster)


# Grouping - Kmeans() in 4 groups----------------------------------------

set.seed(500) # Remove random effect 
K4 <- kmeans(GameTable_F,4) 

ClusterResult4 <- cbind(
    GameTable_F, 
    Cluster = K4$cluster
) %>% as.data.frame()


table(ClusterResult4$Cluster)


# Kmeans(): continuous variable ----------------------------------------
ClusterResultForPlot4 <- ClusterResult4 %>%
    gather(
        key = "Continuous_Variable",
        value = "Normalized_Value",
        1:6
    ) %>%
    mutate(Continuous_Variable = factor(Continuous_Variable, levels = c('Aft','Eve','Mid','Coin','Dia','Car')))


ggplot(data = ClusterResultForPlot4, 
       aes(x=Continuous_Variable, y= Normalized_Value),
       size = 0.7) +
    geom_boxplot() +
    facet_wrap(~Cluster)

# categorical variable: Identity type ----------------------------------------

GameTable_Results <- GameTable %>%
    cbind(K4$cluster) %>%
    as.data.frame()

colnames(GameTable_Results)[ncol(GameTable_Results)] <- "Cluster"

ggplot(data= GameTable_Results) +
    geom_bar(aes(x=Identity)) +
    facet_wrap(~ Cluster)


# categorical variable: Telecom type ----------------------------------------

ggplot(data= GameTable_Results) +
    geom_bar(aes(x=Telecom)) +
    facet_wrap(~ Cluster)



# PCA for visualizing grouping----------------------------------------

set.seed(500)
autoplot(kmeans(GameTable_F[,1:6], 4), data  = GameTable_F)


# Suggestions ----------------------------------------
