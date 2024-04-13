library(caret)
library(xgboost)
library(readxl)
library(Matrix)
library(tidyverse)
library(NbClust)
library(reshape2)
library(randomcoloR)
library(factoextra)


db <- read_excel("data_base_RF.xlsx")
db1 <- db[,6:30]

s <- createDataPartition(db1$YLDY, p = 0.7, list=FALSE)

training <- db1[s,]

test <- db1[-s,]
names(test)

train.outcome <- training$YLDY

train.predictors <- sparse.model.matrix(YLDY ~ .,
                                        data = training
)[, -1]

test.outcome <- test$YLDY

test.predictors <- model.matrix(YLDY ~ .,
                                data = test
)[, -1]


#Convert the matrix objects to DMatrix objects

dtrain <- xgb.DMatrix(train.predictors, label=train.outcome)

dtest <- xgb.DMatrix(test.predictors)

# Train the model
model <- xgboost(
  data = dtrain, max_depth = 20, eta = 1, 
  nthread = 2, nrounds = 500,
  objective = "reg:linear")

# Test the model
pred <- predict(model, dtest)

# Evaluate the performance of model
RMSE(pred,test.outcome)
MAE(pred, test.outcome)

# Examine feature importance
importance_matrix <- xgb.importance(model = model)

tibble(importance_matrix)


vi <- head(importance_matrix, 10)

fig1 <- vi %>%
  ggplot() +
  aes(x = reorder(Feature, Gain), y = Gain, fill = Feature) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  coord_flip() +
  theme_light() + 
  labs(x = "Climate factor importance", 
       fill = "CFI")

fig1

# Tree 1
xgb.plot.tree(model = model, tree=0)


db
db$Season <- as.factor(db$Season)
db$Department <- as.factor(db$Department)
db$Municipality <- as.factor(db$Municipality)

glimpse(db)


############## GAM #######################

library(mgcv)

db3 <- db %>% select(Season, Department, Municipality, NHND, PARM, TREA, TIWA, 
                     YLDY)

m <- gam(YLDY ~ s(NHND, bs = "re") + s(PARM, bs = "re") + te(NHND, PARM), # formuala describing model
             data = db3,           # your data
             method = 'REML',                # or 'ML'
             family = gaussian)              # or something more exotic



# NHND PARM TREA TIWA 

plot(m, rug = TRUE)



db4 <- db3 %>% melt()


db4 %>%
 filter(!(Municipality %in% "Agrado")) %>%
 filter(variable %in% "NHND") %>%
 ggplot() +
 aes(x = Season, y = value, fill = Season) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 labs(x = "Season", y = "NHND (Days)") +
 theme_linedraw() +
 facet_wrap(vars(Municipality), ncol = 4L)

names(db3)

db4 %>%
  filter(!(Municipality %in% "Agrado")) %>%
  filter(variable %in% "PARM") %>%
  ggplot() +
  aes(x = Season, y = value, fill = Season) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  labs(x = "Season", y = "PAR (Mj/m2/dia)") +
  theme_linedraw() +
  facet_wrap(vars(Municipality), ncol = 4L)

########### Grupos de estres luminico - termico ##########################


db5 <- db3 %>% select(NHND, PARM, YLDY)

res.nbclust <- NbClust(db5, distance = "euclidean",
                       min.nc = 2, max.nc = 9, 
                       method = "complete", index ="all")

aa <- as.data.frame(res.nbclust$Best.nc)
c <- data.frame(index_v = c("Cluster", "metric"))
aa <- cbind(c,aa)
aa <- aa %>% gather("index", value = "value", KL:SDbw, -index_v) 
a_clus <- aa %>% filter(index_v == "Cluster")
a_clus$value <- as.factor(a_clus$value)


cluster_number <- a_clus  %>%
  ggplot() +
  aes(x = index, y = value, fill = value) +
  geom_tile(size = 1.2) +
  scale_fill_manual(values = c(`0` = "#F8766D", 
                               `1` = "#C99500", `2` = "#69AE0F", `3` = "#00BC64", `4` = "#00BCC5", `7` = "#45A4F6", `8` = "#C978FB", 
                               `9` = "#FF61C3")) +
  labs(x = "Cluster number prediction index", y = "Cluster number", fill = "Cluster number") +
  theme_gray() + theme(legend.position = "bottom",
                       axis.text.x = element_text(angle = 90, 
                                                  vjust = 0.5, 
                                                  hjust=1))


km.res <- kmeans(scale(db5), 4, nstart = 1000)

n <- 4
palette <- distinctColorPalette(n)

clus <- fviz_cluster(km.res, data = db5,
                        palette = palette,
                        ellipse.type = "euclid", # Concentration ellipse
                        star.plot = TRUE, # Add segments from centroids to items
                        repel = TRUE, # Avoid label overplotting (slow)
                        ggtheme = theme_minimal(),
                        labelsize = 0
)



cluster <-  data.frame(HLS= km.res$cluster)
dim(cluster)
dim(db3)
names(db3)

df1 <- cbind(db3, cluster)

df1$HLS <- as.factor(df1$HLS)
names(db5)

db6 <- db5 %>% select(YLDY) 

km.res_yield <- kmeans(scale(db6), 3, nstart = 1000)



n <- 3
palette <- distinctColorPalette(n)

clus_yield <- fviz_cluster(km.res_yield, data = db5,
                     palette = palette,
                     #ellipse.type = "euclid", # Concentration ellipse
                     star.plot = TRUE, # Add segments from centroids to items
                     repel = TRUE, # Avoid label overplotting (slow)
                     ggtheme = theme_minimal(),
                     labelsize = 0
)

clus_yield

cluster_yield <-  data.frame(YieldC= km.res_yield$cluster)
dim(cluster_yield)

df2 <- cbind(df1, cluster_yield)
df2$YieldC <- as.factor(df2$YieldC)

db1

¿
