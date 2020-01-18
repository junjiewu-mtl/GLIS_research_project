## Junjie Wu  
## China Survey Analysis

rm(list = ls())

pacman::p_load(data.table, bit64, openxlsx, haven, dplyr, corrplot, zoo, 
               matrixStats, plotly, DAAG,PerformanceAnalytics, 
               BiocManager, ISLR, tree, rpart,rpart.plot)

setwd('~/Desktop/GLIS Research/GLIS_research_project/data')

###################### missing value imputation #####################
# dt <- fread('cleaned_01_16_cn.csv')
# #knn
# if(exists(".Random.seed")) rm(.Random.seed)
# 
# dt_imputed <- impute.knn(as.matrix(dt[,c(2:30, 38:69, 71)]))
# dt_q30 <- dt[,c(1,31:37)]
# 
# dt_imputed <- data.table(dt_imputed$data)
# dt_imputed$V1 <- seq.int(nrow(dt_imputed))
# 
# dt_merge <- merge(dt_imputed, dt_q30, by = 'V1')
# 
# dt_merged <- dt_merge[,c(1:30,64:70,31:63)]
# 
write.csv(dt_merged, 'knn_imputed_cn_data.csv', row.names = F)

# ####################################################################
dt <- fread('knn_imputed_cn_data.csv')[,-1]

dt_csv_mean <- sapply(dt[,37:56], mean, na.rm=TRUE)


View(sort(dt_csv_mean, decreasing = T))


# sustainable value
dt[, `:=`(sus_val = rowSums(.SD, na.rm=T)), .SDcols=c(42,51,52,53,50,54,55,56,57)]

# aesthetic values
dt[, aes_val := rowSums(.SD, na.rm=T), .SDcols=c(43:44)]

#Green Consciousness
dt[, green_con := rowSums(.SD, na.rm=T), .SDcols=c(2:6)]

#Consumer Innovativeness
dt[, con_inno := rowSums(.SD, na.rm=T), .SDcols=c(51:57, 59:63)]

################### scatter plot function: ggplotRegression ################### 
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

#descriptive analysis

dt_mean <- sapply(dt[,38:57], mean, na.rm=TRUE)
sort(dt_mean)
################################################################################ 


######################### Q1 ###############################

dt_1 <- dt
dt_a <- dt[, c(65,37,39,40,42,43,51,52,53,50,54,55,56,57)]

dt_a <- dt_a[Q41.Sex < 3,]
dt_a$Q41.Sex <- as.factor(ifelse(dt_a$Q41.Sex<=1, "Male", "Female"))

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dt_a))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dt_a)), size = smp_size)

train <- dt_a[train_ind, ]
test <- dt_a[-train_ind, ]

# dt_a <- data.table(dt_a %>% mutate_if(is.numeric,as.factor))

tree.dt_a = tree(Q41.Sex~., data=train)

summary(tree.dt_a)

# regression tree

rtree.dt_a = rpart(Q41.Sex ~ ., data=train, method = 'class', parms = list(split = "information"))
# Plot the tree using prp command defined in rpart.plot package
prp(rtree.dt_a)

t = test['Q41.Sex']

t_pred = predict(rtree.dt_a,test,type="class")

confMat <- table(test$Q41.Sex,t_pred)

accuracy = sum((as.character(t_pred)) == t)/nrow(t)

summary(rtree.dt_a)


plot(tree.dt_a)
text(tree.dt_a, pretty = 0)

tree.pred = predict(tree.dt_a, dt_a[-train_ind, ], type="class")

with(dt_a[-train_ind,], table(tree.pred, Q41.Sex))

# 
# missmap(dt, main = Missing values vs observed)
# 
# #drop NA 
# dt_a <- data.table(dt_a[complete.cases(dt_a),])
# 
# dt_a <- dt_a[Q41.Sex < 3]
# dt_a[, Sex := Q41.Sex - 1]
# 
# correlations <- cor(dt_a[,1:7])
# corrplot(correlations, method=circle)
# 
# 
# ## 75% of the sample size
# smp_size <- floor(0.75 * nrow(dt_a))
# 
# ## set the seed to make your partition reproducible
# set.seed(123)
# train_ind <- sample(seq_len(nrow(dt_a)), size = smp_size)
# 
# train <- dt_a[train_ind, ]
# test <- dt_a[-train_ind, ]
# 
# # dt_a <- lapply(dt_a, as.factor)
# # dt_a[Q41.Sex] <- as.numeric(dt_a[Q41.Sex])
# 
# mylogit <- glm(Sex ~ Q31a.Fit + Q31c.Fibre.Material + Q31d.Quality.Workmanship + Q31f.Colour + Q31g.Style, data = train, family = binomial)
# 
# summary(mylogit)
# 
# fitted.results <- predict(mylogit,newdata=subset(test,select=c(2,3,4,5,6,7)),type='response')
# fitted.results <- ifelse(fitted.results > 0.5,1,0)
# 
# misClasificError <- mean(fitted.results != test$Sex)
# print(paste('Accuracy',1-misClasificError))
# 
# 
# confint(mylogit)

data(package="ISLR")

carseats <- Carseats

View(head(carseats))

hist(carseats$Sales)

High = ifelse(carseats$Sales<=8, "No", "Yes")
carseats = data.frame(carseats, High)

tree.carseats = tree(High~.-Sales, data=carseats)

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)

######################### Q2 ###############################

dt_2 <- dt
dt_2[, sus_aes := sus_val+aes_val]

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dt_2))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dt_2)), size = smp_size)

train <- dt_2[train_ind, ]
test <- dt_2[-train_ind, ]

linear_2 <- lm(con_inno ~ sus_aes, data = train)
summary(linear_2)

p_dt <- predict(linear_2, test)

actuals_preds <- data.frame(cbind(actuals=test$con_inno, predicteds=p_dt))
correlation_accuracy <- cor(actuals_preds) 
head(actuals_preds)

ggplotRegression(linear_2)



######################### Q3 ###############################

dt_3 <- dt
## 75% of the sample size
smp_size <- floor(0.75 * nrow(dt_3))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dt_3)), size = smp_size)

train <- dt_3[train_ind, ]
test <- dt_3[-train_ind, ]

liner_3 <- lm(green_con ~ sus_val, data = dt_3)
summary(liner_3)


ggplotly(ggplotRegression(liner_3))

p_dt <- predict(liner_3, test)

actuals_preds <- data.frame(cbind(actuals=test$green_con, predicteds=p_dt))
correlation_accuracy <- cor(actuals_preds) 
head(actuals_preds)

# train_eva <- train[,c(65:69)]
# 
# chart.Correlation(train_eva,
#                   method="pearson",
#                   histogram=TRUE,
#                   pch=16)


plot_ly(data = dt_3, x = ~green_con, y = ~sus_val)

