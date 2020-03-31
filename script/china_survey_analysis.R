## Junjie Wu  
## China Survey Analysis

rm(list = ls())

pacman::p_load(data.table, bit64, openxlsx, haven, dplyr, corrplot, zoo, 
               matrixStats, plotly, DAAG,PerformanceAnalytics, 
               BiocManager, ISLR, tree, rpart,rpart.plot, arsenal, rattle,
               RColorBrewer, statsr, tidyverse, arules)

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
# write.csv(dt_merged, 'knn_imputed_cn_data.csv', row.names = F)

####################################################################
dt <- read.csv('knn_imputed_cn_data.csv')[,-1]

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

dt <- data.table(round_df(dt, 0))

#########################  Aggregate Updated Cues #########################  
# directly product cues (1-10)
dt[, `:=`(product_cues_1_10 = rowSums(.SD, na.rm=T)), .SDcols=c(38:43, 47:49)]

# indirectly product cues (11-13)
dt[, `:=`(product_cues_11_13 = rowSums(.SD, na.rm=T)), .SDcols=c(44:46)]

# production-related sustainable cues (item a-g)
dt[, `:=`(production_cues_a_g = rowSums(.SD, na.rm=T)), .SDcols=c(50:56)]

# product-related sustainable cues (item 8-10)
dt[, `:=`(product_cues_8_10 = rowSums(.SD, na.rm=T)), .SDcols=c(41, 48, 49)]

# product-related sustainable cues (item 1-7 and 11-13)
dt[, `:=`(product_cues_8_10 = rowSums(.SD, na.rm=T)), .SDcols=c(38:40, 42:47)]

# aesthetic aspect
dt[, aesthetic_aspect := rowSums(.SD, na.rm=T), .SDcols=c(42:43)]

# functional aspect
dt[, functional_aspect := rowSums(.SD, na.rm=T), .SDcols=c(38, 47)]

# sustainable commitment
dt[, sustainable_commitment := rowSums(.SD, na.rm=T), .SDcols=c(9, 13:18, 20, 28)]

# fashion innovativeness
dt[, fashion_innovativeness := rowSums(.SD, na.rm=T), .SDcols=c(57:62)]

######################### scatter plot function: ggplotRegression ######################### 
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

summary(dt)
sort(dt_mean)

table_one <- tableby(~.,dt)
summary(table_one,text=TRUE)
#write.table(table_one, '../script/cn_preliminary_analysis/cn_h1.csv')

################################################################################ 


######################### Q1 ###############################

dt_individual <- dt[, c(65,41,49:56,42:43)]
dt_individual <- dt_individual[Q41.Sex < 3,]
dt_individual$Q41.Sex <- as.factor(ifelse(dt_individual$Q41.Sex<=1, "Male", "Female"))
#write.csv(dt_individual, '../script/cn_preliminary_analysis/cn_h1_ind.csv', row.names = F)

dt_aes_sus  <- dt[, c(65,70,71)]
dt_aes_sus <- dt_aes_sus[Q41.Sex < 3,]
dt_aes_sus$Q41.Sex <- as.factor(ifelse(dt_aes_sus$Q41.Sex<=1, "Male", "Female"))
#write.csv(dt_aes_sus, '../script/cn_preliminary_analysis/cn_h1_sum.csv', row.names = F)

########## Bayesian inference for two independent means ##########
dt_a_s <- dt[, c(65,70,71)]
dt_a_s[, score_sum := aes_val + sus_val]
dt_a_s[Q41.Sex <1.5, Q41.Sex:=1]
dt_a_s[Q41.Sex >= 1.5, Q41.Sex:=2]
dt_a_s$Q41.Sex <- as.factor(dt_a_s$Q41.Sex)
#write.csv(dt_a_s, '../script/cn_preliminary_analysis/cn_h1_bae.csv', row.names = F)

ggplot(dt_a_s, aes(x = Q41.Sex, y = score_sum)) +
  geom_boxplot()

score_p <- bayes_inference(y = score_sum, x = Q41.Sex, data = dt_a_s, 
                statistic = "mean", 
                type = "ht", alternative = "twosided", null = 0, 
                prior = "JZS", rscale = 1, 
                method = "theoretical")

summary(score_p)

score_post <- bayes_inference(y = score_sum, x = Q41.Sex, data = dt_a_s, 
                              statistic = "mean", 
                              type = "ci", mu_0 = 0, 
                              prior = "JZS", rscale = 1, 
                              method = "simulation")


########### Prediction using MCMC ########### 

#predict mean sum of male
dt_male <- dt_a_s[Q41.Sex == 1,]

male_post <- bayes_inference(y = score_sum, data = dt_male, 
                              statistic = "mean", 
                              type = "ci", mu_0 = 43.9, 
                              prior = "JZS", rscale = 1, 
                              method = "simulation")

m_samples = as.data.frame(male_post$samples)
m_nsim = nrow(m_samples)
m_samples = mutate(m_samples, y_pred = rnorm(m_nsim, mu, sqrt(sig2)))

ggplot(data = m_samples, aes(x = y_pred)) + 
  geom_histogram(aes(y = ..density..), bins = 100) +
  geom_density() + 
  xlab(expression(y[new]))

dplyr::select(m_samples, mu, y_pred) %>%
  map(quantile, probs=c(0.025, 0.1, 0.50, 0.9, 0.975))

#predict mean sum of female
dt_female <- dt_a_s[Q41.Sex == 2,]

female_post <- bayes_inference(y = score_sum, data = dt_female, 
                             statistic = "mean", 
                             type = "ci", mu_0 = 43.9, 
                             prior = "JZS", rscale = 1, 
                             method = "simulation")

f_samples = as.data.frame(female_post$samples)
f_nsim = nrow(f_samples)
f_samples = mutate(f_samples, y_pred = rnorm(f_nsim, mu, sqrt(sig2)))

ggplot(data = f_samples, aes(x = y_pred)) + 
  geom_histogram(aes(y = ..density..), bins = 100) +
  geom_density() + 
  xlab(expression(y[new]))

dplyr::select(f_samples, mu, y_pred) %>%
  map(quantile, probs=c(0.025, 0.1, 0.50, 0.9, 0.975))




###################### Apriori ###################### 

dt_ap <- dt
### Association rules between appareal cues lables and demographic labels ###

#Catagorized value into 3 levels
# sustainable value
dt_ap[,sus_val:= cut(sus_val,3,include.lowest=TRUE,
                             labels=c("Low", "Middle", "High"))]
# aesthetic values
dt_ap[,aes_val:= cut(aes_val,3,include.lowest=TRUE,
                   labels=c("Low", "Middle", "High"))]
#Green Consciousness
dt_ap[,green_con:= cut(green_con,3,include.lowest=TRUE,
                   labels=c("Low", "Middle", "High"))]
#Consumer Innovativeness
dt_ap[,con_inno:= cut(con_inno,3,include.lowest=TRUE,
                   labels=c("Low", "Middle", "High"))]

#Social/Ethical
dt_ap[,soc_eth:= cut(soc_eth,3,include.lowest=TRUE,
                      labels=c("Low", "Middle", "High"))]
#Aes/fun
dt_ap[,aes_fun:= cut(aes_fun,3,include.lowest=TRUE,
                      labels=c("Low", "Middle", "High"))]
#functional
dt_ap[,functional:= cut(functional,3,include.lowest=TRUE,
                      labels=c("Low", "Middle", "High"))]
#symbolic
dt_ap[,symbolic:= cut(symbolic,3,include.lowest=TRUE,
                      labels=c("Low", "Middle", "High"))]
# financial
dt_ap[,financial:= as.factor(ifelse(dt_ap[,"financial"]>3, "High", "Low"))]

# Selected label value 
dt_ap <- dt_ap[,c(63:68,70:78)]
dt_ap <- dt_ap %>%
  mutate_if(is.numeric, as.factor)

#drop edu lev, age, employ
dt_ap <- dt_ap[,-c(64,67,68)]

rules <- apriori(dt_ap,
                 parameter = list(supp = 0.5, conf = 0.3, maxlen = 4))

inspect((sort(rules, by = "confidence"))[1:20])


top10subRules <- head(rules, n = 10, by = "confidence")

plot(top10subRules, method = "graph",  engine = "htmlwidget")



subset.rules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subset.rules)  #> 3913



dt_ap$ID <- seq.int(nrow(dt_ap))

dt_ap <- dt_ap[,-c(30:36, 41:43, 49:56, 58:62, 1:5)]

dt_ap_cat <- data.frame(ifelse(dt_ap[,1:34]>2, "High", "Low"))

dt_ap_cat$ID <- seq.int(nrow(dt_ap_cat))

dtap <- merge(dt_ap[,35:46],dt_ap_cat, by = 'ID')

# drop 
dtap <- dtap %>%
  select(-ID, -total_score) %>% 
  mutate_if(is.numeric, as.factor)


rules <- apriori(dtap,
                 parameter = list(supp = 0.9, conf = 0.9),
                 appearance = list(default="lhs", rhs = c("Q41.Sex=Male")))









inspect(rules)[1:10]


inspect(head(sort(rules, by = "lift")))


#catagorized across gender
dt_aes_sus[,dt_aes_val:= cut(aes_val,3,include.lowest=TRUE,
                             labels=c("Low", "Middle","High"))]

dt_aes_sus[,dt_sus_val:= cut(sus_val,3,include.lowest=TRUE,
                             labels=c("Low", "Middle","High"))]

dt_aes_sus <- dt_aes_sus %>% dplyr::select(Q41.Sex, dt_sus_val, dt_aes_val)

# dt_in_factor <- dt_individual %>%
#   mutate_if(sapply(dt_individual, is.numeric), as.factor)


# df_m <- dt_aes_sus %>% filter(Q41.Sex == "Male") %>% 
#   select(dt_sus_val, dt_aes_val) 
# 
# df_f <- dt_aes_sus %>% filter(Q41.Sex == "Female") %>% 
#   select(dt_sus_val, dt_aes_val) 

dt_ap <- dt[,-(30:36)]

rules.male <- apriori(dt_aes_sus,
                      parameter = list(supp=0.1, conf=0.01),
                      appearance = list(default="lhs", rhs = c("Q41.Sex=Male")))

rules.female <- apriori(dt_aes_sus,
                        parameter = list(supp = 0.1, conf = 0.01),
                        appearance = list(default="lhs", rhs = c("Q41.Sex=Female")))


inspect(head(sort(rules.male, by = "confidence")))
inspect(head(sort(rules.female, by = "confidence")))

############# run apriori separately ############# 
df_m <- dt_aes_sus %>% select(Q41.Sex, dt_sus_val, dt_aes_val) %>% 
  filter(Q41.Sex == "Male") %>% 
  mutate_if(is.numeric,as.factor)

df_f <- dt_aes_sus %>% select(Q41.Sex, dt_sus_val, dt_aes_val) %>% 
  filter(Q41.Sex == "Female") %>% 
  mutate_if(is.numeric,as.factor)

rules.male <- apriori(df_m, control = list(verbose=F),
                      parameter = list(supp = 0.001, conf = 0.5),
                      appearance = list(default="lhs", rhs = c("Q41.Sex=Male")))

rules.female <- apriori(df_f, control = list(verbose=F),
                        parameter = list(supp = 0.001, conf = 0.5),
                        appearance = list(default="lhs", rhs = c("Q41.Sex=Female")))

inspect(head(sort(rules.male, by = "confidence")))
inspect(head(sort(rules.female, by = "confidence")))

####################### regression tree  ####################### 

dt_a <- dt_individual

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dt_a))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dt_a)), size = smp_size)

train <- dt_a[train_ind, ]
test <- dt_a[-train_ind, ]

rtree.dt_a = rpart(Q41.Sex ~ ., data=train, method = 'class',   minsplit = 2, 
                   minbucket = 1, 
                   cp = 0.008)

summary(rtree.dt_a)
# Plot the tree using prp command defined in rpart.plot package
fancyRpartPlot(rtree.dt_a)

t_pred = predict(rtree.dt_a,test,type="class")

confMat <- table(test$Q41.Sex,t_pred)
accuracy <- sum(diag(confMat))/sum(confMat)

mean(test$Q41.Sex == t_pred) 

summary(rtree.dt_a)

tree.dt_a = tree(Q41.Sex~., data=dt_a)

summary(tree.dt_a)

plot(tree.dt_a)
text(tree.dt_a, pretty = 0)

tree.pred = predict(tree.dt_a, dt_a[-train_ind, ], type="class")

with(dt_a[-train_ind,], table(tree.pred, Q41.Sex))

######################### Q2 ###############################

dt_2 <- dt
dt_2[, sus_aes := sus_val+aes_val]

dt_2 <- dt_2[,c(73,74)]

#write.csv(dt_2, '../script/cn_preliminary_analysis/cn_h2.csv', row.names = F)

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

dt_3 <- dt[,c(70,72)]

#write.csv(dt_3, '../script/cn_preliminary_analysis/cn_h3.csv', row.names = F)
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

