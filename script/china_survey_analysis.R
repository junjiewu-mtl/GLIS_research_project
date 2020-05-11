## Junjie Wu  
## China Survey Analysis

rm(list = ls())

pacman::p_load(data.table, bit64, openxlsx, haven, dplyr, corrplot, zoo, 
               matrixStats, plotly, DAAG,PerformanceAnalytics, 
               BiocManager, ISLR, tree, rpart,rpart.plot, arsenal, rattle,
               RColorBrewer, statsr, tidyverse, arules,arulesViz, OneR, rCBA)

setwd('/Users/junjiewu/OneDrive - McGill University/GLIS_Research/GLIS_research_project/data') 

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

#colnames(dt)

# directly product cues (1-10)
dt[, `:=`(product_cues_1_10 = rowSums(.SD, na.rm=T)), .SDcols=c(37:43, 47:49)]

# indirectly product cues (11-13)
dt[, `:=`(product_cues_11_13 = rowSums(.SD, na.rm=T)), .SDcols=c(44:46)]

#product cues (1-13)
dt[, `:=`(product_cues_1_13 = rowSums(.SD, na.rm=T)), .SDcols=c(37:49)]

# production-related sustainable cues (item a-g)
dt[, `:=`(production_cues_a_g = rowSums(.SD, na.rm=T)), .SDcols=c(50:56)]

# product-related sustainable cues (item 8-10)
dt[, `:=`(product_cues_8_10 = rowSums(.SD, na.rm=T)), .SDcols=c(41, 48, 49)]

# product-related sustainable cues (item 1-7 and 11-13)
dt[, `:=`(product_cues_1_7_11_13 = rowSums(.SD, na.rm=T)), .SDcols=c(37:40, 42:47)]

# aesthetic aspect
dt[, aesthetic_aspect := rowSums(.SD, na.rm=T), .SDcols=c(42:43)]

# functional aspect
dt[, functional_aspect := rowSums(.SD, na.rm=T), .SDcols=c(38, 47)]

# sustainable commitment
dt[, sustainable_commitment := rowSums(.SD, na.rm=T), .SDcols=c(9, 13:18, 20, 28)]

# fashion innovativeness
dt[, fashion_innovativeness := rowSums(.SD, na.rm=T), .SDcols=c(57:62)]
# cut at 21
dt$fashion_innovativeness[dt$fashion_innovativeness >= 21] <-  "high"
dt$fashion_innovativeness[dt$fashion_innovativeness != "high"] <-  "low"

# #get the cluster threshold
# dt[,c(1:29,37:63,70:78)] <- bin(dt[,c(1:29,37:63,70:78)], nbins = 2, method = c("clusters"))
# lapply(dt[,c(37:49)] , unique)

#Catagorized value into 2 levels
dt[,c(1:29,37:63,70:78)] <- bin(dt[,c(1:29,37:63,70:78)], label = c('low', 'high'), nbins = 2, method = c("clusters"))


#drop other gender
dt$Q41.Sex <- as.factor(ifelse(dt$Q41.Sex<=1, "Male", "Female"))
######################### scatter plot function: ggplotRegression ######################### 
# ggplotRegression <- function (fit) {
#   
#   require(ggplot2)
#   
#   ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
#     geom_point() +
#     stat_smooth(method = "lm", col = "red") +
#     labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
#                        "Intercept =",signif(fit$coef[[1]],5 ),
#                        " Slope =",signif(fit$coef[[2]], 5),
#                        " P =",signif(summary(fit)$coef[2,4], 5)))
# }
# 
# #descriptive analysis
# 
# dt_mean <- sapply(dt[,38:57], mean, na.rm=TRUE)
# 
# summary(dt)
# sort(dt_mean)
# 
# table_one <- tableby(~.,dt)
# summary(table_one,text=TRUE)
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

## FPgroth on gender

## from product clue ##

dt_fp_cue <- dt[,c(65,70:79)]

dt_fp_cue <- dt_fp_cue %>% mutate_if(is.character,as.factor)

trans_cue<- as(dt_fp_cue,"transactions")

# to gender

total_rules = rCBA::fpgrowth(trans_cue, support=0.1, confidence=0.1, maxLength=3, consequent="Q41.Sex", parallel=TRUE)

rules_gender_cue = data.frame(
  lhs = labels(lhs(total_rules)),
  rhs = labels(rhs(total_rules)), 
  total_rules@quality)

write.csv(rules_gender_cue,'/Users/junjiewu/OneDrive - McGill University/GLIS_Research/GLIS_research_project/script/cn_preliminary_analysis/rules_gender_cue.csv')

# to cue

total_rules = rCBA::fpgrowth(trans_cue, support=0.1, confidence=0.1, maxLength=3, consequent="fashion_innovativeness", parallel=TRUE)

rules_innov_cue = data.frame(
  lhs = labels(lhs(total_rules)),
  rhs = labels(rhs(total_rules)), 
  total_rules@quality)

write.csv(rules_innov_cue,'/Users/junjiewu/OneDrive - McGill University/GLIS_Research/GLIS_research_project/script/cn_preliminary_analysis/rules_innov_cue.csv')


## from all question ##

dt_fp_all<- dt[,-c(30:36,64,66:78)]

dt_fp_all <- dt_fp_all %>% mutate_if(is.character,as.factor)

trans_all<- as(dt_fp_all,"transactions")

# to gender
#set support to 0.1 or won't return any rule for male
total_rules = rCBA::fpgrowth(trans_all, support=0.1, confidence=0.1, maxLength=3, consequent="Q41.Sex", parallel=TRUE)

# save output as dataframe
rules_gender_all = data.frame(
  lhs = labels(lhs(total_rules)),
  rhs = labels(rhs(total_rules)), 
  total_rules@quality)

write.csv(rules_gender_all,'/Users/junjiewu/OneDrive - McGill University/GLIS_Research/GLIS_research_project/script/cn_preliminary_analysis/rules_gender_all.csv')

#to innov
#set support to 0.1 or won't return any rule for male
total_rules = rCBA::fpgrowth(trans_all, support=0.1, confidence=0.1, maxLength=3, consequent="fashion_innovativeness", parallel=TRUE)

# save output as dataframe
rules_gender_all = data.frame(
  lhs = labels(lhs(total_rules)),
  rhs = labels(rhs(total_rules)), 
  total_rules@quality)

write.csv(rules_gender_all,'/Users/junjiewu/OneDrive - McGill University/GLIS_Research/GLIS_research_project/script/cn_preliminary_analysis/rules_innov_all.csv')



dt_a <- dt

### Association rules between appareal cues lables and demographic labels ###

#General Question
dt_ag <- dt_a[,c(73:74)] 

rules_ag <- apriori(dt_ag,
                    parameter = list(supp = 0.1, conf = 0.1, maxlen=2))

inspect((sort(rules_ag, by = "lift"))[1:6])


#QA1
dt_a1 <- dt_a[,c(65,74)] 

rules_a1 <- apriori(dt_a1,
                 parameter = list(supp = 0.1, conf = 0.1, maxlen=2))

inspect((sort(rules_a1, by = "lift"))[1:10])
plotly_arules(rules_a1)
plot(rules_a1, method = "graph",  engine = "htmlwidget")

#QA2
dt_a2 <- dt_a[,c(65,77)] 

rules_a2 <- apriori(dt_a2,
                    parameter = list(supp = 0.1, conf = 0.1, maxlen=2))

inspect((sort(rules_a2, by = "lift"))[1:10])

#QA3
dt_a3 <- dt_a[,c(65,74)] 

rules_a3 <- apriori(dt_a3,
                    parameter = list(supp = 0.1, conf = 0.1, maxlen=2))

inspect((sort(rules_a3, by = "lift"))[1:10])


#QA4
dt_a4 <- dt_a[,c(37:49, 65)]

rules_a4 <- apriori(dt_a4,
                 parameter = list(supp = 0.1, conf = 0.5, maxlen=2),
                 appearance = list(default="rhs", lhs = c("Q41.Sex=Female")))

inspect((sort(rules_a4, by = "lift"))[1:3])

# plot(rules_a4, method = "graph",  engine = "htmlwidget")
# # Filter top 20 rules with highest lift
# subRules2<-head(rules_a4, n=20, by="lift")
# plot(subRules2, method="paracoord")

#QA5
dt_a5 <- dt_a[,c(37:49, 65)]

rules_a5 <- apriori(dt_a5,
                    parameter = list(supp = 0.1, conf = 0.5, maxlen=2),
                    appearance = list(default="rhs", lhs = c("Q41.Sex=Male")))

inspect((sort(rules_a5, by = "lift"))[1:3])

#QA6
dt_a6 <- dt_a[,c(50:56, 65)]

rules_a6 <- apriori(dt_a6,
                    parameter = list(supp = 0.1, conf = 0.5, maxlen=2),
                    appearance = list(default="rhs", lhs = c("Q41.Sex=Female")))

inspect((sort(rules_a6, by = "lift"))[1:3])

#QA7
dt_a7 <- dt_a[,c(50:56, 65)]

rules_a7 <- apriori(dt_a7,
                    parameter = list(supp = 0.1, conf = 0.5, maxlen=2),
                    appearance = list(default="rhs", lhs = c("Q41.Sex=Male")))

inspect((sort(rules_a7, by = "lift"))[1:3])

#
dt_b <- dt

#QB1
dt_b1 <- dt_b[,c(78, 65)]

rules_b1 <- apriori(dt_b1,
                    parameter = list(supp = 0.01, conf = 0.1, maxlen=2),
                    appearance = list(default="lhs", rhs = c("sustainable_commitment=high")))

inspect((sort(rules_b1, by = "lift"))[1:3])

#QB2
dt_b2 <- dt_b[,c(78,74,75)]

rules_b2 <- apriori(dt_b2,
                    parameter = list(supp = 0.01, conf = 0.4, maxlen=2),
                    appearance = list(default="rhs", lhs = c("sustainable_commitment=high")))

inspect((sort(rules_b2, by = "lift"))[1:2])

#QB3
dt_b3 <- dt_b[,c(78,70)]

rules_b3 <- apriori(dt_b3,
                    parameter = list(supp = 0.01, conf = 0.1, maxlen=2),
                    appearance = list(default="rhs", lhs = c("sustainable_commitment=high")))

inspect((sort(rules_b3, by = "lift"))[1:5])

#QB4
dt_b4 <- dt_b[,c(78,73)]

rules_b4 <- apriori(dt_b4,
                    parameter = list(supp = 0.01, conf = 0.1, maxlen=2),
                    appearance = list(default="rhs", lhs = c("sustainable_commitment=high")))

inspect((sort(rules_b4, by = "lift"))[1:5])

#QB5
dt_b5 <- dt_b[,c(37:49, 78)]

rules_b5 <- apriori(dt_b5,
                    parameter = list(supp = 0.1, conf = 0.5, maxlen=2),
                    appearance = list(default="rhs", lhs = c("sustainable_commitment=high")))

inspect((sort(rules_b5, by = "lift"))[1:3])

#QB6
dt_b6 <- dt_b[,c(37:49, 78)]

rules_b6 <- apriori(dt_b6,
                    parameter = list(supp = 0.1, conf = 0.5, maxlen=2),
                    appearance = list(default="rhs", lhs = c("sustainable_commitment=low")))

inspect((sort(rules_b6, by = "lift"))[1:3])


#QB7
dt_b7 <- dt_b[,c(50:56, 78)]

rules_b7 <- apriori(dt_b7,
                    parameter = list(supp = 0.1, conf = 0.5, maxlen=2),
                    appearance = list(default="rhs", lhs = c("sustainable_commitment=high")))

inspect((sort(rules_b7, by = "lift"))[1:3])

#QB8
dt_b8 <- dt_b[,c(50:56, 78)]

rules_b8 <- apriori(dt_b8,
                    parameter = list(supp = 0.1, conf = 0.4, maxlen=2),
                    appearance = list(default="rhs", lhs = c("sustainable_commitment=low")))

inspect((sort(rules_b8, by = "lift"))[1:3])

#QC1
dt_c <- dt
dt_c1 <- dt_c[,c(79, 76)]

rules_c1 <- apriori(dt_c1,
                    parameter = list(supp = 0.1, conf = 0.1, maxlen=2),
                    appearance = list(default="rhs", lhs = c("fashion_innovativeness=low")))

inspect((sort(rules_c1, by = "lift"))[1:5])

#QC2
dt_c2 <- dt_c[,c(79, 77)]

rules_c2 <- apriori(dt_c2,
                    parameter = list(supp = 0.1, conf = 0.1, maxlen=2),
                    appearance = list(default="rhs", lhs = c("fashion_innovativeness=low")))

inspect((sort(rules_c2, by = "lift"))[1:5])

#QC3
dt_c3 <- dt_c[,c(79, 74)]

rules_c3 <- apriori(dt_c3,
                    parameter = list(supp = 0.01, conf = 0.01, maxlen=2),
                    appearance = list(default="rhs", lhs = c("fashion_innovativeness=low")))

inspect((sort(rules_c3, by = "lift"))[1:5])

#QC4
dt_c4 <- dt_c[,c(79, 73)]

rules_c4 <- apriori(dt_c4,
                    parameter = list(supp = 0.01, conf = 0.1, maxlen=2),
                    appearance = list(default="rhs", lhs = c("fashion_innovativeness=high")))

inspect((sort(rules_c4, by = "lift"))[1:5])

#QC5
dt_c5 <- dt_c[,c(79, 37:49)]

rules_c5 <- apriori(dt_c5,
                    parameter = list(supp = 0.1, conf = 0.5, maxlen=2),
                    appearance = list(default="rhs", lhs = c("fashion_innovativeness=high")))

inspect((sort(rules_c5, by = "lift"))[1:5])

#QC6
dt_c6 <- dt_c[,c(79, 38:49)]

rules_c6 <- apriori(dt_c6,
                    parameter = list(supp = 0.1, conf = 0.5, maxlen=2),
                    appearance = list(default="rhs", lhs = c("fashion_innovativeness=low")))

inspect((sort(rules_c6, by = "lift"))[1:3])

#QC7
dt_c7 <- dt_c[,c(79, 50:56)]

rules_c7 <- apriori(dt_c7,
                    parameter = list(supp = 0.01, conf = 0.3, maxlen=2),
                    appearance = list(default="rhs", lhs = c("fashion_innovativeness=high")))

inspect((sort(rules_c7, by = "lift"))[1:3])

#QC8
dt_c8 <- dt_c[,c(79, 50:56)]

rules_c7 <- apriori(dt_c7,
                    parameter = list(supp = 0.01, conf = 0.3, maxlen=2),
                    appearance = list(default="rhs", lhs = c("fashion_innovativeness=low")))

inspect((sort(rules_c7, by = "lift"))[1:3])


#hypothesize 
dt_h <- dt

dt_h1 <- dt_h[,c(65,79,78,74,76)]

#h1
rules_h1 <- apriori(dt_h1,
                    parameter = list(supp = 0.001, conf = 0.1,minlen=4 ,maxlen=4))

rules_subset_1 <- subset(rules_h1, lhs %ain% c("fashion_innovativeness=high","Q41.Sex=Female",
                                         "sustainable_commitment=high"))
rules_subset_1

inspect((sort(rules_subset_1, by = "lift"))[1:3])

#h2
rules_h2 <- apriori(dt_h1,
                    parameter = list(supp = 0.001, conf = 0.1,minlen=4 ,maxlen=4))

rules_subset_2 <- subset(rules_h2, lhs %ain% c("fashion_innovativeness=high","Q41.Sex=Female",
                                             "sustainable_commitment=low"))
rules_subset_2

inspect((sort(rules_subset_2, by = "lift"))[1:4])

#h3
rules_h3 <- apriori(dt_h1,
                    parameter = list(supp = 0.001, conf = 0.1,minlen=4 ,maxlen=4))

rules_subset_3 <- subset(rules_h3, lhs %ain% c("fashion_innovativeness=low","Q41.Sex=Female",
                                               "sustainable_commitment=high"))
rules_subset_3

inspect((sort(rules_subset_3, by = "lift"))[1:4])

#h4
rules_h4 <- apriori(dt_h1,
                    parameter = list(supp = 0.001, conf = 0.1,minlen=4 ,maxlen=4))

rules_subset_4 <- subset(rules_h4, lhs %ain% c("fashion_innovativeness=low","Q41.Sex=Female",
                                               "sustainable_commitment=low"))
rules_subset_4

inspect((sort(rules_subset_4, by = "lift"))[1:4])

#h5
rules_h5 <- apriori(dt_h1,
                    parameter = list(supp = 0.001, conf = 0.1,minlen=4 ,maxlen=4))

rules_subset_5 <- subset(rules_h5, lhs %ain% c("fashion_innovativeness=high","Q41.Sex=Male",
                                               "sustainable_commitment=high"))
rules_subset_5

inspect((sort(rules_subset_5, by = "lift"))[1:4])

#h6
rules_h6 <- apriori(dt_h1,
                    parameter = list(supp = 0.001, conf = 0.1,minlen=4 ,maxlen=4))

rules_subset_6 <- subset(rules_h6, lhs %ain% c("fashion_innovativeness=high","Q41.Sex=Male",
                                               "sustainable_commitment=low"))
rules_subset_6

inspect((sort(rules_subset_6, by = "lift"))[1:4])

#h7
rules_h7 <- apriori(dt_h1,
                    parameter = list(supp = 0.001, conf = 0.1,minlen=4 ,maxlen=4))

rules_subset_7 <- subset(rules_h7, lhs %ain% c("fashion_innovativeness=low","Q41.Sex=Male",
                                               "sustainable_commitment=high"))
rules_subset_7

inspect((sort(rules_subset_7, by = "lift"))[1:4])

#h8
rules_h8 <- apriori(dt_h1,
                    parameter = list(supp = 0.001, conf = 0.1,minlen=4 ,maxlen=4))

rules_subset_8 <- subset(rules_h8, lhs %ain% c("fashion_innovativeness=low","Q41.Sex=Male",
                                               "sustainable_commitment=low"))
rules_subset_8

inspect((sort(rules_subset_8, by = "lift"))[1:4])




atop10subRules <- head(rules_a2, n = 10, by = "confidence")

#plot(top10subRules, method = "graph",  engine = "htmlwidget")



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

