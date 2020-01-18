## Junjie Wu  
## China Data Cleaning

rm(list = ls())

pacman::p_load(data.table, bit64, openxlsx, haven, dplyr)

setwd('~/Desktop/GLIS Research/GLIS_research_project/data')


dt_csv <- fread('01_16_cn_data.csv')

dt_spss <- fread('china_data.csv')

dt_csv[,c(1:11,74,81)] <- NULL
dt_csv <- dt_csv[-(1:2),]

dt_spss[,c(63,64,70)] <- NULL
#dt_csv[,52] <- NULL

# change miss spell colname
colnames(dt_spss)[60] <- "Q35.Faster.Knowing.More.About.New.Fashion.Than.Others"

#reorder column to match spss format
dt_csv <- dt_csv[,c(1:49,56:62,50:55,63:68)]

col_compare <- data.table(V1 = colnames(dt_spss),
                          V2 = colnames(dt_csv))

colnames(dt_csv) <- colnames(dt_spss)


#drop rows with less than 3 answers in Q31 and Q38
# 
# dt_csv_r[dt_csv_r==""] <- NA
# dt_csv_r$q31_na_count <- apply(is.na(dt_csv_r[,37:49]), 1, sum)
# dt_csv_r$q38_na_count <- apply(is.na(dt_csv_r[,50:56]), 1, sum)
# 
# dt_csv_r <- dt_csv_r %>% filter(dt_csv_r[,69] < 3 & dt_csv_r[,70]< 3) %>% 
#   select(-c(q31_na_count,q38_na_count))
# 
# dt_csv_r$na_count <- apply(dt_csv_r, 1, function(x) sum(is.na(x)))
# 
# dt_csv_r <- dt_csv_r[dt_csv_r[,'na_count'] < 10,]
# 
# col_compare <- data.table(V1 = dt_csv_r[,54],
#                           V2 = dt_csv[,54])


#replace answer in Chinese to numerical

#reversed score question
dt_csv[,57:59][dt_csv[,57:59]=="完全同意"] <- 1
dt_csv[,57:59][dt_csv[,57:59]=="同意"] <- 2
dt_csv[,57:59][dt_csv[,57:59]=="保持中立"] <- 3
dt_csv[,57:59][dt_csv[,57:59]=="不同意"] <- 4
dt_csv[,57:59][dt_csv[,57:59]=="完全不同意"] <- 5

dt_csv[,30:36][dt_csv[,30:36]!=""] <- 1

dt_csv[dt_csv=="完全同意" | dt_csv=="非常重要"] <- 5
dt_csv[dt_csv=="同意" | dt_csv=="重要"] <- 4
dt_csv[dt_csv=="保持中立" | dt_csv=="一般重要"] <- 3
dt_csv[dt_csv=="不同意" | dt_csv=="不重要"] <- 2
dt_csv[dt_csv=="完全不同意" | dt_csv=="完全不重要"] <- 1

dt_csv[,63][dt_csv[,63]=="少于我收入的5%"] <- 1
dt_csv[,63][dt_csv[,63]=="收入的5%-10%"] <- 2
dt_csv[,63][dt_csv[,63]=="收入的11%-15%"] <- 3
dt_csv[,63][dt_csv[,63]=="收入的16%-20%"] <- 4
dt_csv[,63][dt_csv[,63]=="收入的21%-25%"] <- 5
dt_csv[,63][dt_csv[,63]=="收入的26%-30%"] <- 6
dt_csv[,63][dt_csv[,63]=="超过我收入的30%"] <- 7

dt_csv[,64][dt_csv[,64]=="学生"] <- 1
dt_csv[,64][dt_csv[,64]=="全职工作"] <- 2
dt_csv[,64][dt_csv[,64]=="兼职工作"] <- 3
dt_csv[,64][dt_csv[,64]=="个体自营"] <- 4
dt_csv[,64][dt_csv[,64]=="无业"] <- 5
dt_csv[,64][dt_csv[,64]=="家庭主妇"] <- 6
dt_csv[,64][dt_csv[,64]=="其他"] <- 7

dt_csv[,65][dt_csv[,65]=="男"] <- 1
dt_csv[,65][dt_csv[,65]=="女"] <- 2
dt_csv[,65][dt_csv[,65]=="其他"] <- 3

dt_csv[,66][dt_csv[,66]=="少于 20,000元"] <- 1
dt_csv[,66][dt_csv[,66]=="20,000元至 39,999元"] <- 2
dt_csv[,66][dt_csv[,66]=="40,000元至 59,999元"] <- 3
dt_csv[,66][dt_csv[,66]=="60,000元至 79,999元"] <- 4
dt_csv[,66][dt_csv[,66]=="80,000元至 99,999元"] <- 5
dt_csv[,66][dt_csv[,66]=="100,000元至 119,999元"] <- 6
dt_csv[,66][dt_csv[,66]=="120,000元至 139,999元"] <- 7
dt_csv[,66][dt_csv[,66]=="140,000元至 159,999元"] <- 8
dt_csv[,66][dt_csv[,66]=="160,000元至 179,999元"] <- 9
dt_csv[,66][dt_csv[,66]=="180,000元至 199,999元"] <- 10
dt_csv[,66][dt_csv[,66]=="于200,000元"] <- 11

dt_csv[,67][dt_csv[,67]=="18-24"] <- 1
dt_csv[,67][dt_csv[,67]=="25-34"] <- 2
dt_csv[,67][dt_csv[,67]=="35-44"] <- 3
dt_csv[,67][dt_csv[,67]=="45-54"] <- 4
dt_csv[,67][dt_csv[,67]=="55-64"] <- 5
dt_csv[,67][dt_csv[,67]=="65-74"] <- 6
dt_csv[,67][dt_csv[,67]=="75及以上"] <- 7

dt_csv[,68][dt_csv[,68]=="高中文凭"] <- 1
dt_csv[,68][dt_csv[,68]=="大专文凭"] <- 2
dt_csv[,68][dt_csv[,68]=="学士学位"] <- 3
dt_csv[,68][dt_csv[,68]=="硕士学位"] <- 4
dt_csv[,68][dt_csv[,68]=="博士学位"] <- 5
dt_csv[,68][dt_csv[,68]=="其他"] <- 6

#replace space with NA
dt_csv[dt_csv==""] <- NA

######################## drop rows with missing ######################## 
#drop rows with less than 3 answers in Q31 and Q38
dt_csv$q31_na_count <- apply(is.na(dt_csv[,37:49]), 1, sum)
dt_csv$q38_na_count <- apply(is.na(dt_csv[,50:56]), 1, sum)

dt_csv <- dt_csv %>% filter(dt_csv[,69] < 3 & dt_csv[,70]< 3) %>% 
  select(-c(q31_na_count,q38_na_count))

dt_csv$na_count <- apply(dt_csv, 1, function(x) sum(is.na(x)))

dt_csv <- dt_csv[dt_csv[,'na_count'] < 10,]

# write.csv(dt_csv,'~/Desktop/GLIS\ Research/num_china_data_fixed.csv')
# write_sav(dt_csv, '~/Desktop/GLIS\ Research/num_china_data_fixed.sav')

# bind_china_dt <- rbind(dt_spss,dt_csv)

dt_csv <- data.table(dt_csv %>% mutate_if(is.character,as.integer))

dt_csv[, total_score := rowSums(.SD), .SDcols = 57:62]


dt_csv_mean <- sapply(dt_csv[,37:56], mean, na.rm=TRUE)


View(sort(dt_csv_mean, decreasing = T))


# write_sav(bind_china_dt, '~/Desktop/GLIS\ Research/bind_china_data_fixed.sav')
write.csv(dt_csv, '~/Desktop/GLIS\ Research/GLIS_research_project/data/cleaned_01_16_cn.csv')
