library(choroplethrMaps)
library(choroplethrZip)
library(ggplot2)
library(ggthemes)
library(plyr)

zips <- data.frame(data(df_zip_demographics))
data(df_zip_demographics)
write.csv(df_zip_demographics,"df_zips_demographics.csv")
MPVDatasetDownloadJune <- read.csv("~/GitHub/Police Violence/MPVDatasetDownloadJune.csv", comment.char="#")
colnames(MPVDatasetDownloadJune)[11] <- "region"
test <- merge(df_zip_demographics,MPVDatasetDownloadJune,by='region')
test <- test[,c(1:13)]
colnames(test)[11:13] <- c("Age","Gender","Race")
colnames(test)[10] <-"Name"
test$incident <- "1"
incidents_demo <- test[,c(1:9,14)]
df_zip_demographics$incident <-"0"
cluster_Dates_all <- rbind(df_zip_demographics,incidents_demo)

gg_cluster <- function(tgt_clu, tgt_col_num) {  # tgt_clu is a integer, tgt_var is a var name.
  g <- ggplot(subset(cluster_Dates_all, incident == 0 | incident == tgt_clu), aes_string(x = colnames(cluster_Dates_all)[tgt_col_num]))
  g1 <- g + geom_density(aes(fill = factor(incident) , alpha = 0.6))
  g2 <- g1 + theme(legend.position = "right") + theme_grey()
  g3 <- g2 + guides(fill=guide_legend(title="Incident Zip"))
  return(g3)
}

gg_cluster(1)

cluster_sum <- function(tgt_clu) {
  g1 <- gg_cluster(tgt_clu, 2)
  g2 <- gg_cluster(tgt_clu, 3)
  g3 <- gg_cluster(tgt_clu, 4)
  g4 <- gg_cluster(tgt_clu, 5)
  g5 <- gg_cluster(tgt_clu, 6)
  g6 <- gg_cluster(tgt_clu, 7)
  g7 <- gg_cluster(tgt_clu, 8)
  g8 <- gg_cluster(tgt_clu, 9)
  g9 <- gg_cluster(tgt_clu, 10)
  g10 <- gg_cluster(tgt_clu, 11)
  g11 <- gg_cluster(tgt_clu, 12)
  multiplot(g1, g2, g3, g4, g5, g6, g7, g8, cols=2)
}

attributes_comp <- function(tgt_col_num) {
  g1 <- gg_cluster(1, tgt_col_num)
  g2 <- gg_cluster(2, tgt_col_num)
  g3 <- gg_cluster(3, tgt_col_num)
  g4 <- gg_cluster(4, tgt_col_num)
  g5 <- gg_cluster(5, tgt_col_num)
  g6 <- gg_cluster(6, tgt_col_num)
  multiplot(g1, g2, g3, g4, g5, g6, cols=2)
}

summary(cluster_Dates_all[which(cluster_Dates_all$incident==1),])
summary(cluster_Dates_all[which(cluster_Dates_all$incident==0),])

cluster_sum(1)

police_map <- incidents_demo[,c(1,10)]

colnames(police_map)[2] <- "value"
police_map$value <- as.numeric(police_map$value)

agg_police_zips <- ddply(police_map, .(region), summarize, Sum=sum(value))

colnames(agg_police_zips)[2] <- "value"

killings_three <- merge(agg_police_zips,df_zip_demographics, by="region")




summary(killings_three[which(killings_three$value==1),])
summary(killings_three[which(killings_three$value==2),])
summary(killings_three[which(killings_three$value==3),])



killings_three <- killings_three[,c(1:10)]
colnames(killings_three)[2] <- "incident"
killings_three <- killings_three[,c(1,3:10,2)]
all_clusters_zips <- rbind(killings_three,df_zip_demographics)


gg_cluster <- function(tgt_clu, tgt_col_num) {  # tgt_clu is a integer, tgt_var is a var name.
  g <- ggplot(subset(all_clusters_zips, incident == 0 | incident == tgt_clu), aes_string(x = colnames(all_clusters_zips)[tgt_col_num]))
  g1 <- g + geom_density(aes(fill = factor(incident), alpha = 0.6))
  g2 <- g1 + theme(legend.position = "right")
  g3 <- g2 + guides(fill=guide_legend(title="PV Incidents"))
  return(g3)
}

cluster_sum(c(1,2,3,4))



lm_killings <- lm(incident ~ total_population + percent_black + percent_hispanic + per_capita_income + median_rent, data=killings_three, na.action = na.omit)
summary(lm_killings)

summary(all_clusters_zips[which(all_clusters_zips$incident==0),])
summary(all_clusters_zips[which(all_clusters_zips$incident==1),])
summary(all_clusters_zips[which(all_clusters_zips$incident==2),])
summary(all_clusters_zips[which(all_clusters_zips$incident==3),])
summary(all_clusters_zips[which(all_clusters_zips$incident==4),])

install.packages("stargazer") #Use this to install it, do this only once
library(stargazer)

stargazer(all_clusters_zips[which(all_clusters_zips$incident==0),],
          all_clusters_zips[which(all_clusters_zips$incident==1),],
          all_clusters_zips[which(all_clusters_zips$incident==2),],
          all_clusters_zips[which(all_clusters_zips$incident==3),],
          all_clusters_zips[which(all_clusters_zips$incident==4),],type = "html",
title="Descriptive statistics/selected variables", digits=1, out="table2.htm")

stargazer(lm_killings, type = "html",
          title="Regression", digits=1, out="table3.htm")





