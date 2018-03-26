library(jsonlite, tibble)
library(ggplot2)
library(tidyr)
library(hexbin)
library(corrplot)
library(dplyr)
options(scipen=999)

# import data and clean up some relevant fields to numerics
BGG = stream_in(file("pruned.json"))
BGG_flat = flatten(BGG)
BGG_flat = subset(BGG_flat, select = -c(stats.numtrading, stats.stddev, stats.numfans, stats.numwantparts, stats.numwish, stats.numhasparts))
BGG_tbl = as.data.frame(BGG_flat)
BGG_tbl$id = as.numeric(as.character(BGG_tbl$id))
BGG_tbl$year = as.numeric(as.character(BGG_tbl$year))
BGG_tbl$weight = as.numeric(as.character(BGG_tbl$weight))
BGG_tbl$mintime = as.numeric(as.character(BGG_tbl$mintime))
BGG_tbl$maxtime = as.numeric(as.character(BGG_tbl$maxtime))
BGG_tbl$age = as.numeric(as.character(BGG_tbl$age))
BGG_tbl$minplayers = as.numeric(as.character(BGG_tbl$minplayers))
BGG_tbl$maxplayers = as.numeric(as.character(BGG_tbl$maxplayers))
BGG_tbl$usrage = as.numeric(as.character(BGG_tbl$usrage))
BGG_tbl$stats.average = as.numeric(as.character(BGG_tbl$stats.average))
BGG_tbl$stats.numowned = as.numeric(as.character(BGG_tbl$stats.numowned))
BGG_tbl$stats.numcomments = as.numeric(as.character(BGG_tbl$stats.numcomments))
BGG_tbl$stats.usersrated = as.numeric(as.character(BGG_tbl$stats.usersrated))
BGG_tbl$stats.avgweight = as.numeric(as.character(BGG_tbl$stats.avgweight))
BGG_tbl$stats.views = as.numeric(as.character(BGG_tbl$stats.views))
BGG_tbl$stats.numprevowned = as.numeric(as.character(BGG_tbl$stats.numprevowned))

#a histogram of all game ratings for games that have been rated at least 10 times
ratings = subset(BGG_tbl, select = c("stats.average", "stats.usersrated"))
ratings = subset(ratings, stats.usersrated > 9)
hist(ratings$stats.average, breaks = c(0.5*0:20), xlim = c(0, 10), xlab = "Rating", main = "All ratings", col = "cadetblue3")

# plot correlation coefficients for many of the fields
corr_vars = BGG_tbl[,c("year", "mintime", "maxtime", "age", "minplayers", "maxplayers", "weight", "stats.average", "stats.numowned", "stats.views", "stats.numprevowned")]
corr_mat = round(cor(corr_vars, method = "pearson", use = "complete.obs"), 2)
corrplot(corr_mat, method = "circle")


# a boxplot of ratings relative to the average length of one game
playtime_rating = subset(BGG_tbl, stats.average > 0 & mintime > 0 & maxtime > 0 & maxtime < 600 & stats.usersrated >9)
playtime_rating = subset(playtime_rating, select=c("stats.average", "mintime", "maxtime"))
playtime_rating$time = (playtime_rating$mintime+playtime_rating$maxtime)/2
playtime_rating$time = cut(playtime_rating$time, breaks = c(30*(0:12)))
playtime_rating_range1 = c(0.5*(0:11))
playtime_rating_range2 = c(0.5*(1:12))
playtime_rating_ranges = paste(playtime_rating_range1, playtime_rating_range2, sep="-")
boxplot(playtime_rating$stats.average~playtime_rating$time, las = 2, xlab="Average playtime (hours)", names = playtime_rating_ranges, ylab="Rating", main="Rating vs. Average playtime", col="cadetblue3", outcol="cadetblue3", outpch=20)


# a boxplot of ratings relative to date of publication since 1961
year_ratings = subset(BGG_tbl, year > 1960 & stats.usersrated > 5)
year_ratings = subset(year_ratings, select=c("year", "stats.average"))
year_ratings$year = cut(year_ratings$year, breaks=c(1960+5*(0:12)), dig.lab = 10)
year_ratings_range1 = 1961+5*(0:11)
year_ratings_range2 = 1965+5*(0:11)
year_ratings_ranges = paste(year_ratings_range1, year_ratings_range2, sep="-")
boxplot(year_ratings$stats.average~year_ratings$year, las = 2, ylab="Rating", names = year_ratings_ranges, col = "cadetblue3", outcol="cadetblue3", outpch=20, main="Rating statistics per 5-year interval since 1961")


# a boxplot of ratings relative to date of publication since 2001
year_ratings2 = subset(BGG_tbl, year > 1999 & stats.usersrated > 5)
year_ratings2 = subset(year_ratings2, select=c("year", "stats.average"))
year_ratings2$year = cut(year_ratings2$year, breaks=c(1999+2*(0:9)), dig.lab = 10)
year_ratings2_range1 = 2000+2*(0:8)
year_ratings2_range2 = 2001+2*(0:8)
year_ratings2_ranges = paste(year_ratings2_range1, year_ratings2_range2, sep="-")
boxplot(year_ratings2$stats.average~year_ratings2$year, las = 2, ylab="Rating", names = year_ratings2_ranges, col = "cadetblue3", outcol="cadetblue3", outpch=20, main="Rating statistics per 2-year interval since 2000")

# a boxplot of weight statistics relative to date of publication since 1961
year_weight = subset(BGG_tbl, year > 1960 & stats.numweights > 5)
year_weight = subset(year_weight, select=c("year", "weight"))
year_weight$year = cut(year_weight$year, breaks=c(1960+5*(0:12)), dig.lab = 10)
year_weight_range1 = 1961+5*(0:11)
year_weight_range2 = 1965+5*(0:11)
year_weight_ranges = paste(year_weight_range1, year_weight_range2, sep="-")
boxplot(year_weight$weight~year_weight$year, las = 2, names = year_weight_ranges, col = "cadetblue3", outcol="cadetblue3", outpch=20, ylab="Weight")


# a boxplot of rating statistics relative to weight
rating_weight = subset(BGG_tbl, stats.usersrated > 9 & weight > 0)
rating_weight = subset(rating_weight, select=c("stats.average", "weight"))
rating_weight$weight = round(rating_weight$weight, digits = 1)
rating_weight$weight = cut(rating_weight$weight, c(0.5+0.5*(0:9)), dig.lab = 1)
boxplot(rating_weight$stats.average~rating_weight$weight, las=2, col = "cadetblue3", outcol="cadetblue3", outpch=20, xlab="Weight", ylab="Rating", main="Rating statistics relative to weight")


#introducing a "retention" factor and plotting it against rating
rating_retention = subset(BGG_tbl, select=c("stats.numowned", "stats.numprevowned", "stats.average"))
rating_retention = subset(rating_retention, stats.numprevowned > 0 & stats.numowned > 10 & stats.average > 0)
rating_retention$retention = rating_retention$stats.numprevowned/rating_retention$stats.numowned
cor(rating_retention)
rating_retention = subset(rating_retention, retention < 0.5)
smoothScatter(rating_retention$retention, rating_retention$stats.average, nbin=128, colramp = colorRampPalette(c("white", blues9)), xlab = "(Number previously owned)/(Number owned)", ylab="Rating", main="Rating and retention rate")
