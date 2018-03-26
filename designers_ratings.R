designers_ratings = subset(BGG_tbl, stats.usersrated > 100)
designers_ratings = subset(designers_ratings, select = c("designers", "stats.average"))
designers_ratings = unnest(designers_ratings, designers)
designers_ratings = subset(designers_ratings, !identical(designers_ratings$designers, "(Uncredited)"))
head(designers_ratings[order(-designers_ratings$stats.average),])

typeof(designers_ratings)

counts = count(designers_ratings, designers)
setkey


designers_ratings = subset(designers_ratings, identical(counts$designers, designers_ratings$designers) & counts$n > 3)
head(counts)






designers_ratings = nest(designers_ratings, stats.average)

head(designers_ratings)
typeof(designers_ratings$data)



boxplot(designers_ratings$stats.average~designers_ratings$designers, las = 2, col = "cadetblue3", outcol="cadetblue3", outpch=20)
