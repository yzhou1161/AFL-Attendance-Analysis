#Twitter API
require(twitteR)
require(data.table)

consumerKey <- "8n8YyN0mf29fOJ2i8fjgVbyGZ"
accessSecret <- "v1DVXH0RqrwphW3Q11hlVKwxUq2AlZEw2ZNZHUK7MAbto"
accessToken <- "356581725-ydbo8VBme2iXUU2ps1NuW2HRouWGSF7Yc9PRU06Z"
consumerSecret <- "MO52W1ziuROIEIqjuwrx10Reba7fRblDrBp9zKkGnaQ4JpgY0W"
setup_twitter_oauth(consumerKey, consumerSecret,accessToken, accessSecret)

#Get Twitter account
afl <- getUser("AFLarenaball")
str(afl)

#user timeline
afl.timeline=userTimeline('AFLarenaball',n=100)
afl.tl.df=twListToDF(afl.timeline)


#followers of a user 
require(data.table)
aflf <- afl$getFollowers()
afl_df = rbindlist(lapply(aflf,as.data.frame))
colnames(afl_df)

#search Twitter
searchTwitter('arena football+game', n=20)[[1]]

#For each follower of @AFLarenaball, collect their follower count, statuses count, screen
#name, username, date created, and they're verified
aflfc = sapply(afl_df, function(x) x$getFollowersCount())
aflsc = sapply(afl_df, function(x) x$getStatusesCount())
aflsn = sapply(afl_df, function(x) x$getScreenName())
aflname = sapply(afl_df, function(x) x$getName())
aflv <- sapply(afl_df, function(x) x$getVerified())
afldate <- afl_df$created
afl_df <- data.frame(aflfc, aflsc, aflsn, aflname, aflv, afldate)

#clean up data on date
#and calculate average status per day for each follower
afldates <- as.Date(strftime(afl_df$afldate, "%Y-%m-%d"))
afltoday <- as.Date("2016-06-10")
afl_df$afldays <- as.numeric(afltoday - afldates)
afl_df$avg <- afl_df2$aflsc / afldays#avg = average status per day
head(afl_df)


require(scatterD3)
##scatterD3
tooltipsafl <- paste(afl_df2$aflname)
afld3 <- scatterD3(x = afl_df2$aflfc, y = afl_df2$avg, size_lab = 30,
                   col_var = afl_df2$aflv, xlab = "Follower Count", 
                   ylab = "Average Status per Day", col_lab = "Verified Account", 
                   point_opacity = .5, tooltip_text = tooltipsafl)
afld3


#better write a function for all teams: average status per day
apd <- function(username, today){
  today <- as.Date(today)
  u <- getUser(username)
  f <- u$getFollowers()
  fc = sapply(f, function(x) x$getFollowersCount())
  sc = sapply(f, function(x) x$getStatusesCount())
  sn = sapply(f, function(x) x$getScreenName())
  name = sapply(f, function(x) x$getName())
  v <- sapply(f, function(x) x$getVerified())
  library(data.table)
  df = rbindlist(lapply(f,as.data.frame))
  date <- df$created
  df2 <- data.frame(fc, sc, sn, name, v, date)
  dates <- as.Date(strftime(df2$date, "%Y-%m-%d"))
  days <- as.numeric(today - dates)
  df2$avg <- df2$sc / days
  return(df2)
}

#scatterD3
sD3 <- function(df){
  tooltips <- paste(df$name)
  d3 <- scatterD3(x = df$fc, y = df$avg, size_lab = 30,
                     col_var = df$v, xlab = "Follower Count", 
                     ylab = "Average Status per Day", col_lab = "Verified Account", 
                     point_opacity = .5, tooltip_text = tooltips)
  return(d3)
}


ps <- apd("soulfootball","2016-06-14")
psd3 <- sD3(ps)
cg <- apd("CLEGladiators","2016-06-10")
cgd3 <- sD3(cg)
ar <- apd("ArizonaRattlers","2016-06-10")
ard3 <- sD3(ar)
op <- apd("ORLPredators","2016-06-10")
opd3 <- sD3(op)
lk <- apd("LAKISS_AFL","2016-06-10")
lkd3 <- sD3(lk)
psteel <- apd("pdxsteel","2016-06-10")
psteeld3 <- sD3(psteel)
js <- apd("jaxsharks","2016-06-10")
jsd3 <- sD3(js)
tbs <- apd("TampaBayStorm","2016-06-10")
tbsd3 <- sD3(tbs)

is.na(js$avg) <- do.call(cbind,lapply(js$avg, is.infinite))

teamlist <- list(js, ps, tbs, psteel, ar, cg, op, lk)
df1 <- lapply(teamlist, function(x) mean(x$avg, na.rm = T))
mean(js$avg,na.rm = T)
median(js$fc)

median(ps$avg,na.rm = T)
median(ps$fc)




#Week2
#plot Twitter discussion by date first
ps <- read.csv("afl/phillysoul.csv",quote = "")

#write a function for word cloud generation
wc <- function(var,n){
  library(tm)
  library(wordcloud)
  t <- sapply(var, function(row) iconv(row, "latin1", "ASCII", sub=""))
  t <- paste(unlist(t), collapse ="") #to get all of the tweets together
  t <- Corpus(VectorSource(t))
  t <- tm_map(t, PlainTextDocument)
  t <- tm_map(t, removePunctuation)
  t <- tm_map(t, removeWords, c(stopwords('english'),'just','like','http','https','httpwww'))
  t <- tm_map(t, content_transformer(tolower),lazy=TRUE)
  name <- deparse(substitute(var))
  pal <- brewer.pal(5, "Dark2")
  png(paste0(name, ".png"), width=8,height=8,units = "in", res = 300)
  wordcloud(t, max.words = n, random.order = FALSE,min.freq = 5, random.color = T, 
            rot.per = T, scale=c(3,.5),colors=pal)
  dev.off()
}
#to generate a word cloud on Twitter discussion about afl, containing a maximum number of 200 words
wc(afl$text, 200)


afl$date <- as.Date(afl$date)


ps$date <- as.character(ps$date)
require(stringr)
ps$date <- str_sub(ps$date, 1, -7)
ps <- subset(ps, nchar(ps$date) == 10)
ps$date <- as.Date(ps$date,format="%Y-%m-%d")
require(scales)
ggplot(ps, aes(x=date)) + geom_bar(stat = "count") + scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))



t <- read.csv("aflteams.csv")
ggplot(t, aes(x = Performance, y = Attendance)) + geom_point()
t1 <- lm(Attendance.rate ~ Tweet.count + pct, data = t)
summary(t1)






noleague <- subset(t, Team != "League")
g1 <- ggplot(noleague, aes(x = Year, y = Attendance, color = Team)) + 
      geom_line(aes(size = Team))+
  scale_size_manual(values = c(.8, .8, .1,.1, .1, .8, .1,.1)) +
  scale_linetype_manual(values = c("solid","solid","dashed","dashed",
                                   "dashed","solid","dashed","dashed"))

g1


target <- subset(t, Team %in% c("Los Angeles","Philadelphia","Arizona","Cleveland","Orlando","Tampa Bay","Jacksonville","Portland"))
ggplot(target[target$Year > 2010,], aes(x = Year, y = Tweet.count, color = Team)) + geom_line(aes(x = Year, y = Tweet.count, color = Team, size = Team, linetype = Team)) +
  scale_linetype_manual(values = c("solid","solid","dashed",
                                   "dashed","dashed", "solid","dashed","dashed"
  )) + scale_size_manual(values = c(.8, .8, .1, .8, .1, .8, .1, .1)) +
  scale_y_continuous(name = "Tweet Count about the Team")




g2 <- ggplot(t, aes(x = Year, y = Tweet.count, color = Team)) + geom_line(aes(x = Year, y = Tweet.count, color = Team, size = Team, linetype = Team)) +
  scale_linetype_manual(values = c("solid","solid","dashed","solid",
                                      "dashed","dashed","solid","dashed","solid"
                                      )) + scale_size_manual(values = c(.8, .5, .1, .8, .1, .1, .8,.1,.5)) +
    scale_y_continuous(name = "Tweet Count about the Team")
g3 <- g2 +  geom_line(aes(x = t$Year[t$Year>2014], y = t$Tweet.count, color = Team, size = Team)) 




ggplot(t, aes(x = pct, y = Attendance.rate, color = Team)) + geom_point()
p1 <- ggplot(t, aes(x = Year, y = Attendance, color = Team)) + geom_line()
p2 <- p1 + geom_line(data = t[which('Team' == 'League'),],linetype = 2)
ggplot(subset(t, Team %in% c('Philadelphia','League')), aes(x = Year, y = Attendance, color = Team)) + geom_line()
ggplot(subset(t, Team == 'Philadelphia'), aes(x = Year, y = Attendance.rate))+geom_line()


t$avgpct <- mean(t)
ggplot(t, aes(x = pct, y = mean(Attendance), color = Team)) + geom_point() + geom_abline()


require(ggplot2)
subt <- subset(t,Team %in% c('Philadelphia','League'))
ggplot() +  
  geom_line(aes(subt, color = Team)) +
  geom_polygon(aes(x = Year, y = Attendance, group = Team), alpha = 0.3) +
  scale_size_manual(values = c(.8,.8))

ggplot(ribbon, aes(Year)) + 
geom_ribbon(aes(x=Year, ymax=League, ymin=Philadelphia), alpha=.3) +
  geom_line(aes(x = Year, y = League)) +
  geom_line(aes(x = Year, y = Philadelphia)) + scale_color_manual(values = c("#56B4E9","#009E73")) +
  scale_y_continuous(name = "Attendance") + scale_size_manual(values = c(.1,.8))


ribbon <- read.csv("geomribbon.csv")




ggplot(t, aes(x = Year, y = Attendance, color = Team)) + 
  geom_line(aes(size = Team))+
  scale_size_manual(values = c(.8, .5, .1, .8,.1, .1, .8, .1,.5))



#Read the Twitter discussion containing "Arena Football" from 2011 to 2016 into R
#And count the number of tweets in 2016
afl.discussion <- read.csv("afl/afl.csv",quote="",row.names=NULL)
dim(afl.discussion[grep("/16 ",afl.discussion$date),])


dim(aflt[grep("/15 ",aflt$username),])
dim(aflt[grep("/14 ",aflt$username),])
dim(aflt[grep("/13 ",aflt$username),])
dim(aflt[grep("/12 ",aflt$username),])
dim(aflt[grep("/11 ",aflt$username),])


dim(aflt[grep("6",aflt$username),])






text2 <- str_replace_all(aflt$text,"[^[:graph:]]", " ") 
freq.afl <- freqlist(text2)

js2 <- read.csv("jacksonville2.csv")
jsn <- js2[grepl("jacksonvillesharks",js2$text),]


require(ggplot2)
require(ggrepel)
require(plyr)
t <- read.csv("aflteams.csv")
sub <- t[t$Year ==2016 & t$Team != "League",]
g1 <- ggplot(sub) + 
  geom_point(aes(x= pct, y = Attendance,color = Team, shape = Ticketmaster),size =4) +
  scale_colour_discrete(guide = FALSE) + 
  geom_text_repel(aes(x = pct, y = Attendance, label=Team,color = Team), size = 4) +
  scale_shape_manual(name = "Tickets Sold on Ticketmaster", values = c(1,19)) + 
  geom_abline(aes(intercept = 6997, slope = 5012),color = "steelblue",alpha =.5) +
  scale_x_continuous(name = "Winning Percentage") + 
  scale_y_continuous(name = "Game Attendance")


sub2 <- aflt[aflt$Team != "League",]

g2 <- ggplot(sub2, aes(x = Year, y = Tweet.count, color = Team)) + 
  geom_line(aes(x = Year, y = Tweet.count, color = Team, size = Team, linetype = Team)) +
  scale_linetype_manual(values = c("dashed","dashed","dashed",
                                   "dashed","dashed","solid","dashed","dashed"
  )) + scale_size_manual(values = c(.6, .6, .6, .6, .6, .8, .6,.6)) +
  scale_y_continuous(name = "Twitter Discussion")
g2

#bar chart with different transparency
t$alpha <- as.factor(ifelse(t$Team=="Philadelphia",1,.5))
ggplot(t, aes(x = Team, y = Followers, fill = Team, alpha = factor(alpha))) +
  scale_colour_discrete(guide = FALSE) +
  geom_bar(stat = "identity") + scale_y_continuous(name = "Twitter Followers") +
  scale_alpha_manual(values = c("0.5"=0.5, "1"=1), guide='none')


+
  geom_line(aes(x = Year, y = Tweet.count, color = Team, size = Team, linetype = Team)) +
  scale_linetype_manual(values = c("solid","dashed","dashed",
                                   "dashed","dashed","solid","dashed","dashed"
  )) + scale_size_manual(values = c(.8, .4, .2, .2, .2, .8, .2,.2))



soul <- read.csv("tweet_activity_metrics_soulfootball_20160316_20160615_en.csv")
head(soul)
class(soul$time)
soul$date <- as.Date(soul$time)


qplot(date, data = soul, geom = "bar")




is.na(tbs$avg) <- do.call(cbind,lapply(tbs$avg, is.infinite))

mean(tbs$avg,na.rm = T)
median(tbs$fc)

statst <- function(t){
  is.na(t$avg) <- do.call(cbind,lapply(t$avg, is.infinite))
  mean.t <- mean(t$avg, na.rm = T)
  mean.f <- mean(t$fc)
  median.t <- median(t$avg, na.rm = T)
  median.f <- median(t$fc)
  df <- data.frame(mean.t, median.t, mean.f, median.f)
  return(df)
}

statst(ar)
statst(tbs)
statst(ps)
statst(js)
statst(op)
statst(cg)
statst(lk)
statst(psteel)


fstats <- read.csv("teamfollower.csv")
head(fstats)
fstats <- rename(fstats, c("X" = "Team"))
ggplot(fstats) + 
  geom_point(aes(x= median.f, y = median.t,color = Team),size = 4) +
  scale_colour_discrete(guide = FALSE) + 
  geom_text_repel(aes(x = median.f, y = median.t, label=Team,color = Team), size = 4) +
  scale_y_continuous(name = "Tweet per Day") + 
  scale_x_continuous(name = "Number of Followers") 


ggplot(fstats) + 
  geom_point(aes(x= mean.f, y = mean.t,color = Team),size = 4) +
  scale_colour_discrete(guide = FALSE) + 
  geom_text_repel(aes(x = mean.f, y = mean.t, label=Team,color = Team), size = 4) +
  scale_y_continuous(name = "Average Status per Day") + 
  scale_x_continuous(name = "Average Number of Followers") 



ggplot(fstats) + 
  geom_point(aes(x= mean.f, y = mean.t,color = Team),size = 4) +
  scale_colour_discrete(guide = FALSE) + 
  geom_text_repel(aes(x = mean.f, y = mean.t, label=Team,color = Team), size = 4) +
  scale_y_continuous(name = "Average Status per Day") + 
  scale_x_continuous(name = "Average Number of Followers") 




psteel2 <- psteel[ order(-psteel[,1]), ]
psteel3 <- psteel2[-1:-2,]
statst(psteel3)




#attendance trend
#highlight Arizona Rattlers, Philadelphia Soul and Cleveland Gladiators in the plot
ggplot(d, aes(x=Year, y = Attendance, color = Team)) + 
  geom_line(aes(x = Year, y = Attendance, color = Team, linetype = Team, size = Team)) +
  scale_linetype_manual(values = c("solid","dashed","dashed",
                                   "dashed","dashed","solid","dashed","dashed"
  )) + scale_size_manual(values = c(1, .4, .4, .4, .4, 1, .4,.4)) +
  scale_y_continuous(name = "Attendance")

ribbon <- read.csv("geomribbon.csv")
ggplot(ribbon) + geom_smooth(aes(x = Year, y = Attendance, color = Team), fill = NA)


#Plot attendance vs winning percentage for AFL teams 2016, highlight teams that rent the staidum
#add a trend line
ggplot(t) + 
  geom_point(aes(x= pct, y = Attendance,color = Team,shape = Renting.the.Stadium),size =4) +
  scale_colour_discrete(guide = FALSE) + 
  scale_shape_manual(name = "Renting the Stadium", values = c(1,19)) + 
  geom_text_repel(aes(x = pct, y = Attendance, label=Team,color = Team), size = 4) +
  geom_abline(aes(intercept = 6997, slope = 5012),color = "steelblue",alpha =.5) +
  scale_x_continuous(name = "Winning Percentage") + 
  scale_y_continuous(name = "Game Attendance") 




t0620 <- read.csv("teamfollower0620.csv")
t0620 <- rename(t0620, c("X" = "team"))
sub0620 <- t
lm1 <- lm(attendance ~ median.t + median.f + pct + followers, sub0620)
summary(lm1)
lm2 <- lm(attendance ~ mean.t + mean.f + pct + followers, t0620)
summary(lm2)
lm3 <- lm(attendance ~ median.t + median.f, sub0620)
summary(lm3)
lm4 <- lm(attendance ~ mean.t + mean.f + followers, sub0620)
summary(lm4)
lm5 <- lm(attendance ~ median.t + median.f + followers, sub0620)
summary(lm5)
lm6 <- lm(attendance ~ median.t + median.f * followers, t0620)
target <- subset(t0620, team %in% c("Arizona","Cleveland","Orlando","Tampa Bay","Jacksonville"))

lm7 <- lm(attendance ~ median.f, target)
summary(lm7)
