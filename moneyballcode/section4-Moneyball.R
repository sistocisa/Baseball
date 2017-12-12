#opening

install.packages("jpeg")
library(jpeg)
test<-readJPEG("moneyball1.jpg")
test2<-readJPEG("moneyball2.jpg")
test3<-readJPEG("moneyball3.jpg")
test4<-readJPEG("moneyball4.jpg")

plot(1, type="n", xlim=c(100, 200), ylim=c(300, 350))
rasterImage(test,100, 300, 150, 350)
rasterImage(test2,100, 300, 200, 350)
rasterImage(test3,100, 300, 200, 350)
rasterImage(test4,100, 300, 200, 350)


#Read data
batting <- read.csv("Batting.csv")
sal <- read.csv('Salaries.csv')


head(batting)
batting[1:7,]
str(batting)
summary(batting)

head(batting$AB)

head(sal)
str(sal)

#Merging data
batting2 <- subset(batting,yearID >= 1985)
combo <- merge(batting2,sal,by=c('playerID','yearID'))
head(combo)
summary(combo)
str(combo)

install.packages("dplyr")
library(dplyr)
avail<- filter(combo,yearID==2001)
head(avail)


#Visualization
library(ggplot2)

MLfilter <- (avail$teamID.x =="BOS") |
  (avail$teamID.x =="ARI") |(avail$teamID.x =="HOU") |
  (avail$teamID.x =="SFN") |(avail$teamID.x =="NYA") |
  (avail$teamID.x =="OAK")

avail2 <- avail[MLfilter,]

#set golbal variable for salary
options("scipen" = 100)

box<- ggplot(data=avail2, aes(x=teamID.x, y=salary))

s <-box +
  geom_jitter(aes(size=H,colour=BB)) +
  ggtitle("Baseball stats about hot 6 MLteam") +
  geom_boxplot(alpha = 0.7) +
  xlab("Team") + ylab("Salary") +
  theme(axis.title.x = element_text(colour = "Blue",size=15),
        axis.title.y = element_text(colour = "Blue",size=15))

s
s + coord_cartesian(ylim=c(0,3000000))


p <-ggplot(data=avail,aes(x=salary,y=H,colour=lgID.y))
p + geom_point()
p + geom_point(aes(colour = ifelse(H>240,2,''))) + geom_text(aes(label=ifelse(H>240,
                          avail$H,'')),colour='black',vjust = -0.5, nudge_x = 0.05)
#that player is ichiro(seatle)
p + geom_point(aes(colour = ifelse(salary>20000000,2,''))) + geom_text(aes(label=ifelse(salary>20000000,
         avail$H,'')),colour='black',vjust = -0.5, nudge_x = 0.05)
#that player is alex rodriguez (texas)

p + geom_point(aes(y=BB))
p + geom_point(aes(y=HR))
p + geom_point(aes(y=RBI))


q <-ggplot(data=avail,aes(x=salary,y=H/AB,colour=G))
q + geom_point()
q + geom_point(aes(size=HR))

table(avail$teamID.x)
table(avail$lgID.y)

#create Data
head(avail)
head(avail$X2B)
avail$BA <- avail$H/ avail$AB
tail(avail$BA,5)
avail$OBP <- (avail$H + avail$BB + avail$HBP)/
  (avail$AB + avail$BB + avail$HBP + avail$SF)
avail$X1B <- avail$H - avail$X2B - avail$X3B - avail$HR
avail$SLG <- ((1 * avail$X1B) + (2 * avail$X2B) + 
                  (3 * avail$X3B) + (4 * avail$HR) ) / avail$AB

head(avail)
avail2 <- avail[,c('playerID','H','OBP','SLG','BA',
                   'AB','salary')]

avail2$save <- ifelse(avail2$playerID=='',"B","A")
table(avail2$save)

lost_players <- subset(avail,playerID %in% c('giambja01','damonjo01','saenzol01') )

lost_players
lost_players <- lost_players[,c('playerID','H','OBP','SLG','BA',
                                'AB','salary')]

head(lost_players)

avail2$save[avail2$playerID==lost_players$playerID[1]] <- "B"
avail2$save[avail2$playerID==lost_players$playerID[2]] <- "B"
avail2$save[avail2$playerID==lost_players$playerID[3]] <- "B"
table(avail2$save)


avail <- filter(avail,salary<8000000,OBP>0)
avail <- filter(avail,AB >= 500)
possible <- head(arrange(avail,desc(OBP)),10)
possible <- possible[,c('playerID','H','OBP','SLG','BA',
                            'AB','salary')]
possible
possible[2:4,]
possible2 <- possible[2:4,]
possible2

avail2$save[avail2$playerID==possible2$playerID[1]] <- "C"
avail2$save[avail2$playerID==possible2$playerID[2]] <- "C"
avail2$save[avail2$playerID==possible2$playerID[3]] <- "C"
table(avail2$save)

#y H > OBP
v <-ggplot(data=avail2,aes(x=salary,y=OBP))
v + geom_point(aes(colour=save,size=save))



ops <- data.frame(lost_players$OBP + lost_players$SLG, 
                  possible2$OBP + possible2$SLG)
colnames(ops) <- c('lost_ops','pre_ops')
ops



               