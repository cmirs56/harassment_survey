
library(tidyverse)
library(readr)
library(car)
library(ggplot2)


# load data ---------------------------------------------------------------

read.csv.any <- function(text, sep = "", ...) {
  encoding <- as.character(guess_encoding(text)[1,1])
  setting <- as.character(tools::file_ext(text))
  if(sep != "" | !(setting  %in% c("csv", "txt")) ) setting <- "custom"
  separate <- list(csv = ",", txt = "\n", custom = sep)
  result <- read.table(text, sep = separate[[setting]], fileEncoding = encoding, ...)
  return(result)
}
df <- read.csv.any("c:/Users/MC/R/harassment_survey/harassment_raw.csv", header = TRUE, na.strings = c("NA", ""))
df

str(df)
head(df)
summary(df)


# harassment --------------------------------------------------------------

# 정규분포 검정
# Ho: normal / H1: non-normal 

shapiro.test(df$harassment)   #non-normal distribution 

hist(df$harassment, breaks = 20)
hist(df$harassment, freq = FALSE, breaks = 20, main = "Kernel Density Plot of df$harassment")
lines(density(df$harassment), col = "blue", lwd = 3)

qqnorm(df$harassment)
qqline(df$harassment)


# perception --------------------------------------------------------------

shapiro.test(df$perception)   #H1

hist(df$perception, breaks = 5)
hist(df$perception, freq = FALSE, breaks = 5, main = "Kernel Density Plot of df$perception")
lines(density(df$perception), col = "blue", lwd = 3)

qqnorm(df$perception)
qqline(df$perception)


# change data to normal distribution --------------------------------------

df <- transform(df, harassment_recip = 1/harassment)
hist(df$harassment_recip, breaks = 20, freq = TRUE)

df <- transform(df, perception_recip = 1/perception)
hist(df$perception_recip, breaks = 5, freq = TRUE)

shapiro.test(df$harassment_recip)
shapiro.test(df$perception_recip)

###Box-Cox Transformation, boxcox() 함수

library(MASS)
library(forecast)

boxcox(df$perception ~ 1)
p <- powerTransform(df$perception)
y <- bcPower(df$perception, p$lambda)
hist(y)
qqnorm(y)
hist(df$perception)

df$harassment_bc <- df$harassment + 1
boxcox(df$harassment_bc ~ 1)
p <- powerTransform(df$harassment_bc)
y <- bcPower(df$harassment_bc, p$lambda)
hist(y)
qqnorm(y)
hist(df$harassment)


# harassment by joining type ------------------------------------------------------------
 
table(df$join)

with(df, tapply(harassment, join, summary))

boxplot(harassment ~ join, 
        data = df,
        main = "harassment by joining type",
        xlab = "join",
        ylab = "harassment")

ggplot(df, aes(x=harassment)) +
  geom_histogram(binwidth = 5) +
  facet_grid(join ~ .) +
  ggtitle("harassment by joining type")

# below is useless
var.test(harassment ~ join, data = df)  #p-value = 0.01223, H1, var.equal = FALSE

t.test(harassment ~ join, 
       data = df,
       alternative = c("two.sided"),
       var.equal = FALSE,
       conf.level = 0.95)               #p-value = 0.1412, H0 



# Wilcoxon rank sum test (혹은 Mann-Whitney U-test)_nonparametric test 

wilcox.test(harassment ~ join, 
            data = df, 
            alternative = c("two.sided"),
            mu = 0,
            conf.int = FALSE,
            conf.level = 0.95)        #p-value = 0.4431, H0 


# harassment by gender ------------------------------------------------------------------

wilcox.test(harassment ~ gender, 
            data = df, 
            alternative = c("two.sided"),
            mu = 0,
            conf.int = FALSE,
            conf.level = 0.95)        #p-value = 0.1773, H0

# perception by joining type ------------------------------------------------------------

table(df$perception)

with(df, tapply(perception, join, summary))

boxplot(perception ~ join, 
        data = df,
        main = "perception by joining type",
        xlab = "join",
        ylab = "perception")

ggplot(df, aes(x=perception)) +
  geom_histogram(binwidth = 1) +
  facet_grid(join ~ .) +
  ggtitle("perception by joining type")

# Wilcoxon rank sum test (혹은 Mann-Whitney U-test)_nonparametric test 

wilcox.test(perception ~ join, 
            data = df, 
            alternative = c("two.sided"),
            mu = 0,
            conf.int = FALSE,
            conf.level = 0.95)       #p-value = 0.05942, H0

# perception by gender ------------------------------------------------------------------

wilcox.test(perception ~ gender, 
            data = df, 
            alternative = c("two.sided"),
            mu = 0,
            conf.int = FALSE,
            conf.level = 0.95)        #p-value = 0.4086, H0


# harassment_factor > 3  -------------------------------------------------------------

kruskal.test(harassment ~ factor(age), data = df)
kruskal.test(harassment ~ factor(position), data = df)
kruskal.test(harassment ~ factor(duty), data = df)
kruskal.test(harassment ~ factor(affiliate), data = df)
kruskal.test(harassment ~ factor(seniority), data = df)
kruskal.test(harassment ~ factor(job), data = df)
kruskal.test(harassment ~ factor(location), data = df)

library(userfriendlyscience)
posthocTGH(df$position, y = df$harassment, method = 'games-howell')
posthocTGH(df$seniority, y = df$harassment, method = 'games-howell')


# perception_factor > 3 ---------------------------------------------------

kruskal.test(perception ~ factor(age), data = df)
kruskal.test(perception ~ factor(position), data = df)
kruskal.test(perception ~ factor(duty), data = df)
kruskal.test(perception ~ factor(affiliate), data = df)
kruskal.test(perception ~ factor(seniority), data = df)
kruskal.test(perception ~ factor(job), data = df)
kruskal.test(perception ~ factor(location), data = df)

posthocTGH(df$seniority, y = df$perception, method = 'games-howell')
posthocTGH(df$job, y = df$perception, method = 'games-howell')
posthocTGH(df$location, y = df$perception, method = 'games-howell')


# monnBook  ---------------------------------------------------------------

library(moonBook)
mytable(df)

df$position <- as.factor(df$position)
df$seniority <- as.factor(df$seniority)
df$job <- as.factor(df$job)
df$location <- as.factor(df$location)

str(df)

mytable(job ~ harassment, data = df)
mytable(reason ~ harassment, data = df)


# chart -------------------------------------------------------------------

ggplot(df, aes(location, harassment))+
  geom_point(mapping = aes(shape = seniority, color = job))+
  geom_boxplot()+
  theme_bw()

ggplot(df, aes(location, harassment))+
  geom_point(mapping = aes(color = job))+
  geom_boxplot()+
  theme_bw()

ggplot(df, aes(position, harassment))+
  geom_point(mapping = aes(color = seniority))+
  geom_boxplot()+
  theme_bw()

ggplot(df, aes(seniority, harassment))+
  geom_point(mapping = aes(color = position))+
  geom_boxplot()+
  theme_bw()

ggplot(df, aes(relation, harassment))+
  geom_point(mapping = aes(color = position))+
  geom_boxplot()+
  theme_bw()

ggplot(df, aes(location, harassment))+
  geom_point(mapping = aes(color = relation))+
  geom_boxplot()+
  theme_bw()
