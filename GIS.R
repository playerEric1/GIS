df = read.csv(file = "survey_2021_cleaned.csv", stringsAsFactors = TRUE)
tail(df)
dim(df)
nrow(df)
ncol(df)

hist(df$Height, main = "Height", xlab = "cm")
# calculate and add the mean
mean_val = mean(df$Height)
abline(v = mean_val, lwd = 3)
# calculate and add the standard deviation lines around the mean
sdev = sd(df$Height)
# minus 1 sd
abline(v = mean_val-sdev, lwd = 3, lty = 2)
# plus 1 sd
abline(v = mean_val+sdev, lwd = 3, lty = 2)


# assign the table vector to tab
tab = table(df$AlevelGeog)
# calculate the proportions
prop = as.vector(tab/sum(tab))
# put into a data frame
data.frame(Count = tab, Prop = round(prop, 3))

boxplot(Height ~ Gender, data=df, col=c("red", "yellow3"), las=2, horizontal = T)
boxplot(Height ~ BloodDonor, data=df,
        col=c("red", "salmon"), las=2, horizontal = T, ylab = "Blood Donor")
with(df, aggregate(Hand_Span, by=list(BloodDonor) , FUN=summary))
# less than 110 mm: lower.tail = TRUE
pnorm(110, mean = mean(df$Hand_Span), sd = sd(df$Hand_Span), lower.tail = TRUE)

# part2
str(df)
summary(df)
plot(df$ShoeSize, df$Height, pch = 19, cex = 0.6)
## Create a sample of the population
sample.10 <- sample(pop, 70)
## Mean and s.d.
mean(sample.10)
sd(sample.10)
## Standard Error
sd(sample.10)/sqrt(length(sample.10))


# create a vector of sample sizes
X = seq(10, 100, 10)
X
# create a vector of sample errors from these
# an empty vector to be populated in the for loop below
SE = vector()
# now loop though each value in X
for (i in X){
  # create a sample for the ith value in X
  # set the seed - see the Info box below
  set.seed(12)
  sample.i = sample(pop, i)
  # calculate the SE
  se.i = sd(sample.i)/sqrt(length(sample.i))
  # add to the SE vector
  SE = append(SE, se.i)
}
# check the result
SE
# plot the SEs
plot(x = X, y = SE,
     pch = 19, col = "dodgerblue",
     xlab = "Sample Size", ylab = "Standard Error",
     main = "Impact of sample size on Standard Error")
lines(x = X, y = SE)

# you have already created the sample with
sample.10 <- sample(pop, 70)
m <- mean(sample.10)
std <- sd(sample.10)
n <- length(sample.10)
error <- qnorm(0.975)*std/sqrt(n)
lower.bound <- m-error
upper.bound <- m+error
m
std
upper.bound
lower.bound

## assign a histogram object to a variable
h <- hist(pop, breaks = 15, plot = F)
## create a vector of colours
col = rep("tomato", length(h$breaks))
## colour the histogram yellow between the Confidence Intervals
index <- h$breaks > lower.bound & h$breaks < upper.bound
col[index] <- "#FFDC00"
## colour the histogram grey for values outside of Mean+St Dev and Mean-St Dev
index <- h$breaks < m-std | h$breaks > m+std
col[index] <- "grey"
plot(h, col = col,
     main = "Confidence intervals & St Dev, n = 10",
     xlab = "Height"
     )

ht = df$Height
htc = ht[ht >= 180]
htc
length(htc)

mean(df[df[,'StateSecondary16'] == "Yes", c(14)])
mean(df[df[,'StateSecondary16'] == "No", c(14)])



library(tidyverse)
tb=as_tibble(df)
tb
tb_sel = select(tb, Height, GymHours, SocialMediaHours, Piercings)
cor(tb_sel)
df %>% as_tibble() %>% select(Height, GymHours, SocialMediaHours, Piercings) %>%
  ggpairs(aes(alpha = 0.9))

# part3
set.seed(-666)
sample.30 = sample(pop, 30)
h = hist(pop, breaks = 10, plot = F)
sample.30 = sample(pop, 30)
se.from.sample = function(x) {
  sample.i = sample(pop, x)
  se.i = sd(sample.i)/sqrt(length(sample.i))
  # return the SE
  return(se.i)
}
head(pc_data)
head(df$PostCode)
df$PostCode = str_replace_all(df$PostCode, " ", "")
df$PostCode = toupper(df$PostCode)
head(df$PostCode)
dim(df)
df = inner_join(df, pc_data, by = c("PostCode" = "pc"))
head(df)
help(rainbow)

par(mar = c(4,17,1,1))
boxplot(Height ~ oac, data=df, col=hcl.colors(8, palette = "Blue-Red 3", alpha = 1, fixup = TRUE), las=2,
        horizontal = T, ylab = "")

help(margin)

skewness(df$Piercings)
# It is not normally distributed 0.9190024 (need to be recalculated)


skewness(rnorm(10000))

kurtosis(df$GymHours)
cor(df$ShoeSize, df$Height)
plot(df$ShoeSize, df$Height, pch = 19, cex = 0.6,
     xlab = "Shoe Size", ylab = "Height")
abline(lm(Height~ShoeSize, data = df), lwd = 2, col = "darkgreen", lty = 2)

cor.test(df$Hand_Span, df$Height)
# p-value = 0.006477 The relationship is significant. This means the correlation would be expected to be found 6/1000 times from randomly selected data. Unsurprisingly this tells us to reject the Null Hypothesis of no relationship and to accept the alternative
#hypothesis, with a pvalue (probability value) of near to 0. This correlation is so unlikely, that it would only
#occur once out of 22,000,000,000,000,000 randomly selected of data, and therefore extremely surprising!

library(sf)
library(maps)
library(tmap)
table(df$Diet)
# convert to sf
df_sf = st_as_sf(df, coords = c("long", "lat"),
                 crs = 4326, agr = "constant")
tmap_mode("view")
tm_shape(df_sf) +
  tm_dots(col = "StateSecondary16", palette = "Set1") +
  tm_basemap('OpenStreetMap')
# reset the tmap plot mode
tmap_mode("plot")

tm_shape(df_sf) +
  tm_dots(col = "blue", size = 0.2, alpha = 0.5)
tm_dots(col = "StateSecondary16", palette = "Set1") +
  tm_basemap('OpenStreetMap')