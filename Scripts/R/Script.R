library(ggplot2)

#As suggested we will collect data twice. 
#1) Test Trials
#2) Re Test Trials
#Note: Right Arrow --> the targer was present
#Left Arrow --> the ttarget was not present

Test.Mono <- read.csv("Test Mono.csv", header = T)
ReTest.Mono <- read.csv("ReTest Mono.csv", header = T)

#Renaming LeftArrow As 0(target absent) and Right Arrow 1(target present)   
Test.Mono$response <- gsub("LeftArrow", 0, Test.Mono$response)
Test.Mono$response <- gsub("RightArrow", 1, Test.Mono$response)
Test.Mono$response <- as.numeric(Test.Mono$response)
ReTest.Mono$response <- gsub("LeftArrow", 0, ReTest.Mono$response)
ReTest.Mono$response <- gsub("RightArrow", 1, ReTest.Mono$response)
ReTest.Mono$response <- as.numeric(ReTest.Mono$response)

#Matching trials to compare correct and incorrect trials
Test.Mono$Match <- ifelse(as.numeric(Test.Mono$targetPresent) == as.numeric(Test.Mono$response), 'correct', 'incorrect')
ReTest.Mono$Match <- ifelse(as.numeric(ReTest.Mono$targetPresent) == as.numeric(ReTest.Mono$response), 'correct', 'incorrect')

#Subset correct trials to check the accuracy


#Look at Reaction time distribution
hist(Test.Mono$RT, prob = T)
lines(density(Test.Mono$RT, adjust = 2), col = "darkgreen", lwd = 2)
hist(ReTest.Mono$RT, prob = T)
lines(density(ReTest.Mono$RT, adjust = 4), col = "darkblue", lwd = 2)

#Plot time series of subjects: maybe we can identify individual differences
plot.ts(Test.Mono[1:50, 6])
plot.ts(Test.Mono[51:100, 6])
plot.ts(ReTest.Mono[1:50, 6])
plot.ts(ReTest.Mono[51:100, 6])

# plot comparision between subjects
ggplot(Removed.Test.Mono, aes(x = setSize, y = RT, fill = subID)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    ylab('Response Time (s)') + xlab('Set Size') 
# It looks like Chris is heavily affecting our data, we should consider removing his data altogether (as an outlier in our scarce dataset)
ggplot(Removed.ReTest.Mono, aes(x = setSize, y = RT, fill = subID)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    ylab('Response Time (s)') + xlab('Set Size')
# Sagar might have to be removed from the ReTest data too
ggplot(Removed.Test.Bi, aes(x = setSize, y = RT, fill = subID)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    ylab('Response Time (s)') + xlab('Set Size') 
# It looks like Chris is heavily affecting our data, we should consider removing his data altogether (as an outlier in our scarce dataset)
ggplot(Removed.ReTest.Bi, aes(x = setSize, y = RT, fill = subID)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    ylab('Response Time (s)') + xlab('Set Size')

###############################
 # Statistics:: Mono
###############################
#Some Descriptive Statistics 
library(psych)
TestDescriptive <- describeBy(Test.Mono$RT, Test.Mono$setSize)
print("Descriptive Statistic for Test Trial")
TestDescriptive
ReTestDescriptive <- describeBy(ReTest.Mono$RT, ReTest.Mono$setSize)
print("Descriptive Statistic for Re-test Trial:")
ReTestDescriptive
#Note: For RT before normalization 'Median' is the important statistic. Howevevr, after normalisation of RT mean and sd.

#Correlation Between RT and Subject Responses
print("Correlation between reaction time and given reponse- Test Trial:")
cor(Test.Mono$RT, Test.Mono$response, method = c("pearson"))
print("Correlation between reaction time and given reponse- ReTest Trial")
cor(ReTest.Mono$RT, ReTest.Mono$response, method = c("pearson"))
#Correlation between test-retest scores
print("Correlation between test and retest scores")
cor(Test.Mono$RT, ReTest.Mono$RT, method = c("pearson"))

# t-Test and pearson correlation
#among 4,8,16 set sizes first
t.test(Test.Mono[Test.Mono$setSize == 4,]$RT, Test.Mono[Test.Mono$setSize == 8,]$RT, paired = T)
cor(Test.Mono[Test.Mono$setSize == 4,]$RT, Test.Mono[Test.Mono$setSize == 8,]$RT, method = c("pearson"))
t.test(Test.Mono[Test.Mono$setSize == 4,]$RT, Test.Mono[Test.Mono$setSize == 16,]$RT, paired = T)
cor(Test.Mono[Test.Mono$setSize == 4,]$RT, Test.Mono[Test.Mono$setSize == 16,]$RT, method = c("pearson"))
t.test(Test.Mono[Test.Mono$setSize == 16,]$RT, Test.Mono[Test.Mono$setSize == 8,]$RT, paired = T)
cor(Test.Mono[Test.Mono$setSize == 16,]$RT, Test.Mono[Test.Mono$setSize == 8,]$RT, method = c("pearson"))

#ANOVA
Test.Mono$subID <- as.factor(Test.Mono$subID)
aov_testTrials <- aov(RT ~ setSize + Error(subID / setSize), data = Test.Mono)
summary(aov_testTrials) 
ReTest.Mono$subID <- as.factor(Test.Mono$subID)
aov_RetestTrials <- aov(RT ~ setSize + Error(subID / setSize), data = ReTest.Mono)
summary(aov_RetestTrials)

#####################################################################################################################################################
                                                        # The same thing with Bisected
#####################################################################################################################################################

Test.Bi <- read.csv("Test Bisected.csv", header = T)
ReTest.Bi <- read.csv("ReTest Bisected.csv", header = T)

Test.Bi$response <- gsub("LeftArrow", 0, Test.Bi$response)
Test.Bi$response <- gsub("RightArrow", 1, Test.Bi$response)
Test.Bi$response <- as.numeric(Test.Bi$response)
ReTest.Bi$response <- gsub("LeftArrow", 0, ReTest.Bi$response)
ReTest.Bi$response <- gsub("RightArrow", 1, ReTest.Bi$response)
ReTest.Bi$response <- as.numeric(ReTest.Bi$response)
Test.Bi$Match <- ifelse(as.numeric(Test.Bi$targetPresent) == as.numeric(Test.Bi$response), 'correct', 'incorrect')
ReTest.Bi$Match <- ifelse(as.numeric(ReTest.Bi$targetPresent) == as.numeric(ReTest.Bi$response), 'correct', 'incorrect')

hist(Test.Bi$RT, prob = T)
lines(density(Test.Bi$RT, adjust = 2), col = "darkgreen", lwd = 2)
hist(ReTest.Bi$RT, prob = T)
lines(density(ReTest.Bi$RT, adjust = 2), col = "darkblue", lwd = 2)

plot.ts(Test.Bi[1:50, 6])
plot.ts(Test.Bi[51:100, 6])
plot.ts(ReTest.Bi[1:50, 6])
plot.ts(ReTest.Bi[51:100, 6])

# plot comparision between subjects
ggplot(Test.Bi, aes(x = setSize, y = RT, fill = subID)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    ylab('Response Time (s)') + xlab('Set Size')
ggplot(ReTest.Bi, aes(x = setSize, y = RT, fill = subID)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    ylab('Response Time (s)') + xlab('Set Size')

##############################
# Statistics:: Bisected
##############################
TestDescriptive.Bi <- describeBy(Test.Bi$RT, Test.Bi$setSize)
print("Descriptive Statistic for Test Trial")
TestDescriptive.Bi
ReTestDescriptive.Bi <- describeBy(ReTest.Bi$RT, ReTest.Bi$setSize)
print("Descriptive Statistic for Re-test Trial:")
ReTestDescriptive.Bi

print("Correlation between reaction time and given reponse- Test Trial:")
cor(Test.Bi$RT, Test.Bi$response, method = c("pearson"))
print("Correlation between reaction time and given reponse- ReTest Trial")
cor(ReTest.Bi$RT, ReTest.Bi$response, method = c("pearson")) # There is a NA value in response
print("Correlation between test and retest scores")
cor(Test.Bi$RT, ReTest.Bi$RT, method = c("pearson"))

t.test(Test.Bi[Test.Bi$setSize == 4,]$RT, Test.Bi[Test.Bi$setSize == 8,]$RT, paired = T)
cor(Test.Bi[Test.Bi$setSize == 4,]$RT, Test.Bi[Test.Bi$setSize == 8,]$RT, method = c("pearson"))
t.test(Test.Bi[Test.Bi$setSize == 4,]$RT, Test.Bi[Test.Bi$setSize == 16,]$RT, paired = T)
cor(Test.Bi[Test.Bi$setSize == 4,]$RT, Test.Bi[Test.Bi$setSize == 16,]$RT, method = c("pearson"))
t.test(Test.Bi[Test.Bi$setSize == 16,]$RT, Test.Bi[Test.Bi$setSize == 8,]$RT, paired = T)
cor(Test.Bi[Test.Bi$setSize == 16,]$RT, Test.Bi[Test.Bi$setSize == 8,]$RT, method = c("pearson"))

#ANOVA
Test.Bi$subID <- as.factor(Test.Mono$subID)
aov_Bi.testTrials <- aov(RT ~ setSize + Error(subID / setSize), data = Test.Bi)
summary(aov_Bi.testTrials)
ReTest.Bi$subID <- as.factor(Test.Bi$subID)
aov_Bi.RetestTrials <- aov(RT ~ setSize + Error(subID / setSize), data = ReTest.Bi)
summary(aov_Bi.RetestTrials)


#####################################################################################################################################################
#To check significance among the SetSizes of Mono and Bi
#Now, perform (1)t-test
#             (2) correlation,
#             (3)ANOVA before normalisation
#             (4)Normalizing RT(&check the strength of normality)
#             (5)ANOVA with normalised RT
#             (6)Comparision Plot(SetSizes on X-Label and RT on Y-Label )
#             (7)Logistic regressio model.
#####################################################################################################################################################


# make a table with 7 columns.....(c1) SubID (c2) SetSize (c3) trial....(c4) RT for Mono Test...then Retest...Bi test...retest
bigTable <- data.frame(SubId = c(), SetSize = c(), RT.test.mono = c())
Test.Mono.min <- Test.Mono[,c("subID", "setSize", "RT", "Match")]
Test.Mono.min <- Test.Mono.min[with(Test.Mono.min, order(subID, setSize)),] 
ReTest.Mono.min <- ReTest.Mono[,c("subID", "setSize", "RT", "Match")]
ReTest.Mono.min <- ReTest.Mono.min[with(ReTest.Mono.min, order(subID, setSize)),]
Test.Bi.min <- Test.Bi[,c("subID", "setSize", "RT", "Match")]
Test.Bi.min <- Test.Bi.min[with(Test.Bi.min, order(subID, setSize)),]
ReTest.Bi.min <- ReTest.Bi[,c("subID", "setSize", "RT", "Match")]
ReTest.Bi.min <- ReTest.Bi.min[with(ReTest.Bi.min, order(subID, setSize)),]
bigTable <- Test.Mono.min[with(Test.Mono.min, order(subID, setSize)),] 
colnames(bigTable) <- c("subID", "setSize", "RT.Test.Mono", "Match.Test.Mono")
bigTable$RT.ReTest.Mono <- ReTest.Mono.min$RT
bigTable$RT.Test.Bi <- Test.Bi.min$RT
bigTable$RT.ReTest.Bi <- ReTest.Bi.min$RT
bigTable$Match.ReTest.Mono <- ReTest.Mono.min$Match
bigTable$Match.Test.Bi <- Test.Bi.min$Match
bigTable$Match.ReTest.Bi <- ReTest.Bi.min$Match
bigTable$Match.Test.Mono <- gsub("incorrect", 1, bigTable$Match.Test.Mono)
bigTable$Match.Test.Mono <- gsub("correct", 0, bigTable$Match.Test.Mono)
bigTable$Match.ReTest.Mono <- gsub("incorrect", 1, bigTable$Match.ReTest.Mono)
bigTable$Match.ReTest.Mono <- gsub("correct", 0, bigTable$Match.ReTest.Mono)
bigTable$Match.Test.Bi <- gsub("incorrect", 1, bigTable$Match.Test.Bi)
bigTable$Match.Test.Bi <- gsub("correct", 0, bigTable$Match.Test.Bi)
bigTable$Match.ReTest.Bi <- gsub("incorrect", 1, bigTable$Match.ReTest.Bi)
bigTable$Match.ReTest.Bi <- gsub("correct", 0, bigTable$Match.ReTest.Bi)
bigTable[is.na(bigTable)] <- 1 # translate NAs to incorrect for simplicity

# Check incorrect trials
subset(bigTable,Match.Test.Mono == 1 | Match.ReTest.Mono == 1 | Match.Test.Bi == 1 | Match.ReTest.Bi == 1)
# This could be troublesome... Bisected test has rarely been completed with less than 3 mistakes


# First of all, we need to determine if a participant was inconsistent with the answers 
errors_allowed <- 3
subsIds <- unique(bigTable$subID)
for (participant in subsIds) {
    pp_bigTable <- subset(bigTable, subID == participant)
    # Determine for each experiment if the participant should be considered
    # If they aren't, we recode their RTs to NAs and later ignore them
    if (sum(as.numeric(pp_bigTable$Match.Test.Mono)) > errors_allowed) {
        pp_bigTable$RT.Test.Mono <- NA
    }
    if (sum(as.numeric(pp_bigTable$Match.Test.Bi)) > errors_allowed) {
        pp_bigTable$RT.Test.Bi <- NA
    }
    if (sum(as.numeric(pp_bigTable$Match.ReTest.Mono)) > errors_allowed) {
        pp_bigTable$RT.ReTest.Mono <- NA
    }
    if (sum(as.numeric(pp_bigTable$Match.ReTest.Bi)) > errors_allowed) {
        pp_bigTable$RT.ReTest.Bi <- NA
    }
    # copy back the participants updated data
    bigTable[bigTable$subID == participant,] <- pp_bigTable
}

# OPTIONAL: We could also define a minimum RT and discard every trial with a lower RT than that

# Looks like we could try to transform it an make it more normalised


# But first lets locate and get rid of outliers
rt_mean.test.mono = mean(bigTable$RT.Test.Mono, na.rm = TRUE)
rt_mean.retest.mono = mean(bigTable$RT.ReTest.Mono, na.rm = TRUE)
rt_mean.test.bi = mean(bigTable$RT.Test.Bi, na.rm = TRUE)
rt_mean.retest.bi = mean(bigTable$RT.ReTest.Bi, na.rm = TRUE)
rt_sd.test.mono = sd(bigTable$RT.Test.Mono, na.rm = TRUE)
rt_sd.retest.mono = sd(bigTable$RT.ReTest.Mono, na.rm = TRUE)
rt_sd.test.bi = sd(bigTable$RT.Test.Bi, na.rm = TRUE)
rt_sd.retest.bi = sd(bigTable$RT.ReTest.Bi, na.rm = TRUE)
# abs($x - mean) <= 3*sd as done in previous lab and in the literature (2 or 3 times the sd from the mean)
deviation_allowed <- 2
# to_remove.test.mono <- Test.Mono[abs(Test.Mono$RT - rt_mean.test.mono) > deviation_allowed*rt_sd.test.mono,]
# to_remove.retest.mono <- ReTest.Mono[abs(ReTest.Mono$RT - rt_mean.retest.mono) > deviation_allowed*rt_sd.retest.mono,]
# to_remove.test.bi <- Test.Bi[abs(Test.Bi$RT - rt_mean.test.bi) > deviation_allowed*rt_sd.test.bi,]
# to_remove.retest.bi <- ReTest.Bi[abs(ReTest.Bi$RT - rt_mean.retest.bi) > deviation_allowed*rt_sd.retest.bi,]
to_remove.test.mono <- bigTable[abs(bigTable$RT.Test.Mono - rt_mean.test.mono) > deviation_allowed*rt_sd.test.mono,]
to_remove.test.mono <- to_remove.test.mono[!is.na(to_remove.test.mono$RT.Test.Mono),]
to_remove.retest.mono <- bigTable[abs(bigTable$RT.ReTest.Mono - rt_mean.retest.mono) > deviation_allowed*rt_sd.retest.mono,]
to_remove.retest.mono <- to_remove.retest.mono[!is.na(to_remove.retest.mono$RT.ReTest.Mono),]
to_remove.test.bi <- bigTable[abs(bigTable$RT.Test.Bi - rt_mean.test.bi) > deviation_allowed*rt_sd.test.bi,]
to_remove.test.bi <- to_remove.test.bi[!is.na(to_remove.test.bi$RT.Test.Bi),]
to_remove.retest.bi <- bigTable[abs(bigTable$RT.ReTest.Bi - rt_mean.retest.bi) > deviation_allowed*rt_sd.retest.bi,]
to_remove.retest.bi <- to_remove.retest.bi[!is.na(to_remove.retest.bi$RT.ReTest.Bi),]

# remove values from our table
bigTable[(row.names(bigTable) %in% row.names(to_remove.test.mono)),c("RT.Test.Mono")] <- NA
bigTable[(row.names(bigTable) %in% row.names(to_remove.retest.mono)),c("RT.ReTest.Mono")] <- NA
bigTable[(row.names(bigTable) %in% row.names(to_remove.test.bi)),c("RT.Test.Bi")] <- NA
bigTable[(row.names(bigTable) %in% row.names(to_remove.retest.bi)),c("RT.ReTest.Bi")] <- NA
# Removed.Test.Mono <- Test.Mono[! (row.names(Test.Mono) %in% row.names(to_remove.test.mono)),]
# Removed.ReTest.Mono <- ReTest.Mono[! (row.names(ReTest.Mono) %in% row.names(to_remove.retest.mono)),]
# Removed.Test.Bi <- Test.Bi[! (row.names(Test.Bi) %in% row.names(to_remove.test.bi)),]
# Removed.ReTest.Bi <- ReTest.Bi[! (row.names(ReTest.Bi) %in% row.names(to_remove.retest.bi)),]

# Visualize
hist(bigTable$RT.Test.Mono)
hist(bigTable$RT.ReTest.Mono)
hist(bigTable$RT.Test.Bi)
hist(bigTable$RT.ReTest.Bi)

# right skewed data as expected of RTs -> log-transform it

bigTable$RT.Test.Mono <- log(bigTable$RT.Test.Mono)
bigTable$RT.ReTest.Mono <- log(bigTable$RT.ReTest.Mono)
bigTable$RT.Test.Bi <- log(bigTable$RT.Test.Bi)
bigTable$RT.ReTest.Bi <- log(bigTable$RT.ReTest.Bi)

# Check normality of data in our vectors
shapiro.test(bigTable$RT.Test.Mono)
shapiro.test(bigTable$RT.ReTest.Mono)
shapiro.test(bigTable$RT.Test.Bi)
shapiro.test(bigTable$RT.ReTest.Bi)

hist(bigTable$RT.Test.Mono)
hist(bigTable$RT.ReTest.Mono)
hist(bigTable$RT.Test.Bi)
hist(bigTable$RT.ReTest.Bi)

# none of them follow a normal distribution but it doesn't really matter
# Because we didn't have vectors of equal sizes so wilcox.test or t.test wouldn't be applicable
# We will have to stick to linear models

# Linear models

# ReTest.Mono --> Test.Bi

lm1 <- lm(RT.Test.Bi ~ RT.ReTest.Mono, bigTable, na.action = na.exclude)
summary(lm1)
# significant intercept and ReTest values
# Let's look at the residuals
bigTable$predicted1.Test.Bi <- predict(lm1)
bigTable$residuals1.Test.Bi <- resid(lm1)
ggplot(bigTable, aes(x = RT.ReTest.Mono, y = RT.Test.Bi)) +
    geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
    geom_segment(aes(xend = RT.ReTest.Mono, yend = predicted1.Test.Bi), alpha = .2) +
    geom_point(aes(color = abs(residuals1.Test.Bi), size = abs(residuals1.Test.Bi))) + 
    scale_color_continuous(low = "green", high = "red") + 
    guides(color = FALSE, size = FALSE) + 
    geom_point(aes(y = predicted1.Test.Bi), shape = 2) + 
    theme_bw()
# The plot shows graphically the size of the residual value using a colour code 
# (red is longer line to green - smaller line) and size of point. The size of residual 
# is the length of the vertical line from the point to where it meets the regression line.

# This linear model fit is not ideal judging by the residuals, but statistically significant

lm2 <- lm(RT.Test.Bi ~ RT.ReTest.Mono + setSize, data = bigTable, na.action = na.exclude)
summary(lm2)
bigTable$predicted2.Test.Bi <- predict(lm2)
bigTable$residuals2.Test.Bi <- resid(lm2)
ggplot(bigTable, aes(x = RT.ReTest.Mono, y = RT.Test.Bi)) +
    geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
    geom_segment(aes(xend = RT.ReTest.Mono, yend = predicted2.Test.Bi), alpha = .2) +
    geom_point(aes(color = abs(residuals2.Test.Bi), size = abs(residuals2.Test.Bi))) + 
    scale_color_continuous(low = "green", high = "red") + 
    guides(color = FALSE, size = FALSE) + 
    geom_point(aes(y = predicted2.Test.Bi), shape = 2) + 
    theme_bw()
# This plot now loses some meaning because we are using 2 variables instead of only one but I didn't want to start looking into how to 3d plot
wilcox.test(bigTable$predicted1.Test.Bi, bigTable$predicted2.Test.Bi)
# This linear model offers better results judging by the output of the model, but no real difference statistically speaking

lm3 <- lm(RT.Test.Bi ~ RT.ReTest.Mono * setSize, data = bigTable, na.action = na.exclude)
summary(lm3)
bigTable$predicted3.Test.Bi <- predict(lm3)
bigTable$residuals3.Test.Bi <- resid(lm3)
ggplot(bigTable, aes(x = RT.ReTest.Mono, y = RT.Test.Bi)) +
    geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
    geom_segment(aes(xend = RT.ReTest.Mono, yend = predicted3.Test.Bi), alpha = .2) +
    geom_point(aes(color = abs(residuals3.Test.Bi), size = abs(residuals3.Test.Bi))) + 
    scale_color_continuous(low = "green", high = "red") + 
    guides(color = FALSE, size = FALSE) + 
    geom_point(aes(y = predicted3.Test.Bi), shape = 2) + 
    theme_bw()
# This plot now loses some meaning but I used it anyway
wilcox.test(bigTable$predicted2.Test.Bi, bigTable$predicted3.Test.Bi)
# This model does not offer anything new either.

# Let's try a mixed model that adds a random variation on subjects Ids
library(lme4)
lmx1 <- lmer(RT.Test.Bi ~ RT.ReTest.Mono * setSize + (1|subID), data = bigTable, na.action = na.exclude)
summary(lmx1)
# The t-values in this model are not significant save for the intercept
lmx2 <- lmer(RT.Test.Bi ~ RT.ReTest.Mono + setSize + (1|subID), data = bigTable, na.action = na.exclude)
summary(lmx2)
# The t-value for RT.ReTest.Mono is not significant either, and that is the one we were most interested in
# Also the residuals look very bad compared to the previous linear models
wilcox.test(bigTable$predicted2.Test.Bi, predict(lmx2))
# But again no significant difference between them.


# (Re)Test.Mono --> Test.Bi

lm4 <- lm(RT.Test.Bi ~ RT.ReTest.Mono + RT.Test.Mono, bigTable, na.action = na.exclude)
summary(lm4)
bigTable$predicted4.Test.Bi <- predict(lm4)
bigTable$residuals4.Test.Bi <- resid(lm4)
# ggplot(bigTable, aes(x = RT.ReTest.Mono, y = RT.Test.Bi)) +
#     geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
#     geom_segment(aes(xend = RT.ReTest.Mono, yend = predicted1.Test.Bi), alpha = .2) +
#     geom_point(aes(color = abs(residuals1.Test.Bi), size = abs(residuals1.Test.Bi))) + 
#     scale_color_continuous(low = "green", high = "red") + 
#     guides(color = FALSE, size = FALSE) + 
#     geom_point(aes(y = predicted1.Test.Bi), shape = 2) + 
#     theme_bw()
lm5 <- lm(RT.Test.Bi ~ RT.ReTest.Mono + RT.Test.Mono + setSize, bigTable, na.action = na.exclude)
summary(lm5)
bigTable$predicted5.Test.Bi <- predict(lm5)
bigTable$residuals5.Test.Bi <- resid(lm5)

lm6 <- lm(RT.Test.Bi ~ RT.ReTest.Mono * RT.Test.Mono * setSize, bigTable, na.action = na.exclude)
summary(lm6)
bigTable$predicted6.Test.bi <- predict(lm6)
bigTable$residuals6.Test.Bi <- resid(lm6)

# Mixed models
lmx3 <- lmer(RT.Test.Bi ~ RT.ReTest.Mono * RT.Test.Mono * setSize + (1|subID), data = bigTable, na.action = na.exclude)
summary(lmx3)
lmx4 <- lmer(RT.Test.Bi ~ RT.ReTest.Mono + RT.Test.Mono + setSize + (1|subID), data = bigTable, na.action = na.exclude)
summary(lmx4)


# everything --> ReTest.Bi
lm7 <- lm(RT.ReTest.Bi ~ RT.Test.Mono + RT.ReTest.Mono + RT.Test.Bi, bigTable, na.action = na.exclude)
summary(lm7)
bigTable$predicted7.ReTest.Bi <- predict(lm7)
bigTable$residuals7.ReTest.Bi <- resid(lm7)

lm8 <- lm(RT.ReTest.Bi ~ RT.ReTest.Mono + RT.Test.Bi + setSize, bigTable, na.action = na.exclude)
summary(lm8)

lmx5 <- lmer(RT.ReTest.Bi ~ RT.ReTest.Mono + RT.Test.Mono + setSize + (1|subID), data = bigTable, na.action = na.exclude)
# There is an error because not all levels of the factor subID appear in this subset of data.
# We need more reliable data to run this model
summary(lmx5)


## CORRELATION TESTS
library(corrplot)
library(RColorBrewer)
M <- cor(bigTable[, c(2,3,5,6,7)], use = "complete.obs")
corrplot(M, type = "upper", order="hclust", col = brewer.pal(n=8, name="RdYlBu"))

library(Hmisc)
rcorr(as.matrix(bigTable[,c(2,3,5,6,7)]))

# Funnily enough, the strongest correlation exists between ReTest.Mono and Test.Bi 
#### I STOPPED WORKING HERE ####

# # Check with transformations
# shapiro.test(log(Removed.Test.Mono$RT))
# shapiro.test(log(Removed.ReTest.Mono$RT))
# shapiro.test(log(Removed.Test.Bi$RT))
# shapiro.test(log(Removed.ReTest.Bi$RT))
# # ... I've tried with different transformations and I can't really get something very normally distributed for all of them
# hist(log(Removed.Test.Mono$RT))
# hist(log(Removed.ReTest.Mono$RT))
# hist(log(Removed.Test.Bi$RT))
# hist(log(Removed.ReTest.Bi$RT))

# DATA CANNOT BE TRANSFORMED SO IT RESEMBLES A NORMAL DISTRIBUTION!!
# which means that we will have to use wilcox.test instead of t.test, no problem :)



Removed.bigTable <- data.frame(SubId = c(), SetSize = c(), RT.test.mono = c())
Removed.Test.Mono.min <- Removed.Test.Mono[,c("subID", "setSize", "RT")]
Removed.Test.Mono.min <- Removed.Test.Mono.min[with(Removed.Test.Mono.min, order(subID, setSize)),] 
Removed.ReTest.Mono.min <- Removed.ReTest.Mono[,c("subID", "setSize", "RT")]
Removed.ReTest.Mono.min <- Removed.ReTest.Mono.min[with(Removed.ReTest.Mono.min, order(subID, setSize)),]
Removed.Test.Bi.min <- Removed.Test.Bi[,c("subID", "setSize", "RT")]
Removed.Test.Bi.min <- Removed.Test.Bi.min[with(Removed.Test.Bi.min, order(subID, setSize)),]
Removed.ReTest.Bi.min <- Removed.ReTest.Bi[,c("subID", "setSize", "RT")]
Removed.ReTest.Bi.min <- Removed.ReTest.Bi.min[with(Removed.ReTest.Bi.min, order(subID, setSize)),]
Removed.bigTable <- Removed.Test.Mono.min[with(Removed.Test.Mono.min, order(subID, setSize)),] 
colnames(Removed.bigTable) <- c("subID", "setSize", "RT.Test.Mono")
Removed.bigTable$RT.ReTest.Mono <- Removed.ReTest.Mono.min$RT
Removed.bigTable$RT.Test.Bi <- Removed.Test.Bi.min$RT
Removed.bigTable$RT.ReTest.Bi <- Removed.ReTest.Bi.min$RT



##################################################################################
# t.test
##################################################################################
# Since data is not normally distributed we cannot use t.test, use wilcox.test instead
wilcox.test(Removed.Test.Mono[Removed.Test.Mono$setSize == 4,]$RT, Removed.Test.Mono[Removed.Test.Mono$setSize == 8,]$RT, paired = T)
wilcox.test(Removed.Test.Mono[Removed.Test.Mono$setSize == 4,]$RT, Removed.Test.Mono[Removed.Test.Mono$setSize == 16,]$RT, paired = T)
wilcox.test(Removed.Test.Mono[Removed.Test.Mono$setSize == 8,]$RT, Removed.Test.Mono[Removed.Test.Mono$setSize == 16,]$RT, paired = T)

wilcox.test(Removed.ReTest.Mono[Removed.ReTest.Mono$setSize == 4,]$RT, Removed.ReTest.Mono[Removed.ReTest.Mono$setSize == 8,]$RT, paired = T)
wilcox.test(Removed.ReTest.Mono[Removed.ReTest.Mono$setSize == 4,]$RT, Removed.ReTest.Mono[Removed.ReTest.Mono$setSize == 16,]$RT, paired = T)
wilcox.test(Removed.ReTest.Mono[Removed.ReTest.Mono$setSize == 8,]$RT, Removed.ReTest.Mono[Removed.ReTest.Mono$setSize == 16,]$RT, paired = T)

wilcox.test(Removed.Test.Bi[Removed.Test.Bi$setSize == 4,]$RT, Removed.Test.Bi[Removed.Test.Bi$setSize == 8,]$RT, paired = T)
wilcox.test(Removed.Test.Bi[Removed.Test.Bi$setSize == 4,]$RT, Removed.Test.Bi[Removed.Test.Bi$setSize == 16,]$RT, paired = T)
wilcox.test(Removed.Test.Bi[Removed.Test.Bi$setSize == 8,]$RT, Removed.Test.Bi[Removed.Test.Bi$setSize == 16,]$RT, paired = T)

wilcox.test(Removed.ReTest.Bi[Removed.ReTest.Bi$setSize == 4,]$RT, Removed.ReTest.Bi[Removed.ReTest.Bi$setSize == 8,]$RT, paired = T)
wilcox.test(Removed.ReTest.Bi[Removed.ReTest.Bi$setSize == 4,]$RT, Removed.ReTest.Bi[Removed.ReTest.Bi$setSize == 16,]$RT, paired = T)
wilcox.test(Removed.ReTest.Bi[Removed.ReTest.Bi$setSize == 8,]$RT, Removed.ReTest.Bi[Removed.ReTest.Bi$setSize == 16,]$RT, paired = T)
# unequal length in some vectors :(


##################################################################################
#Linear Regression
##################################################################################
#Mono; Useless
lm1 <- lm(RT.Test.Mono ~ setSize, data = bigTable) 
summary(lm1)
lm2 <- lm(RT.ReTest.Mono ~ setSize, data = bigTable) 
summary(lm2)
lm3 <- lm(RT.ReTest.Mono ~ setSize + RT.Test.Mono, data = bigTable) 
summary(lm3)

#Bisected; These have some significance
lm4 <- lm(RT.Test.Bi ~ setSize, data = bigTable) 
summary(lm4)
lm5 <- lm(RT.ReTest.Bi ~ setSize, data = bigTable) 
summary(lm5)
lm6 <- lm(RT.ReTest.Bi ~ setSize + RT.Test.Bi, data = bigTable) 
summary(lm6)

#Correlation Plot
library(corrplot)
Cor.BigTable <- cor(bigTable[,3:6])
corrplot(Cor.BigTable, method = "ellipse") #However there are correlations
