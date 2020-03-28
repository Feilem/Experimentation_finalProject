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
# subID setSize RT.Test.Mono Match.Test.Mono RT.ReTest.Mono RT.Test.Bi RT.ReTest.Bi Match.ReTest.Mono Match.Test.Bi Match.ReTest.Bi
# 44   Test.Andy.Mono       4     1.022601               0       0.774129   3.325279     0.721085                 0             0               1
# 45   Test.Andy.Mono       4     0.662751               0       0.719098   0.795752     1.379976                 0             1               0
# 46   Test.Andy.Mono       4     0.864442               1       0.725549   1.859321     1.291054                 0             0               0
# 55   Test.Andy.Mono       4     0.415995               1       1.135912   0.722925     1.056195                 0             1               1
# 67   Test.Andy.Mono       4     0.625293               0       0.645164   1.105199     1.367160                 0             1               0
# 54   Test.Andy.Mono       8     0.472221               0       0.545964   3.719829     1.037642                 0             1               0
# 77   Test.Andy.Mono       8     0.487250               0       0.686664   1.776919     1.208873                 0             1               0
# 81   Test.Andy.Mono       8     0.536066               1       0.481230   1.843471     1.302744                 0             0               0
# 82   Test.Andy.Mono       8     1.056558               0       1.240927   1.378475     1.392273                 0             1               0
# 53   Test.Andy.Mono      16     0.386070               0       0.695152   2.151008     1.559100                 0             1               1
# 56   Test.Andy.Mono      16     1.317168               0       0.727048   3.400925     1.602325                 0             1               0
# 58   Test.Andy.Mono      16     0.395950               0       0.760044   1.577691     1.693980                 0             0               1
# 59   Test.Andy.Mono      16     0.968468               0       0.602416   1.923217     2.144558                 1             1               1
# 68   Test.Andy.Mono      16     0.494132               0       1.072377   1.836041     1.474255                 0             0               1
# 169 Test.Chris.Mono       4     1.778905               1       0.998169   1.665980     1.259648                 0             0               0
# 176 Test.Chris.Mono       4     6.352540               1       0.561635   1.309500     1.030976                 0             0               0
# 185 Test.Chris.Mono       4     6.833061               1       0.451500   1.129292     3.219935                 0             0               0
# 190 Test.Chris.Mono       4     0.779953               0       0.433808   1.453047     1.097752                 0             1               0
# 200 Test.Chris.Mono       8     0.475671               0       0.424514   2.723750     0.884290                 0             1               0
# 181 Test.Chris.Mono      16     9.777698               1       0.594789   3.355250     0.839927                 0             0               0
# 127   Test.Olu.Mono       4     1.131750               1       0.417781   1.610419     1.481705                 1             0               0
# 155   Test.Olu.Mono       4     0.536886               0       0.525237   1.117270     1.516515                 0             0               1
# 143   Test.Olu.Mono       8     0.568991               0       0.531027   1.686639     1.218058                 1             1               0
# 146   Test.Olu.Mono       8     0.757705               0       0.836562   2.114260     1.859983                 0             0               1
# 150   Test.Olu.Mono       8     0.647550               0       0.676805   1.926689     1.940301                 0             1               0
# 151   Test.Olu.Mono       8     0.550023               0       0.555727   2.316651     1.723965                 0             1               0
# 160   Test.Olu.Mono       8     0.740951               0       0.539978   2.230870     0.869770                 0             1               0
# 131   Test.Olu.Mono      16     0.355638               0       0.731908   2.417254     3.425683                 0             1               1
# 132   Test.Olu.Mono      16     0.549093               0       0.658698   2.837205     2.718554                 0             1               1
# 138   Test.Olu.Mono      16     0.501017               1       0.527358   2.702946     3.314746                 0             0               0
# 142   Test.Olu.Mono      16     0.723670               0       0.744663   3.137612     3.821991                 0             0               1
# 145   Test.Olu.Mono      16     0.841687               0       0.589166   3.087987     3.606982                 0             1               0
# 162   Test.Olu.Mono      16     0.776668               0       0.704553   3.860501     3.275416                 0             0               1
# 115 Test.Rahul.Mono       8     0.581227               0       0.539465   0.875412     1.947853                 0             0               1
# 123 Test.Rahul.Mono       8     0.596566               1       0.704800   2.740868     1.728188                 0             0               0
# 85  Test.Rahul.Mono      16     1.669200               0       0.502888   2.072777     1.554504                 0             0               1
# 93  Test.Rahul.Mono      16     0.511546               0       0.594899   2.189776     2.336344                 0             1               0
# 117 Test.Rahul.Mono      16     0.574051               0       0.506789   3.226851     2.488250                 0             0               1
# 5   Test.Sagar.Mono       4     0.739722               0       0.560575   1.636073     3.394640                 0             1               0
# 21  Test.Sagar.Mono       4     0.508559               0       2.098871   1.430768     1.555479                 0             0               1
# 8   Test.Sagar.Mono       8     0.808220               0       0.761140   2.740488     8.240092                 0             0               1
# 14  Test.Sagar.Mono       8     0.807050               1       0.718280   4.491528     4.255629                 0             0               0
# 11  Test.Sagar.Mono      16     0.743576               0       0.511656   4.221471     6.748745                 0             0               1
# 15  Test.Sagar.Mono      16     0.692419               0       0.380413   8.266542     8.972391                 0             0               1
# 17  Test.Sagar.Mono      16     0.643167               0       1.461503   4.182225     6.788067                 0             0               1
# 23  Test.Sagar.Mono      16     1.003473               0       0.808719   1.896279    10.000000                 0             1               1
# 26  Test.Sagar.Mono      16     0.692832               0       1.737056   7.002669     8.239263                 0             1               0
# 42  Test.Sagar.Mono      16     0.674351               0       1.761816   4.165721    10.000000                 0             0               1

# This could be troublesome... Bisected test has rarely been completed with less than 3 mistakes

# Looks like we could try to transform it an make it more normalised

# First of all, we need to determine if a participant was inconsistent with the answers 
errors_allowed <- 3
subsIds <- unique(bigTable$subID)



# But first lets locate and get rid of outliers
rt_mean.test.mono = mean(bigTable$RT.Test.Mono)
rt_mean.retest.mono = mean(bigTable$RT.ReTest.Mono)
rt_mean.test.bi = mean(bigTable$RT.Test.Bi)
rt_mean.retest.bi = mean(bigTable$RT.ReTest.Bi)
rt_sd.test.mono = sd(bigTable$RT.Test.Mono)
rt_sd.retest.mono = sd(bigTable$RT.ReTest.Mono)
rt_sd.test.bi = sd(bigTable$RT.Test.Bi)
rt_sd.retest.bi = sd(bigTable$RT.ReTest.Bi)
# abs($x - mean) <= 3*sd as done in previous lab and in the literature (2 or 3 times the sd from the mean)
deviation_allowed <- 2
to_remove.test.mono <- Test.Mono[abs(Test.Mono$RT - rt_mean.test.mono) > deviation_allowed*rt_sd.test.mono,]
to_remove.retest.mono <- ReTest.Mono[abs(ReTest.Mono$RT - rt_mean.retest.mono) > deviation_allowed*rt_sd.retest.mono,]
to_remove.test.bi <- Test.Bi[abs(Test.Bi$RT - rt_mean.test.bi) > deviation_allowed*rt_sd.test.bi,]
to_remove.retest.bi <- ReTest.Bi[abs(ReTest.Bi$RT - rt_mean.retest.bi) > deviation_allowed*rt_sd.retest.bi,]
Removed.Test.Mono <- Test.Mono[! (row.names(Test.Mono) %in% row.names(to_remove.test.mono)),]
Removed.ReTest.Mono <- ReTest.Mono[! (row.names(ReTest.Mono) %in% row.names(to_remove.retest.mono)),]
Removed.Test.Bi <- Test.Bi[! (row.names(Test.Bi) %in% row.names(to_remove.test.bi)),]
Removed.ReTest.Bi <- ReTest.Bi[! (row.names(ReTest.Bi) %in% row.names(to_remove.retest.bi)),]

# Visualize
hist(Removed.Test.Mono$RT)
hist(Removed.ReTest.Mono$RT)
hist(Removed.Test.Bi$RT)
hist(Removed.ReTest.Bi$RT)

# Check normality of data in our vectors
shapiro.test(Removed.Test.Mono$RT)
shapiro.test(Removed.ReTest.Mono$RT)
shapiro.test(Removed.Test.Bi$RT)
shapiro.test(Removed.ReTest.Bi$RT)
# none of them follow a normal distribution

# Check with transformations
shapiro.test(log(Removed.Test.Mono$RT))
shapiro.test(log(Removed.ReTest.Mono$RT))
shapiro.test(log(Removed.Test.Bi$RT))
shapiro.test(log(Removed.ReTest.Bi$RT))
# ... I've tried with different transformations and I can't really get something very normally distributed so

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
