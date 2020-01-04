#Reading Dataset

bankruptcy <- read.csv("C:\\Users\\its_t\\Downloads\\Bankruptcy.csv")
head(bankruptcy)

# Exploratory Data Analysis
summary(bankruptcy)

# Check the distributions of all independent variables

# The plot putput will come in 2 pages. First will have R1 to R12
# and then the next will have R13 to R24
par(mfrow = c(3,4))

i <- 4
for (i in 4:ncol(bankruptcy)) 
{
  hist((bankruptcy[,i]), main = paste("Distibution of ", colnames(bankruptcy[i])), xlab = colnames(bankruptcy[i]))
}

# Distribution of the bankruptcy indicator in the dataset
par(mfrow = c(1,1))
barplot(table(bankruptcy$D), main = "Frequency of Bankruptcy Flag")

# Check the correlation between independent variables and the bankruptcy flag
library(GGally)
ggcorr(bankruptcy[,-1], label = TRUE)

# Splitting dataset into Training and Test data
# Sampling for training and testing datasets

set.seed(12420053)

# We randomly select 80% of the observations as a part of the training data set and
# 20% of the observations as the test data set. 
train.ind <- sample(seq_len(nrow(bankruptcy)), size = floor(0.80*nrow(bankruptcy)))
bankruptcy.train <- bankruptcy[train.ind,]
bankruptcy.test <- bankruptcy[-train.ind,]

# Model creation using Logistic Regression
library(MASS)
model.full <- glm(D ~ R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8 + R9 + R10 +
                    R11 + R12 + R13 + R14 + R15 + R16 + R17 + R18 + R19 + R20 +
                    R21 + R22 + R23 + R24,
                  family = "binomial" ,data = bankruptcy.train)
model.null <- glm(D ~ 1, family = "binomial" ,data = bankruptcy.train)
step.BIC <- step(model.null, scope = list(lower = model.null, upper = model.full), direction = "both", k = log(nrow(bankruptcy.train)))

summary(step.BIC)

## Result:
## The ouptut gave us 4 predictors namely - R9, R10, R17, R18

# ----
# In-Sample performance of the model
# Plot the ROC curve to get the area under the curve

final.model <- glm(D ~ R9 + R10 + R17 + R18, family = "binomial" ,data = bankruptcy.train)
library(fields)
library(verification)
prob.insample <- predict(final.model, type = "response")
roc.plot(bankruptcy.train$D == "1", prob.insample)$roc.vol

## Result:
## The above figure shows the ROC Curve for the training Dataset
## and the Area under the Curve (AUC) obtained is 90.95%.


# ----
# Optimal Cutoff
searchgrid = seq(0.01, 0.99, 0.01)
result = cbind(searchgrid, NA)

cost1 <- function(r, pi) {
  weight1 = 15
  weight0 = 1
  c1 = (r == 1) & (pi < cutoff)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > cutoff)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}

for (i in 1:length(searchgrid)) {
  cutoff <- result[i, 1]
  # assign the cost to the 2nd col
  result[i, 2] <- cost1(bankruptcy.train$D, predict(final.model, type = "response"))
}
plot(result, ylab = "Cost in Training Set", main = "Asymmetric Cost Function")

opt.cutoff <- searchgrid[which(result[,2]==min(result[,2]))]
opt.cutoff

bankruptcy.train$pred <-  ifelse(prob.insample>opt.cutoff,1,0)
table(pred = bankruptcy.train$pred , true = bankruptcy.train$D)

mean(bankruptcy.train$pred != bankruptcy.train$D)

# Out-of-Sample Performance of the Model
# Plot the ROC Curve for test dataset

prob.outsample <- predict(final.model, bankruptcy.test, type = "response")
roc.plot(bankruptcy.test$D == "1", prob.outsample)$roc.vol

## Result:
## The above figure shows the ROC Curve for the training Dataset
## and the Area under the Curve (AUC) obtained is 91.47%.



#----
# Calculate the asymmetric classification rate for the test data set
bankruptcy.test$pred <-  ifelse(prob.outsample>opt.cutoff,1,0)
table(pred = bankruptcy.test$pred , true = bankruptcy.test$D)

mean(bankruptcy.test$pred != bankruptcy.test$D)

## Conlcusion:
## The misclassification rate obtained for the test dataset is 0.5925.
## This means our model shows nearly 41% accuracy in predicting
## the bankruptcy status for observations in the test dataset.


