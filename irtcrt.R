# Set options for better readability
options(digits = 3)
options(scipen = 999)
options(max.print = 5000)


library(tidyverse)
odf<-read.csv("bshitq.csv")
cat(names(odf), sep = "\n")


odf <- odf %>% 
  mutate(across(.cols = 52:58, .fns = ~na_if(.x, 9)))



# Remove cases with low response (n = 105 -> 102)
wdf <- odf[rowMeans(is.na(odf)) <= 0.2, ]
sapply(odf[,3:75], function(x) print(unique(x)))

# Step 1: Install and load the ltm package
install.packages("ltm")
library(ltm)

# Step 2: Load your data frame
# Assuming your data frame is named 'odf'

# Step 3: Subset your data frame
# Step 4: Fit the 2PL model
model <- ltm(vars ~ z1)

#model <- ltm(odf[, c(52:58)] ~ z1)
model <- ltm(odf[, c(52:55,57:58)] ~ z1)



# Step 5: Extract item parameters
coef(model)
summary(model)

model$log.Lik

# Step 6: Plot characteristic curves
plot(model, type = "ICC")
plot(model, type = "IIC")
plot(model, type = "IIC",items=0)

?ltm::plot.ltm()
factor.scores(model)
factor.scores(model)$score.dat$z1




model2 <- ltm(odf[, c(52:55,57:58)] ~ z1+z2)

coef(model2)
anova(model,model2)
plot(model2, type = "loadings")



library(ltm)

data <- odf[, c(52:55,57:58)]
data <- data[complete.cases(data),]
data <- data.frame(lapply(data, as.numeric))
class(data)
str(data)
tpm(data, IRT.param = TRUE)
model3 <- tpm(data, IRT.param = TRUE)

plot(model3, type = "ICC")
plot(model3, type = "IIC")
plot(model3, type = "IIC",items=0)

anova(model,model3)
