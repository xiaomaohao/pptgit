library(tidyverse)
library(strengejacke)
library(modelsummary)
library(rio)
ds <- import("/Users/yangyongye/Documents/Learning_R/books/qrm_related_data/oklahoma_crosssection.csv")
ds
# chapter 7
ols1 <- lm(ds$glbcc_risk ~ ds$ideol)
summary(ols1)

modelsummary(ols1)

data.frame(ols1$residuals) %>%
  ggplot(aes(ols1$residuals)) +
  geom_histogram(bins = 16)

data.frame(ols1$residuals) %>%
  ggplot(aes(ols1$residuals)) +
  geom_density(adjust = 1.5)

# chapter 9

ds.omit <- filter(ds) %>%
  dplyr::select(glbcc_risk,ideol) %>%
  na.omit()
ols1 <- lm(glbcc_risk ~ ideol, data = ds.omit)
summary(ols1)

Se <- sqrt(sum(ols1$residuals^2)/(length(ds.omit$glbcc_risk)-2))
Se

#9.2.3

ggplot(ds.omit, aes(ideol, glbcc_risk)) +
  geom_smooth(method = lm)

# chapter 11 -----
A <- matrix(c(3,2,1,4),2,2)
A
A.inverse <- solve(A)
A.inverse

A %*% A.inverse

y <- matrix(c(6,11,4,3,5,9,10),7,1)
X <- matrix(c(1,1,1,1,1,1,1,4,7,2,1,3,7,8,5,2,6,9,4,3,2,4,3,4,6,5,4,5),7,4)
X
X.prime <- t(X)
X.prime
X.prime.X <- X.prime %*% X
X.prime.X

X.prime.X.inv<-solve(X.prime.X)
X.prime.X.inv

X.prime.X.inv.X.prime<-X.prime.X.inv %*% X.prime
X.prime.X.inv.X.prime

b<-X.prime.X.inv.X.prime %*% y
b
lm(y~0+X)

y.hat <- X %*% b
y.hat

e <- y-y.hat
e

library(rstatix)
ds %>% get_summary_stats(glbcc_risk,ideol,age)

# chapter 12------
ds.temp <- filter(ds) %>% dplyr::select(glbcc_risk, ideol, age) %>%
  na.omit()

ols1 <- lm(glbcc_risk ~ ideol+age, data = ds.temp)
summary(ols1)
ols2 <- lm(glbcc_risk ~ age, data = ds.temp)
summary(ols2)

ols3 <- lm(ideol ~ age, data = ds.temp)
summary(ols3)

ds.temp$ols2.resids <- ols2$residuals
ds.temp$ols3.resids <- ols3$residuals
ols4 <- lm(ols2.resids ~ 0 + ols3.resids,data = ds.temp)
summary(ols4)
tab_model(ols1,ols2,ols3,ols4)
#multiple regression
library(psych)
describe(data.frame(ds.temp$glbcc_risk,ds.temp$ideol,
                    ds.temp$age))

ols1 <- lm(glbcc_risk ~ age+ideol, data=ds.temp)
summary(ols1)

options(scipen=999)

ds.temp <- filter(ds) %>%
  dplyr::select(glbcc_risk, age, education, income, ideol) %>%
  mutate(income=as.numeric(income)) %>%
  na.omit()
ols3 <- lm(glbcc_risk ~ age + education + income + ideol, data = ds.temp)
summary(ols3)

class(ds.temp$income)




ds200 <- ds.temp[sample(1:nrow(ds.temp), 200, replace=FALSE),]
library(scatterplot3d)
s3d <-scatterplot3d(ds200$age,
                    ds200$ideol,
                    ds200$glbcc_risk
                    ,pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")
s3d$plane3d(ols1)


#chapter 13-------

names(ds)
library(sjPlot)

library(car)
scatterplotMatrix(data.frame(ds.temp$glbcc_risk,
                             ds.temp$ideol,ds.temp$age),
                  diagonal="density")


ds.temp <- filter(ds) %>%
  dplyr::select(glbcc_risk, age, education, income, ideol) %>%
  na.omit() %>%
  mutate(income=as.numeric(income))


ols1 <- lm(glbcc_risk ~ age + education + income, data = ds.temp)

summary(ols1)
tab_model(ols1)

ols2 <- lm(glbcc_risk ~ age + education + income + ideol, data = ds.temp)
summary(ols2)
tab_model(ols1,ols2)

anova(ols1,ols2)

#chapter 14------

#**dummy variable-----
ds.temp <- filter(ds) %>%
  dplyr::select(glbcc_risk, age, education, income, ideol,gend,wtr_use) %>%
  na.omit() %>%
  mutate(income=as.numeric(income))
str(ds.temp)

ols1 <- lm(glbcc_risk ~ age + education + income + ideol + gend, data = ds.temp)
tab_model(ols1)

#**factor IV-----
ols3 <- lm(glbcc_risk ~ age + education + income + ideol + gend + factor(wtr_use), data = ds.temp)
tab_model(ols3)



#**interaction---
#https://interactions.jacob-long.com/articles/interactions.html
#could use above example
ols2 <- lm(glbcc_risk ~ age + income + education + gend * ideol, data = ds.temp)
summary(ols2)
tab_model(ols2)


ds.temp$gend.factor <- factor(ds.temp$gend, levels=c(0,1),labels=c("Female","Male"))
library(effects)
ols3 <- lm(glbcc_risk~ age + income + education + ideol * gend.factor, data = ds.temp)
plot(effect("ideol*gend.factor",ols3),ylim=0:10)

stan.ds <- ds.temp %>%
  dplyr::select(glbcc_risk, age, education, income, ideol, gend) %>%
  scale %>%
  data.frame()

ols3 <- lm(glbcc_risk ~ age + education + income + ideol + gend, data = stan.ds)
summary(ols3)
tab_model(ols3)
x <- 5

#chapter 15-----
ds.small <- filter(ds) %>%
  dplyr::select("glbcc_risk", "age", "education", "income", "ideol") %>%
  slice(1:500) %>%
  mutate(income=as.numeric(income)) %>%
  na.omit() %>%
  filter(income<999000)

#hist(ds.small$income)

ols1 <-  lm(glbcc_risk ~ age + education + income + ideol, data = ds.small)
summary(ols1)

library(modelsummary)
modelsummary(ols1,stars = T)
plot(ols1)

tab_model(ols1)
library(gtsummary)
tbl_regression(ols1)
#lineariy
ds.small$fit.r <- ols1$residuals
ds.small$fit.p <- ols1$fitted.values


ds.small %>%
  pivot_longer(c("age", "education", "income", "ideol", "fit.p"),names_to = "variable", values_to = "value") %>%
  ggplot(aes(value, fit.r, group = variable)) +
  geom_point(shape = 1) +
  geom_smooth(method = loess) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ variable, scales = "free")


ds.small$age2 <- ds.small$age^2
ds.small$edu2 <- ds.small$education^2
ds.small$inc2 <- ds.small$income^2
ds.small$ideology2<-ds.small$ideol^2
ols2 <- lm(glbcc_risk ~ age+age2+education+edu2+income+inc2+ideol+ideology2, data=ds.small)
summary(ols2)

anova(ols1, ols2)
library(lmtest)
resettest(ols1,power=2:3,type="regressor")

#heteroscedasticity
ggplot(ds.small, aes(fit.p, fit.r)) +
  geom_jitter(shape = 1) +
  geom_hline(yintercept = 0, color = "red") +
  ylab("Residuals") +
  xlab("Fitted")

library(car)
ncvTest(ols1)


library(car)
hccm(ols1) %>% diag() %>% sqrt()

library(car)
robust.se <- function(model) {
  s <- summary(model)
  wse <- sqrt(diag(hccm(ols1)))
  t <- model$coefficients/wse
  p <- 2*pnorm(-abs(t))
  results <- cbind(model$coefficients, wse, t, p)
  dimnames(results) <- dimnames(s$coefficients)
  results
}


summary(ols1)
robust.se(ols1)

#independence of E
library(lmtest)
dwtest(ols1)

#normaliy

p1 <- ggplot(ds.small, aes(fit.r)) +
  geom_histogram(bins = 10, color = "black", fill = "white")

p2 <- ggplot(ds.small, aes(fit.r)) +
  geom_density() +
  stat_function(fun = dnorm, args = list(mean = mean(ds.small$fit.r),
                                         sd = sd(ds.small$fit.r)),
                color = "dodgerblue", size = 2, alpha = .5)
p3 <- ggplot(ds.small, aes("", fit.r)) +
  geom_boxplot()
p4 <- ggplot(ds.small, aes(sample = fit.r)) +
  stat_qq(shape = 1) +
  stat_qq_line(size = 1.5, alpha = .5)

shapiro.test(ols1$residuals)

#outliers
ggplot(ds.small, aes(row.names(ds.small), fit.r)) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, color = "red")

#  Sort the residuals
output.1 <- sort(ols1$residuals)  # smallest first
output.2 <- sort(ols1$residuals, decreasing = TRUE) # largest first

#  The head function return the top results, the argument 1 returns 1 variable only
head(output.1, 1) # smallest residual absolute value
head(output.2, 1) # largest residual absolute value
ds.small[c(298,94),c("age","education","income","ideol","glbcc_risk")] # [c(row numbers),c(column numbers)]

df <- 2/sqrt(500)
df

df.ols1 <- dfbetas(ols1)
df.ols1[1:5,]

melt(df.ols1, varnames = c("index", "variable")) %>%
  ggplot(aes(index, value)) +
  geom_point() +
  geom_hline(yintercept = df) +
  geom_hline(yintercept = -df) +
  facet_wrap(~ variable, scales = "free")

names(df.ols1) <- row.names(ds.small)
df.ols1[abs(df.ols1) == max(abs(df.ols1))]


#Multicollinearity
ds %>%
  dplyr::select(age, education, income, ideol) %>%
  na.omit() %>%
  data.frame() %>%
  cor()

library(car)
vif(ols1)
1/vif(ols1)



#chapter 16------

ds.temp <- ds %>%
  dplyr::select(glbcc, age, education, income, ideol, gend) %>%
  na.omit() %>%
  mutate(income=as.numeric(income))

logit1 <- glm(glbcc ~ age + gend + education + income, data = ds.temp, family = binomial())
summary(logit1)

logit0 <- glm(glbcc ~ 1, data = ds.temp)
summary(logit0)
tab_model(logit1)
logLik(logit0)
logLik(logit1)
G <- 2*(-1523 - (-1631))
G
pchisq(G, df = 3, lower.tail = FALSE)

logit2 <- glm(glbcc ~ age + gend + education + income + ideol,
              family = binomial(), data = ds.temp)
summary(logit2)
tab_model(logit2)

anova(logit1, logit2, test = "Chisq")
pseudoR2 <- 1 - (logit2$deviance/logit2$null.deviance)
pseudoR2
aic.logit2 <- logit2$deviance + 2*6
aic.logit2
logit2$aic


logit2 %>% coef() %>% exp()


library(broom)
log.data <- data.frame(age = mean(ds.temp$age),
                       gender = mean(ds.temp$gender),
                       education = mean(ds.temp$education),
                       income = mean(ds.temp$income),
                       ideol = 1:7)
log.data <- logit2 %>%
  augment(newdata = log.data, type.predict = "response")
log.data
log.df <- log.data %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)

ggplot(log.df, aes(ideol, .fitted)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper, width = .2))


##other,手动计算干扰项的标准差，变量系数的标准差

ols <- lm(mpg ~ disp + drat + wt, mtcars)
X <- model.matrix(ols)
rdf <- nrow(X) - ncol(X)
s.sq <- as.vector((t(ols$residuals) %*% ols$residuals) / rdf)

sigma <- sqrt(s.sq)
sigma
summary(ols)

xtx <- t(X) %*% X

sigma2 <- solve(xtx) * s.sq

all.equal(sigma2, vcov(ols))
vcov(ols)
sqrt(diag(sigma2))
