
#生成回归的表???
#- tab_model from sjplot
#- summ from jtools  #https://jtools.jacob-long.com/
# - interactions package https://interactions.jacob-long.com/
# -stargazor from stargazor
# - tbl_regression from gtsummary
# -modelsummary from modelsummary

#Summary of Regression Models as HTML Table-------
# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)

# sample data
data("efc")
efc <- as_factor(efc, c161sex, c172code)

m1 <- lm(barthtot ~ c160age + c12hour + c161sex + c172code, data = efc)
m2 <- lm(neg_c_7 ~ c160age + c12hour + c161sex + e17age, data = efc)
tab_model(m1)
#
data(mtcars)
m.mtcars <- lm(mpg ~ cyl + hp + wt, data = mtcars)
tab_model(m.mtcars)
#
set.seed(2)
dat <- data.frame(
  y = runif(100, 0, 100),
  drug = as.factor(sample(c("nonsense", "useful", "placebo"), 100, TRUE)),
  group = as.factor(sample(c("control", "treatment"), 100, TRUE))
)

pretty_names <- lm(y ~ drug * group, data = dat)
tab_model(pretty_names)

tab_model(m1,auto.label = F)
tab_model(pretty_names,auto.label = F)

#
tab_model(m1, m2)
#glm
m3 <- glm(
  tot_sc_e ~ c160age + c12hour + c161sex + c172code,
  data = efc,
  family = poisson(link = "log")
)

efc$neg_c_7d <- ifelse(efc$neg_c_7 < median(efc$neg_c_7, na.rm = TRUE), 0, 1)
m4 <- glm(
  neg_c_7d ~ c161sex + barthtot + c172code,
  data = efc,
  family = binomial(link = "logit")
)

tab_model(m3, m4)
tab_model(m3, m4, transform = NULL, auto.label = FALSE)

#
#install.packages("pscl")
library(pscl)

data("bioChemists")
m5 <- zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd + ment, data = bioChemists)

tab_model(m5)

tab_model(m1, m3, m5, auto.label = FALSE, show.ci = FALSE)
tab_model(m1, show.se = TRUE, show.std = TRUE, show.stat = TRUE)
tab_model(m3, m4, show.ci = FALSE, show.p = FALSE, auto.label = FALSE)
tab_model(
  m1, show.se = TRUE, show.std = TRUE, show.stat = TRUE,
  col.order = c("p", "stat", "est", "std.se", "se", "std.est")
)
tab_model(m1, collapse.ci = TRUE)
tab_model(m1, collapse.se = TRUE,show.ci = F)

tab_model(
  m1, m2,
  pred.labels = c("Intercept", "Age (Carer)", "Hours per Week", "Gender (Carer)",
                  "Education: middle (Carer)", "Education: high (Carer)",
                  "Age (Older Person)"),
  dv.labels = c("First Model", "M2"),
  string.pred = "Coeffcient",
  string.ci = "Conf. Int (95%)",
  string.p = "P-Value"
)

#including reference level of categorical predictor
#install.packages('TMB', type = 'source')
library(glmmTMB)
data("Salamanders")
model <- glm(
  count ~ spp + Wtemp + mined + cover,
  family = poisson(),
  data = Salamanders
)

tab_model(model)
tab_model(model,show.reflvl = T)
tab_model(model,show.reflvl = T, prefix.labels = "varname")

#show asterisks insted of numeric p_values
tab_model(m1,m2,p.style = "a")

#automatic matching for named verctors
summary(m1)
pl <- c(
  `(Intercept)` = "Intercept",
  e17age = "Age (Older Person)",
  c160age = "Age (Carer)",
  c12hour = "Hours per Week",
  barthtot = "Barthel-Index",
  c161sex2 = "Gender (Carer)",
  c172code2 = "Education: middle (Carer)",
  c172code3 = "Education: high (Carer)",
  a_non_used_label = "We don't care"
)

tab_model(
  m1, m2, m3, m4,
  pred.labels = pl,
  dv.labels = c("Model1", "Model2", "Model3", "Model4"),
  show.ci = FALSE,
  show.p = FALSE,
  transform = NULL
)

#keep or remove coefficients from table
tab_model(m1, terms = c("c160age", "c12hour"))
tab_model(m1, rm.terms = c("c172code2", "c161sex2"))


#Summary of Mixed Models as HTML Table------
# load required packages
library(sjPlot)
library(lme4)
data("sleepstudy")
data("efc")
efc$cluster <- as.factor(efc$e15relat)

m1 <- lmer(neg_c_7 ~ c160age + c161sex + e42dep + (1 | cluster), data = efc)
m2 <- lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)

tab_model(m1, m2)
tab_model(m1,p.val = "kr",show.df=T)

#generalized linear mixed models
data("efc")
efc$neg_c_7d <- ifelse(efc$neg_c_7 < median(efc$neg_c_7, na.rm = TRUE), 0, 1)
efc$cluster <- as.factor(efc$e15relat)
m3 <- glmer(
  neg_c_7d ~ c160age + c161sex + e42dep + (1 | cluster),
  data = efc,
  family = binomial(link = "logit")
)

tab_model(m1, m3)

#more complex models
library(glmmTMB)
data("Salamanders")
m4 <- glmmTMB(
  count ~ spp + mined + (1 | site),
  ziformula = ~ spp + mined,
  family = truncated_poisson(link = "log"),
  data = Salamanders
)

tab_model(m1, m3, m4, show.ci = FALSE)
#> Warning: mu of 2.5 is too close to zero,
#estimate of random effect variances may be unreliable.

#Summary of Bayesian Models as HTML Table

# load required packages
library(sjPlot)
library(insight)
library(httr)
#install.packages("brms")
library(brms)

# load sample models

# zinb <- read.csv("http://stats.idre.ucla.edu/stat/data/fish.csv")
# set.seed(123)
# m1 <- brm(bf(
#     count ~ persons + child + camper + (1 | persons),
#     zi ~ child + camper + (1 | persons)
#   ),
#   data = zinb,
#   family = zero_inflated_poisson()
# )
m1 <- insight::download_model("brms_zi_2")

# data(epilepsy)
# set.seed(123)
# epilepsy$visit <- as.numeric(epilepsy$visit)
# epilepsy$Base2 <- sample(epilepsy$Base, nrow(epilepsy), replace = TRUE)
# f1 <- bf(Base ~ zAge + count + (1 |ID| patient))
# f2 <- bf(Base2 ~ zAge + Trt + (1 |ID| patient))
# m2 <- brm(f1 + f2 + set_rescor(FALSE), data = epilepsy)
m2 <- insight::download_model("brms_mv_3")
tab_model(m1)
tab_model(m2)
tab_model(m2,shshow.ci50 = T)
tab_model(m1,m2)

#Plotting Estimates (Fixed Effects) of Regression Models-----
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

data(efc)
theme_set(theme_sjplot())

# create binary response
y <- ifelse(efc$neg_c_7 < median(na.omit(efc$neg_c_7)), 0, 1)

# create data frame for fitting model
df <- data.frame(
  y = to_factor(y),
  sex = to_factor(efc$c161sex),
  dep = to_factor(efc$e42dep),
  barthel = efc$barthtot,
  education = to_factor(efc$c172code)
)

# set variable label for response
set_label(df$y) <- "High Negative Impact"

# fit model
m1 <- glm(y ~., data = df, family = binomial(link = "logit"))
plot_model(m1)
plot_model(m1,vline.color = "red")
plot_model(m1,sort.est = T)
plot_model(m1, order.terms = c(6, 7, 1, 2, 3, 4, 5))
plot_model(m1,transform = NULL)
plot_model(m1,transform = "plogis")
plot_model(m1,show.values = T,value.offset = 0.3)

#
data(iris)
m2 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Species, data = iris)
# variable names as labels, but made "human readable"
# separating dots are removed
plot_model(m2)
# to use variable names even for labelled data
plot_model(m1, axis.labels = "", title = "my own title")
# keep only coefficients sex2, dep2 and dep3
plot_model(m1, terms = c("sex2", "dep2", "dep3"))
# remove coefficients sex2, dep2 and dep3
plot_model(m1, rm.terms = c("sex2", "dep2", "dep3"))
plot_model(m2, type = "std")

#bayesian models
if (require("rstanarm", quietly = TRUE)) {
  # make sure we apply a nice theme
  library(ggplot2)
  theme_set(theme_sjplot())

  data(mtcars)
  m <- stan_glm(mpg ~ wt + am + cyl + gear, data = mtcars, chains = 1)

  # default model
  plot_model(m)
  # same model, with mean point estimate, dot-style for point estimate
  # and different inner/outer probabilities of the HDI
  plot_model(
    m,
    bpe = "mean",
    bpe.style = "dot",
    prob.inner = .4,
    prob.outer = .8
  )
}

plot_model(
  m1,
  colors = "Accent",
  show.values = TRUE,
  value.offset = .4,
  value.size = 4,
  dot.size = 3,
  line.size = 1.5,
  vline.color = "blue",
  width = 1.5
)

#simple regression diagnosis
#http://www.sthda.com/english/articles/39-regression-model-diagnostics/
#161-linear-regression-assumptions-and-diagnostics-in-r-essentials/
library(tidyverse)
library(broom)
theme_set(theme_classic())

# Load the data
install.packages("datarium")
data("marketing", package = "datarium")
# Inspect the data
sample_n(marketing, 3)
model <- lm(sales ~ youtube, data = marketing)
model2 <- lm(sales ~ youtube + facebook, data = marketing)
model
model.diag.metrics <- augment(model)
head(model.diag.metrics)

ggplot(model.diag.metrics, aes(youtube, sales)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = youtube, yend = .fitted), color = "red", size = 0.3)


par(mfrow = c(2, 2))
plot(model)
plot(model2)

library(ggfortify)
autoplot(model)

# Add observations indices and
# drop some columns (.se.fit, .sigma) for simplification
model.diag.metrics <- model.diag.metrics %>%
  mutate(index = 1:nrow(model.diag.metrics)) %>%
  select(index, everything(), -.se.fit, -.sigma)
# Inspect the data
head(model.diag.metrics, 4)

plot(model, 1)
plot(model, 3)
model2 <- lm(log(sales) ~ youtube, data = marketing)
plot(model2, 3)

plot(model, 2)
#A value of this statistic above 2(p + 1)/n indicates an observation with high
#leverage (P. Bruce and Bruce 2017); where, p is the number of predictors and n
#is the number of observations.
plot(model, 5)

#A rule of thumb is that an observation has high influence if Cook’s distance
#exceeds 4/(n - p - 1)(P. Bruce and Bruce 2017), where n is the number of
#observations and p the number of predictor variables.# Cook's distance
plot(model, 4)
plot(model, 4, id.n = 5)
model.diag.metrics %>%
  top_n(3, wt = .cooksd)
# Residuals vs Leverage
plot(model, 5)
#
df2 <- data.frame(
  x = c(marketing$youtube, 500, 600),
  y = c(marketing$sales, 80, 100)
)
model2 <- lm(y ~ x, df2)
# Cook's distance
plot(model2, 4)
# Residuals vs Leverage
plot(model2, 5)


#mulitple regression diagnosis------
#https://stats.idre.ucla.edu/r/seminars/introduction-to-regression-in-r/

#some other example which show regression assumpitons are violated
#https://www.andrew.cmu.edu/user/achoulde/94842/homework/regression_diagnostics.html
#https://ademos.people.uic.edu/Chapter12.html
library(car)
#install.packages("alr3")
#install.packages("faraway")
library(alr3)
library(faraway)

#part1------
#read the data from csv file
d <- read.csv(
  "https://stats.idre.ucla.edu/wp-content/uploads/2019/02/elemapi2v2.csv")

#We can also read the data from the local computer
#we can set the working directory by setwd()
#d <- read.csv("C:/.../regR/elemapi2v2.csv")
class(d)  #class of object d, returns data.frame
names(d)  #retuns names of variables (columns)
dim(d)    #returns number of rows and columns of data frame
str(d)      #returns structure of variables in the data frame d
summary(d)  #Summary statistics of variables in d
help(lm)    #shows R Documentation for function lm()

#multiple regression model of DV api00 and DVs enroll, meals, and full
m1 <- lm(api00 ~ enroll, data = d)
print(m1)   #Prints coefficients of the model
summary(m1) #Prints summary of the model
r <- cor(d$api00, d$enroll) #correlation coefficient of api00 and enroll
r ^ 2       #this is equal to r-squared in simple regression
ls(m1)      #list of components in object class lm
m1$coefficients # returns coefficients of the model
m1$fitted.values[1:10] #a vector of fitted values
residuals <- m1$resid  #a vector of residuals
coefficients(m1)     # returns coefficients of the model
confint(m1)          # returns a matrix of Confidence Interval for coefficients

plot(api00 ~ enroll, data = d) #scatter plot of api00 vs. enroll
abline(m1, col = "blue")    #Add regression line to the scatter plot
#adding labels(school number) to the scatter plot
plot(api00 ~ enroll, data = d)
text(d$enroll, d$api00+20, labels = d$snum, cex = .7)
abline(m1, col = "blue")


anova(m1) #Anova table
#multiple regression model of DV api00 and DVs enroll, meals, and full
m2 <- lm(api00 ~  enroll + meals + full, data = d)
summary(m2) #summary of model m2
anova(m2)   #anova table of model m2

sum(anova(m2)$Sum) #sum of RSS and SSreg
(400 - 1) * var(d$api00) #Total sum of squre

#Standardized regression model
m2.sd <- lm(scale(api00) ~  scale(enroll) + scale(meals) + scale(full), data = d)
summary(m2.sd)#coefficients are standardized


# install packages for part 2, Regression Diagnostics------

#install.packages("car")
#install.packages("alr3")
#install.packages("faraway")

#loading packages into working environment
library(car)
library(faraway)
library(alr3)
#scatter plot matrix from package car
scatterplotMatrix(~ api00 + enroll + meals + full, data =d)
#studentized residuals
er.std <- rstandard(m2)
#plot of studentized residuals
plot(er.std, ylab="Standardized Residual", ylim=c(-3.5,3.5))
#adding horizental lines for cut of value of outliers
abline(h =c(-3,0,3), lty = 2)
#determine which row is outlier(outside of cut of values of -3 and 3)
index <- which(er.std > 3 | er.std < -3)


#label School name to points that are out of bounds
text(index-20, er.std[index] , labels = d$snum[index])
#print row number of values that are out of bounds
index
#print school names that are out of bounds
d$snum[index]

#Bonferroni p-values for testing outliner
outlierTest(m2)

#a vector containing the diagonal of the 'hat' matrix
h <- influence(m2)$hat

#half normal plot of leverage from package faraway
halfnorm(h, ylab = "leverage")


#the cut of value for cook's distance
cutoff <- 4/((nrow(d)-length(m2$coefficients)-2))
#plot cook's distance
plot(m2, which = 4, cook.levels = cutoff)

#cook's distance, studentized residuals, and leverage in the same plot
influencePlot(m2, main="Influence Plot",
              sub="Circle size is proportial to Cook's Distance" )

#4 diagnostic plots to intentify influential points
infIndexPlot(m2)

#residual vs. fitted value plot for Homoscedasticity
plot(m2$resid ~ m2$fitted.values)
#add horizental line from 0
abline(h = 0, lty = 2)

#residual vs. fitted value and all predictors plus test for curvature
residualPlots(m2)

#residual plot vs. school id
plot(m2$resid ~ d$snum)
abline(h = 0, lty = 2)

#plot(m2$resid[-1] ~ m2$resid[-400])
#Normal Quantile to Quantile plot
qqnorm(m2$resid)
qqline(m2$resid)


car::vif(m2) #variance inflation factor

#model specification added variable plot
avPlots(m2)

#codes for part 3-------
class(d$api00)   #class of api00
class(d$yr_rnd)  #class of yr_rnd
class(d$mealcat) #class of mealcat

#summary of api00, yr_rnd, and mealcat
summary(d$api00)
summary(d$yr_rnd)
summary(d$mealcat)

table(d$yr_rnd)    #frequency table for yr_rnd
table(d$mealcat)   #frequency table for mealcat

#creat two new variables as factor and add to the data frame
d$yr_rnd_F <- factor(d$yr_rnd)
d$mealcat_F <- factor(d$mealcat)

class(d$yr_rnd_F)
class(d$mealcat_F)

#levels of factor
levels(d$yr_rnd_F)
levels(d$mealcat_F)

#summary of factor
summary(d$yr_rnd_F)
summary(d$mealcat_F)

#regression of api00 with yr_rnd_F
m3 <- lm(api00 ~ yr_rnd_F, data = d)
summary(m3)

#scatter plot api00 against yr_rnd
plot(api00 ~ yr_rnd, data = d)
abline(m3)  # add regression line to the plot

# mean of api00 when yr_rnd_F is at level "0". school is not year round
mean(d$api00[d$yr_rnd_F == "0"])
# mean of api00 when yr_rnd_F is at level "1". school is year round
mean(d$api00[d$yr_rnd_F == "1"])


#using aggregate to find the mean for each group of year school round
aggregate(api00 ~ yr_rnd_F, FUN=mean, data = d)

#t test for equality of mean of api00 for two group of year round and not year round
#with equal variance assumption
t.test(api00 ~ yr_rnd_F, data = d, var.equal = TRUE)


#anova table
anova(m3)


#square of t value from the t-test is the same as F value from anova
10.7815 ^ 2

#regression model of api00 against categorical variable mealcat_F with 3 levels
m4 <- lm(api00 ~ mealcat_F, data = d)
summary(m4)

#aggregate the mean of api00 for each category in mealcat_F
aggregate(api00 ~ mealcat_F, FUN=mean, data = d)


#relevel factor mealcat_F and make group "3" as the reference group
d$mealcat_F <- relevel(d$mealcat_F, ref = "3")
m5 <- lm(api00 ~ mealcat_F, data = d)
summary(m5)


#
remotes::install_github("datalorax/equatiomatic")
install.packages("texPreview")
library(texPreview)
knitr::kable(head(iris,5),format = 'latex')%>%
  tex_preview()

#lindia-----
# create linear model

# visualize diagnostic plots with a call to lindia
install.packages("lindia")
library(lindia)
data()
cars_lm <- lm(mpg ~ wt + hp + gear, data = mtcars)

library(ggplot2)
gg_diagnose(cars_lm,theme = theme_bw())
par(mfrow = c(2,2))
plot(cars_lm)

gg_resfitted(cars_lm) + theme_bw()
gg_reshist(cars_lm)
gg_resX(cars_lm)

#lemon-----

#devtools::install_github("stefanedwards/lemon")
library(lemon)
library(lemon)
ggplot(mtcars, aes(x=cyl, y=mpg)) +
  geom_point() +
  coord_capped_cart(bottom='both', left='none') +
  theme_light() + theme(panel.border=element_blank(), axis.line = element_line())

(p <- ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) +
    geom_point(position=position_jitter(width=0.1)) +
    coord_flex_cart(bottom=brackets_horisontal(), left=capped_vertical('both')) +
    theme_light() + theme(panel.border=element_blank(), axis.line = element_line())
)

#
x <- c(3,4,5,6,7)
y <- c(2,5,3,5,8)
df <- data.frame(x,y)
ggplot(df,aes(x=x,y=y)) +
  geom_pointline(distance = 2,linecolor = "red",lineend = "butt")

# modelsummary-------
library(modelsummary)
library(kableExtra)
library(gt)

# modelplot:

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv'
dat <- read.csv(url)

# rescale mm -> cm
dat$bill_length_cm <- dat$bill_length_mm / 10
dat$flipper_length_cm <- dat$flipper_length_mm / 10

mod <- lm(bill_length_cm ~ flipper_length_cm + species, data = dat)

modelplot(mod)
modelplot(mod, coef_omit = "Interc")
cm <- c('speciesChinstrap' = 'Chinstrap',
        'speciesGentoo' = 'Gentoo',
        'flipper_length_cm' = 'Flipper length (cm)')
modelplot(mod, coef_map = cm)


models <- list()
models[['Small model']] <- lm(bill_length_cm ~ flipper_length_cm, data = dat)
models[['Medium model']] <- lm(bill_length_cm ~ flipper_length_cm + body_mass_g, data = dat)
models[['Large model']] <- lm(bill_length_cm ~ flipper_length_cm + body_mass_g + species, data = dat)

modelsummary(models, statistic = 'conf.int')
modelplot(models, coef_omit = "Interc")
modelplot(models, facet = T)

install.packages("wesanderson")
library(wesanderson)

#
url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
dat <- read.csv(url)

models <- list(
  "OLS 1"     = lm(Donations ~ Literacy + Clergy, data = dat),
  "Poisson 1" = glm(Donations ~ Literacy + Commerce, family = poisson, data = dat),
  "OLS 2"     = lm(Crime_pers ~ Literacy + Clergy, data = dat),
  "Poisson 2" = glm(Crime_pers ~ Literacy + Commerce, family = poisson, data = dat),
  "OLS 3"     = lm(Crime_prop ~ Literacy + Clergy, data = dat)
)

modelsummary(models, statistic = "conf.int",conf_level = .95, gof_omit = ".*")
modelsummary(models, statistic = c("conf.int", "s.e. ={std.error}"),conf_level = .95, gof_omit = ".*")
modelsummary(models, gof_omit = ".*",
             statistic = c("conf.int",
                           "s.e. = {std.error}",
                           "t = {statistic}",
                           "p = {p.value}"))
modelsummary(models, gof_omit = ".*",
             estimate = "{estimate} [{conf.low}, {conf.high}]",
             statistic = NULL)

library(sandwich)
modelsummary(models, statistic_override = vcovHC, statistic = 'p.value')
modelsummary(models,
             statistic_override = list(vcov, vcovHC, vcovHAC, vcovHC, vcov))
vcov_matrices <- lapply(models, vcovHC)
modelsummary(models, statistic_override = vcov_matrices)

custom_stats <- list(`OLS 1` = c(`(Intercept)` = 2, Literacy = 3, Clergy = 4),
                     `Poisson 1` = c(`(Intercept)` = 3, Literacy = -5, Commerce = 3),
                     `OLS 2` = c(`(Intercept)` = 7, Literacy = -6, Clergy = 9),
                     `Poisson 2` = c(`(Intercept)` = 4, Literacy = -7, Commerce = -9),
                     `OLS 3` = c(`(Intercept)` = 1, Literacy = -5, Clergy = -2))
modelsummary(models, statistic_override = custom_stats)
modelsummary(models, align="lrrrrr")
modelsummary(models, align=c("l", "r", "r", "r", "r", "r"), title = "This is a title for my table",notes = list("1. first line","2.seconde line"))

# datasummary

datasummary_skim(dat)
library(modelsummary)
library(tidyverse)

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv'
penguins <- read.csv(url)
datasummary_skim(penguins)
datasummary_skim(penguins, type = "categorical")
datasummary(All(penguins) ~ Mean + SD + Histogram, data = penguins, output = "flextable")

datasummary_correlation(mtcars)

# Download and read data
training <- 'https://vincentarelbundock.github.io/Rdatasets/csv/Ecdat/Treatment.csv'
training <- read.csv(training)

# Rename and recode variables
training <- training %>%
  mutate(`Earnings Before` = re75 / 1000,
         `Earnings After` = re78 / 1000,
         Treatment = ifelse(treat == TRUE, 'Treatment', 'Control'),
         Married = ifelse(married == TRUE, 'Yes', 'No')) %>%
  select(`Earnings Before`,
         `Earnings After`,
         Treatment,
         Ethnicity = ethn,
         Age = age,
         Education = educ,
         Married)

caption <- 'Descriptive statistics about participants in a job training experiment. The earnings are displayed in 1000s of USD. This table was created using the "datasummary" function from the "modelsummary" package for R.'
reference <- 'Source: Lalonde (1986) American Economic Review.'

library(modelsummary)
datasummary_balance(~Treatment,
                    data = training,
                    title = caption,
                    notes = reference)

datasummary(flipper_length_mm ~ Mean, data = penguins)
datasummary(flipper_length_mm + body_mass_g ~ Mean + SD,
            data = penguins)
datasummary(All(penguins) ~ Mean + SD,
            data = penguins)
datasummary(flipper_length_mm + body_mass_g ~ mean * sex,
            data = penguins)

datasummary(body_mass_g ~ sex * (mean + sd),
            data = penguins)

datasummary(sex * (body_mass_g + flipper_length_mm) ~ mean + sd,
            data = penguins)

datasummary(flipper_length_mm + body_mass_g ~ mean * island * sex,
            data = penguins)

datasummary(flipper_length_mm + body_mass_g ~ mean * sex * island,
            data = penguins)

datasummary((`Flipper length (mm)` = flipper_length_mm) + (`Body mass (g)` = body_mass_g) ~
              island * ((Avg. = Mean) + (Std.Dev. = SD)),
            data = penguins)

datasummary(Heading("Flipper length (mm)") * flipper_length_mm + Heading("Body mass (g)") * body_mass_g ~ island * (Mean + SD),
            data = penguins)

datasummary(sex * (flipper_length_mm + bill_length_mm) + Heading("Body mass (g)") * body_mass_g ~ Mean + SD,
            data = penguins)

#count and percentage
datasummary(species * sex + 1 ~ N + Percent(),
            data = penguins)


# gglm
devtools::install_github("graysonwhite/gglm")
library(gglm)
data(mtcars)
model <- lm(mpg ~ ., data = mtcars)
plot(model)
library(tidyverse)
gglm(model)
ggplot(model) + stat_fitted_resid()
ggplot(model) + stat_cooks_leverage()
ggplot(model) + stat_cooks_obs()
ggplot(model) + stat_resid_hist()
ggplot(model) + stat_normal_qq()
ggplot(model) + stat_resid_leverage()
ggplot(model) + stat_scale_location()
