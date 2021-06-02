##overview
#t.test
#x2 test
#anova
#correlation
#pca
#factor
#clustor
#lm

# t.test ------------------------------------------------------------------
#https://uc-r.github.io/t_test
#t-test

library(tidyverse)
library(rio)
library(tidyverse)
library(broom)
library(kableExtra)
student <- import("/Users/yangyongye/microcloud/projects/survey_data/ceps/2014-2015data/cepsw2studentCN.dta")
library(sjPlot)
view_df(student)


# one sample t-test
t.test(student$w2cog3pl, mu=18, alternative = "less")
# two sample test
t.test(w2cogscore ~ w2a10,data = student)
t.test(w2cogscore ~ w2a10,data = student)

#paired t test
chn_mat <- student %>% select(w2chn,w2mat) %>% drop_na() %>%
  pivot_longer(c(w2chn, w2mat), names_to = "subject",values_to = "score")
str(chn_mat)
ggplot(chn_mat,aes(subject,score))+
  geom_boxplot()
t.test(score ~ subject, data = chn_mat, paired =T)

t.test(w2a09~w2a05,data=student)












#wilcox.test() is used when we do not want to assume the data to follow a normal distribution.
wilcox.test(midwest$percollege, mu=32, alternative = "less")







# anova -------------------------------------------------------------------

#example from http://personality-project.org/r/r.guide/r.anova.html#oneway
#1单因素方差分析

student %>% group_by(w2coglmh) %>% summarise(mean_chn=mean(w2chn,na.rm=T))

weight <- student %>% select(w2c02, w2a12,w2c09) %>% filter(w2c02<150)
chn_anova <- aov(w2c02 ~factor(w2a12), data = weight)
summary(chn_anova)
TukeyHSD(chn_anova)

# two way anova

chn_anova2 <- aov(w2c02 ~factor(w2a12)*factor(w2c09), data = weight)
summary(chn_anova2)



#
#tukey test
#https://rpubs.com/aaronsc32/post-hoc-analysis-tukey
tukey.test <- TukeyHSD(aov_exl)
tukey.test
plot(tukey.test)

#2多因素方差分析
datafilename="http://personality-project.org/r/datasets/R.appendix2.data"
data.ex2=read.table(datafilename,header=T)   #read the data into a table
aov.ex2 <- aov(Alertness~Gender*Dosage, data = data.ex2)
summary(aov.ex2)
print(model.tables(aov.ex2, "means"), digits = 3)
boxplot(Alertness ~ Dosage*Gender, data = data.ex2)

##spcialized package
p_load(tidyverse,ggpubr,rstatix,datarium)
data("PlantGrowth")
set.seed(1234)
PlantGrowth %>% sample_n_by(group,size=1)
levels(PlantGrowth$group)

PlantGrowth <- PlantGrowth %>% reorder_levels(group,order = c("ctrl","trt1","trt2"))

PlantGrowth %>% group_by(group) %>%
  get_summary_stats(weight,type="mean_sd")

ggboxplot(PlantGrowth,x="group",y="weight")

#outlier
PlantGrowth %>% group_by(group) %>%
  identify_outliers(weight)
#normality
model <- lm(weight ~ group,data = PlantGrowth)
ggqqplot(residuals(model))
shapiro_test(residuals(model))

PlantGrowth %>% group_by(group) %>%
  shapiro_test(weight)


ggqqplot(PlantGrowth,"weight",facet.by = "group")
#Homogneity of variance assumption
plot(model,1)
PlantGrowth %>% levene_test(weight ~ group)
# oneway anova
res.aov <- PlantGrowth %>% anova_test(weight ~ group)
res.aov

pwc <- PlantGrowth %>% tukey_hsd(weight ~ group)
pwc

pwc <- pwc %>% add_xy_position(x="group")
ggboxplot(PlantGrowth,x="group",y="weight")+
  stat_pvalue_manual(pwc,hide.ns = T)+
  labs(subtitle = get_test_label(res.aov,detailed = T),
       caption = get_pwc_label(pwc))
#Relaxing the homogeneity of variance assumption
res.aov2 <- PlantGrowth %>% welch_anova_test(weight ~ group)
pwc2 <- PlantGrowth %>% games_howell_test(weight ~ group)

pwc2 <- pwc2 %>% add_xy_position(x="group",step.increase = 1)

ggboxplot(PlantGrowth,x="group",y="weight")+
  stat_pvalue_manual(pwc2,hide.ns = T)+
  labs(subtitle = get_test_label(res.aov2,detailed = T),
       caption = get_pwc_label(pwc2))

## two wag anova
set.seed(123)
data("jobsatisfaction",package = "datarium")
jobsatisfaction %>% sample_n_by(gender,education_level,size = 1)

jobsatisfaction %>% group_by(gender,education_level) %>%
  get_summary_stats(score,type = "mean_sd")

bxp <- ggboxplot(jobsatisfaction,x="gender",y="score",color="education_level",palette = "jco")
bxp

#outlier
jobsatisfaction %>% group_by(gender,education_level) %>%
  identify_outliers(score)
#nomality
model <- lm(score ~ gender*education_level,data = jobsatisfaction)
ggqqplot(residuals(model))
shapiro_test(residuals(model))
# normalty by group
jobsatisfaction %>% group_by(gender,education_level) %>% shapiro_test(score)
ggqqplot(jobsatisfaction,"score",ggtheme = theme_bw())+
           facet_grid(gender ~ education_level)
#homeity of variance assumption
jobsatisfaction %>% levene_test(score ~ gender*education_level)

res.aov <- jobsatisfaction %>% anova_test(score ~ gender * education_level)
res.aov
model <- lm(score ~ gender * education_level,data = jobsatisfaction)
jobsatisfaction %>% group_by(gender) %>%
  anova_test(score ~ education_level, error = model)
p_load(emmeans)
pwc <- jobsatisfaction %>% group_by(gender) %>% emmeans_test(score ~ education_level,p.adjust.method = "bonferroni")
pwc
res.aov

jobsatisfaction %>% pairwise_t_test(score ~ education_level,p.adjust.method = "bonferroni")
model <- lm(score ~ gender*education_level,data = jobsatisfaction)
jobsatisfaction %>% emmeans_test(score ~ education_level,p.adjust.method = "bonferroni",model=model)


pwc <- pwc %>% add_xy_position(x = "gender")
bxp +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
# chi square --------------------------------------------------------------

cog <- table(student$w2coglmh, student$w2c23)
cog
chisq.test(cog)

library(sjPlot)
sjt.xtab(student$w2c23,student$w2d05,show.col.prc = T)

sjt.xtab(student$w2a15,student$w2a16,show.col.prc = T)


##example
# Import the data
file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"
housetasks <- read.delim(file_path, row.names = 1)
# head(housetasks)
#install.packages("gplots")
library(gplots)
dt <- as.table(as.matrix(housetasks))
balloonplot(t(dt), main="housetasks", xlab = "", ylab = "", label= F, show.margins = F)

mosaicplot(dt, shade = T, las = 2, main= "housetasks")

chisq <- chisq.test(housetasks)
chisq
chisq$observed
chisq$statistic
chisq$expected
round(chisq$residuals,3)

#install.packages("corrplot")
library(corrplot)
corrplot(chisq$residuals, is.corr = F)


# correlation -------------------------------------------------------------
#install.packages("ppcor")

st_height <- student %>% filter(w2c01<200) %>% filter(w2c01>150)
st_height %>% ggplot(aes(x=w2c01, y = w2eng)) +
  geom_point(alpha= 0.5)+
  geom_smooth(method = "lm")

student3 <- st_height %>% drop_na(w2c01, w2eng)
cor(student3$w2c01, student3$w2eng)
cor.test(student3$w2c01, student3$w2eng)




library(readxl)
library(ggplot2)
library(gridExtra)
library(corrgram)
library(corrplot)
library(Hmisc)
library(ppcor)
##corelaion

corrgram(mtcars)
corrgram(mtcars, lower.panel = panel.shade, upper.panel = panel.pts)

cor_matrix <- cor(mtcars, use = "complete.obs")
corrplot.mixed(cor_matrix, lower = "circle", upper="number", tl.pos = "lt", diag = "u")
cor_matrix
cor_m <- tidy(cor_matrix)

cor(mtcars$mpg, mtcars$wt)
cor.test(mtcars$mpg, mtcars$wt)

rcorr(as.matrix(mtcars[,1:3]))

#spearman correlation: Great when variables are measured on an ordinal scale.
cor(mtcars$mpg, mtcars$wt, method = "spearman")
cor.test(mtcars$mpg, mtcars$wt,method = "spearman")
rcorr(as.matrix(mtcars[,1:3]), type="spearman")

#Kendall's tau:Less sensitive to outliers and more accurate with smaller sample sizes.
cor(mtcars$mpg, mtcars$wt, method = "kendall")
cor.test(mtcars$mpg, mtcars$wt,method = "kendall")

# pca ---------------------------------------------------------------------

mtcars.pca <- prcomp(mtcars[,c(1:7, 10,11)], center = T, scale. = T)
summary(mtcars.pca)
str(mtcars.pca)

#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(mtcars.pca)
ggbiplot(mtcars.pca, labels=rownames(mtcars))

mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3),
                    "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US",
                    rep("Europe", 3))

ggbiplot(mtcars.pca,ellipse=TRUE,  labels=rownames(mtcars), groups=mtcars.country)

ggbiplot(mtcars.pca,ellipse=TRUE,choices=c(3,4),   labels=rownames(mtcars), groups=mtcars.country)

# factor analysis ---------------------------------------------------------

#install.packages("psych")
library(psych)
data(bfi)
bfi_new <- bfi[complete.cases(bfi),]
bfi_cor <- cor(bfi_new)
factors_data <- fa(r=bfi_cor, nfactors = 6)
factors_data

# cluster analysis --------------------------------------------------------
#install.packages("factoextra")
library(factoextra)
library(tidyverse)
USArrests %>%
  scale() %>%                           # Scale the data
  dist() %>%                            # Compute distance matrix
  hclust(method = "ward.D2") %>%        # Hierarchical clustering
  fviz_dend(cex = 0.5, k = 4, palette = "jco")

#install.packages("remotes")
remotes::install_github("tidymodels/infer")
remotes::install_github("tidymodels/corrr")

library(infer)
gss %>%
  specify(age ~ partyid)

gss %>% specify(college ~ partyid, success = "degree") %>%
  hypothesise(null="independence") %>%
  generate(reps = 1000, type = "permute")

gss %>% specify(response = hours) %>%
  hypothesise(null = "point", mu=40) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")

gss %>% specify(age ~ college) %>%
  hypothesise(null="independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate("diff in means", order = c("degree", "no degree"))


point_estimate <- gss %>% specify(response = hours) %>%
  calculate(stat = "mean")
point_estimate

null_dist <- gss %>%
  specify(response = hours) %>%
  hypothesise(null = "point", mu = 40) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean")
null_dist

null_dist %>% visualise() +
  shade_p_value(obs_stat = point_estimate, direction = "two_sided")

null_dist %>% get_p_value(obs_stat = point_estimate, direction = "two-sided")
null_dist %>% get_confidence_interval(point_estimate = point_estimate, level = 0.95,type = "se")

null_f_distn <- gss %>% specify(age ~ partyid) %>%
  hypothesise(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F")

null_f_distn
null_f_distn_theorectical <- gss %>% specify(age ~ partyid) %>%
  hypothesise(null = "independence") %>%
  calculate(stat = "F")
F_hat <- gss %>% specify(age ~ partyid) %>%
  calculate(stat = "F")
F_hat

visualize(null_f_distn_theorectical, method = "theoretical") +
  shade_p_value(obs_stat = F_hat, direction = "greater")

visualise(null_f_distn, method = "both") +
  shade_p_value(obs_stat = F_hat, direction = "greater")


# T-test

gss %>% t_test(response = hours, mu=40)

gss %>% t_test( formula = hours ~ college, order = c("degree", "no degree"), alternative = "two-sided")


# anova

# calculate the observed statistic
observed_f_statistic <- gss %>%
  specify(age ~ partyid) %>%
  calculate(stat = "F")
gss %>%
  specify(age ~ partyid) %>%
  hypothesize(null = "independence") %>%
  visualize(method = "theoretical") +
  shade_p_value(observed_f_statistic,
                direction = "greater")

# chisa_test

chisq_test(gss, college ~ finrela)

#corr
library(corrr)
d <- correlate(mtcars, quiet = TRUE)
library(tidyverse)
d %>% filter(cyl > .7)
d %>% select(term, mpg, cyl, disp)

d %>% focus(mpg, cyl)

d %>% focus(mpg:drat, mirror = TRUE) %>%
  shave() %>%
  rplot()
