#anova----
library(xtable)
data("tli")
fm1 <- aov(tlimth ~ sex + ethnicty + grade + disadvg, data = tli)
fm2 <- xtable(fm1)

library(rstatix)
fm1 %>% anova_summary(detailed = T) %>% flextable()
fm2 %>% xtable_to_flextable()

#install.packages("flextable")
library(flextable)
ft <- xtable_to_flextable(fm2, hline.after = c(1))
ft
#t-test----
data("sleep")
tt <- t.test(extra ~ group, data = sleep)
tt
library(broom)
tt_br <- tidy(tt)
tt_br
flextable(tt_br)

library(flextable)
flextable(tt_br)
#chi-2-----
library(MASS)
tbl <- table(survey$Smoke, survey$Exer)
tbl
chi_2 <- chisq.test(tbl)
chi_2 <- tidy(chi_2)
chi_2
flextable(chi_2)


#tadaatoolbox
# table does not come out, have not figure out why
#remotes::install_github("tadaadata/tadaatoolbox")
library(tadaatoolbox)
tadaa_t.test(ngo,stunzahl,geschl,print = "markdown")
tadaa_aov(stunzahl ~ jahrgang * geschl, data = ngo, print = "html")
tadaa_aov(deutsch ~ jahrgang, data = ngo, type = 1, print = "markdown")
tadaa_nom(ngo$geschl, ngo$abschalt, print = "markdown")


#broom and other

library(webshot)
webshot("https://www.r-project.org/", "r.png")





