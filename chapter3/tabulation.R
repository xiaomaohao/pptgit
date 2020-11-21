## table learning 
## good packages for making table

# kable and kableExtra: Format highly complex and beautiful table
# DT():creating interactive table
# formattable()
# tadaatoolbox():used for t-test, chi-sqr, anova
# stargazer: used to create  tables after modeling
# summarytools** package:table for frequency, summary, and cross table
# xtable():mainly for Latex or html
# huxtable:It is meant to be a replacement for packages like xtable
# others: apsrtable, memisc, texreg and outreg,pastecs()
# strengejake package: sjt.xtab(sjplot),frq & descr(sjmisc)
# table1, generate the first table of normal journal article
# gt,formating table
# flextable, create three line table. used for formating

## table categories:
# table creation: base\sjmics\sjplot\table1\gtsummary\rstatix
# table formatting: gt\formattable\kable\flextable
# responsive table: reactable\DT


#**base --------------------------------------------------------------------
data("mtcars")
head(mtcars)
str(mtcars)
class(mtcars$cyl)
table(mtcars$cyl)
table(mtcars$cyl,mtcars$vs)
cro_ta <- table(mtcars$cyl,mtcars$vs)
class(cro_ta)
summary(mtcars$mpg)


#**rstatix ----------------
library(rstatix)
data("ToothGrowth")

#get_summary_stats()
ToothGrowth %>% get_summary_stats(len)
ToothGrowth %>% group_by(dose,supp) %>% 
  get_summary_stats(len, type = "common")

ToothGrowth %>% get_summary_stats(len, type = "robust")
ToothGrowth %>% get_summary_stats(len, type = "five_number")
ToothGrowth %>% get_summary_stats(len, type = "mean_sd")

ToothGrowth %>% get_summary_stats(len,dose, show = c("mean","sd", "median","iqr"))

# freq_table()
ToothGrowth %>% freq_table(supp, dose)

#**flextable-----
#https://davidgohel.github.io/flextable/articles/overview.html
#不支持中???
library(flextable)
library(officer)
myft <- flextable(head(mtcars),
                  col_keys = c("am","carb","gear","mpg","drat"))
myft

myft <- theme_vanilla(myft)
myft

myft <- merge_v(myft, j=c("am","carb"))
myft <- set_header_labels(myft,carb="# carb.")
myft <- autofit(myft)
myft

myft %>%  italic(j=1) %>% bg(bg="#C90000",part = "header") %>% 
  color(color="white",part = "header") %>% 
  color(~drat>3.5,~drat,color = "red") %>% 
  bold(~drat > 3.5, ~drat, bold = T) %>% 
  autofit()

print(myft, preview = "docx")
print(myft, preview = "pptx")


ft <- flextable(head(mtcars))
ft <- autofit(ft)
ft
ppt <- read_pptx()
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = ft, location = officer::ph_location_left()) 

print(ppt, target = "example.pptx")

doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "example.docx")

ft <- flextable(head(iris))
dims <- flextable_dim(ft)
dims

img_file <- tempfile(fileext = ".png")
save_as_image(ft, path = img_file)
webshot::install_phantomjs()

library(officer)
library(magrittr)
library(flextable)
data <- data.frame(`国家`=c("中国","China","中国","China","中国","China"),
                   B=c("祖国","Country","祖国","Country","祖国","Country"))
ft <- flextable(data)
ft <- font(ft, i = c(2,4,6), fontname = "Arial Black", part = "body")
ft <- font(ft, i = c(1,3,5), fontname = "Arial Black", part = "body")
ft <- bold(ft, bold = TRUE, part = "body")
ft <- autofit(ft)
doc <- read_docx() %>% body_add_flextable(ft)
print(doc,target="trouble.docx")
print(ft, preview = "pptx")
ft

library(tidyverse)
mtt <- mtcars %>% rename("机关"=gear)
myft <- flextable(head(mtcars), 
                  col_keys = c("carb", "gear", "mpg", "drat" )) %>% 
  font(fontname = "Hei",part = "header") %>% 
  set_header_labels(carb="机关") %>% 
  merge_v(j=~gear) %>% 
  fix_border_issues() %>% 
  dim_pretty()
myft

#**gtsummary-------
#http://www.danieldsjoberg.com/gtsummary/
#remotes::install_github("ddsjoberg/gtsummary")
#install.packages("gtsummary")
library(gt)

library(gtsummary)
trial[c("trt", "age", "grade")] %>%
  tbl_summary(by = trt, missing = "no") %>%
  modify_header(stat_by = md("**{level}** N =  {n} ({style_percent(p)}%)")) %>%
  add_n() %>%
  bold_labels() %>%
  as_gt() %>%
  tab_spanner(columns = starts_with("stat_"), md("**Chemotherapy Treatment**"))

#tbl_summary
library(gtsummary)
library(gt)
library(dplyr)

trial2 <- trial %>% select(trt,age,grade)

trial2 %>% tbl_summary()

##
trial2 %>% tbl_summary(by = trt) %>% 
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
  add_overall() %>% 
  add_n() %>% 
  modify_header(label ~ "**Variable**") %>% 
  modify_spanning_header(c("stat_1","stat_2") ~ "**Treatment Received**") %>% 
  modify_footnote(
    starts_with("stat_") ~ "Median (IQR) or Frequency (%)"
  ) %>% 
  bold_labels()

#Modifying tbl_summary() function arguments
trial2 %>% 
  tbl_summary(
    by=trt,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} /{N} ({p}%)"),
    digits = all_continuous() ~ 2, missing = "no",
    label = grade ~ "Tumor Grade",
    missing_text = "(Missing)"
  )

trial %>% select(age, marker) %>% 
  tbl_summary(type = all_continuous() ~ "continuous2",
              statistic = all_continuous() ~ c("{median} ({p25},{p75})","{min},{max}"),
              missing = "no")


#tbl_cross()

trial %>% 
  tbl_cross(row = stage, col = trt, percent = "cell") %>% 
  add_p()


#**DT learning------
DT::datatable(iris)  # https://rstudio.github.io/DT/
library(DT)
iris
datatable(iris)

#arguments
datatable(head(iris), class = "cell-border stripe")
datatable(head(mtcars))
datatable(head(mtcars), rownames = F)

datatable(head(iris), colnames = c("here", "are", "some", "new", "names"))
datatable(head(iris), colnames = c("a better name" = "Sepal.Width"))

datatable(
  head(iris),
  caption = "Table 1. This is a simple caption for the table"
)

datatable(
  head(iris),
  caption = htmltools::tags$caption(
    style = "caption-side: bottom; text-align: center;",
    "Table:",htmltools::em("This is a simple cation for the table.")
  )
)

datatable(head(iris, 20),options = list(
  columnDefs = list(list(className = 'dt-center', targets =5)),
  pageLength = 5,
  lengthMenu = c(5, 10, 15, 20)
))

datatable(head(mtcars, 30), options = list(
  ordre= list(list(2, 'asc'), list(4, "desc"))
))

datatable(head(iris), options = list(dom ='t'))
datatable(head(iris), options = list(dom = 'ft'))


datatable(head(iris,20), options = list(
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color':'#000','color':'#fff'});",
    "}"
  )
))


#sjmisc ------------------------------------------------------------------
# frq & descr

#frq()
library(sjmisc)
data(efc)
frq(efc$e42dep)
frq(efc$e42dep,out="viewer")
frq(efc$e42dep,out = "viewer",sort.frq="asc",show.na = F,
    title = "Table 1.frequency table")

# with grouped data frames, in a pipe


efc %>%
  group_by(e16sex) %>%
  frq(e42dep,out="browser")

#descr()

descr(efc, 
      e17age,
      c160age, 
      out = "browser",show=c("n","mean","sd","range","skewness"))

efc %>% select(e42dep, e15relat, c172code) %>% descr(out = "txt")
efc %>% select(e42dep, e15relat, c172code) %>% descr(out = "browser")

efc %>%
  group_by(e16sex,c175empl) %>%
  select(e16sex, e42dep, e15relat, c172code) %>%
  descr(out="viewer")

efc %>%
  group_by(e16sex, c172code) %>%
  descr(e16sex, c172code, e17age, c160age,out = "viewer")

# flat_table() not be taught
flat_table(efc, e42dep, e16sex)
tab <- flat_table(efc, e42dep, c172code, e16sex)

library(dplyr)
efc %>%
  group_by(e16sex) %>%
  select(e16sex, c172code, e42dep) %>%
  flat_table()
efc %>%
  group_by(e16sex, e42dep) %>%
  select(e16sex, e42dep, c172code, n4pstu) %>%
  flat_table()

# sjPlot-----
#sjt.xtab
library(sjPlot)
sjt.xtab(efc$e16sex,efc$e42dep,show.cell.prc = T)

sjt.xtab(efc$e16sex,efc$e42dep,
         var.labels = c("监护人的性别","老人的???别"),
         title = "这是???个标???")

sjt.xtab(efc$e16sex,efc$e42dep,show.cell.prc = F,emph.total = T)

sjt.xtab(efc$e16sex,efc$e42dep,
         CSS = list(css.table="border: 2px solid;",
                    css.tdata="border: 1px solid;",
                    css.horline = "border-bottom: double blue;"))

sjt.xtab(efc$e42dep,efc$quol_5,statistics = "kendall")
sjt.xtab(efc$e42dep,efc$quol_5,
         statistics = "spearman",
         exact=F,
         continuity=T)

#table1------
#https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

library(boot)
melanoma2 <- melanoma
melanoma2$status <- factor(melanoma2$status,levels = c(2,1,3),labels = c("Alive","Melanoma","Non-melanlma death"))

library(table1)
table1(~factor(sex) + age + factor(ulcer) + thickness | status, data= melanoma2)

melanoma2$sex <- factor(melanoma2$sex, levels = c(1,0),
                        labels = c("male","female"))
melanoma2$ulcer <- factor(melanoma2$ulcer, levels = c(0,1),
                          labels = c("Absent","Present"))
label(melanoma2$sex) <- "Sex"
label(melanoma2$age) <- "Age"
label(melanoma2$ulcer) <- "Ulceration"
label(melanoma2$thickness) <- "Thickness"
units(melanoma2$age) <- "years"
units(melanoma2$thickness) <- "mm"

table1(~ sex + age + ulcer + thickness | status, data = melanoma2,overall = "Total")

#
labels <- list(variables = list(sex="Sex",
                                age="Age(years)",
                                ulcer="Ulceration",
                                thickness="Thickness(mm)"),
               groups=list("","","Death"))

levels(melanoma2$status) <- c("Alive","Melanoma","Non-melanoma")
strata <-  c(list(Total=melanoma2),split(melanoma2,melanoma2$status))

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

table1(strata,labels,groupspan = c(1, 1, 2),
       render.continuous =my.render.cont, render.categorical = my.render.cat)

#example
f <- function(x, n, ...) factor(sample(x, n, replace=T, ...), levels=x)
set.seed(427)

n <- 146
dat <- data.frame(id=1:n)
dat$treat <- f(c("Placebo", "Treated"), n, prob=c(1, 2)) # 2:1 randomization
dat$age   <- sample(18:65, n, replace=TRUE)
dat$sex   <- f(c("Female", "Male"), n, prob=c(.6, .4))  # 60% female
dat$wt    <- round(exp(rnorm(n, log(70), 0.23)), 1)

# Add some missing data
dat$wt[sample.int(n, 5)] <- NA

label(dat$age)   <- "Age"
label(dat$sex)   <- "Sex"
label(dat$wt)    <- "Weight"
label(dat$treat) <- "Treatment Group"

units(dat$age)   <- "years"
units(dat$wt)    <- "kg"

table1(~ age + sex + wt | treat,data=dat)
table1( ~ age + sex + wt | treat, data = dat, overall = F)

table1(~ age + wt | treat*sex, data = dat)
table1(~ age + wt | sex*treat, data= dat, overall = F)
table1(~ treat + age + age + wt, data= dat)

#
dat$dose <- (dat$treat != "Placebo")*sample(1:2, n, replace=T)
dat$dose <- factor(dat$dose, labels=c("Placebo", "5 mg", "10 mg"))

strata <- c(split(dat, dat$dose), list("All treated"=subset(dat, treat=="Treated")), list(Overall=dat))

labels <- list(
  variables=list(age=render.varlabel(dat$age),
                 sex=render.varlabel(dat$sex),
                 wt=render.varlabel(dat$wt)),
  groups=list("", "Treated", ""))

table1(strata, labels, groupspan=c(1, 3, 1))

#
table1(strata, labels, groupspan=c(1, 3, 1),
       render.continuous=c(.="Mean (CV%)", .="Median [Min, Max]",
                           "Geo. mean (Geo. CV%)"="GMEAN (GCV%)"))

#
rndr <- function(x, name, ...) {
  if (!is.numeric(x)) return(render.categorical.default(x))
  what <- switch(name,
                 age = "Median [Min, Max]",
                 wt  = "Mean (SD)")
  parse.abbrev.render.code(c("", what))(x)
}

table1(~ age + sex + wt | treat, data=dat,
       render=rndr)

table1(~ age + sex + wt | treat, data=dat, topclass="Rtable1-zebra")
table1(~ age + sex + wt | treat, data=dat, topclass="Rtable1-grid")
table1(~ age + sex + wt | treat, data=dat, topclass="Rtable1-grid Rtable1-shade Rtable1-times")


#
library(MatchIt) 
data(lalonde)

lalonde$treat    <- factor(lalonde$treat, levels=c(0, 1, 2), labels=c("Control", "Treatment", "P-value"))
lalonde$black    <- factor(lalonde$black)
lalonde$hispan   <- factor(lalonde$hispan)
lalonde$married  <- factor(lalonde$married)
lalonde$nodegree <- factor(lalonde$nodegree)
lalonde$black    <- as.logical(lalonde$black == 1)
lalonde$hispan   <- as.logical(lalonde$hispan == 1)
lalonde$married  <- as.logical(lalonde$married == 1)
lalonde$nodegree <- as.logical(lalonde$nodegree == 1)

label(lalonde$black)    <- "Black"
label(lalonde$hispan)   <- "Hispanic"
label(lalonde$married)  <- "Married"
label(lalonde$nodegree) <- "No high school diploma"
label(lalonde$age)      <- "Age"
label(lalonde$re74)     <- "1974 Income"
label(lalonde$re75)     <- "1975 Income"
label(lalonde$re78)     <- "1978 Income"
units(lalonde$age)      <- "years"

rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- lalonde[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- t.test(y ~ lalonde$treat)$p.value
    } else {
      p <- chisq.test(table(y, droplevels(lalonde$treat)))$p.value
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}

table1(~ age + black + hispan + married + nodegree + re74 + re75 + re78 | treat,
       data=lalonde, droplevels=F, render=rndr, render.strat=rndr.strat, overall=F)


#
dat <- expand.grid(i=1:50, group=LETTERS[1:3])
dat <- cbind(dat, matrix(round(exp(rnorm(6*nrow(dat))), 1), nrow=nrow(dat)))
names(dat)[3:8] <- paste0("V", 1:6)

table1(~ V1 + V2 + V3 + V4 + V5 + V6 | group, data=dat,
       topclass="Rtable1-grid Rtable1-center",
       render="Mean (CV%)<br/>Median [Min, Max]<br/>GMean (GCV%)")

table1(~ V1 + V2 + V3 + V4 + V5 + V6 | group, data=dat,
       topclass="Rtable1-grid Rtable1-center",
       render="Mean (CV%)<br/>Median [Min, Max]<br/>GMean (GCV%)",
       transpose=TRUE)

#gt------
#https://gt.rstudio.com/articles/intro-creating-gt-tables.html
library(dplyr)
islands_tbl <- tibble(
  name = names(islands),
  size=islands
) %>% arrange(desc(size)) %>% slice(1:10) 
islands_tbl

library(gt)
gt_tab <- gt(data= islands_tbl)
gt_tab

gt_tbl <- gt_tab %>% tab_header(title="Large Landmasses of the World",
                                subtitle = "The Top ten largest are presented")
gt_tbl

gt(islands_tbl[1:3,]) %>% 
  tab_header(title = md("**Large Landmasses of the World**"),
             subtitle = md("The *top two* largest are presented"))

gt_tbl <- gt_tbl %>%  tab_source_note(source_note = "Source: The World Almanac and Book of Facts:, 1975, page 406.") %>% 
  tab_source_note(source_note = md("Reference: McNeil, d. r. (1997),*Interactive Data Analysis*.Wiley."))
gt_tbl <- gt_tbl %>% tab_footnote(
  footnote = "The America.",
  locations = cells_data(
    columns = vars(name),
    rows = 3:4
  )
)
gt_tbl

# Determine the row that contains the
# largest landmass ('Asia')
largest <- 
  islands_tbl %>% 
  arrange(desc(size)) %>%
  slice(1) %>%
  pull(name)

# Create two additional footnotes, using the
# `columns` and `where` arguments of `data_cells()`
gt_tbl <- 
  gt_tbl %>%
  tab_footnote(
    footnote = md("The **largest** by area."),
    locations = cells_data(
      columns = vars(size),
      rows = name == largest)
  ) %>%
  tab_footnote(
    footnote = "The lowest by population.",
    locations = cells_data(
      columns = vars(size),
      rows = size == min(size))
  )

# Show the gt Table
gt_tbl

# the stub
gt_tbl <- 
  islands_tbl %>%
  gt(rowname_col = "name")

# Show the gt Table
gt_tbl

gt_tbl <- gt_tbl %>% tab_stubhead_label(label = "landmass")
gt_tbl

##
gt_tbl <- 
  gt_tbl %>%
  tab_header(
    title = "Large Landmasses of the World",
    subtitle = "The top ten largest are presented"
  ) %>%
  tab_source_note(
    source_note = "Source: The World Almanac and Book of Facts, 1975, page 406."
  ) %>%
  tab_source_note(
    source_note = md("Reference: McNeil, D. R. (1977) *Interactive Data Analysis*. Wiley.")
  ) %>%
  tab_footnote(
    footnote = md("The **largest** by area."),
    locations = cells_data(
      columns = vars(size),
      rows = largest)
  ) %>%
  tab_footnote(
    footnote = "The lowest by population.",
    locations = cells_data(
      columns = vars(size),
      rows = contains("arc"))
  )

# Show the gt Table
gt_tbl


# Create three row groups with the
# `tab_row_group()` function
gt_tbl <- 
  gt_tbl %>% 
  tab_row_group(
    group = "continent",
    rows = 1:6
  ) %>%
  tab_row_group(
    group = "country",
    rows = c("Australia", "Greenland")
  ) %>%
  tab_row_group(
    group = "subregion",
    rows = c("New Guinea", "Borneo")
  )

# Show the gt Table
gt_tbl



# Modify the `airquality` dataset by adding the year
# of the measurements (1973) and limiting to 10 rows
airquality_m <- 
  airquality %>%
  mutate(Year = 1973L) %>%
  slice(1:10)

# Create a display table using the `airquality`
# dataset; arrange columns into groups
gt_tbl <- 
  gt(data = airquality_m) %>%
  tab_header(
    title = "New York Air Quality Measurements",
    subtitle = "Daily measurements in New York City (May 1-10, 1973)"
  ) %>%
  tab_spanner(
    label = "Time",
    columns = vars(Year, Month, Day)
  ) %>%
  tab_spanner(
    label = "Measurement",
    columns = vars(Ozone, Solar.R, Wind, Temp)
  )

# Show the gt Table
gt_tbl

# Move the time-based columns to the start of
# the column series; modify the column labels of
# the measurement-based columns
gt_tbl <- 
  gt_tbl %>%
  cols_move_to_start(
    columns = vars(Year, Month, Day)
  ) %>%
  cols_label(
    Ozone = html("Ozone,<br>ppbV"),
    Solar.R = html("Solar R.,<br>cal/m<sup>2</sup>"),
    Wind = html("Wind,<br>mph"),
    Temp = html("Temp,<br>&deg;F")
  )

# Show the gt Table
gt_tbl



# formattable -------------------------------------------------------------
#devtools::install_github("renkun-ken/formattable")
# example 1
library(formattable)
p <- percent(c(0.1, 0.02, 0.03, 0.12))
p

#example 2
p <- data.frame(
  id = c(1, 2, 3, 4, 5), 
  name = c("A1", "A2", "B1", "B2", "C1"),
  balance = accounting(c(52500, 36150, 25000, 18300, 7600), format = "d"),
  growth = percent(c(0.3, 0.3, 0.1, 0.15, 0.15), format = "d"),
  ready = formattable(c(TRUE, TRUE, FALSE, FALSE, TRUE), "yes", "no"))
p

#example 3
df <- data.frame(
  id = 1:10,
  name = c("Bob", "Ashley", "James", "David", "Jenny", 
           "Hans", "Leo", "John", "Emily", "Lee"), 
  age = c(28, 27, 30, 28, 29, 29, 27, 27, 31, 30),
  grade = c("C", "A", "A", "C", "B", "B", "B", "A", "C", "C"),
  test1_score = c(8.9, 9.5, 9.6, 8.9, 9.1, 9.3, 9.3, 9.9, 8.5, 8.6),
  test2_score = c(9.1, 9.1, 9.2, 9.1, 8.9, 8.5, 9.2, 9.3, 9.1, 8.8),
  final_score = c(9, 9.3, 9.4, 9, 9, 8.9, 9.25, 9.6, 8.8, 8.7),
  registered = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE)


formattable(df, list(
  age = color_tile("lightblue", "orange"),
  grade = formatter("span", style = x ~ ifelse(x == "A", 
                                               style(color = "green", font.weight = "bold"), NA)),
  area(col = c(test1_score, test2_score)) ~ normalize_bar("pink", 0.2),
  final_score = formatter("span",
                          style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
                          x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),
  registered = formatter("span",
                         style = x ~ style(color = ifelse(x, "green", "red")),
                         x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
))

formattable(df) # could be used to see table in Viewer


# kable -------------------------------------------------------------------

knitr::kable(head(mtcars[, 1:4]), "pandoc")
knitr::kable(head(mtcars[, 1:4]), "markdown") #only the formats pandoc and markdown are portable
knitr::kable(head(mtcars[, 1:4]), "html")
knitr::kable(head(mtcars[, 1:4]), "latex") #used for pdf document
knitr::kable(head(mtcars[, 1:4]), "rst")

iris2 <- head(iris)
knitr::kable(iris2, col.names = gsub("[.]", " ", names(iris)))
knitr::kable(iris2, align = "lccrr")
knitr::kable(iris2, caption = "An example table caption.")

d <- cbind(X1 = runif(3), X2 = 10^c(3, 5, 7), X3 = rnorm(3, 0, 1000))
# at most 4 decimal places
knitr::kable(d, digits = 4)
# round columns separately
knitr::kable(d, digits = c(5, 0, 2))
# do not use the scientific notation
knitr::kable(d, digits = 3, format.args = list(scientific = FALSE))
# do not use the scientific notation
knitr::kable(d, digits = 3, format.args = list(scientific = FALSE))
# add commas to big numbers
knitr::kable(d, digits = 3, format.args = list(big.mark = ",", 
                                               scientific = FALSE))

d[rbind(c(1, 1), c(2, 3), c(3, 2))] <- NA
knitr::kable(d)  # NA is displayed by default
options(knitr.kable.NA = "**")
knitr::kable(d)

knitr::kable(
  list(
    head(cars, 3),
    head(mtcars[, 1:3], 5)
  ),
  caption = 'Two tables placed side by side.',
  booktabs = TRUE, valign = 't'
)


#kableExtra------
library(knitr)
library(kableExtra)
dt <- mtcars[1:5, 1:6]

dt %>% kable() %>% kable_styling()
kable(dt) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))

kable(dt) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

kable(dt) %>% 
  kable_styling(bootstrap_options = "striped", full_width = F)

kable(dt) %>% 
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

kable(dt) %>% 
  kable_styling(bootstrap_options = "striped", font_size = 9)

# column/row specification
text_tbl <- data.frame(
  Items = c("Item 1", "Item 2", "Item 3"),
  Features = c(
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin vehicula tempor ex. Morbi malesuada sagittis turpis, at venenatis nisl luctus a. ",
    "In eu urna at magna luctus rhoncus quis in nisl. Fusce in velit varius, posuere risus et, cursus augue. Duis eleifend aliquam ante, a aliquet ex tincidunt in. ", 
    "Vivamus venenatis egestas eros ut tempus. Vivamus id est nisi. Aliquam molestie erat et sollicitudin venenatis. In ac lacus at velit scelerisque mattis. "
  )
)

kable(text_tbl) %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1, bold=T, border_right = T) %>% 
  column_spec(2, width = "30em", background = "yellow")

kable(dt) %>% 
  kable_styling("striped", full_width = F) %>% 
  column_spec(5:7, bold=T) %>% 
  row_spec(3:5, bold = T, color = "white", background = "#D7261E")

kable(dt) %>% 
  kable_styling("striped", full_width = F) %>% 
  row_spec(0, angle = -45)

#cell/text specification
library(dplyr)
mtcars[1:10, 1:2] %>% 
  mutate(
    car = row.names(.),
    mpg = cell_spec(mpg, color = ifelse(mpg>20, "red", "blue")),
    cyl = cell_spec(cyl, color = "white", align = "c", angle = 45,
                    background = factor(cyl, c(4, 6, 8),
                                        c("#666666","#999999", "#BBBBBB")))
  ) %>% 
  select(car, mpg, cyl) %>% 
  kable(escape = F) %>% 
  kable_styling("striped", full_width = F)


iris[1:10, ] %>% 
  mutate_if(is.numeric, function(x){
    cell_spec(x, bold = T,
              color = spec_color(x, end = 0.9),
              font_size = spec_font_size(x))
  }) %>% 
  mutate(Species = cell_spec(
    Species, color = "white", bold = T,
    background = spec_color(1:10, end = 0.9, option = "B", direction = -1)
  )) %>% 
  kable(escape = F, align = 'c') %>% 
  kable_styling(c("striped", "condensed"), full_width = F)


library(formattable)
mtcars[1:5, 1:4] %>% 
  mutate(
    car= row.names(.),
    mpg = color_tile("white", "orange")(mpg),
    cyl = cell_spec(cyl, angle = (1:5)*60,
                    background = "red", color = "white", align = "center"),
    disp = ifelse(disp>200,
                  cell_spec(disp, color = "red", bold = T),
                  cell_spec(disp, color = "gredd", italic = T)),
    hp = color_bar("lightgreen")(hp)
  ) %>% 
  select(car, everything()) %>% 
  kable(escape = F) %>% 
  kable_styling("hover", full_width = F) %>% 
  column_spec(5, width = "3cm") %>% 
  add_header_above(c(" ", "hello"= 2, "World"=2))


# grouped columns/rows
kable(dt) %>% 
  kable_styling("striped") %>% 
  add_header_above(c(" "=1, "Group 1"=2, "Group 2" =2, "Group 3"=2))

kable(dt) %>% 
  kable_styling(c("striped", "bordered")) %>% 
  add_header_above(c(" ", "Group 1" =2, "Group 2" =2, "Group 3"=2)) %>% 
  add_header_above(c(" ", "Group 4" = 4, "Group 5" =2))%>% 
  add_header_above(c(" ", "GROUP 6" = 6))

kable(mtcars[1:10, 1:6], caption = "Group Rows") %>% 
  kable_styling("striped", full_width = F) %>% 
  group_rows("group1", 4,7) %>% 
  group_rows("Group 2", 8, 10)

kable(mtcars[1:10, 1:6], caption = "group rows") %>% 
  kable_styling("striped", full_width = F) %>% 
  group_rows(index = c(" " = 3, "Group -1" =4, "group 2"=3))



kable(dt) %>% 
  kable_styling("striped", full_width = F) %>% 
  group_rows("group1", 3, 5, label_row_css = "background-color: #666; color: #fff;")

kable(dt) %>% 
  kable_styling("striped", full_width = F) %>% 
  add_indent(c(1, 3, 5))

collapse_rows_dt <- data.frame(C1 = c(rep("a", 10), rep("b",5)),
                               C2 = c(rep("c",7), rep("d", 3), rep("c", 2),
                                      rep("d", 3)),
                               C3 = 1:15,
                               C4 = sample(c(0,1), 15, replace = TRUE))
kable(collapse_rows_dt, align = "c") %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1, bold = T) %>% 
  collapse_rows(columns = 1:2,valign ="top")


kable(dt, align = "c") %>% 
  kable_styling(full_width = F) %>% 
  footnote(general = "here is a general commnents of the table",
           number = c("footnote 1;", "Footnote 2;"),
           alphabet = c("footnote A", "Footnote B;"),
           symbol = c("footnote symbol 1;", "Footnote Symbol 2")
           )

kable(dt, align = "c") %>%
  kable_styling(full_width = F) %>%
  footnote(general = "Here is a general comments of the table. ",
           number = c("Footnote 1; ", "Footnote 2; "),
           alphabet = c("Footnote A; ", "Footnote B; "),
           symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2"),
           general_title = "General: ", number_title = "Type I: ",
           alphabet_title = "Type II: ", symbol_title = "Type III: ",
           footnote_as_chunk = T, title_format = c("italic", "underline")
  )

dt_footnote <- dt
names(dt_footnote)[2] <- paste0(names(dt_footnote)[2], 
                                footnote_marker_symbol(1))
row.names(dt_footnote)[4] <- paste0(row.names(dt_footnote)[4], 
                                    footnote_marker_alphabet(1))
kable(dt_footnote, align = "c", 
      # Remember this escape = F
      escape = F) %>%
  kable_styling(full_width = F) %>%
  footnote(alphabet = "Footnote A; ",
           symbol = "Footnote Symbol 1; ",
           alphabet_title = "Type II: ", symbol_title = "Type III: ",
           footnote_as_chunk = T)

# html only features
kable(cbind(mtcars, mtcars)) %>% 
  kable_styling() %>% 
  scroll_box(width = "500px", height = "200px")

kable(cbind(mtcars, mtcars)) %>% 
  kable_styling() %>% 
  scroll_box(width = "100%", height = "200px")


#reactable----
# install.packages("devtools")
#devtools::install_github("glin/reactable")

library(reactable)
reactable(iris)
reactable(iris[1:5, ], columns = list(
  Sepal.Length = colDef(name = "Sepal Length"),
  Sepal.Width = colDef(name = "Sepal Width"),
  Species = colDef(align = "center")
))

reactable(
  iris[1:5, ],
  defaultColDef = colDef(
    header = function(value) gsub("."," ",value, fixed = T),
    cell = function(value) format(value, nsmall = 1),
    align = "center",
    minWidth = 70,
    headerStyle = list(background="$f7f7f8")
  ),
  columns = list(
    Species = colDef(minWidth = 140)
  ),
  bordered = T,
  highlight = T
)
#sorting
reactable(
  iris[48:52, ],
  defaultSortOrder = "desc",
  columns = list(
    Species = colDef(defaultSortOrder = "asc")
  )
)

reactable(iris[48:52, ],defaultSorted = c("Species", "Petal.Length"))

#filtering
data <- MASS::Cars93[1:20,c("Manufacturer","Model","Type","AirBags","Price")]
reactable(data,filterable = T,minRows = 10)
reactable(data, filterable = T, columns = list(
  Price = colDef(filterable = F)
))
#searching

data <-  MASS::Cars93[1:20,c("Manufacturer","Model","Type","AirBags","Price")]
reactable(data,searchable = T,minRows = 10)

#Pagination
reactable(iris[1:6, ], defaultPageSize = 4)
reactable(iris[1:6, ], defaultPageSize = 4,minRows = 4,searchable = T)
reactable(iris[1:50, ], paginationType = "jump",defaultPageSize = 4)
reactable(iris[1:50, ], paginationType = "simple",defaultPageSize = 4)

#grouping and aggregation
data <- MASS::Cars93[10:22, c("Manufacturer", "Model", "Type", "Price", "MPG.city")]
reactable(data, groupBy = "Manufacturer")

data <- MASS::Cars93[14:38, c("Type", "Price", "MPG.city", "DriveTrain", "Man.trans.avail")]

reactable(data, groupBy = "Type", columns = list(
  Price = colDef(aggregate = "max"),
  MPG.city = colDef(aggregate = "mean", format = colFormat(digits = 1)),
  DriveTrain = colDef(aggregate = "unique"),
  Man.trans.avail = colDef(aggregate = "frequency")
))





#janitor-----
library(dplyr)
humans <- starwars %>%
  filter(species == "Human")
library(janitor)
t1 <- humans %>% tabyl(eye_color)
t1
t1 %>% adorn_totals("row") %>% 
  adorn_pct_formatting()

t2 <- humans %>% tabyl(gender,eye_color)
t2
t2 %>% adorn_percentages("row") %>% 
  adorn_pct_formatting(digits = 2) %>% 
  adorn_ns()
class(t2)

t3 <- humans %>% tabyl(eye_color,skin_color,gender)
t3
library(purrr)
humans %>%
  tabyl(eye_color, skin_color, gender, show_missing_levels = FALSE) %>%
  adorn_totals("row") %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title

humans %>%
  tabyl(gender, eye_color) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()
