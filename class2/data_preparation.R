vignette("dplyr")
vignette("two-table")
vignette("colwise")
vignette("rowwise")

# VERB ----------------------------------------------------------

## import export convert data

#install.packages("rio")
library(rio)
install_formats()

binary <- import("data/binary.sas7bdat")  #sas file
ceps <- import("data/ceps_brd.dta")  # state file
demo_sav <- import("data/demo.sav")  # spss file
demo_text <- import("data/demo.txt")  # txt file
demo_xlsx <- import("data/demo.xlsx") # excel file

export(mtcars, "data/mtcars.csv")
export(mtcars, "data/mtcars.xlsx")

convert("data/mtcars.csv","data/mtcars.dta")

## browse data and clean names

library(tidyverse)
view(binary)
glimpse(binary)
glimpse(ceps)

library(skimr)
skim(ceps)

library(janitor)

skim(binary)

binary_new <- binary %>% clean_names()
skim(binary_new)

funding <- import("data/enrollment_rate.xlsx")
glimpse(funding)

funding_new <- funding %>% clean_names()
glimpse(funding_new)


## Col: select & rename------

rm(mpg) #
student <- import("/Users/yangyongye/microcloud/projects/survey_data/ceps/2014-2015data/cepsw2studentCN.dta")
#student <- read_dta("cepsw2studentCN.dta")

library(sjPlot)
view_df(student,show.frq = TRUE,show.prc = TRUE)



library(tidyverse)

heigh <- select(student,w2a05,w2c01)
heigh2 <- student %>% select(w2a05,w2c01)

student1 <- student %>% select(ids, w2cogscore)
student2 <- student %>% select(ids, ctyids, w2c01,w2c02)


student_s <- student %>%
  select(w2c01, w2c02, w2c09, w2c23, w2d05, w2b18, w2a15, w2a16, w2a17, w2a05,w2chn:w2upeng,
         w2a02, w2coglmh, w2cogtype, w2cogscore, w2cog3pl, ids, clsids, schids,
         ctyids, w2frame, w2subsample)
view_df(student_s)

student_s %>% export("student_sample.dta")

student_s1 <- student %>% select(contains("cog"),ends_with("ids"),
                                 starts_with("w2a20"),num_range("w2a",22:32))

#student_rename1 <- student %>% select(iq=w2cogscore)

student_rename2 <- student_s1 %>% rename(iq=w2cogscore)

iris_new <- iris %>% clean_names()

iris_species <- iris_new %>% select(where(is.numeric))
glimpse(iris_species)

iris_width <- iris_new %>% select(where(is.numeric) & contains("width"))
glimpse(iris_width)

df2 <- tibble(x1 = 1, x2 = "a", x3 = 2, y1 = "b", y2 = 3, y3 = "c", y4 = 4)

df2 %>% select(any_of(vars))

df2 %>% select(all_of(vars))

df2 %>% rename_with(toupper)
df2 %>% rename_with(toupper, starts_with("x"))

## Col: relocate----

student_shanghai <- student_shanghai %>% relocate(w2d05)
glimpse(iris)
iris_new <- iris %>% relocate(where(is.factor),.after = Petal.Length)
glimpse(iris_new)
iris_new <- iris %>% clean_names()
glimpse(iris_new)

## Col: mutate----

stu_mutate <- student %>% select(height=w2c01,weight= w2c02) %>%
  mutate(bmi=weight/(height/100)^2,height_foo=height/30.48) %>%
  filter(height_foo < 7 & height_foo >4)


hist(stu_mutate$heigh)
summary(stu_mutate$bmi)


##Col: recode------

# Recode() in package *dplyr*

library(tidyverse)

x <- sample(c("a", "b", "c"), 10, replace = TRUE)
x
recode(x, a = "Apple")

# rec
install.packages("devtools")
library(devtools)
devtools::install_github("strengejacke/sjmisc")

install.packages("sjmisc")
library(sjmisc)

#library(strengejacke)
#library(tidyverse)
library(sjmisc)
data(efc)
view_df(efc)
table(efc$e42dep, useNA = "always")
table(rec(efc$e42dep, rec = "1=1; 2=2; 3=3; 4=4; NA=5"), useNA = "always")


table(rec(efc$e42dep, rec="1,2=1; 3,4=2"), useNA = "always")

efc %>%
  select(e42dep) %>%
  rec(rec = "1,2=1; 3,4=2",
      val.labels = c("low dependency", "high dependency")) %>%
  str()

efc %>%
  select(e42dep, e17age) %>%
  mutate(dependency_rev= rec(e42dep, rec="rev")) %>%
  head()

table(rec(efc$e42dep, rec = "2=1; else=2"), useNA = "always")
table(rec(efc$e42dep, rec= "rev"), useNA = "always")

table(rec(efc$e42dep, rec="min:3 =1; 4 = 2"), useNA = "always")
table(rec(efc$e42dep, rec= "2=1; else=2"), useNA = "always")
table(rec(efc$e42dep, rec = "rev"), useNA = "always")

head(efc[, 6:9])
head(rec(efc[, 6:9], rec = "1=10; 2=20;3=30;4=40"))
table(efc$e15relat)

table(rec(efc$e15relat, rec = "1,2,4=1; else=copy"))

dummy <-rec(efc, c160age, e17age,
            rec="15:30=1[young];31:55 =2[middle];56:max=3[old]", append = FALSE)
frq(dummy)

dummy<-c("M", "F", "F", "X")
rec(dummy, rec = "M=male;F= female; X= refused")
rec(efc$e42dep, rec = "1=first; 2=2nd;3=third; else=hi")

data(iris)
table(rec(iris, Species, rec= "setosa=huhu; else=copy", append = F))

table(rec(
  iris, Sepal.Length, rec = "lo:5=1; 5.01:6.5=2; 6.501:max=3", append = F
))


rec(efc, contains("cop"), c161sex:c175empl,
    rec = "0, 1= 0; else=1",
    append = F)


# dicho
data(efc)
summary(efc$c12hour)
table(efc$c12hour)
table(dicho(efc$c12hour, dich.by = "mean"))
table(dicho(efc$c12hour, dich.by=40))

head(efc[, 6:10])
library(dplyr)
efc %>%
  select(6:10) %>%
  dicho(dich.by = 2) %>%
  head()

dicho(efc, c12hour, e17age, c160age, append = F)

frq(dicho(
  efc, e42dep,
  var.label = "Dependency(dicho)", val.labels = c("lower", "higher"),
  append = F
))

mtcars %>%
  dicho(disp, append = F) %>%
  table()

mtcars %>%
  group_by(cyl) %>%
  summarise(median=median(disp))

median(mtcars$disp)
p <- function(x) dplyr::n_distinct(x)>10
dicho_if(efc, predicate = p, append = F)

#split_var()

table(efc$neg_c_7)
table(split_var(efc$neg_c_7, n=3))
split_var(efc, neg_c_7, pos_v_4, e17age, n=3, append = F)

table(efc$e42dep)
table(split_var(efc$e42dep, n= 2))
table(split_var(efc$e42dep, n= 2, inclusive = T))

#group_var
age <- abs(round(rnorm(100, 65, 20)))
age.grp <- group_var(age, size = 10)
age.grpvar <- group_labels(age, size = 10)
table(age.grp)
print(age.grpvar)


## Row: filter----
summary(student_s$w2c01)
hist(student_s$w2c01)
library(tidyverse)
student_dropna <- student_s %>% drop_na(w2c01)
summary(student_dropna$w2c01)
student_filter1 <- student_dropna %>% filter(w2c01<210)
hist(student_filter1$w2c01)
student_filter11 <- student_filter1 %>% filter(w2c01>100)

student_filter2 <- student_dropna %>% filter(w2c01<250 & w2c01>100)
hist(student_filter2$w2c01)

student_shanghai <- student_filter2 %>%
  filter(w2frame==2 | w2frame==1)

only <- student %>% filter(w2a05==1)

versicolor <- iris %>% filter(Species=="versicolor")

iris_new2 <- iris %>% arrange(desc(Sepal.Length))


## Row: arrange-----
stu_arrange <- student_shanghai %>% arrange(w2c01)
stu_arrange2 <- student_shanghai %>% arrange(desc(w2c01))



## Row: slice----
view(starwars)
dim(starwars)
starwars %>% slice(5:10)
starwars %>% slice_head(n= 3)
starwars %>% slice_tail(n=5)
starwars %>% slice_sample(n=5)
starwars %>% slice_sample(prop =0.1)
starwars %>% filter(!is.na(height)) %>%
  slice_max(height, n= 3)

srrr <- student %>% select(w2c01) %>% slice_max(w2c01,n=10)

#group_by & summarize----
## summarize??mean score of class, rank of student in each class

stu_rank <- student_shanghai %>% group_by(clsids) %>%
  mutate(meaniq=mean(w2cogscore),rank_class=rank(w2cog3pl))


stu_rank <- student %>%
  select(clsids,w2cogscore,w2cog3pl) %>%
  group_by(clsids) %>%
  mutate(meaniq=mean(w2cogscore),rank_class=rank(w2cog3pl))

stu_rank <- student %>%
  select(clsids,w2cogscore,w2cog3pl) %>%
  group_by(clsids) %>%
  summarise(meaniq=mean(w2cogscore))


stu_rank2 <- stu_rank %>% arrange(clsids) %>% group_by(clsids) %>%
  arrange(w2cogscore) %>% select(clsids,w2cogscore,rank_class, everything())




# preclass example

# rowwise & c_cross-----
df <- tibble(x = 1:2, y = 3:4, z = 5:6)

df %>% rowwise() %>% ungroup()
df %>% mutate(m=mean(c(x,y,z)))
df %>% rowwise() %>% mutate(m=mean(c(x,y,z)))

df <- tibble(name = c("Mara", "Hadley"), x = 1:2, y = 3:4, z = 5:6)
df %>% rowwise() %>%
  summarise((m=mean(c(x, y, z))))
df %>% rowwise() %>%
  summarise((m=mean(c(x, y, z))))

df <- tibble(id = 1:6, w = 10:15, x = 20:25, y = 30:35, z = 40:45)
df
rf <- df %>% rowwise(id)

rf %>% mutate(total=sum(c(w, x, y, z)))
rf %>% summarise(total=sum(c(w, x, y, z)))
rf %>% mutate(total=sum(c_across(w:z)))
rf %>% mutate(total=sum(c_across(where(is.numeric))))

rf %>% mutate(total=sum(c_across(w:z))) %>%
  ungroup() %>%
  mutate(across(w:z, ~ . / total))

#across-----
#row_wise summary function
df %>% mutate(total=rowSums(across(where(is.numeric))))

df <- tibble(
  x=list(1, 2:3,4:6)
)
df %>% mutate(l=length(x))
df %>%
  rowwise() %>%
  mutate(l=length(x))

#colwise
starwars
starwars %>%
  summarise(across(where(is.character),~ length(unique(.x))))
starwars %>% group_by(species) %>%
  filter(n()>1) %>%
  summarise(across(c(sex, gender, homeworld), ~ length(unique(.x))))

starwars %>%
  group_by(homeworld) %>%
  filter(n()>1) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE)))

min_max <- list(
  min = ~min(.x, na.rm = TRUE),
  max = ~max(.x, na.rm = TRUE)
)
starwars %>% summarise(across(where(is.numeric),min_max))
starwars %>% summarise(across(where(is.numeric),min_max,.names="{.fn}.{.col}"))

starwars %>% summarise(
  across(where(is.numeric), ~min(.x, na.rm = TRUE), .names = "min_{.col}"),
  across(where(is.numeric), ~max(.x, na.rm = TRUE), .names = "max_{.col}")
)

df <- tibble(x= 1:3, y = 3:5, z = 5:7)
mult <- list(x = 1, y = 10, z =100)

df %>% mutate(across(all_of(names(mult)), ~.x*mult[[cur_column()]]))

df <- data.frame(x=c(1, 2, 3), y=c(1, 4, 9))
df %>%
  summarise(n=n(), across(where(is.numeric), sd))


starwars %>% filter(across(everything(), ~ !is.na(.x)))
starwars %>% distinct(across(contains("color")))

starwars %>% count(across(contains("col")),sort = TRUE)


#extract----
df <- data.frame(x = c(NA, "a-b", "a-d", "b-c", "d-e"))
df
df %>% extract(x,"A")
df %>% extract(x,c("A","B"),"([[:alnum:]]+)-([[:alnum:]]+)")
df %>% extract(x,c("A","B"),"([a-d]+)-([a-d]+)")

#separate-----

table3
table3 %>%
  separate(rate, into = c("cases", "population"))
table3 %>%
  separate(rate, into = c("cases", "population"), sep = "/")

table3 %>%
  separate(rate, into = c("cases", "population"), convert = TRUE)
table3 %>%
  separate(year, into = c("century", "year"), sep = 2)
df <- data.frame(x = c(NA, "a.b", "a.d", "b.c"))
df %>% separate(x,c("A","B"))
df %>% separate(x,c(NA,"B"))

df <- data.frame(x = c("a", "a b", "a b c", NA))
df %>% separate(x, c("a", "b"))
df %>% separate(x, c("a", "b"), extra = "drop", fill = "right")
df %>% separate(x, c("a", "b"), extra = "merge", fill = "left")
df %>% separate(x, c("a", "b", "c"))

df <- data.frame(x = c("x: 123", "y: error: 7"))
df %>% separate(x, c("key", "value"), ": ", extra = "merge")
df <- data.frame(x = c(NA, "a?b", "a.d", "b:c"))
df %>% separate(x, c("A","B"), sep = "([\\.\\?\\:])")

df <- data.frame(x = c("a:1", "a:2", "c:4", "d", NA))
df %>% separate(x, c("key","value"), ":") %>% str
df %>% separate(x, c("key","value"), ":", convert = TRUE) %>% str

var <- "x"
df %>% separate(!!var, c("key","value"), ":")

#unite-----

table5
table5 %>%
  unite(new, century, year)
table5 %>%
  unite(new, century, year, sep = "")
df <- expand_grid(x = c("a", NA), y = c("b", NA))
df
df %>% unite("z", x:y, remove = FALSE)
df %>% unite("z", x:y, na.rm = TRUE, remove = FALSE)
df %>%
  unite("xy", x:y) %>%
  separate(xy, c("x", "y"))



#TWO-TABLE VERB-----
# join-----

library(tidyverse)
#install.packages("nycflights13")
library(nycflights13)

airlines
flights
airports
planes
weather

planes %>%
  count(tailnum) %>%
  filter(n>1)

weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n>1)

# mutating joins: left_join,right_join, full_join, inner_join
flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

flights2 %>%
  left_join(airlines, by="carrier")

left_join(flights,airlines, by="carrier")


flights2 %>%
  left_join(weather)

flights2 %>%
  left_join(planes, by="tailnum")

flights2 %>%
  left_join(airports, c("dest"="faa"))

#filtering joins: semi_join,anti_join
top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
top_dest

#flights %>%
#  filter(dest %in% top_dest$dest)

flights %>% semi_join(top_dest)
flights %>% anti_join(top_dest)

flights

flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)

#row binding------
df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1
)
df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2
)

intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2)
setdiff(df2, df1)


#LONG&WIDE-----
#tidyr----

library(tidyr)
library(dplyr)
library(readr)



#install.packages("skimr")
#library(skimr)
#skim(relig_income)
data("relig_income")
View(relig_income)
## pivot_longer----
relig_income %>% pivot_longer(-religion,names_to = "income",values_to = "count")


billboard
billboard %>% pivot_longer(cols = starts_with("wk"),
                           names_to = "week",
                           values_to = "rank",
                           values_drop_na = FALSE)

billboard %>% pivot_longer(cols = starts_with("wk"),
                           names_to = "week",
                           names_prefix = "wk",
                           names_ptypes = list(week=integer()),
                           values_to = "rank",
                           values_drop_na = TRUE)

who
who %>% pivot_longer(
  cols = new_sp_m014:newrel_f65,
  names_to = c("diagnosis", "gender", "age"),
  names_pattern = "new_?(.*)_(.)(.*)",
  values_to = "count"
)

who %>% pivot_longer(
  cols = new_sp_m014:newrel_f65,
  names_to = c("diagnosis", "gender", "age"),
  names_pattern = "new_?(.*)_(.)(.*)",
  names_ptypes = list(
    gender=factor(levels = c("f","m")),
    age=factor(
      levels = c("014","1524","2534","3544","4554","5564","65"),
      ordered = T
    )
  ),
  values_to = "count"
)


family <- tribble(
  ~family,  ~dob_child1,  ~dob_child2, ~gender_child1, ~gender_child2,
  1L, "1998-11-26", "2000-01-29",             1L,             2L,
  2L, "1996-06-22",           NA,             2L,             NA,
  3L, "2002-07-11", "2004-04-05",             2L,             2L,
  4L, "2004-10-10", "2009-08-27",             1L,             1L,
  5L, "2000-12-05", "2005-02-28",             2L,             1L,
)
family <- family %>% mutate_at(vars(starts_with("dob")),parse_date)
family

family %>% pivot_longer(-family,
                        names_to = c(".value","child"),
                        names_sep = "_",
                        values_drop_na = TRUE)
anscombe
anscombe %>% pivot_longer(everything(),
                          names_to = c(".value","set"),
                          names_pattern = "(.)(.)") %>% arrange(set)


pnl <- tibble(
  x = 1:4,
  a = c(1, 1,0, 0),
  b = c(0, 1, 1, 1),
  y1 = rnorm(4),
  y2 = rnorm(4),
  z1 = rep(3, 4),
  z2 = rep(-2, 4),
)
pnl %>% pivot_longer(
  -c(x,a,b),
  names_to = c(".value","time"),
  names_pattern = "(.)(.)"
)

df <- tibble(x = 1:3, y = 4:6, y = 5:7, y = 7:9, .name_repair = "minimal")
df
df %>% pivot_longer(-x,names_to = "name",values_to = "value")

## pivot_wider-----
fish_encounters
fish_encounters %>% pivot_wider(names_from = station,values_from = seen)

fish_encounters %>% pivot_wider(names_from = station, values_from = seen, values_fill = list(seen=0))

warpbreaks <- warpbreaks %>% as_tibble() %>% select(wool,tension,breaks)
warpbreaks
warpbreaks %>% count(wool,tension)

warpbreaks %>% pivot_wider(names_from = wool,values_from = breaks)
warpbreaks %>% pivot_wider(names_from = wool,values_from=breaks,values_fn=list(breaks=mean))


production <- expand_grid(
  product = c("A", "B"),
  country = c("AI", "EI"),
  year = 2000:2014
) %>%
  filter((product == "A" & country == "AI") | product == "B") %>%
  mutate(production = rnorm(nrow(.)))
production

production %>% pivot_wider(
  names_from = c(product,country),
  values_from = production
)

us_rent_income
us_rent_income %>% pivot_wider(names_from = variable,values_from = c(estimate,moe))

contacts <- tribble(
  ~field, ~value,
  "name", "Jiena McLellan",
  "company", "Toyota",
  "name", "John Smith",
  "company", "google",
  "email", "john@google.com",
  "name", "Huxley Ratcliffe"
)
contacts <- contacts %>% mutate(person_id=cumsum(field=="name"))
contacts
contacts %>%  pivot_wider(names_from = field,values_from = value)

world_bank_pop
pop2 <- world_bank_pop %>% pivot_longer(
  `2000`:`2017`,names_to = "year",values_to = "value"
)
pop2
pop2 %>% count(indicator)

pop3 <- pop2 %>% separate(indicator,c(NA,"area","variable"))
pop3
pop3 %>% pivot_wider(names_from = variable,values_from = value)


multi <- tribble(
  ~id, ~choice1, ~choice2, ~choice3,
  1, "A", "B", "C",
  2, "C", "B",  NA,
  3, "D",  NA,  NA,
  4, "B", "D",  NA
)
multi
multi2 <- multi %>%
  pivot_longer(-id,values_drop_na = T) %>%
  mutate(checked=TRUE)
multi2
multi2 %>% pivot_wider(id_cols = id,names_from = value,
                       values_from = checked,
                       values_fill = list(checked=FALSE))

# setup -------------------------------------------------------------------

library(strengejacke)
library(tidyverse)
library(haven)
#library(pastecs)
library(DT)
library(car)
ttrd <- read_dta("/Users/yangyongye/R/discipline/data/teacher_qd1097.dta")
ttrd <- as.data.frame(ttrd)

ttrd_s <- ttrd %>% sample_n(300)
write_dta(ttrd_s,"02data_preparation/teacher.dta")

addup <- function(x,y){
  x+y
}
addup(3,4)

# frequency of misconduct for all class ------------------------
library(haven)
ttrd <- read_dta("02data_preparation/teacher.dta")
library(tidyverse)
var_mean <- ttrd %>% select(index, num_range("V", 20:43)) %>%
  gather(V20:V43, key = "vars", value = "valu") %>%
  group_by(vars) %>%
  summarise(valu = mean(valu)) %>%
  mutate(mis = c("言语粗俗，不懂礼貌","乱扔垃圾，不讲卫生","仪容仪表不合规范", "对老师或家长撒谎",
                           "不交作业", "作业抄袭", "考试作弊","迟到、早退、旷课等不守作息制度的行为",
                           "听课不认真，如睡觉、看课外书等", "校级班级集体活动时喧闹，不听指挥",
                           "扰乱课堂秩序，如交头接耳、喧哗吵闹", "课间时追逐打闹",
                           "不认真完成校内班内值日活动", "不服管教，顶撞老师",
                           "在校内外吸烟、酗酒", "赌博、吸毒",
                           "观看、收听色情、淫秽音像制品、读物",
                           "进入营业性歌舞厅、游戏厅、网吧等场所",
                           "辱骂甚至殴打教师", "辱骂他人，与他人打架", "参与团伙或聚众斗殴",
                           "故意损坏公物或他人物品",
                           "偷窃或强行向他人索要财物","携带管制刀具等危险品进校园"))


var_mean$mis <- reorder(var_mean$mis, var_mean$valu)

ggplot(var_mean,aes(x=mis,y = valu)) + geom_bar(stat = "identity",fill="violetred4")


ggplot(var_mean,aes(x=mis,y = valu)) + geom_bar(stat = "identity",fill="violetred4") +
  theme(text = element_text(family = "FangSong_GB2312")) +
  coord_flip() +
  labs(title ="图1.中小学生违规行为频繁程度排序图", x = "", y = "频繁程度",
       caption = "注：基于中小学教师主观评价结果(N=1097)")


# example for *rec()* function

library(strengejacke)
ttrd_rec <- ttrd %>%
  select(num_range("Q",1:4)) %>%
  rec(Q3,rec= "1:6 = 1 [小学]; 7:9=2 [初中]; 10:12=3 [高中];
      13:15 = 4[中职]", append = T) %>%
  select(-Q3) %>%
  rename(Q3 = Q3_r)

frq(ttrd$Q3,out = "viewer")
frq(ttrd_rec$Q3,out = "viewer")


# factor derived from the chapter 15 of r4ds------

library(tidyverse)
gss_cat

gss_cat %>% count(race)
ggplot(gss_cat, aes(race))+
  geom_bar()
ggplot(gss_cat, aes(race))+
  geom_bar() +
  scale_x_discrete(drop = F)

relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = T),
    tvhours = mean(tvhours, na.rm = T),
    n = n()
  )
ggplot(relig_summary, aes(tvhours, relig)) +geom_point()

ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
         geom_point()

relig_summary %>%
  mutate(relig= fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig))+
  geom_point()

rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = T),
    tvhours = mean(tvhours, na.rm = T),
    n= n())
ggplot(rincome_summary, aes(age, fct_reorder(rincome, age)))+geom_point()
ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable")))+
  geom_point()

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop =n/sum(n))

ggplot(by_age, aes(age, prop, colour = marital))+geom_line()
ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop)))+
  geom_line()+ labs(colour = "marital")

gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital))+
  geom_bar()

gss_cat %>% count(partyid)

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat",
                              "Other"                 = "No answer",
                              "Other"                 = "Don't know",
                              "Other"                 = "Other party"
  )) %>%count(partyid)


gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = T) %>%
  print(n=Inf)

#

#***pacakages used to explore data-----




#sjplot:view_df----
#easy to see value labels and variable labels.

library(rio)
ceps_st <- import("sampledata/student_sample.dta")
library(sjPlot)
view_df(ceps_st)
view_df(ceps_st,show.frq = T)


#janitor------
# # Create a data.frame with dirty names

enrollment <- import("sampledata/enrollment_rate.xlsx")
names(enrollment)
library(janitor)
enrollment %>% clean_names() %>% names()

separ_name <- import("sampledata/demo.xlsx")
names(separ_name)
separ_name %>% clean_names() %>% names()

##

test_df <- as.data.frame(matrix(ncol = 6))
names(test_df) <- c("firstName", "ábc@!*", "% successful (2009)",
                    "REPEAT VALUE", "REPEAT VALUE", "")
test_df %>% clean_names()
make.names(names(test_df))

df1 <- data.frame(a = 1:2, b = c("big", "small")) # a factor by default
df2 <- data.frame(a = 10:12, b = c("medium", "small", "big"), c = 0, stringsAsFactors = FALSE)
df3 <- df1 %>%
  dplyr::mutate(b = as.character(b))

compare_df_cols(df1, df2, df3)
compare_df_cols(df1, df2, df3, return = "mismatch")

compare_df_cols(df1, df2, df3, return = "mismatch", bind_method = "rbind")

mtcars %>%
  tabyl(gear, cyl) %>%
  adorn_totals("col") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>%
  adorn_title()

mtcars %>% get_dupes(wt, cyl)

q <- data.frame(v1 = c(1, NA, 3),
                v2 = c(NA, NA, NA),
                v3 = c("a", NA, "b"))
q %>%
  remove_empty(c("rows", "cols"))
a <- data.frame(good = 1:3, boring = "the same")
a %>% remove_constant()


#skimr-----
library(skimr)
skim(iris)
skim(iris) %>% is_skim_df()

skim(iris) %>% tibble::as_tibble()

#visdat-----
#devtools::install_github("ropensci/visdat")
library(visdat)
vis_dat(airquality)
vis_miss(airquality)
vis_miss(airquality, cluster = TRUE)
vis_miss(airquality, sort_miss = TRUE)

chickwts_diff <- chickwts
chickwts_diff[sample(1:nrow(chickwts), 30),sample(1:ncol(chickwts), 2)] <- NA

vis_compare(chickwts_diff, chickwts)

vis_expect(airquality, ~.x >= 25)

vis_cor(airquality)
vis_guess()

messy_vector <- c(TRUE,
                  T,
                  "TRUE",
                  "T",
                  "01/01/01",
                  "01/01/2001",
                  NA,
                  NaN,
                  "NA",
                  "Na",
                  "na",
                  "10",
                  10,
                  "10.1",
                  10.1,
                  "abc",
                  "$%TG")

set.seed(1114)
messy_df <- data.frame(var1 = messy_vector,
                       var2 = sample(messy_vector),
                       var3 = sample(messy_vector))

vis_guess(messy_df)
vis_dat(messy_df)

# naniar----
# package for dealing with missing values, this package is great
#drop_na from tidyr
df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
df %>% drop_na()
df %>% drop_na(x)

#replace_na from tidyr
df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"), z = list(1:5, NULL, 10:20))
df %>% replace_na(list(x = 0, y = "unknown"))

p_load(naniar)
ggplot(airquality, aes(x = Solar.R, y = Ozone)) + geom_miss_point()


gg_miss_var(airquality)   # to see the amount of missing values
gg_miss_var(airquality) + theme_bw()
gg_miss_var(airquality) + labs(y = "Look at all the missing ones")
gg_miss_var(airquality, facet = Month)

library(tidyr)
replace_na(airquality,Solar.R=99)  #Missing values turns into a value (NA –> -99)
# Value becomes a missing value (-99 –> NA)
# replace NA with value
#http://naniar.njtierney.com/articles/replace-with-na.html
df <- tibble::tribble(
  ~name,           ~x,  ~y,              ~z,
  "N/A",           1,   "N/A",           -100,
  "N A",           3,   "NOt available", -99,
  "N / A",         NA,  29,              -98,
  "Not Available", -99, 25,              -101,
  "John Smith",    -98, 28,              -1)

df
df %>% replace_with_na(replace = list(x=-99))
df %>% replace_with_na(replace = list(x=c(-99,-98)))
df %>% replace_with_na(replace = list(x = c(-99,-98),z=c(-99,-98)))

df %>% replace_with_na_all(condition = ~.x ==-99)
na_strings <- c("NA","N A","N / A","N/A","Not Available","NOt available")
df %>% replace_with_na_all(condition = ~.x %in% na_strings)

df %>% replace_with_na_at(.vars = c("x","z"),condition = ~exp(.x)<1)
df %>% replace_with_na_if(.predicate = is.character,condition = ~.x %in% ("N/A"))

df2 <- df %>% replace_with_na_if(.predicate = is.character,
                                 condition = ~.x %in% (na_strings))

gg_miss_upset(df2)
gg_miss_case(df2)

#DataExplorer
# not a good package.

p_load(DataExplorer,nycflights13)
data_list <- list(airlines, airports,flights,planes, weather)
plot_str(data_list,type = "r")

merge_airlines <- merge(flights, airlines, by = "carrier", all.x = TRUE)
merge_planes <- merge(merge_airlines, planes, by = "tailnum", all.x = TRUE, suffixes = c("_flights", "_planes"))
merge_airports_origin <- merge(merge_planes, airports, by.x = "origin", by.y = "faa", all.x = TRUE, suffixes = c("_carrier", "_origin"))
final_data <- merge(merge_airports_origin, airports, by.x = "dest", by.y = "faa", all.x = TRUE, suffixes = c("_origin", "_dest"))

introduce(final_data)

plot_intro(final_data)

plot_missing(final_data)
final_data <- drop_columns(final_data,"speed")
plot_missing(final_data)
plot_bar(final_data)
plot_histogram(final_data)


#dlookr------
library(pacman)
p_load(dlookr,nycflights13,tidyverse,visdat)
dim(flights)
flights

# know the missing and unique values
diagnose(flights)
diagnose(flights, year, month, day)
diagnose(flights, year:day)
flights %>% diagnose() %>%
  select(-unique_count, -unique_rate) %>%
  filter(missing_count>0) %>%
  arrange(desc(missing_count))

diagnose_numeric(flights)   # to see outliers
diagnose_category(flights)

diagnose_outlier(flights)

flights %>% plot_outlier(arr_delay)
flights %>%
  plot_outlier(diagnose_outlier(flights) %>%
                 filter(outliers_ratio>=0.5) %>%
                 select(variables) %>%
                 unlist)
flights %>% diagnose_report()
flights %>% diagnose_report(output_format = "html")

