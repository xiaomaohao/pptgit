
library(tidyverse)
library(haven)
student <- read_dta("/Users/yangyongye/microcloud/projects/survey_data/ceps/2014-2015data/cepsw2studentCN.dta")
data <- student %>% select(ids,schids,w2cogscore,w2eng,w2upeng,w2b15,
                           relation=w2a17,eco=w2a09,height=w2c01,weight=w2c02)

#1
table(data$eco,data$relation)
#2
data2 <- data %>% filter(w2upeng==100) %>%drop_na(schids,w2eng) %>%
  group_by(schids) %>% summarise(eng_mean=mean(w2eng)) %>% arrange(eng_mean)

data2$schids <- reorder(data2$schids, data2$eng_mean)
data2 %>% ggplot(aes(schids,eng_mean)) + geom_bar(stat = "identity")+coord_flip()

#3
data3 <- data %>%  filter(height>100 & height<250) %>% filter(weight>10 & weight<150)
data3 %>% ggplot(aes(height,weight))+geom_point()

#4
data %>% drop_na(w2b15) %>% ggplot(aes(w2cogscore,fill=as.factor(w2b15)))+geom_density()

#5
teacher <- read_dta("/Users/yangyongye/microcloud/projects/survey_data/ceps/2014-2015data/cepsw2teacherCN.dta")
teacher1 <- teacher %>% group_by(w2tchc02) %>% mutate(sum=n())
teacher1 %>% ggplot(aes(w2tchc02,sum))+geom_line()
