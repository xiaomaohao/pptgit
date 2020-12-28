

library(tidyverse)
library(haven)
student <- read_dta("/Users/yangyongye/microcloud/projects/survey_data/ceps/2014-2015data/cepsw2studentCN.dta")
#student <- read_dta("cepsw2studentCN.dta")

#Q1
data1 <- student %>% select(ids,w2clsids,schids,w2cogscore,w2mat,w2upmat,w2eng,
                              w2upeng,w2a17,w2a28,w2d09,w2b1100:w2b1111) %>% 
  filter(w2upmat==150 & w2upeng==150) %>% 
  arrange(desc(w2cogscore)) %>% 
  drop_na() %>% 
  mutate(total=w2eng + w2mat) %>% 
  rename(nfriend=w2d09,pexpection=w2a28)

#Q2
data2 <- data1 %>% group_by(schids) %>% summarise(mat_mean=mean(w2mat))
write_dta(newdata2,"news.dta")

#Q3
teacher <- read_dta("/Users/yangyongye/microcloud/projects/survey_data/ceps/2014-2015data/cepsw2teacherCN.dta")
teacher1 <- teacher %>% filter(w2tchsubject==1)
data_st <- data1 %>% left_join(teacher1, by="w2clsids") %>% rename(schids_x=schids.x,schids_y=schids.y)

write_dta(data_st, "data_st.dta")
#Q4
data4 <- data1 %>% gather(num_range("w2b",1101:1111), key = "interest", value = "valu")%>% 
  group_by(ids) %>% summarise(sum=sum(valu))

data4 <- data1 %>% pivot_longer(num_range("w2b",1101:1111), names_to = "interest", values_to = "valu")%>% 
  group_by(ids) %>% summarise(sum=sum(valu))
