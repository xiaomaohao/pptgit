

p_load(tidyverse, rio,strengejacke,rstatix,DT)
wave12 <- import("/Users/yangyongye/microcloud/projects/survey_data/ceps/2014-2015data/wave12.dta")

wave12_s <- wave12 %>% select(ends_with("ids"),brd1=c18, brd2=w2b15, ends_with("chn"), ends_with("mat"), ends_with("eng"),starts_with("w2c25"),
                              frame, subsample, grade9, stcog, cog3pl,
                              a11, c09, c12, stsex, sthkplace, sthktype, stmigrant, stlb_4c,
                              stlb_2c, st0, stbrd, steco_5c, steco_3c, stonly, stsib, stsibrank, stmedu, stfedu, stprhedu, bc07,
                              schhkplace, schhkplace_4c, schhktype, schhktype_4c, schstlb, schstlb_4c, schtype_4c, schtype_2c,
                              schrank_5c, schbrd, schloc_5c, ctyedu, ctyeduz, ctyeduc, ctytype,ctyplace,b14a1,b14a2,computer=b13,kinderg=c01,expect=b30,prelation=stprrel,
                              afterclass=ba02,health=bd13,house=be23,neighbour_f=be30,fml_loc=be29,tract=hra05,neighbour_s=pla24,st_rate=plb02,
                              st_quality=plb04,w2clsids,w2cogscore,w2clsra,w2status,spsch_cls,w1_w2)


wave12 %>% select(w2a02, w2b15) %>% view_df()
view_df(wave12_s,show.frq = T)


# lollipop and dumbell----
ceps_rank <- wave12 %>% group_by(w2a02) %>%
  mutate(w2b15=rec(w2b15,rec="1 =1; 2=0")) %>%
  summarise(ratio= mean(w2b15,na.rm=T)) %>%
  ungroup() %>%
  drop_na(w2a02)

library(ggalt)

theme_set(theme_linedraw(base_family = "SimHei"))
theme_set(theme_gray())
ceps_rank %>%
  ggplot(aes(y=fct_reorder(to_label(w2a02), ratio), x= ratio)) +
  geom_lollipop(point.colour = "blue", point.size = 4, horizontal = 1)

## data preparation
fund<- import("/Users/yangyongye/microcloud/projects/2017edufinance/funding2017.xlsx")
#fund$location <- factor(fund$location, levels=as.character(fund$location))  # for right ordering of the dumbells

fund %>%
  ggplot(aes(x = per_ele_2016,xend=per_ele_2017, y= reorder(location, per_ele_2016)))+
  geom_dumbbell(colour="#a3c4dc", size=1.5, colour_xend="#0e668b",
                dot_guide=TRUE, dot_guide_size=0.15)
fund %>% ggplot(aes(x = per_ele_2016,xend=per_ele_2017, y= location))+
  geom_dumbbell(colour="#a3c4dc", size=1.5, colour_xend="#0e668b",
                dot_guide=TRUE, dot_guide_size=0.15)


wave12_s %>% count(ctyplace)


ceps_ctyplace <- wave12 %>%
  count(w2a02) %>%
  drop_na(w2a02) %>%
  mutate(ratio= n/sum(n),
         w2a02=to_label(w2a02)) %>%
  arrange(desc(w2a02)) %>%
  mutate(lab.ypos = cumsum(ratio) - 0.5*ratio)


# pie and donut----
library(scales)
ceps_ctyplace %>% ggplot(aes(x = "", y = ratio, fill = w2a02)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = percent(ratio)), color = "red")+
  scale_fill_viridis_d(option = "B") +
  theme_void()

ceps_ctyplace %>% ggplot(aes(x = 2, y = ratio, fill = w2a02)) +
  geom_col(color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = percent(ratio)), color = "red")+
  scale_fill_viridis_d(option = "D") +
  theme_void()+
  xlim(0.5, 2.5)

wave12 %>% ggplot(aes(w2a02))+
  geom_bar()

# treemap----
student_num <- import("/Users/yangyongye/R/ggplot2learning/data/students_2016.xlsx")
type12 <- student_num %>% group_by(学段, 类别1, 类别2) %>%
  summarise(stu_no = sum(在校生数))


library(treemapify)



type12 %>% ggplot(aes(area= stu_no,fill=学段, subgroup=学段)) +
  geom_treemap()+
  geom_treemap_text(aes(label=paste(类别2, stu_no, sep = "\n"),family="KaiTi"),fontface="italic", color= "red", place = "middle", grow = FALSE) +
  scale_fill_brewer("Dark2")



## density

library(readxl)
library(flexdashboard)
library(tidyverse)
library(ggalt)                # for geom_dumbbell
library(formattable)
library(strengejacke)         # rec()
library(RColorBrewer)
library(tmap)# change decimal to percent


library(rgdal)                # readOGR()
library(sf)
library(ggspatial)

## map-----
## data preparation
library(rio)
fund<- import("/Users/yangyongye/microcloud/projects/2017edufinance/funding2017.xlsx")
fund$location <- factor(fund$location, levels=as.character(fund$location))  # for right ordering of the dumbells

china_sp <- st_read(dsn = "/Users/yangyongye/microcloud/projects/全国省级、地市级、县市级行政区划shp/中国的基础数据/中国基础数据/中国行政区_包含南海九段线.shp")
china_no <- st_read(dsn = "/Users/yangyongye/microcloud/projects/全国省级、地市级、县市级行政区划shp/中国的基础数据/中国基础数据/中国行政区.shp")



china_no$NAME <- rec(china_no$NAME, rec="北京市=北京;天津市=天津; else=copy")
fund$location <- str_remove(fund$location, "[市省]")
china_fund <- left_join(china_no, fund, by = c("NAME" = "location"))

ggplot() +
  geom_sf(data = china_fund, aes(fill=per_ele_2016))+
  geom_sf(data=china_sp)+
  scale_fill_viridis_c()+
  annotation_scale()+
  annotation_north_arrow(location="tr",style = north_arrow_fancy_orienteering())+
  theme_void()

#font------
# windows 上 theme中直接调动中文字体即可，但mac不行。解决方法是安装showtext.
library(showtext)
showtext_auto()
library(tidyverse)
p1 <- ggplot(mtcars, aes(wt, mpg,color=factor(cyl))) +
  geom_point() +
  labs(title = "汽车重量与油耗之间的关系图")

# themes: whole and individual elements-----
p1
p1 + theme_bw()
p1 + theme_classic()

library(ggthemes)
library(scales)
library(RColorBrewer)
p1 +
  labs(subtitle = "来自于1973年的数据",
       caption = "Source: 1973 dataset",
       x ="重量",
       y = "油耗",
       color = "发动机"
       ) +
  scale_y_continuous(limits = c(10,35),breaks = breaks_extended(3))+
  scale_x_continuous(breaks = breaks_width(2))+
  scale_color_brewer(palette = "Dark2") +
  theme(axis.title = element_text(colour = "red",size = 8))

# scale----

