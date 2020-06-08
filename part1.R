library(tidyverse)
library(readxl)

pop <- read_excel("dbdata.xlsx")

# Increase of the Negro population in the United States of America 
bl_pop <- pop %>% filter(Total == "Black or African American alone") 
ggplot(data = bl_pop, aes(x = reorder(Year, -Estimate), y = Estimate,width=.25)) + 
  geom_bar(stat="identity",color = "darkred", fill="darkred") + theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_rect(fill = "cornsilk",colour = "cornsilk"),
        plot.background = element_rect(fill = "cornsilk"),axis.title.x = element_blank(),
        axis.title.y = element_blank(),axis.ticks = element_blank(), 
        axis.text.x=element_blank(),axis.text.y= element_text(color = "black", face = "bold")) + 
  coord_flip() +
  scale_x_discrete(labels=c("2018 - 41,617,764", "2017 - 41,393,491", "2016 - 40,893,369",  
                            "2015 - 40,695,277",  "2014 - 40,379,066", "2013 - 39,919,371", 
                            "2012 - 39,623,138",  "2011 - 39,189,528","2010 - 38,874,625"))  + 
  ggtitle("Increase of the African American population in the United States of America")


# Comparative Rate of Increase of the White and Negro population of the US
w_pop <- pop %>% filter(Total == "White alone") %>% rename("White"="Estimate") 
w_pop$Total <- NULL
b_pop <- bl_pop %>% rename("Black"="Estimate")
b_pop$Total <- NULL
wb_pop <- merge(w_pop,b_pop,by="Year")

#Divide Population by 10^6 (million)
wb_pop$Black <- as.numeric(as.character(wb_pop$Black)) / 1000000
wb_pop$White <- as.numeric(as.character(wb_pop$White)) / 1000000

ggplot(wb_pop,aes(x= Year)) + 
  geom_line(aes(y = Black, color = "darkred"), size =3) + 
  geom_line(aes(y = White, color = "darkgreen"),size =3) +
  scale_color_manual(values = c("darkgreen","darkred"),name  ="Race",labels=c("White","Black")) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.background = element_rect(fill = "cornsilk",color = "cornsilk"),
        plot.background = element_rect(fill = "cornsilk"),legend.background = element_rect(fill="cornsilk"),
        legend.position=c(.5,.5), axis.title.x = element_blank(),axis.title.y = element_blank(),
        plot.title = element_text(size = 10,face = "bold"),axis.text.y= element_text(color = "black", face = "bold"),
        axis.text.x= element_text(color = "black", face = "bold")) + 
  scale_y_continuous(limits = c(25,250), breaks=seq(25,250,25)) +
  scale_x_discrete(limits = c(2010,2011,2012,2013,2014,2015,2016,2017,2018)) + 
  ggtitle("Comparative Growth Rate of the United States White and African American populations (in millions)")


# Proportion of Negroes in the Total Population of the United States
library(maps)
t_pop <- pop %>% filter(Total %in% c("Population for one race:","Population of one race:")) %>% rename("Population_One_Race"="Estimate") 
t_pop$Total <- NULL
tb_pop <- merge(t_pop,b_pop,by="Year")
tb_pop <- add_row(tb_pop, Year = 1890, Population_One_Race = 7470040*8, Black = 7470040)
tb_pop <- transform(tb_pop, prop = (Black / Population_One_Race)*100)

us <-map_data("usa")

#US population
p1 <-  ggplot() + geom_polygon(data = us, aes(x=long, y = lat, group = group), fill = NA, color = "red") + coord_fixed(1.3) + 
  theme_void() + theme(panel.background = element_rect(fill = "cornsilk",colour = "cornsilk"))

#Black population
p2 <- ggplot() + geom_polygon(data=us, aes(x=long, y = lat, group = group))  + coord_fixed(1.3) + 
  theme_void() + theme(panel.background = element_rect(fill = "cornsilk",colour = "cornsilk"))

annotation <- data.frame(x = -90,y = 35,label = "13.2% African American")
note <- data.frame(x = -110,y = 50,label = "Proportion of African Americans in the Total Population of the United States")
p3 <- p1 + annotation_custom(ggplotGrob(p2), xmin = -130, xmax = -70, ymin = 33, ymax = 42) + 
  geom_text(data=annotation, aes( x=x, y=y, label=label),color="darkgreen",fontface="bold") +
  geom_text(data=note, aes( x=x, y=y, label=label),color="black",fontface="bold") 
p3





