
library(dplyr)
library(ggplot2)
library(tidyverse)
library("scales")

dat <- read.csv(file.choose())

box_fun <- function(data ,box_x,box_y,fill = "blue", size =10 , angle = 90){ 
  ggsave(
    ggplot(data ,aes(x=box_x, y = box_y , show.legend = F )) + 
      geom_boxplot( fill = fill , alpha=0.4,outlier.shape = NA ) + 
      geom_jitter(width = 0.2, size = 0.4)+
      stat_boxplot(geom = "errorbar", width=0.5, size=0.3)+
      theme_update() + 
      theme(axis.text.x = element_text(size = (size/2)+3, angle = angle),
            axis.text.y = element_text(size = (size/2)+3), 
            , aspect.ratio = 2/3,axis.title=element_text(size= (size/2)+7), 
            panel.background= element_rect(fill = 'white')
            ,panel.grid=element_line(color= 'Gainsboro'),
            axis.line = element_line(color= 'black' , size = 0.5)
      ) +
      scale_x_discrete(breaks =box_x )+
      scale_y_continuous(labels = label_scientific(digits = 3))
    ,filename = "plot.png" , width=10, height=10 ,dpi=300) 
}








