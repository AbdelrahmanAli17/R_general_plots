
library(dplyr)
library(ggplot2)
library(tidyverse)
library("scales")

dat <- read.csv(file.choose())

#--------------------------------box plot for features ----------------------------
box_fun <- function(data ,box_x,box_y,title_col,fill = "Label", size =10 , angle = 90 ){ 
  ggsave(
    ggplot(data ,aes(x=box_x, y = box_y,fill = fill,show.legend = F ))+ 
      geom_boxplot( fill = fill , alpha=0.4,outlier.shape = NA ) + 
      geom_jitter(width = 0.2, size = 0.4)+
      stat_boxplot(geom = "errorbar", width=0.5, size=0.3)+
      theme_update() + 
      theme(axis.text.x = element_text(size = (size/2)+3, angle = angle),
            axis.text.y = element_text(size = (size/2)+3), 
            panel.background= element_rect(fill = 'white')
            ,panel.grid=element_line(color= 'Gainsboro'),
            axis.line = element_line(color= 'black' , size = 0.5)
      ) +
      labs( x = '' ) +
      scale_y_continuous(labels = label_scientific(digits = 3))+
      facet_wrap(vars({{title_col}}), scales = 'free' ,as.table = T )
    ,filename = "plot.png" , width=10, height=10 ,dpi=300) 
}

#test function 
#den_fun(dat,dat$value, dat$variable, "pink",10 ,90, .0001, .0002) 


#----------------------------------------------------------
#------------- box plot for group samples ----------------------------------------

box_fun <- function(data ,box_x,box_y, size =10 , angle = 90){ 
  ggsave(
    ggplot(data ,aes(x=den_x,y=box_y, fill = "Lebal",show.legend = F )) + 
      geom_boxplot( fill = fill , alpha=0.4,outlier.shape = NA ) + 
      geom_jitter(width = 0.2, size = 0.4)+
      stat_boxplot(geom = "errorbar", width=0.5, size=0.3)+
      theme_update() + 
      theme(axis.text.x = element_text(size = (size/2)+3, angle = angle),
            axis.text.y = element_text(size = (size/2)+3), 
            panel.background= element_rect(fill = 'white')
            ,panel.grid=element_line(color= 'Gainsboro'),
            axis.line = element_line(color= 'black' , size = 0.5)
      ) +
      labs( x = '' ) +
      scale_y_continuous(labels = label_scientific(digits = 3))
    ,filename = "plot.png" , width=10, height=10 ,dpi=300) 
}






