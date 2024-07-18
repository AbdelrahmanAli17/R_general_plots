library(dplyr)
library(ggplot2)
library(tidyverse)
library("scales") 

#dat <- read.csv(file.choose())

#---------------------- point plot for group Samples ----------------------------

line_fun <- function(data ,line_x,line_y,title_col,fill = "blue"  , size =10 , angle = 90){ 
  ggsave(
    ggplot(data ,aes(x=line_x, y = line_y , show.legend = F )) + 
      geom_line(stat = 'identity' , fill = fill , group = 1 ) + 
      theme_update() + 
      theme(axis.text.x = element_text(size = (size/2)+3, angle = angle),
            axis.text.y = element_text(size = (size/2)+3), 
            , aspect.ratio = 2/3, axis.title=element_text(size = (size/2)+7), 
            panel.background= element_rect(fill = 'white')
            ,panel.grid=element_line(color= 'Gainsboro'),
            axis.line = element_line(color= 'black' , size = 0.5)
      ) +
      labs(x = 'All features' , y = "Mean group") +
      scale_x_discrete(breaks =line_x )+
      scale_y_continuous(labels = label_scientific(digits = 3))
    ,filename = "plot.png" , width=20, height=20 ,dpi=100) 
} 



#---------------------- point plot for group features ----------------------------
line_fun <- function(data ,line_x,line_y,title_col,fill = "blue"  , size =10 , angle = 90){ 
  ggsave(
    ggplot(data ,aes(x=line_x, y = line_y , show.legend = F )) + 
      geom_line(stat = 'identity' , fill = fill , group = 1 ) + 
      theme_update() + 
      theme(axis.text.x = element_text(size = (size/2)+3, angle = angle),
            axis.text.y = element_text(size = (size/2)+3),
            , aspect.ratio = 2/3,axis.title=element_text(size = (size/2)+7), 
            panel.background= element_rect(fill = 'white')
            ,panel.grid=element_line(color= 'Gainsboro'),
            axis.line = element_line(color= 'black' , size = 0.5)
      ) +
      labs(x = 'Groups' , y = "values") +
      scale_x_discrete(breaks =line_x )+
      scale_y_continuous(labels = label_scientific(digits = 3))+
      facet_wrap(vars({{title_col}}), scales = 'free' ,as.table = T )
    ,filename = "plot.png" , width=20, height=20 ,dpi=300) 
}

