library(dplyr)
library(ggplot2)
library(tidyverse)
library("scales") 

#dat <- read.csv(file.choose())

#---------------------- bar plot for group Samples ----------------------------

bar_fun <- function(data ,bar_x,bar_y,title_col,fill = "blue"  , width =0.4 , size =10 , angle = 90){ 
ggsave(
ggplot(data,aes(x =bar_x , y =bar_y,show.legend = F)) + 
  geom_bar(stat = 'identity' , fill = fill, width=width)  + 
  theme_update() + 
  theme(axis.text.x = element_text(size = (size/2)+3, angle = angle),
        axis.text.y = element_text(size = (size/2)+3), 
        , aspect.ratio = 2/3, axis.title=element_text(size = (size/2)+7), 
        panel.background= element_rect(fill = 'white')
        ,panel.grid=element_line(color= 'Gainsboro'),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x = 'All features' , y = "Mean group") +
  scale_x_discrete(breaks =bar_x )+
  scale_y_continuous(labels = label_scientific(digits = 3))
 ,filename = "plot.png" , width=20, height=20 ,dpi=100) 
} 



#---------------------- bar plot for group features ----------------------------
bar_fun <- function(data ,bar_x,bar_y,title_col,fill = "blue"  , width =0.4 , size =10 , angle = 90 ){ 
  ggsave(
    ggplot(data ,aes(x=bar_x, y = bar_y , show.legend = F )) + 
      geom_bar(stat = 'identity' , fill = fill, width=width) + 
      theme_update() + 
      theme(axis.text.x = element_text(size = (size/2)+3, angle = angle),
            axis.text.y = element_text(size = (size/2)+3),
            , aspect.ratio = 2/3,axis.title=element_text(size = (size/2)+7), 
            panel.background= element_rect(fill = 'white')
            ,panel.grid=element_line(color= 'Gainsboro'),
            panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      labs(x = 'Groups' , y = "values") +
      scale_x_discrete(breaks =bar_x )+
      scale_y_continuous(labels = label_scientific(digits = 3))+
      facet_wrap(vars({{facet}}), scales = 'free' ,as.table = T )
    ,filename = "plot.png" , width=20, height=20 ,dpi=300) 
}




