#script to hold themes for different types of plots

library(ggplot2)

#create theme for ggplots
themerails<-theme(axis.title = element_text(size=13),
                  axis.text = element_text(size=10),
                  axis.line.x.top = element_blank(),
                  axis.line.y.right = element_blank(),
                  axis.line.x.bottom = element_line(size=.5),
                  axis.line.y.left = element_line(size=.5),
                  legend.key = element_blank(),
                  legend.text = element_text(size=13),
                  panel.background = element_blank())
