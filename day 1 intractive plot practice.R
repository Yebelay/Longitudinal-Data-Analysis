
## Packages 

#- tidyverse, lattice, 

library(ggplot2)
# Create the ggplot line plot
ggplot(subset(Infant, ind <= 100), aes(age, weight, color = factor(ind))) +
  geom_line()+theme(legend.position = "none")

ggplot(growth2, aes(age, measure, color = factor(ind)))+
  geom_line()

# Box Plot

p <- ggplot(growth2, aes(factor(age), measure, color=factor(age)))+ geom_boxplot()+
  labs(x="Age", y="Measure", title = "Box plot for distance measure over time", 
       color = "Age group")+theme_bw()
ggplotly(p)

library(plotly)
plot_ly(growth2, x=~age, y=~measure, color = ~factor(ind), type = "scatter", mode="l")

# Create a ggplot object with facets
gg_plot <- ggplot(growth2, aes(x = age, y = measure, color = factor(ind))) +
  geom_line() +
  facet_wrap(vars(sex))

# Convert ggplot object to plotly plot
plot <- ggplotly(gg_plot)



