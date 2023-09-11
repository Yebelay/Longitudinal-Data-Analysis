
library(naniar)
library(dlo)
miss_var_summary(airquality)
miss_var_table(airquality)


missing_counts <- colSums(is.na(data))

missing_counts_df <- data.frame(variable = names(missing_counts), 
                                count = missing_counts)


library(dlookr)

data %>%
  diagnose() %>%
  filter(missing_count > 0) %>% 
  arrange(desc(missing_count))


HIVdata %>%
  group_by(time) %>% 
  diagnose() %>%
  filter(missing_count > 0) %>% 
  arrange(desc(missing_count))


data %>% 
  plot_na_pareto(col = "blue")




library(dlookr)
HIVdata %>% group_by(time) %>% 
  plot_na_pareto(col = "blue")

missing_summary <- HIVdata %>%
  group_by(time) %>%
  summarize(missing_proportion = mean(is.na(cd4)))
missing_summary


library(ggplot2)
ggplot(missing_summary, 
       aes(x = factor(time),
                            y = missing_proportion, 
                            fill = factor(time)))+
  geom_bar(stat = "identity") 





library(naniar)
HIVdata %>% 
  group_by(time) %>% 
  miss_var_summary()




View(HIVdata)



fit2 <- lmer(cd4 ~ time + (1 | id), 
             data = Hdata)
summary(fit2)


library(zoo)
LOCF_data2 <- HIVdata %>% arrange(id) %>% 
  group_by(id) %>% na.locf() 
View(LOCF_data2)



library(mice)
data.mice <- HIVdata %>% arrange(id) %>% 
  mice(m=5, method = "pmm", 
                  printFlag=FALSE, print = FALSE)

library(tidyr)
impdata <- mice::complete(data.mice, action = "long", inc = F)
View(impdata)

library(naniar)
mcar_test(HIVdata)



