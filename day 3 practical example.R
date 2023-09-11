
library(ggplot2)

p <- ggplot(subset(Infant, ind<=100), aes(age, weight, color=factor(ind)))+
  geom_line()+theme(legend.position = "none")
p

library(plotly)

ggplotly(p)


library(readr)
growth2 <- read_csv("data/growth2.csv")

str(growth2)
p1 <- ggplot(growth2, aes(age,measure, color=factor(ind) ))+
  geom_line()+
  facet_wrap(vars(sex))+theme_bw()
ggplotly(p1)

# Box Plot

p1 <- ggplot(growth2, aes(factor(age), measure, color=factor(age)))+ geom_boxplot()+
  labs(x="Age", y="Measure", title = "Box plot for distance measure over time", 
       color = "Age group")+theme_bw()
ggplotly(p1)


## Calculate variance in ggplot

library(ggplot2)

# Calculate mean within each age and sex group
mean_data <- with(Infant, aggregate(weight, by = list(age, sex), FUN = mean))

colnames(mean_data) <- c("age", "sex", "mean")

# Create the ggplot
ggplot(mean_data, aes(x = age, y = mean, group = factor(sex), color = factor(sex))) +
  geom_line() + geom_point() +
  labs(title = "Mean Weight by Age and Sex",
       x = "Age", y = "Mean Weight") +
  scale_color_discrete(name = "Sex") +
  theme_bw()


library(ggplot2)
variance_data <- with(Infant, aggregate(weight, by = list(age, sex), FUN = var))

colnames(variance_data) <- c("age", "sex", "variance")
# Create the ggplot
ggplot(variance_data, aes(x = age, y = variance, 
                          group = factor(sex), color = factor(sex))) +
  geom_line() + geom_point() + theme_bw()




## Using lme4 package
library(lme4)

library(lme4)

# Fit the linear mixed-effects model using ML estimation
fit1 <- lmer(measure ~ sex + sex * age + (1 | ind), data = growth, method = "ML")

summary(fit1)

# Extract fixed-effect coefficients
coef(fit1)

# Get the summary of the model
summary(fit1)$coefficients

ranef(fit1, condVar = TRUE)

# Extract the random-effects summary
summary_fit1$varcor



## using nlme

library(nlme)
fit2 <- lme(fixed = measure ~ sex + sex * age,
                   data = growth, random = ~ 1 | ind)
summary(fit2)


model1 <- gls(measure ~ sex + sex * age  , correlation = corAR1(form = ~ 1 | ind), data = growth)


## Day 3

fit <- glm(formula = y ~ treatn * time, family = binomial, data = toenail)
summary(fit)

## to extract the fixed parts only
summary(fit)$coefficients
# or 
round(summary(fit)$coefficients, digits = 4)

# Fit the GEE models
fit1 <- geeglm(y ~ treatn + time + treatn*time, id = idnum, data = toenail,
               family = binomial(link = "logit"), corstr = "exchangeable", scale.fix = TRUE)
summary(fit1)

fit2 <- update(fit1, corstr = "ar1")
summary(fit2)
fit3 <- update(fit2, corstr = "unstructured")
summary(fit3)$coefficients

QIC(fit1, fit2, fit3)


### GLMM 

library(lme4)
glmmFit <- glmer(y ~ treatn*time + (1 | idnum), family = binomial("lo"), 
                 data = toenail, nAGQ = 15)
summary(glmmFit)

  
glmmFit_slope <- glmer(y ~ treatn*time + (time | idnum), family = binomial("logit"), 
                   data = toenail)
summary(glmmFit_slope)

library(GLMMadaptive)
glmmFit2 <- mixed_model(y ~ treatn*time, random = ~ 1 | idnum, 
                        family = binomial(), data = toenail, nAGQ = 15)
summary(glmmFit2)







