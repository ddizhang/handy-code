library(dplyr)
library(tidyr)

tr_pre = rnorm(1000, 10, 3)
ctr_pre = rnorm(1000, 9, 3)

tr_post = rnorm(1000, 16, 3)
ctr_post = rnorm(1000, 10, 3)


dat = 
rbind(
data.frame(group = 'tr', 
           period = 'pre',
           value = tr_pre),

data.frame(group = 'ctr', 
           period = 'pre',
           value = ctr_pre),

data.frame(group = 'tr', 
           period = 'post',
           value = tr_post),

data.frame(group = 'ctr', 
           period = 'post',
           value = ctr_post)
)




# regression with interaction term
model = lm(value ~ group + period + group:period, data = dat)
summary(model)

enroll_effect = abs(unname(model$coefficients[names(model$coefficients) == 'groupctr:periodpost']))
effect_sd = coef(summary(model))['groupctr:periodpost', "Std. Error"]

effect_lower = enroll_effect - qt(0.975, nrow(dat)-4)*effect_sd
effect_upper = enroll_effect + qt(0.975, nrow(dat)-4)*effect_sd
c(effect_lower, effect_upper)


# t-test
test_res = t.test(tr_post-tr_pre, ctr_post-ctr_pre, var.equal = TRUE)
test_res
test_res$estimate[2]-test_res$estimate[1]

test_res$p.value



# regression 

head(dat)

dat2 = 
rbind(
data.frame(group = 'tr', diff = tr_post - tr_pre),
data.frame(group = 'ctr', diff = ctr_post - ctr_pre))

model2 = lm(diff ~ group, data = dat2)
summary(model2)



summary(model)
