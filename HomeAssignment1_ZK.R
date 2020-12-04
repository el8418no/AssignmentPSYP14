#Dataset
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
View(data_sample_1)

#Check data
summary(data_sample_1)
model1 %>% plot(which = 4)
model2 %>% plot(which = 4)
data_sample_1 = data_sample_1 %>% slice(-c(93,150))

#models
model1<- lm(pain ~ age + sex, data = data_sample_1)
summary(model1)

model2<-lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum,
           data = data_sample_1)
summary(model2)

#AIC
AIC(model1)
AIC(model2)

#coef_table()function
coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}

coef_table(model1)
coef_table(model2)

summary(data_sample_1)

#Assumption of linear regression
#QQ plot
model1 %>% plot(which = 2)
model2 %>% plot(which = 2)

#histogram
hist_model1 = enframe(residuals(model1))
hist_model1 %>% ggplot() + aes(x = value) + geom_histogram()
hist_model2 = enframe(residuals(model1))
hist_model2 %>% ggplot() + aes(x = value) + geom_histogram()

#skew and kurtosis
describe(residuals(model1))
describe(residuals(model2))

#multicollinearity
model1 %>% vif()
model2 %>% vif()
