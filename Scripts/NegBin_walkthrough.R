###########################################
## Negative Binomial GLM walkthrough



####  make a model  ####
library(MASS)
m1 <- glm.nb(cpue_dat ~ Year_f + reef_f, data = mydata, link = "log")


####  Get summary of model  ####
summary(m1)


####  Extract Coefficients  ####
coef(m1)


####  Fitted values and Residuals  ####
m1$fitted
m1$residuals


####  Plotting Predictions using predict()  ####

#make a prediction grid of the treatment levels
pred.mat <-  expand.grid(Year_f = unique(levels(mydata$Year_f)),
                         reef_f = unique(levels(mydata$reef_f)))


########  Method 1 - Get predicted response only  ########  
pred.mat$phat <- predict(m1, pred.mat, type = "response")

########  Method 2 - Get predicted value and the predicted standard error, untransformed  ########  
pred.mat <- cbind(pred.mat, (predict(m1, pred.mat, type = "link", se.fit=TRUE)))


#Calculate expected values and confidence intervals to response scale from Method 2
pred.mat <- pred.mat %>% 
  mutate(reef = ifelse(reef_f == 1, "Reef", "Bare"),                       #Change from binary code to names
         reef = factor(reef, levels =c("Reef", "Bare")),                   #Set factor levels for legend order
         cpue_test = exp(fit),                                             #Convert to response scale from log link
         LCL = exp(fit - 1.96 * se.fit),                                   #Calculate lower bound
         UCL = exp(fit + 1.96 * se.fit))                                   #Calculate upper bound



########  Method 1 - Plot  ########
#plot of predicted cpue from type = "response"
ggplot(pred.mat) +
  geom_line(aes(Year_f, phat, group = reef, lty = reef))+
  geom_point(aes(Year_f, phat, group = reef, shape = reef)) +
  ggtitle("predict(m1, pred.mat, type = 'response')") + ylab("Predicted CPUE") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5), legend.position="bottom")



#######  Method 2 - Plot  ########
#Plot of predicted cpue using se fit with confidence intervals
ggplot(pred.mat, aes(Year_f, cpue_test)) +
  geom_ribbon(data = pred.mat, 
              aes(x = Year_f, 
                  ymin = LCL, 
                  ymax = UCL, 
                  group = reef, 
                  fill = reef), 
              alpha = .25) +
  geom_line(aes(Year_f, cpue_test, group = reef, lty = reef), show.legend = F) +
  geom_point(aes(Year_f, cpue_test, group = reef, shape = reef), show.legend = F) +
  scale_fill_manual("", values = c("Reef" ="royalblue", "Bare" ="orange")) +
  ggtitle('pred.mat, (predict(m1, pred.mat, type = "link", se.fit=TRUE))') + ylab("Predicted CPUE") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5), legend.position="bottom")






#####################  Extract Predicted Values using lsmeans/emmeans  ##################
lsmean_pred <- as.data.frame(summary(lsmeans(m1, "Year_f", by = c("Year_f", "reef_f")), type = "response"))
lsmean_pred <- lsmean_pred %>% 
  mutate(reef = ifelse(reef_f == 1, "Reef", "Bare"),
         reef = factor(reef, levels =c("Reef", "Bare")))

#Plot them
#summary(lsmeans(m1, "Year_f", by = c("Year_f", "reef_f")), type = "response")
ggplot(lsmean_pred, aes(Year_f, response)) +
  geom_ribbon(data = lsmean_pred, 
              aes(x = Year_f, 
                  ymin = asymp.LCL, 
                  ymax = asymp.UCL, 
                  group = reef, 
                  fill = reef), 
              alpha = .25) +
  geom_line(aes(Year_f, response, group = reef, lty = reef), show.legend = F) +
  geom_point(aes(Year_f, response, group = reef, shape = reef), show.legend = F) +
  scale_fill_manual("", values = c("Reef" ="royalblue", "Bare" ="orange")) +
  ggtitle(p_title) + 
  ylab("Predicted Mincount") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5), legend.position="bottom")



#mincounts by year and reef
obs_count <- mydata %>% group_by(Year_f, reef_f) %>% summarise(avg_mincount = mean(cpue_dat, na.rm = T))
obs_count$reef <- ifelse(obs_count$reef_f == 0, "Bare", "Reef")
obs_count <- obs_count[complete.cases(obs_count),] #delete missing rows


#Plot with Observed Data
ggplot(lsmean_pred, aes(Year_f, response)) +
  geom_ribbon(data = lsmean_pred, 
              aes(x = Year_f, 
                  ymin = asymp.LCL, 
                  ymax = asymp.UCL, 
                  group = reef, 
                  fill = reef), 
              alpha = .25) +
  geom_line(aes(Year_f, response, group = reef, lty = "Predicted")) +
  geom_point(aes(Year_f, response, group = reef, shape = reef), show.legend = F) +
  geom_line(data = obs_count, aes(Year_f, avg_mincount, group = reef, lty = "Observed")) +
  geom_point(data = obs_count, aes(Year_f, avg_mincount, shape = reef), show.legend = F) +
  facet_wrap(~reef, ncol = 1)+
  scale_fill_manual("95% C.I.", 
                    values = c("Reef" ="royalblue", "Bare" ="orange")) +
  scale_linetype_manual("Data Source",
                        labels =  c("Observed","Predicted"),
                        values = c("dashed","solid")) +
  ggtitle(p_title) + 
  ylab("Predicted Mincount") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5), legend.position="bottom")






#Compare Predicted Index to standardised cpue from observed data
overall_meancpue <- mean(mydata$cpue_dat, na.rm = T)
obscpue_df <- mydata %>% group_by(Year_f) %>%  summarise(ob_scpue = mean(cpue_dat)/overall_meancpue) %>% 
  mutate(Year_n = as.numeric(as.character(Year_f)))


mean_pred <- mean(lsmean_pred$response)
lsmean_pred <- lsmean_pred %>% mutate(Std_Index = response/mean_pred)


#Comparing predicted index to std_cpue
ggplot(lsmean_pred, aes(Year_f, Std_Index)) +
  geom_line(aes(Year_f, Std_Index, group = reef, lty = reef, color = reef), size = 1) +
  geom_line(data = obscpue_df, aes(Year_f, ob_scpue, group = 1, lty = "Observed", color = "Observed"), size = 1) +
  geom_point(aes(Year_f, Std_Index, color = reef)) +
  geom_point(data = obscpue_df, aes(Year_f, ob_scpue, color = "Observed")) +
  ggtitle(p_title) + 
  ylab("Standardized Index of Abundance") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5), legend.position="bottom")









#######  Standardized Index of Abundance for just Year  ######


#So this is what you get without caring about reef
lsmean_noreef <- as.data.frame(summary(lsmeans(m1, "Year_f", by = c("Year_f")), type = "response"))


#Standardize response
mean_pred <- mean(lsmean_noreef$response)
lsmean_noreef <- lsmean_noreef %>% mutate(Std_Index = response/mean_pred,
                                          CV_Index = SE/mean_pred,
                                          StdLCI = asymp.LCL/mean_pred,
                                          StdUCI = asymp.UCL/mean_pred)




#Comparing predicted index to std_cpue
ggplot(lsmean_noreef, aes(Year_f, Std_Index)) +
  geom_ribbon(aes(x = Year_f, 
                  ymin = StdLCI, 
                  ymax = StdUCI,
                  group = 1), fill ="gray90") +
  geom_line(aes(Year_f, Std_Index, 
                group = 1, 
                color = "Predicted", 
                lty = "Predicted"), size = 1) +
  geom_line(data = obscpue_df, 
            aes(Year_f, ob_scpue, 
                group = 1, 
                lty = "Observed", 
                color = "Observed"), size = 1) +
  geom_point(aes(Year_f, Std_Index, color = "Predicted")) +
  geom_point(data = obscpue_df, 
             aes(Year_f, ob_scpue, color = "Observed")) +
  ggtitle(p_title) + 
  ylab("Standardized Index of Abundance") +
  theme(axis.text.x=element_text(angle=90, vjust=0.5), legend.position="bottom")
