




















































































































































``` {r Subsetting_analytic_files}

### Creating data file for those who start as Not-at-Risk and later transition to At-Risk

Transitioned_Data <- Comp_DF[, -c(3:30,56:111)]

Transitioned_Users <- Transitioned_Data %>%
  arrange(USERID, EVENTDATE) %>%
  group_by(USERID) %>%
  mutate(ever_zero_so_far = cummax(PERIOD_RISK == 0)) %>%
  filter(PERIOD_RISK == 1 & lag(ever_zero_so_far, default = FALSE)) %>%
  ungroup() %>%
  distinct(USERID) %>%
  pull(USERID)

# Step 2: Subset to keep ALL rows for those USERIDs
Transitioned_Data <- Transitioned_Data %>%
  filter(USERID %in% Transitioned_Users)

Transitioned_Data <- Transitioned_Data %>%
  arrange(USERID, EVENTDATE) %>%
  group_by(USERID) %>%
  mutate(
    PERIOD = cumsum(!is.na(MONTHLY))  # increments every time a MONTHLY checkpoint is hit
  ) %>%
  ungroup()

period_use_sums <- Transitioned_Data %>%
  group_by(USERID, PERIOD) %>%
  summarise(Comp_JOURNEY_USE_SUM = sum(Comp_JOURNEY_USE_D, na.rm = TRUE), .groups = "drop")

Monthly_Risk_Transition <- Transitioned_Data %>%
  left_join(period_use_sums, by = c("USERID", "PERIOD"))

Monthly_DF <- Comp_DF[which(Comp_DF$MONTHLY == 1), -c(3:30)]  ### Update columns here if needed to remove appropriate vars

#Monthly_DF$TOTAL_JOURNEY_USE_M <- rowSums(Monthly_DF[grep("MONTHLY_SUM", names(Monthly_DF))])

Monthly_DF_Risk <- Monthly_DF[which(Monthly_DF$ANY_RISK == 1),]

Monthly_DF_Risk_R <- Monthly_DF_Risk[, -grep("_SUM", names(Monthly_DF_Risk))]

obs_per_person <- table(Monthly_DF_Risk_R$USERID)

Monthly_DF_Risk_Modeling <- Monthly_DF_Risk_R[Monthly_DF_Risk_R$USERID %in% names(obs_per_person[obs_per_person >= 3]), ]

Daily_DF <- Comp_DF[which(Comp_DF$DAILY == 1), -c(3:30)]  ### Update columns here if needed to remove appropriate vars

Daily_DF$TOTAL_JOURNEY_USE_D <- rowSums(Daily_DF[grep("DAILY_SUM", names(Daily_DF))])

Daily_DF_Risk <- Daily_DF[which(Daily_DF$ANY_RISK == 1),]

Daily_DF_Risk_R <- Daily_DF_Risk[, -grep("_SUM", names(Daily_DF_Risk))]

obs_per_person <- table(Daily_DF_Risk_R$USERID)

Daily_DF_Risk_Modeling <- Daily_DF_Risk_R[Daily_DF_Risk_R$USERID %in% names(obs_per_person[obs_per_person >= 3]), ]

Monthly_Dep_Anx_GRP_1 <- Monthly_DF_Risk_Modeling[which(Monthly_DF_Risk_Modeling$DEP_ANX_RISK_GROUP == 1),]

Monthly_Dep_Anx_GRP_2 <- Monthly_DF_Risk_Modeling[which(Monthly_DF_Risk_Modeling$DEP_ANX_RISK_GROUP == 2),]

Monthly_Dep_Anx_GRP_3 <- Monthly_DF_Risk_Modeling[which(Monthly_DF_Risk_Modeling$DEP_ANX_RISK_GROUP == 3),]

Monthly_Stress_GRP_1 <- Monthly_DF_Risk_Modeling[which(Monthly_DF_Risk_Modeling$STRESS_RISK_GROUP == 1),]

Monthly_Stress_GRP_2 <- Monthly_DF_Risk_Modeling[which(Monthly_DF_Risk_Modeling$STRESS_RISK_GROUP == 2),]

Monthly_Stress_GRP_3 <- Monthly_DF_Risk_Modeling[which(Monthly_DF_Risk_Modeling$STRESS_RISK_GROUP == 3),]

```

``` {r monthly_out_pred_correlations}

Outcomes <- c("STRESSLEVEL", "MANAGEFEELINGSLEVEL", "FEELINGSFREQUENCY", "DAYSEMOTIONALLYCHALLENGING", "MANAGESTRESSLEVEL")

Pred_Out_M <- Monthly_DF[, which(grepl("MONTHLY_SUM", names(Monthly_DF)) | names(Monthly_DF) %in% Outcomes)]

Cors_list <- NULL

for (i in Outcomes) {
  
  ctest_M <- corr.test(Pred_Out_M)
  r_value_M <- round(ctest_M$r[, i], 4)
  p_value_M <- round(ctest_M$p[, i], 4)
  n_value_M <- ctest_M$n[, i]
  
  Monthly_Cors <- data.frame(Correlation = r_value_M, P_Value = p_value_M, N = n_value_M, Outcome = i)
  
  Monthly_Cors$Predictor <- sub(".*\\.", "", rownames(Monthly_Cors))
  
  Cors_list[[i]] <- Monthly_Cors
  
}

Monthly_Cors_DF <- do.call(rbind, Cors_list)

rownames(Monthly_Cors_DF) <- NULL

```

``` {r daily_out_pred_correlations}

Pred_Out_D <- Daily_DF[, which(grepl("DAILY_SUM", names(Monthly_DF)) | names(Monthly_DF) %in% Outcomes)]

for (i in Outcomes){
  
  ctest_D <- corr.test(Pred_Out_D)
  r_value_D <- round(ctest_D$r[, i], 4)
  p_value_D <- round(ctest_D$p[, i], 4)
  n_value_D <- ctest_D$n[, i]
  
  Daily_Cors <- data.frame(Correlation = r_value_D, P_Value = p_value_D, N = n_value_D, Outcome = i)
  
  Daily_Cors$Predictor <- sub(".*\\.", "", rownames(Daily_Cors))
  
  Cors_list[[i]] <- Daily_Cors
  
}

Daily_Cors_DF <- do.call(rbind, Cors_list)

rownames(Daily_Cors_DF) <- NULL

```

``` {r monthly_analyses}

###Risk Signals → Journey Use → Improvement
###Risk Signals → Journey Use → No Improvement
###Risk Signals → Limited/No Journey Use → Worsening Risk

# Standardize (z-score) TOTAL_JOURNEY_USE_M

Monthly_DF_Risk_Modeling <- Monthly_DF_Risk_Modeling %>%
  group_by(USERID) %>%
  mutate(DATE_NUMERIC = as.numeric(EVENTDATE - min(EVENTDATE)) + 1) %>%
  ungroup()

Monthly_DF_Risk_Modeling$TOTAL_JOURNEY_USE_M_z <- scale(Monthly_DF_Risk_Modeling$TOTAL_JOURNEY_USE_M)[,1]





















# Random intercept model: allows each person to have their own baseline stress level

model1 <- lmer(STRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 + TOTAL_JOURNEY_USE_M_z | USERID), data = Monthly_DF_Risk_Modeling)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(MANAGESTRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 + TOTAL_JOURNEY_USE_M_z | USERID), data = Monthly_DF_Risk_Modeling)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(MANAGEFEELINGSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 + TOTAL_JOURNEY_USE_M_z | USERID), data = Monthly_DF_Risk_Modeling)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(FEELINGSFREQUENCY ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 + TOTAL_JOURNEY_USE_M_z | USERID), data = Monthly_DF_Risk_Modeling)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(DAYSEMOTIONALLYCHALLENGING ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 + TOTAL_JOURNEY_USE_M_z | USERID), data = Monthly_DF_Risk_Modeling)

summary(model1)

```

``` {r daily_analyses}

###Risk Signals → Journey Use → Improvement
###Risk Signals → Journey Use → No Improvement
###Risk Signals → Limited/No Journey Use → Worsening Risk



# Install and load the tvem and tidyverse packages

# Example formula where the effect of 'predictor1' varies over 'time'
# grouped by 'subject_id'
tvem_model_D <- tvem(
  formula = FEELINGRATING ~ TOTAL_JOURNEY_USE_D_z,
  data = Daily_DF_Risk_Modeling,
  id = USERID,
  time = DATE_NUMERIC,
  family = gaussian() # Use binomial() for binary outcomes
)

# View the summary of the varying coefficients
summary(tvem_model_D)

# Adjust index/name based on what str() shows you
coef_df_D <- tvem_model_D$grid_fitted_coefficients$TOTAL_JOURNEY_USE_D_z

coef_df_D$time <- tvem_model_D$time_grid

library(ggplot2)

ggplot(coef_df_D, aes(x = time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
  geom_line(aes(y = estimate), color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Time-Varying Effect of Total Use on Stress Level",
    x = "Time (days since first observation)",
    y = "Estimated Effect of Total Daily Journey Use on STRESSLEVEL"
  ) +
  theme_minimal(base_size = 13)












# Standardize (z-score) TOTAL_JOURNEY_USE_M

Daily_DF_Risk_Modeling$TOTAL_JOURNEY_USE_D_z <- scale(Daily_DF_Risk_Modeling$TOTAL_JOURNEY_USE_D)[,1]

Daily_DF_Risk_Modeling <- Daily_DF_Risk_Modeling %>%
  group_by(USERID) %>%
  mutate(DATE_NUMERIC = as.numeric(EVENTDATE - min(EVENTDATE)) + 1) %>%
  ungroup()

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(FEELINGRATING ~ TOTAL_JOURNEY_USE_D_z + (1 | USERID), data = Daily_DF_Risk_Modeling)

summary(model1)

model2 <- lmer(FEELINGRATING ~ TOTAL_JOURNEY_USE_D_z + (1 + TOTAL_JOURNEY_USE_D_z | USERID), data = Daily_DF_Risk_Modeling)

summary(model2)

model3 <- lmer(FEELINGRATING ~ TOTAL_JOURNEY_USE_D_z * DATE_NUMERIC + (1 | USERID), data = Daily_DF_Risk_Modeling)

summary(model3)

model4 <- lmer(FEELINGRATING ~ TOTAL_JOURNEY_USE_D_z * DATE_NUMERIC + (1 + TOTAL_JOURNEY_USE_D_z | USERID), data = Daily_DF_Risk_Modeling)

summary(model4)

```





``` {r daily_model_plots}

predictions <- ggpredict(model3, terms = "TOTAL_JOURNEY_USE_D_z")

# 3. Plot automatically using ggplot2 logic under the hood
plot(predictions)

# Alternatively, customize it further with standard ggplot2 layers
ggplot(predictions, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "blue") +
  labs(x = "Days", y = "Predicted Reaction Time", title = "Model Fixed Effects")









```


``` {r tvem_monthly_test}

tvem_model_M <- tvem(
  formula = STRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z,
  data = Monthly_Dep_Anx_GRP_1,
  id = USERID,
  time = DATE_NUMERIC,
  family = gaussian() # Use binomial() for binary outcomes
)

# Adjust index/name based on what str() shows you
coef_df_M <- tvem_model_M$grid_fitted_coefficients$TOTAL_JOURNEY_USE_M_z

coef_df_M$time <- tvem_model_M$time_grid

ggplot(coef_df_M, aes(x = time)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.2) +
  geom_line(aes(y = estimate), color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Time-Varying Effect of Total Use on Stress Level",
    x = "Time (days since first observation)",
    y = "Estimated Effect of Total Daily Journey Use on STRESSLEVEL"
  ) +
  theme_minimal(base_size = 13)




```


























``` {r monthly_analyses}

###Risk Signals → Journey Use → Improvement
###Risk Signals → Journey Use → No Improvement
###Risk Signals → Limited/No Journey Use → Worsening Risk

# Standardize (z-score) TOTAL_JOURNEY_USE_M


Monthly_Dep_Anx_GRP_1 <- Monthly_Dep_Anx_GRP_1 %>%
  group_by(USERID) %>%
  mutate(DATE_NUMERIC = as.numeric(EVENTDATE - min(EVENTDATE)) + 1) %>%
  ungroup()

Monthly_Dep_Anx_GRP_1$TOTAL_JOURNEY_USE_M_z <- scale(Monthly_Dep_Anx_GRP_1$TOTAL_JOURNEY_USE_M)[,1]

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(STRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_1)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(MANAGESTRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_1)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(MANAGEFEELINGSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_1)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(FEELINGSFREQUENCY ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_1)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(DAYSEMOTIONALLYCHALLENGING ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_1)

summary(model1)






Monthly_Dep_Anx_GRP_2 <- Monthly_Dep_Anx_GRP_2 %>%
  group_by(USERID) %>%
  mutate(DATE_NUMERIC = as.numeric(EVENTDATE - min(EVENTDATE)) + 1) %>%
  ungroup()

Monthly_Dep_Anx_GRP_2$TOTAL_JOURNEY_USE_M_z <- scale(Monthly_Dep_Anx_GRP_2$TOTAL_JOURNEY_USE_M)[,1]

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(STRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_2)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(MANAGESTRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_2)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(MANAGEFEELINGSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_2)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(FEELINGSFREQUENCY ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_2)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(DAYSEMOTIONALLYCHALLENGING ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_2)

summary(model1)












Monthly_Dep_Anx_GRP_3 <- Monthly_Dep_Anx_GRP_3 %>%
  group_by(USERID) %>%
  mutate(DATE_NUMERIC = as.numeric(EVENTDATE - min(EVENTDATE)) + 1) %>%
  ungroup()

Monthly_Dep_Anx_GRP_3$TOTAL_JOURNEY_USE_M_z <- scale(Monthly_Dep_Anx_GRP_3$TOTAL_JOURNEY_USE_M)[,1]

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(STRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_3)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(MANAGESTRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_3)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(MANAGEFEELINGSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_3)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(FEELINGSFREQUENCY ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_3)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(DAYSEMOTIONALLYCHALLENGING ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_3)

summary(model1)
















Monthly_Stress_GRP_1 <- Monthly_Stress_GRP_1 %>%
  group_by(USERID) %>%
  mutate(DATE_NUMERIC = as.numeric(EVENTDATE - min(EVENTDATE)) + 1) %>%
  ungroup()

Monthly_Stress_GRP_1$TOTAL_JOURNEY_USE_M_z <- scale(Monthly_Stress_GRP_1$TOTAL_JOURNEY_USE_M)[,1]

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(STRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_1)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(MANAGESTRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_1)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(MANAGEFEELINGSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_1)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(FEELINGSFREQUENCY ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_1)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(DAYSEMOTIONALLYCHALLENGING ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_1)

summary(model1)






Monthly_Stress_GRP_2 <- Monthly_Stress_GRP_2 %>%
  group_by(USERID) %>%
  mutate(DATE_NUMERIC = as.numeric(EVENTDATE - min(EVENTDATE)) + 1) %>%
  ungroup()

Monthly_Stress_GRP_2$TOTAL_JOURNEY_USE_M_z <- scale(Monthly_Stress_GRP_2$TOTAL_JOURNEY_USE_M)[,1]

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(STRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_2)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(MANAGESTRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_2)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(MANAGEFEELINGSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_2)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(FEELINGSFREQUENCY ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_2)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(DAYSEMOTIONALLYCHALLENGING ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_2)

summary(model1)












Monthly_Stress_GRP_3 <- Monthly_Stress_GRP_3 %>%
  group_by(USERID) %>%
  mutate(DATE_NUMERIC = as.numeric(EVENTDATE - min(EVENTDATE)) + 1) %>%
  ungroup()

Monthly_Stress_GRP_3$TOTAL_JOURNEY_USE_M_z <- scale(Monthly_Stress_GRP_3$TOTAL_JOURNEY_USE_M)[,1]

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(STRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_3)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(MANAGESTRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_3)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(MANAGEFEELINGSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_3)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(FEELINGSFREQUENCY ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_3)

summary(model1)

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(DAYSEMOTIONALLYCHALLENGING ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Stress_GRP_3)

summary(model1)




Monthly_Dep_Anx_GRP_3 <- Monthly_Dep_Anx_GRP_3 %>%
  group_by(USERID) %>%
  mutate(DATE_NUMERIC = as.numeric(EVENTDATE - min(EVENTDATE)) + 1) %>%
  ungroup()

Monthly_Dep_Anx_GRP_3$TOTAL_JOURNEY_USE_M_z <- scale(Monthly_Dep_Anx_GRP_3$TOTAL_JOURNEY_USE_M)[,1]

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(STRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + (1 | USERID), data = Monthly_Dep_Anx_GRP_3)

summary(model1)

write.csv(abc, "C:/Users/Owner/Downloads/test.csv", row.names = F)

table(Daily_DF$ANY_RISK)

table(Monthly_DF$ANY_RISK)

Monthly_DF$TOTAL_JOURNEY_USE_M_z <- scale(Monthly_DF$TOTAL_JOURNEY_USE_M)[,1]


Monthly_DF <- Monthly_DF %>%
  group_by(USERID) %>%
  mutate(DATE_NUMERIC = as.numeric(EVENTDATE - min(EVENTDATE)) + 1) %>%
  ungroup()

Monthly_DF$TOTAL_JOURNEY_USE_M_z <- scale(Monthly_DF$TOTAL_JOURNEY_USE_M)[,1]

# Random intercept model: allows each person to have their own baseline stress level
model1 <- lmer(STRESSLEVEL ~ TOTAL_JOURNEY_USE_M_z * DATE_NUMERIC + ANY_RISK + (1 | USERID), data = Monthly_DF)

summary(model1)


testdf <- testdf[, -c(27:82)]

model1 <- lmer(TOTAL_JOURNEY_USE_M_z ~ ANY_RISK + (1 | USERID), data = testdf)

summary(model1)




Monthly_Risk_Transition_TEST <- Monthly_Risk_Transition %>%
  arrange(USERID, PERIOD) %>%
  group_by(USERID) %>%
  mutate(
    risk_onset_period = ifelse(any(PERIOD_RISK == 1), PERIOD[which(PERIOD_RISK == 1)[1]], NA),
    phase = case_when(
      is.na(risk_onset_period) ~ NA_character_,
      PERIOD < risk_onset_period ~ "before",
      PERIOD >= risk_onset_period ~ "after"
    )
  ) %>%
  ungroup()

Monthly_Risk_Transition_TEST$Comp_JOURNEY_USE_SUM_z <- scale(Monthly_Risk_Transition_TEST$Comp_JOURNEY_USE_SUM)[,1]



testmodel <- lmer(
  STRESSLEVEL ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 1)
)

summary(testmodel)

testmodel <- lmer(
  MANAGESTRESSLEVEL ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 1)
)

summary(testmodel)

testmodel <- lmer(
  MANAGEFEELINGSLEVEL ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 1)
)

summary(testmodel)


testmodel <- lmer(
  FEELINGSFREQUENCY ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 1)
)

summary(testmodel)


testmodel <- lmer(
  DAYSEMOTIONALLYCHALLENGING ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 1)
)

summary(testmodel)




testmodel <- lmer(
  STRESSLEVEL ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 2)
)

summary(testmodel)

testmodel <- lmer(
  MANAGESTRESSLEVEL ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 2)
)

summary(testmodel)

testmodel <- lmer(
  MANAGEFEELINGSLEVEL ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 2)
)

summary(testmodel)


testmodel <- lmer(
  FEELINGSFREQUENCY ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 2)
)

summary(testmodel)


testmodel <- lmer(
  DAYSEMOTIONALLYCHALLENGING ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 2)
)

summary(testmodel)



testmodel <- lmer(
  STRESSLEVEL ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 3)
)

summary(testmodel)

testmodel <- lmer(
  MANAGESTRESSLEVEL ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 3)
)

summary(testmodel)

testmodel <- lmer(
  MANAGEFEELINGSLEVEL ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 3)
)

summary(testmodel)


testmodel <- lmer(
  FEELINGSFREQUENCY ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 3)
)

summary(testmodel)


testmodel <- lmer(
  DAYSEMOTIONALLYCHALLENGING ~ Comp_JOURNEY_USE_SUM_z * phase + (1 | USERID),
  data = Monthly_Risk_Transition_TEST %>% filter(!is.na(phase)),
  subset = (DEP_ANX_RISK_GROUP == 3)
)

summary(testmodel)

bbb <- daily_cases_low_feeling[which(daily_cases_low_feeling$SUM_COMP_JOURNEY_USE_D_BEFORE_1WK > 5000),c(1,115)]

abb <- Comp_DF[which(Comp_DF$USERID == 5768671),]

table(Daily_Checks$FEELINGRATING)

```
