library(tidyverse)
library(viridis)
library(survey)
library(ggplot2)

impute_sex <- function (x,method,p = NA, true_x = NA){
  if(method == "popn prop"){
    sex = ifelse(x %in% c('m','f'),c('m','f')[x],
                 sample(c('m','f'),size = sum(x %in% 'o'),replace=TRUE,prob=c(.5,.5)))
  } else if(method == "impute female"){
    sex = ifelse(x %in% c('m','f'),c('m','f')[x],'f')
  } else if(method == "bad model estimate"){
    sex = ifelse(x %in% c('m','f'),c('f','m')[x],
                 sample(c('m','f'),size = sum(x %in% 'o'),replace=TRUE,prob=p))
  } else if(method == "best model estimate"){
    sex = true_x
  } else if(method == "remove"){
    sex = ifelse(x %in% c('m','f'),c('m','f')[x],NA)
  }
  sex <- factor(sex, level = c('m','f'))
  return(sex)
}

impute_gender <- function (x,method,p = NA,true_x){
  if(method == "popn prop"){
    gender = c(x[1]*.98,x[2]*.98,.02)
  } else if(method == "best model estimate"){
    gender = true_x
  } 
  return(gender)
}

mat_condition = as.matrix(data.frame(cond = 1:4,
                          y_mean_m = c(0,10,10,10),
                          y_mean_f = c(0,10, 0, -10),
                          y_mean_o = c(0, 0, 0, 0)))
rownames(mat_condition) <- c("none","m,f same","f,o same", "all diff")

popn_demog <- c(.49,.49,.02)
nrep = 100
nsamp = 500

store_df <- expand.grid(cond = 1:4, reps=1:nrep, impute = c("sample","popn"), method = c("popn prop","impute female","bad model estimate","best model estimate","remove"), prop_resp_male = seq(0,1,.01),
                        est_imp = NA, est_true = NA, popn_val = NA, 
                        est_gender_male = NA, est_gender_female = NA, est_gender_other = NA, 
                        est_sex_male = NA, est_sex_female = NA,  
                        popn_val_male = NA, popn_val_female = NA, popn_val_other = NA)
keep_sims <- store_df$impute=="sample" | (store_df$impute=="popn" & store_df$method %in% c("popn prop","best model estimate"))
store_df<-store_df[keep_sims,]


for (i in 1:nrow(store_df)) {
  df_samp <-
    data.frame(gender = sample(
      c('m', 'f', 'o'),
      size = nsamp,
      replace = TRUE,
      prob = popn_demog
    ))
  df_samp$gender <- factor(df_samp$gender, level = c('m', 'f', 'o'))
  cond <- store_df$cond[i]
  df_samp$y <-
    rnorm(nsamp, mat_condition[cond, as.numeric(df_samp$gender) + 1], 1)
  df_samp$true_sex_answer <-
    factor(ifelse(
      df_samp$gender == "m",
      sample(
        c('m', 'f'),
        size = sum(df_samp$gender == "m"),
        replace = TRUE,
        prob = c(1 - .01, .01)
      ),
      ifelse(
        df_samp$gender == "f",
        sample(
          c('m', 'f'),
          sum(df_samp$gender == "f"),
          replace = TRUE,
          prob = c(.01, 1 - .01)
        ),
        ifelse(
          store_df$prop_resp_male[i] == 1,
          'm',
          ifelse(
            store_df$prop_resp_male[i] == 0,
            'f',
            sample(
              c('m', 'f'),
              replace = TRUE,
              sum(df_samp$gender == "o" &
                    !store_df$prop_resp_male[i] %in% c(0, 1)),
              prob = c(store_df$prop_resp_male[i], 1 - store_df$prop_resp_male[i])
            )
          )
        )
      )
    ),
    levels = c("m", "f"))
if(store_df$impute[i] == "sample"){
  df_samp$imp_sex <-
    impute_sex(
      df_samp$gender,
      method = store_df$method[i],
      p = c(.5, .5),
      true_x = df_samp$true_sex_answer
    )
  
  prop_popn <-
    c(.49 + store_df$prop_resp_male[i] * .02,
      .49 + (1 - store_df$prop_resp_male[i]) * .02)
  imp_weights <-
    c(
      nsamp * prop_popn[1] / sum(df_samp$imp_sex == "m", na.rm = TRUE),
      nsamp * prop_popn[2] / sum(df_samp$imp_sex == "f", na.rm = TRUE)
    )
  df_samp$imp_wts <- imp_weights[as.numeric(df_samp$imp_sex)]
  true_weights <-
    c(
      nsamp * prop_popn[1] / sum(df_samp$true_sex_answer == "m"),
      nsamp * prop_popn[2] / sum(df_samp$true_sex_answer == "f")
    )
  df_samp$true_wts <-
    true_weights[as.numeric(df_samp$true_sex_answer)]
  store_df$est_sex_male[i] <-
    sum((df_samp$y * df_samp$imp_wts)[df_samp$imp_sex == "m"], na.rm = TRUE) /
    sum(df_samp$imp_wts[df_samp$imp_sex == "m"], na.rm = TRUE)
  store_df$est_sex_female[i] <-
    sum((df_samp$y * df_samp$imp_wts)[df_samp$imp_sex == "f"], na.rm = TRUE) /
    sum(df_samp$imp_wts[df_samp$imp_sex == "f"], na.rm = TRUE)
} else{
  true_prop_gender <- popn_demog
  true_popn_sex <- c(.49 + store_df$prop_resp_male[i] * .02,
      .49 + (1 - store_df$prop_resp_male[i]) * .02)
  imp_prop_popn <- impute_gender(true_popn_sex,
                                 method = store_df$method[i],
                                 p = c(.5,.5),
                                 true_prop_gender)
  imp_weights <-
    c(
      nsamp * imp_prop_popn[1] / sum(df_samp$gender == "m", na.rm = TRUE),
      nsamp * imp_prop_popn[2] / sum(df_samp$gender == "f", na.rm = TRUE),
      nsamp * imp_prop_popn[3] / sum(df_samp$gender == "o", na.rm = TRUE)
    )
  df_samp$imp_wts <- imp_weights[as.numeric(df_samp$gender)]
  true_weights <-
    c(
      nsamp * true_prop_gender[1] / sum(df_samp$gender == "m", na.rm = TRUE),
      nsamp * true_prop_gender[2] / sum(df_samp$gender == "f", na.rm = TRUE),
      nsamp * true_prop_gender[3] / sum(df_samp$gender == "o", na.rm = TRUE)
    )
  df_samp$true_wts <-
    true_weights[as.numeric(df_samp$gender)]
}
  store_df$est_imp[i] <-
    sum(df_samp$y * df_samp$imp_wts, na.rm = TRUE) / sum(df_samp$imp_wts, na.rm = TRUE)
  store_df$est_true[i] <-
    sum(df_samp$y * df_samp$true_wts, na.rm = TRUE) / sum(df_samp$true_wts, na.rm = TRUE)
  store_df$popn_val[i] <- sum(mat_condition[cond, 2:4] * popn_demog)
  store_df$est_gender_male[i] <-
    sum((df_samp$y * df_samp$imp_wts)[df_samp$gender == "m"], na.rm = TRUE) /
    sum(df_samp$imp_wts[df_samp$gender == "m"], na.rm = TRUE)
  store_df$est_gender_female[i] <-
    sum((df_samp$y * df_samp$imp_wts)[df_samp$gender == "f"], na.rm = TRUE) /
    sum(df_samp$imp_wts[df_samp$gender == "f"], na.rm = TRUE)
  store_df$est_gender_other[i] <-
    sum((df_samp$y * df_samp$imp_wts)[df_samp$gender == "o"], na.rm = TRUE) /
    sum(df_samp$imp_wts[df_samp$gender == "o"], na.rm = TRUE)
  store_df$popn_val_male[i] <- mat_condition[cond, 2]
  store_df$popn_val_female[i] <- mat_condition[cond, 3]
  store_df$popn_val_other[i] <- mat_condition[cond, 4]
  print(i)``
}

summary_df <- store_df %>% 
  group_by(prop_resp_male, cond, method, impute)%>%
  summarise(bias_imp_mean = mean((popn_val - est_imp)^2,na.rm = TRUE),bias_imp_low = quantile((popn_val - est_imp)^2,probs = .10),bias_imp_up = quantile((popn_val - est_imp)^2,probs = .90),
            bias_gender_male_mean = mean((popn_val_male - est_gender_male)^2,na.rm = TRUE),bias_gender_male_low = quantile((popn_val_male - est_gender_male)^2,probs = .10,na.rm = TRUE),bias_gender_male_up = quantile((popn_val_male - est_gender_male)^2,probs = .90,na.rm = TRUE),
            bias_gender_female_mean = mean((popn_val_female - est_gender_female)^2,na.rm = TRUE),bias_gender_female_low = quantile((popn_val_female - est_gender_female)^2,probs = .10,na.rm = TRUE),bias_gender_female_up = quantile((popn_val_female - est_gender_female)^2,probs = .90,na.rm = TRUE),
            bias_gender_other_mean = mean((popn_val_other - est_gender_other)^2,na.rm = TRUE),bias_gender_other_low = quantile((popn_val_other - est_gender_other)^2,probs = .10,na.rm = TRUE),bias_gender_other_up = quantile((popn_val_other - est_gender_other)^2,probs = .90,na.rm = TRUE),
            bias_sex_male_mean = mean((popn_val_male - est_sex_male)^2,na.rm = TRUE),bias_sex_male_low = quantile((popn_val_male - est_sex_male)^2,probs = .10,na.rm = TRUE),bias_sex_male_up = quantile((popn_val_male - est_sex_male)^2,probs = .90,na.rm = TRUE),
            bias_sex_female_mean = mean((popn_val_female - est_sex_female)^2,na.rm = TRUE),bias_sex_female_low = quantile((popn_val_female - est_sex_female)^2, probs = .10,na.rm = TRUE),bias_sex_female_up = quantile((popn_val_female - est_sex_female)^2, probs = .90,na.rm = TRUE))%>%
  unite("method_type",method, impute, sep = "_") 

summary_df$method_type <- as.factor(summary_df$method_type)

summary_df$cond <- factor(summary_df$cond, labels = c("No gender differences","Male, female same, other different","Female, other same, male different", "All different"))


cols <- c("best model estimate_sample" = "darkblue", 
          "best model estimate_popn" = "blue", 
          "popn prop_sample" = "darkgreen", 
          "popn prop_popn" = "green",
          "bad model estimate_sample" = "red",
          "remove_sample" = "orange",
          "impute female_sample" = "magenta")
col_labels <- c("best model estimate_sample" = "Estimate sex in sample, best model", 
          "best model estimate_popn" = "Estimate gender in popn, best model", 
          "popn prop_sample" = "Estimate sex in sample, by popn prop", 
          "popn prop_popn"  = "Estimate gender in popn, by popn prop",
          "bad model estimate_sample" = "Estimate sex in sample, worst model",
          "remove_sample" =  "Remove those who respond other",
          "impute female_sample" =  "Estimate female sex for those who respond other")

ggplot(summary_df, aes(x=prop_resp_male,y=bias_imp_mean, colour = method_type, fill = method_type))+
 geom_ribbon(aes(ymin=bias_imp_low,ymax=bias_imp_up), alpha = .1, colour =NA)+
  geom_line()+
  ylab("Log mean square error")+ xlab("Proportion of non-binary respondents who respond male on census binary question")+
  facet_wrap(.~cond,nrow=2)+
  scale_y_log10()+
  scale_fill_manual(values = cols,
                      labels=col_labels)+
  scale_colour_manual(values = cols,
                      labels=col_labels)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  guides(fill=guide_legend(nrow=4,byrow=TRUE))

ggsave('Figures/popn_est_1.png')

summary_df_long <- summary_df %>%
  select(-bias_imp_mean,-bias_imp_low,-bias_imp_up) %>%
  pivot_longer(cols = bias_gender_male_mean:bias_sex_female_up,
               names_to = c("bias","type","group","stat"),
               names_pattern = "(.*)[:punct:](.*)[:punct:](.*)[:punct:](.*)",
               values_to = "est")%>%
  select(-bias) %>%
  pivot_wider(names_from = "stat",
              values_from = "est") %>%
  filter(type=="sex")%>%
  rename(bias_mse = "mean", 
         bias_mse_low ="low", 
         bias_mse_up ="up")

to_sex <- as_labeller(c(`female` = "Female Sex", `male` = "Male Sex"))
to_gender <- as_labeller(c(`female` = "Female Gender", `male` = "Male Gender", `other` = "Other Gender"))

ggplot(summary_df_long, aes(x=prop_resp_male,y=bias_mse,
                            colour = method_type, fill = method_type))+
  geom_ribbon(aes(ymin=bias_mse_low,ymax=bias_mse_up), alpha = .1, colour =NA)+
  geom_line()+
  ylab("Log mean square error")+ xlab("Proportion of non-binary respondents who respond male on census binary question")+
  facet_grid(cond~group, labeller = labeller(group = to_sex))+
  scale_fill_manual(values = cols,
                       labels=col_labels)+
  scale_colour_manual(values = cols,
                      labels=col_labels)+
  scale_y_log10()+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  guides(fill=guide_legend(nrow=4,byrow=TRUE))

ggsave('Figures/sex_est_1.png')

summary_df_long_gender <- summary_df %>%
  select(-bias_imp_mean,-bias_imp_low,-bias_imp_up) %>%
  pivot_longer(cols = bias_gender_male_mean:bias_sex_female_up,
               names_to = c("bias","type","group","stat"),
               names_pattern = "(.*)[:punct:](.*)[:punct:](.*)[:punct:](.*)",
               values_to = "est")%>%
  select(-bias) %>%
  pivot_wider(names_from = "stat",
              values_from = "est") %>%
  filter(type=="gender")%>%
  rename(bias_mse = "mean", 
         bias_mse_low ="low", 
         bias_mse_up ="up")

ggplot(summary_df_long_gender, aes(x=prop_resp_male,y=bias_mse,
                            colour = method_type, fill = method_type))+
  geom_ribbon(aes(ymin=bias_mse_low,ymax=bias_mse_up), alpha = .1, colour =NA)+
  geom_line()+
  ylab("Log mean square error")+ xlab("Proportion of non-binary respondents who respond male on census binary question")+
  facet_grid(cond~group, labeller = labeller(group = to_gender))+
  scale_fill_manual(values = cols,
                    labels=col_labels)+
  scale_colour_manual(values = cols,
                      labels=col_labels)+
  scale_y_log10()+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  guides(fill=guide_legend(nrow=4,byrow=TRUE))

ggsave('Figures/gender_est_1.png')

