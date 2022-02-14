library(tidyverse)
library(viridis)
library(survey)
library(ggplot2)
library(brms)

dir_cluster <- '/mnt/lustre/projects/Mona0070/lkennedy/gender_measure'
slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')

# coerce the value to an integer
iter <- as.numeric(slurm_arrayid)

options(brms.backend = 'cmdstanr')

impute_sex <- function (x,method,p = NA, true_x = NA){
  if(method == "popn prop"){
    sex = ifelse(x %in% c('m','f'),c('m','f')[x],
                 sample(c('m','f'),size = sum(x %in% 'nb'),replace=TRUE,prob=c(.5,.5)))
  } 
  sex <- factor(sex, level = c('m','f'))
  return(sex)
}

impute_gender <- function (x,method,p = NA){
  if(method == "popn prop"){
    replace_sex = sample.int(length(x),size = round(.02*length(x)))
    gender <- as.character(x)
    gender[replace_sex] <- "nb"
    gender <- as.factor(gender)
  } else if(method == "best model estimate"){
    gender = true_x
  } 
  return(gender)
}

mat_condition = as.matrix(data.frame(cond = 1:4,
                                     y_mean_m = c(0,10,10,10),
                                     y_mean_f = c(0,10, 0, -10),
                                     y_mean_o = c(0, 0, 0, 0)))
rownames(mat_condition) <- c("none","m,f same","f,nb same", "all diff")

popn_demog <- c(.49,.49,.02)
nrep = 1
nsamp = 500
npopn = 10000

set.seed(12934)
seed_number<-sample.int(50000,500,replace=FALSE) 

conditions_df <- expand.grid(cond = 1:4, reps=1:nrep, method = c("Impute Sex - Model","Impute Sex - Female","Impute Sex - Popn Demographics","Impute Gender - Model" ,"Impute Gender - Popn Demographics", "Remove NB"), prop_resp_male = c(0,.5,1),#seq(0,1,.01),
                        nb_representation = c("over","under"))

final_df <- data.frame(matrix(ncol = 30,nrow = 0))
  
  
colnames(final_df) =c("estimate","mean_y","bias_y","ci_width_y","mean_y_sex_m","bias_y_sex_m","ci_width_y_sex_m","mean_y_sex_f" ,"bias_y_sex_f","ci_width_y_sex_f","mean_y_gender_m",
                       "bias_y_gender_m","ci_width_y_gender_m","mean_y_gender_f","bias_y_gender_f","ci_width_y_gender_f","mean_y_gender_nb","bias_y_gender_nb","ci_width_y_gender_nb","cond","reps","method","prop_resp_male","nb_representation",
                      "cover_y","cover_sex_m","cover_sex_f","cover_gender_m","cover_gender_f","cover_gender_nb")

for (i in 1:nrow(conditions_df)) {
  set.seed(seed_number[iter])
  print(conditions_df[i,])
  #create a population with  gender according to population demographics
  df_popn <-
    data.frame(gender = sample(
      c('m', 'f', 'nb'),
      size = npopn,
      replace = TRUE,
      prob = popn_demog
    ), 
    age_group = sample(
      c('young', 'middle', 'older'),
      size = npopn,
      replace = TRUE,
      prob = c(.25,.5,.25)
    ),
    educat = sample(
      c('hs', 'college', 'college+'),
      size = npopn,
      replace = TRUE,
      prob = c(.25,.5,.25)
    ))
  df_popn$gender <- factor(df_popn$gender, level = c('m', 'f', 'nb'))
  df_popn$educat <- factor(df_popn$educat)
  df_popn$age_group <- factor(df_popn$age_group)
  
  #produce a continuous outcome y for every respondent based on gender and condition
  cond <- conditions_df$cond[i]
  df_popn$y <-
    rnorm(npopn, mat_condition[cond, as.numeric(df_popn$gender) + 1]+ 
            c(-2,0,2)[df_popn$age_group] +
            c(-2.3,-.1,1)[df_popn$age_group], 4)
  #How would each respondent have answered sex on the census
  #Assumes some respondents will answer gender = f and sex = m (and vice versa)
  true_sex_answer <- as.character(nrow(df_popn))
  for(j in 1:nrow(df_popn)){
    true_sex_answer[j] <- ifelse(
      df_popn$gender[j] == "m",
      sample(
        c('m', 'f'),
        size = 1,
        replace = TRUE,
        prob = c(1 - .01, .01)
      ),
      ifelse(
        df_popn$gender[j] == "f",
        sample(
          c('m', 'f'),
          1,
          replace = TRUE,
          prob = c(.01, 1 - .01)
        ),
        ifelse(
          conditions_df$prop_resp_male[i] == 1,
          'm',
          ifelse(
            conditions_df$prop_resp_male[i] == 0,
            'f',
            sample(
              c('m', 'f'),
              replace = TRUE,1,
              prob = c(conditions_df$prop_resp_male[i], 1 - conditions_df$prop_resp_male[i])
            )
          )
        )
      )
    )
  }
  df_popn$true_sex_answer <-factor(true_sex_answer, levels = c("m", "f"))
  
  if(conditions_df$nb_representation[i]=="over"){
    p_response = c(.1,.1,.8) #male, female, nb
  }else{
    p_response = c(.45,.45,.1) #male, female, nb
  }
  df_popn$prob_sample <- inv_logit_scaled(p_response[df_popn$gender]+
                                                  rnorm(3,0,.5)[df_popn$age_group]+
                                                  rnorm(3,0,.5)[df_popn$educat])
  df_samp_n <- sample.int(nrow(df_popn),nsamp,replace=FALSE, prob = df_popn$prob_sample)
  df_samp <- df_popn[df_samp_n,]
  
  df_secondary <- df_popn %>%
    slice_sample(n=1000)
  
  if(grepl("Impute Sex",conditions_df$method[i])){
    if(conditions_df$method[i] == "Impute Sex - Popn Demographics"){
      df_samp$imp_sex <-
        impute_sex(
          df_samp$gender,
          method = "popn prop",
          p = c(.5, .5),
          true_x = df_samp$true_sex_answer
        )
      df_popn$imp_sex <- df_popn$true_sex_answer
    }
    if(conditions_df$method[i] == "Impute Sex - Model"){
      prior_sex = c(prior_string("normal(0,.1)", class = "sd"),
                prior(normal(0,1), class = Intercept))
      sex_model <- brm(true_sex_answer ~ (1|gender) +(1|educat) + (1|age_group), data = df_secondary,
                             family = bernoulli(link = "logit"),
                       prior=prior_sex)
      df_samp$imp_sex <- factor(c("m","f")[t(posterior_predict(sex_model, newdata = df_samp, ndraws = 1,allow_new_levels = TRUE, sample_new_levels = "gaussian"))+1])
      df_popn$imp_sex <- df_popn$true_sex_answer
    }
    if(conditions_df$method[i] == "Impute Sex - Female"){
      df_samp$imp_sex <-df_samp$gender
      df_samp$imp_sex[df_samp$imp_sex=="nb"]<-"f"
      df_popn$imp_sex <- df_popn$true_sex_answer
    }
    #create and save weighted estimates
    popn_freq <- list(as.data.frame(svytable(~imp_sex, design = svydesign(ids=~1, data=df_popn))),
                      as.data.frame(svytable(~educat, design = svydesign(ids=~1, data=df_popn))),
                      as.data.frame(svytable(~age_group, design = svydesign(ids=~1, data=df_popn))))
    df_svy_dsn <- svydesign(ids=~1,data = df_samp)
    df_svy_dsn_rep <- as.svrepdesign(df_svy_dsn)
    df_samp_rake <-rake(df_svy_dsn_rep, list(~imp_sex,~educat,~age_group),
                        popn_freq)
    
    popn_freq <- as.data.frame(svytable(~imp_sex, design = svydesign(ids=~1, data=df_popn)))
    df_svy_dsn <- svydesign(ids=~1,data = df_samp)
    df_svy_dsn_rep <- as.svrepdesign(df_svy_dsn)
    df_samp_rake <-rake(df_svy_dsn_rep, list(~imp_sex),
                        list(popn_freq))
    mean_y <- svymean(~y,df_samp_rake)
    mean_y_gender <- svyby(~y, ~gender, df_samp_rake, svymean)
    mean_y_sex <- svyby(~y, ~imp_sex, df_samp_rake, svymean)
    
    
    store_df <- data.frame(estimate = "wtd", mean_y = mean_y[1],
                  bias_y = mean(df_popn$y)-mean_y[1],
                  ci_width_y = 2*1.96*sqrt(attr(mean_y,"var")),
                  #sex estimates  
                  mean_y_sex_m = mean_y_sex[1,2],
                  bias_y_sex_m = mean(df_popn$y[df_popn$true_sex_answer =="m"])-mean_y_sex[1,2],
                  ci_width_y_sex_m = 2*1.96*mean_y_sex[1,3],
                    
                  mean_y_sex_f = mean_y_sex[2,2],
                  bias_y_sex_f = mean(df_popn$y[df_popn$true_sex_answer=="f"])-mean_y_sex[2,2],
                  ci_width_y_sex_f =2*1.96*mean_y_sex[2,3],
                  #gender estimates
                  mean_y_gender_m = mean_y_gender[1,2],
                  bias_y_gender_m = mean(df_popn$y[df_popn$gender=="m"])-mean_y_gender[1,2],
                  ci_width_y_gender_m = 2*1.96*mean_y_gender[1,3],
                    
                  mean_y_gender_f = mean_y_gender[2,2],
                  bias_y_gender_f = mean(df_popn$y[df_popn$gender=="f"])-mean_y_gender[2,2],
                  ci_width_y_gender_f = 2*1.96*mean_y_gender[2,3],
                    
                  mean_y_gender_nb = mean_y_gender[3,2],
                  bias_y_gender_nb = mean(df_popn$y[df_popn$gender=="nb"])-mean_y_gender[3,2],
                  ci_width_y_gender_nb = 2*1.96*mean_y_gender[3,3],
                  conditions_df[i,],
                  cover_y = mean(df_popn$y)> (mean_y[1]-1.96*sqrt(attr(mean_y,"var"))) & mean(df_popn$y) < (mean_y[1]+1.96*sqrt(attr(mean_y,"var"))),
                  cover_m_sex = mean(df_popn$y[df_popn$true_sex_answer=="m"]) > (mean_y_sex[1,2]-1.96*mean_y_sex[1,3]) &  mean(df_popn$y[df_popn$true_sex_answer=="m"]) < (mean_y_sex[1,2]+1.96*mean_y_sex[1,3]),
                  cover_f_sex = mean(df_popn$y[df_popn$true_sex_answer=="f"]) > (mean_y_sex[2,2]-1.96*mean_y_sex[2,3]) &  mean(df_popn$y[df_popn$true_sex_answer=="f"]) < (mean_y_sex[2,2]+1.96*mean_y_sex[2,3]),
                  cover_m_gender = mean(df_popn$y[df_popn$gender=="m"]) > (mean_y_gender[1,2]-1.96*mean_y_gender[1,3]) &  mean(df_popn$y[df_popn$gender=="m"]) < (mean_y_gender[1,2]+1.96*mean_y_gender[1,3]),
                  cover_f_gender = mean(df_popn$y[df_popn$gender=="f"]) > (mean_y_gender[2,2]-1.96*mean_y_gender[2,3]) &  mean(df_popn$y[df_popn$gender=="f"]) < (mean_y_gender[2,2]+1.96*mean_y_gender[2,3]),
                  cover_nb_gender = mean(df_popn$y[df_popn$gender=="nb"]) > (mean_y_gender[3,2]-1.96*mean_y_gender[3,3]) &  mean(df_popn$y[df_popn$gender=="nb"]) < (mean_y_gender[3,2]+1.96*mean_y_gender[3,3])
                  )
    
    
    #create and save MRP estimates
    prior_y = c(prior(normal(0,3), class = b),
                prior(normal(0,3), class = Intercept),
                prior(normal(0,3), class = sd),
                prior(normal(0,3), class = sigma))
    
    m_impute <- brm(y~imp_sex+ (1|age_group) + (1|educat), data = df_samp, prior = prior_y)
    
    df_popn_ps <- df_popn %>%
      group_by(gender,educat, age_group,imp_sex)%>%
      summarise(Nj = n())
    m_preds <- posterior_predict(m_impute, newdata = df_popn_ps,allow_new_levels = TRUE, sample_new_levels = "gaussian")
    posterior_y <- apply(m_preds,1,function(x){sum(x*df_popn_ps$Nj)/sum(df_popn_ps$Nj)})
    #gender
    posterior_y_gender_m <- apply(m_preds[,df_popn_ps$gender=="m"],1,function(x){sum(x*df_popn_ps$Nj[df_popn_ps$gender=="m"])/sum(df_popn_ps$Nj[df_popn_ps$gender=="m"])})
    posterior_y_gender_f <- apply(m_preds[,df_popn_ps$gender=="f"],1,function(x){sum(x*df_popn_ps$Nj[df_popn_ps$gender=="f"])/sum(df_popn_ps$Nj[df_popn_ps$gender=="f"])})
    posterior_y_gender_nb <- apply(m_preds[,df_popn_ps$gender=="nb"],1,function(x){sum(x*df_popn_ps$Nj[df_popn_ps$gender=="nb"])/sum(df_popn_ps$Nj[df_popn_ps$gender=="nb"])})
    #sex
    posterior_y_sex_m <- apply(m_preds[,df_popn_ps$imp_sex=="m"],1,function(x){sum(x*df_popn_ps$Nj[df_popn_ps$imp_sex=="m"])/sum(df_popn_ps$Nj[df_popn_ps$imp_sex=="m"])})
    posterior_y_sex_f <- apply(m_preds[,df_popn_ps$imp_sex=="f"],1,function(x){sum(x*df_popn_ps$Nj[df_popn_ps$imp_sex=="f"])/sum(df_popn_ps$Nj[df_popn_ps$imp_sex=="f"])})
    
    store_df <- rbind(store_df,
                      data.frame(estimate = "mrp", mean_y = median(posterior_y),
                                 bias_y = mean(df_popn$y)-median(posterior_y),
                                 ci_width_y = quantile(posterior_y,.975)-quantile(posterior_y,.025),
                                 #sex estimates  
                                 mean_y_sex_m = median(posterior_y_sex_m),
                                 bias_y_sex_m = mean(df_popn$y[df_popn$true_sex_answer=="m"])- median(posterior_y_sex_m),
                                 ci_width_y_sex_m = quantile(posterior_y_sex_m,.975)-quantile(posterior_y_sex_m,.025),
                                 
                                 mean_y_sex_f = median(posterior_y_sex_f),
                                 bias_y_sex_f = mean(df_popn$y[df_popn$true_sex_answer=="f"])- median(posterior_y_sex_f),
                                 ci_width_y_sex_f = quantile(posterior_y_sex_f,.975)-quantile(posterior_y_sex_f,.025),
                                 
                                 #gender estimates
                                 mean_y_gender_m = median(posterior_y_gender_m),
                                 bias_y_gender_m = mean(df_popn$y[df_popn$gender=="m"])- median(posterior_y_gender_m),
                                 ci_width_y_gender_m = quantile(posterior_y_gender_m,.975)-quantile(posterior_y_gender_m,.025),
                                 
                                 mean_y_gender_f = median(posterior_y_gender_f),
                                 bias_y_gender_f = mean(df_popn$y[df_popn$gender=="f"])- median(posterior_y_gender_f),
                                 ci_width_y_gender_f = quantile(posterior_y_gender_f,.975)-quantile(posterior_y_gender_f,.025),
                                 
                                 mean_y_gender_nb = median(posterior_y_gender_nb),
                                 bias_y_gender_nb = mean(df_popn$y[df_popn$gender=="nb"])- median(posterior_y_gender_nb),
                                 ci_width_y_gender_nb = quantile(posterior_y_gender_nb,.975)-quantile(posterior_y_gender_nb,.025),
                                 conditions_df[i,],
                                 cover_y = median(posterior_y)> quantile(posterior_y,.025) & median(posterior_y) < quantile(posterior_y,.975),
                                 cover_m_sex = mean(df_popn$y[df_popn$true_sex_answer=="m"]) > quantile(posterior_y_sex_m,.025) &  mean(df_popn$y[df_popn$true_sex_answer=="m"]) < quantile(posterior_y_sex_m,.975),
                                 cover_f_sex = mean(df_popn$y[df_popn$true_sex_answer=="f"]) > quantile(posterior_y_sex_f,.025) &  mean(df_popn$y[df_popn$true_sex_answer=="f"]) < quantile(posterior_y_sex_f,.975),
                                 cover_m_gender = mean(df_popn$y[df_popn$gender=="m"]) > quantile(posterior_y_gender_m,.025) &  mean(df_popn$y[df_popn$gender=="m"]) < (mean_y_gender[1,2]+1.96*mean_y_gender[1,3]),
                                 cover_f_gender = mean(df_popn$y[df_popn$gender=="f"]) > quantile(posterior_y_gender_f,.025)&  mean(df_popn$y[df_popn$gender=="f"]) < quantile(posterior_y_gender_f,.975),
                                 cover_nb_gender = mean(df_popn$y[df_popn$gender=="nb"]) > quantile(posterior_y_gender_nb,.025) &  mean(df_popn$y[df_popn$gender=="nb"]) < quantile(posterior_y_gender_nb,.975)
                                 )
                      )
  }
  else if(grepl("Impute Gender",conditions_df$method[i])){
    if(conditions_df$method[i] == "Impute Gender - Popn Demographics"){
      df_popn$imp_gender <-
        impute_gender(
          df_popn$true_sex_answer,
          method = "popn prop",
          p = c(.5, .5))
      df_samp$imp_gender <- df_samp$gender
      df_secondary$imp_gender <- df_secondary$gender
    }
    if(conditions_df$method[i] == "Impute Gender - Model"){
      prior_secondary = c(prior(normal(1,.5), class = Intercept),
                    prior(normal(0,.1), class = b),
                    prior(normal(0,.1), class = sd, dpar = mum),
                    prior(normal(0,.1), class = sd, dpar = muf))
      gender_model <- df_secondary %>%
        mutate(gender = fct_relevel(df_secondary$gender,"nb"))%>%
        brm(gender ~ true_sex_answer +(1|educat) + (1|age_group), data = .,
                       family = categorical(link = "logit"), prior=prior_secondary)
      df_popn$imp_gender <- factor(c("nb","m","f")[t(posterior_predict(gender_model, newdata = df_popn, ndraws = 1,allow_new_levels = TRUE, sample_new_levels = "gaussian"))])
      df_samp$imp_gender <- df_samp$gender
    }
    
    #create and save weighted estimates
    popn_freq <- list(as.data.frame(svytable(~imp_gender, design = svydesign(ids=~1, data=df_popn))),
                      as.data.frame(svytable(~educat, design = svydesign(ids=~1, data=df_popn))),
                      as.data.frame(svytable(~age_group, design = svydesign(ids=~1, data=df_popn))))
    df_svy_dsn <- svydesign(ids=~1,data = df_samp)
    df_svy_dsn_rep <- as.svrepdesign(df_svy_dsn)
    df_samp_rake <-rake(df_svy_dsn_rep, list(~imp_gender,~educat,~age_group),
                        popn_freq)
    mean_y <- svymean(~y,df_samp_rake)
    mean_y_gender <- svyby(~y, ~gender, df_samp_rake, svymean)
    mean_y_sex <- svyby(~y, ~true_sex_answer, df_samp_rake, svymean)
    
    
    store_df <- data.frame(estimate = "wtd", mean_y = mean_y[1],
                           bias_y = mean(df_popn$y)-mean_y[1],
                           ci_width_y = 2*1.96*sqrt(attr(mean_y,"var")),
                           #sex estimates  
                           mean_y_sex_m = mean_y_sex[1,2],
                           bias_y_sex_m = mean(df_popn$y[df_popn$true_sex_answer =="m"])-mean_y_sex[1,2],
                           ci_width_y_sex_m = 2*1.96*mean_y_sex[1,3],
                           
                           mean_y_sex_f = mean_y_sex[2,2],
                           bias_y_sex_f = mean(df_popn$y[df_popn$true_sex_answer=="f"])-mean_y_sex[2,2],
                           ci_width_y_sex_f =2*1.96*mean_y_sex[2,3],
                           #gender estimates
                           mean_y_gender_m = mean_y_gender[1,2],
                           bias_y_gender_m = mean(df_popn$y[df_popn$gender=="m"])-mean_y_gender[1,2],
                           ci_width_y_gender_m = 2*1.96*mean_y_gender[1,3],
                           
                           mean_y_gender_f = mean_y_gender[2,2],
                           bias_y_gender_f = mean(df_popn$y[df_popn$gender=="f"])-mean_y_gender[2,2],
                           ci_width_y_gender_f = 2*1.96*mean_y_gender[2,3],
                           
                           mean_y_gender_nb = mean_y_gender[3,2],
                           bias_y_gender_nb = mean(df_popn$y[df_popn$gender=="nb"])-mean_y_gender[3,2],
                           ci_width_y_gender_nb = 2*1.96*mean_y_gender[3,3],
                           conditions_df[i,], 
                           cover_y = mean(df_popn$y)> (mean_y[1]-1.96*sqrt(attr(mean_y,"var"))) & mean(df_popn$y) < (mean_y[1]+1.96*sqrt(attr(mean_y,"var"))),
                           cover_m_sex = mean(df_popn$y[df_popn$true_sex_answer=="m"]) > (mean_y_sex[1,2]-1.96*mean_y_sex[1,3]) &  mean(df_popn$y[df_popn$true_sex_answer=="m"]) < (mean_y_sex[1,2]+1.96*mean_y_sex[1,3]),
                           cover_f_sex = mean(df_popn$y[df_popn$true_sex_answer=="f"]) > (mean_y_sex[2,2]-1.96*mean_y_sex[2,3]) &  mean(df_popn$y[df_popn$true_sex_answer=="f"]) < (mean_y_sex[2,2]+1.96*mean_y_sex[2,3]),
                           cover_m_gender = mean(df_popn$y[df_popn$gender=="m"]) > (mean_y_gender[1,2]-1.96*mean_y_gender[1,3]) &  mean(df_popn$y[df_popn$gender=="m"]) < (mean_y_gender[1,2]+1.96*mean_y_gender[1,3]),
                           cover_f_gender = mean(df_popn$y[df_popn$gender=="f"]) > (mean_y_gender[2,2]-1.96*mean_y_gender[2,3]) &  mean(df_popn$y[df_popn$gender=="f"]) < (mean_y_gender[2,2]+1.96*mean_y_gender[2,3]),
                           cover_nb_gender = mean(df_popn$y[df_popn$gender=="nb"]) > (mean_y_gender[3,2]-1.96*mean_y_gender[3,3]) &  mean(df_popn$y[df_popn$gender=="nb"]) < (mean_y_gender[3,2]+1.96*mean_y_gender[3,3]))
    
    
    #create and save MRP estimates
    prior_y = c(prior(normal(0,3), class = Intercept),
                prior(normal(0,3), class = sd),
                prior(normal(0,3), class = sigma))
    
    m_impute <- brm(y~(1|imp_gender) + (1|age_group) + (1|educat), data = df_samp, prior = prior_y)
    
    df_popn_ps <- df_popn %>%
      group_by(imp_gender,educat, age_group,true_sex_answer)%>%
      summarise(Nj = n())
    m_preds <- posterior_predict(m_impute, newdata = df_popn_ps,allow_new_levels = TRUE, sample_new_levels = "gaussian")
    posterior_y <- apply(m_preds,1,function(x){sum(x*df_popn_ps$Nj)/sum(df_popn_ps$Nj)})
    #gender
    posterior_y_gender_m <- apply(m_preds[,df_popn_ps$imp_gender=="m"],1,function(x){sum(x*df_popn_ps$Nj[df_popn_ps$imp_gender=="m"])/sum(df_popn_ps$Nj[df_popn_ps$imp_gender=="m"])})
    posterior_y_gender_f <- apply(m_preds[,df_popn_ps$imp_gender=="f"],1,function(x){sum(x*df_popn_ps$Nj[df_popn_ps$imp_gender=="f"])/sum(df_popn_ps$Nj[df_popn_ps$imp_gender=="f"])})
    posterior_y_gender_nb <- apply(m_preds[,df_popn_ps$imp_gender=="nb"],1,function(x){sum(x*df_popn_ps$Nj[df_popn_ps$imp_gender=="nb"])/sum(df_popn_ps$Nj[df_popn_ps$imp_gender=="nb"])})
    #sex
    posterior_y_sex_m <- apply(m_preds[,df_popn_ps$true_sex_answer=="m"],1,function(x){sum(x*df_popn_ps$Nj[df_popn_ps$true_sex_answer=="m"])/sum(df_popn_ps$Nj[df_popn_ps$true_sex_answer=="m"])})
    posterior_y_sex_f <- apply(m_preds[,df_popn_ps$true_sex_answer=="f"],1,function(x){sum(x*df_popn_ps$Nj[df_popn_ps$true_sex_answer=="f"])/sum(df_popn_ps$Nj[df_popn_ps$true_sex_answer=="f"])})
    
    store_df <- rbind(store_df,
                      data.frame(estimate = "mrp", mean_y = median(posterior_y),
                                 bias_y = mean(df_popn$y)-median(posterior_y),
                                 ci_width_y = quantile(posterior_y,.975)-quantile(posterior_y,.025),
                                 #sex estimates  
                                 mean_y_sex_m = median(posterior_y_sex_m),
                                 bias_y_sex_m = mean(df_popn$y[df_popn$true_sex_answer=="m"])- median(posterior_y_sex_m),
                                 ci_width_y_sex_m = quantile(posterior_y_sex_m,.975)-quantile(posterior_y_sex_m,.025),
                                           
                                mean_y_sex_f = median(posterior_y_sex_f),
                                bias_y_sex_f = mean(df_popn$y[df_popn$true_sex_answer=="f"])- median(posterior_y_sex_f),
                                ci_width_y_sex_f = quantile(posterior_y_sex_f,.975)-quantile(posterior_y_sex_f,.025),
                                 
                                #gender estimates
                                mean_y_gender_m = median(posterior_y_gender_m),
                                bias_y_gender_m = mean(df_popn$y[df_popn$gender=="m"])- median(posterior_y_gender_m),
                                ci_width_y_gender_m = quantile(posterior_y_gender_m,.975)-quantile(posterior_y_gender_m,.025),
                                
                                mean_y_gender_f = median(posterior_y_gender_f),
                                bias_y_gender_f = mean(df_popn$y[df_popn$gender=="f"])- median(posterior_y_gender_f),
                                ci_width_y_gender_f = quantile(posterior_y_gender_f,.975)-quantile(posterior_y_gender_f,.025),
                                           
                                mean_y_gender_nb = median(posterior_y_gender_nb),
                                bias_y_gender_nb = mean(df_popn$y[df_popn$gender=="nb"])- median(posterior_y_gender_nb),
                                ci_width_y_gender_nb = quantile(posterior_y_gender_nb,.975)-quantile(posterior_y_gender_nb,.025),
                                conditions_df[i,],
                                cover_y = median(posterior_y)> quantile(posterior_y,.025) & median(posterior_y) < quantile(posterior_y,.975),
                                cover_m_sex = mean(df_popn$y[df_popn$true_sex_answer=="m"]) > quantile(posterior_y_sex_m,.025) &  mean(df_popn$y[df_popn$true_sex_answer=="m"]) < quantile(posterior_y_sex_m,.975),
                                cover_f_sex = mean(df_popn$y[df_popn$true_sex_answer=="f"]) > quantile(posterior_y_sex_f,.025) &  mean(df_popn$y[df_popn$true_sex_answer=="f"]) < quantile(posterior_y_sex_f,.975),
                                cover_m_gender = mean(df_popn$y[df_popn$gender=="m"]) > quantile(posterior_y_gender_m,.025) &  mean(df_popn$y[df_popn$gender=="m"]) < (mean_y_gender[1,2]+1.96*mean_y_gender[1,3]),
                                cover_f_gender = mean(df_popn$y[df_popn$gender=="f"]) > quantile(posterior_y_gender_f,.025)&  mean(df_popn$y[df_popn$gender=="f"]) < quantile(posterior_y_gender_f,.975),
                                cover_nb_gender = mean(df_popn$y[df_popn$gender=="nb"]) > quantile(posterior_y_gender_nb,.025) &  mean(df_popn$y[df_popn$gender=="nb"]) < quantile(posterior_y_gender_nb,.975)
                                )
                      )
  }
  else if(grepl("Remove NB",conditions_df$method[i])){
    df_samp$imp_sex <- df_samp$gender
    df_samp <- df_samp %>%
      filter(imp_sex != "nb")
    df_popn$imp_sex <- df_popn$true_sex_answer
    
    #create and save weighted estimates
    popn_freq <- list(as.data.frame(svytable(~imp_sex, design = svydesign(ids=~1, data=df_popn))),
                      as.data.frame(svytable(~educat, design = svydesign(ids=~1, data=df_popn))),
                      as.data.frame(svytable(~age_group, design = svydesign(ids=~1, data=df_popn))))
    df_svy_dsn <- svydesign(ids=~1,data = df_samp)
    df_svy_dsn_rep <- as.svrepdesign(df_svy_dsn)
    df_samp_rake <-rake(df_svy_dsn_rep, list(~imp_sex,~educat,~age_group),
                        popn_freq)
    
    popn_freq <- as.data.frame(svytable(~imp_sex, design = svydesign(ids=~1, data=df_popn)))
    df_svy_dsn <- svydesign(ids=~1,data = df_samp)
    df_svy_dsn_rep <- as.svrepdesign(df_svy_dsn)
    df_samp_rake <-rake(df_svy_dsn_rep, list(~imp_sex),
                        list(popn_freq))
    mean_y <- svymean(~y,df_samp_rake)
    mean_y_gender <- svyby(~y, ~gender, df_samp_rake, svymean)
    mean_y_sex <- svyby(~y, ~imp_sex, df_samp_rake, svymean)
    
    
    store_df <- data.frame(estimate = "wtd", mean_y = mean_y[1],
                           bias_y = mean(df_popn$y)-mean_y[1],
                           ci_width_y = 2*1.96*sqrt(attr(mean_y,"var")),
                           #sex estimates  
                           mean_y_sex_m = mean_y_sex[1,2],
                           bias_y_sex_m = mean(df_popn$y[df_popn$true_sex_answer =="m"])-mean_y_sex[1,2],
                           ci_width_y_sex_m = 2*1.96*mean_y_sex[1,3],
                           
                           mean_y_sex_f = mean_y_sex[2,2],
                           bias_y_sex_f = mean(df_popn$y[df_popn$true_sex_answer=="f"])-mean_y_sex[2,2],
                           ci_width_y_sex_f =2*1.96*mean_y_sex[2,3],
                           #gender estimates
                           mean_y_gender_m = mean_y_gender[1,2],
                           bias_y_gender_m = mean(df_popn$y[df_popn$gender=="m"])-mean_y_gender[1,2],
                           ci_width_y_gender_m = 2*1.96*mean_y_gender[1,3],
                           
                           mean_y_gender_f = mean_y_gender[2,2],
                           bias_y_gender_f = mean(df_popn$y[df_popn$gender=="f"])-mean_y_gender[2,2],
                           ci_width_y_gender_f = 2*1.96*mean_y_gender[2,3],
                           
                           mean_y_gender_nb = mean_y_gender[3,2],
                           bias_y_gender_nb = mean(df_popn$y[df_popn$gender=="nb"])-mean_y_gender[3,2],
                           ci_width_y_gender_nb = 2*1.96*mean_y_gender[3,3],
                           conditions_df[i,],
                           cover_y = mean(df_popn$y)> (mean_y[1]-1.96*sqrt(attr(mean_y,"var"))) & mean(df_popn$y) < (mean_y[1]+1.96*sqrt(attr(mean_y,"var"))),
                           cover_m_sex = mean(df_popn$y[df_popn$true_sex_answer=="m"]) > (mean_y_sex[1,2]-1.96*mean_y_sex[1,3]) &  mean(df_popn$y[df_popn$true_sex_answer=="m"]) < (mean_y_sex[1,2]+1.96*mean_y_sex[1,3]),
                           cover_f_sex = mean(df_popn$y[df_popn$true_sex_answer=="f"]) > (mean_y_sex[2,2]-1.96*mean_y_sex[2,3]) &  mean(df_popn$y[df_popn$true_sex_answer=="f"]) < (mean_y_sex[2,2]+1.96*mean_y_sex[2,3]),
                           cover_m_gender = mean(df_popn$y[df_popn$gender=="m"]) > (mean_y_gender[1,2]-1.96*mean_y_gender[1,3]) &  mean(df_popn$y[df_popn$gender=="m"]) < (mean_y_gender[1,2]+1.96*mean_y_gender[1,3]),
                           cover_f_gender = mean(df_popn$y[df_popn$gender=="f"]) > (mean_y_gender[2,2]-1.96*mean_y_gender[2,3]) &  mean(df_popn$y[df_popn$gender=="f"]) < (mean_y_gender[2,2]+1.96*mean_y_gender[2,3]),
                           cover_nb_gender = mean(df_popn$y[df_popn$gender=="nb"]) > (mean_y_gender[3,2]-1.96*mean_y_gender[3,3]) &  mean(df_popn$y[df_popn$gender=="nb"]) < (mean_y_gender[3,2]+1.96*mean_y_gender[3,3]))
    
    
    #create and save MRP estimates
    
    #create and save MRP estimates
    prior_y = c(prior(normal(0,3), class = b),
                prior(normal(0,3), class = Intercept),
                prior(normal(0,3), class = sd),
                prior(normal(0,3), class = sigma))
    
    m_impute <- brm(y~imp_sex+ (1|age_group) + (1|educat), data = df_samp, prior = prior_y)
    df_popn_ps <- df_popn %>%
      group_by(gender,educat, age_group,imp_sex)%>%
      summarise(Nj = n())
    m_preds <- posterior_predict(m_impute, newdata = df_popn_ps,allow_new_levels = TRUE, sample_new_levels = "gaussian")
    posterior_y <- apply(m_preds,1,function(x){sum(x*df_popn_ps$Nj)/sum(df_popn_ps$Nj)})
    #gender
    posterior_y_gender_m <- apply(m_preds[,df_popn_ps$gender=="m"],1,function(x){sum(x*df_popn_ps$Nj[df_popn_ps$gender=="m"])/sum(df_popn_ps$Nj[df_popn_ps$gender=="m"])})
    posterior_y_gender_f <- apply(m_preds[,df_popn_ps$gender=="f"],1,function(x){sum(x*df_popn_ps$Nj[df_popn_ps$gender=="f"])/sum(df_popn_ps$Nj[df_popn_ps$gender=="f"])})
    #sex
    posterior_y_sex_m <- apply(m_preds[,df_popn_ps$imp_sex=="m"],1,function(x){sum(x*df_popn_ps$Nj[df_popn_ps$imp_sex=="m"])/sum(df_popn_ps$Nj[df_popn_ps$imp_sex=="m"])})
    posterior_y_sex_f <- apply(m_preds[,df_popn_ps$imp_sex=="f"],1,function(x){sum(x*df_popn_ps$Nj[df_popn_ps$imp_sex=="f"])/sum(df_popn_ps$Nj[df_popn_ps$imp_sex=="f"])})
    
    store_df <- rbind(store_df,
                      data.frame(estimate = "mrp", mean_y = median(posterior_y),
                                 bias_y = mean(df_popn$y)-median(posterior_y),
                                 ci_width_y = quantile(posterior_y,.975)-quantile(posterior_y,.025),
                                 #sex estimates  
                                 mean_y_sex_m = median(posterior_y_sex_m),
                                 bias_y_sex_m = mean(df_popn$y[df_popn$true_sex_answer=="m"])- median(posterior_y_sex_m),
                                 ci_width_y_sex_m = quantile(posterior_y_sex_m,.975)-quantile(posterior_y_sex_m,.025),
                                 
                                 mean_y_sex_f = median(posterior_y_sex_f),
                                 bias_y_sex_f = mean(df_popn$y[df_popn$true_sex_answer=="f"])- median(posterior_y_sex_f),
                                 ci_width_y_sex_f = quantile(posterior_y_sex_f,.975)-quantile(posterior_y_sex_f,.025),
                                 
                                 #gender estimates
                                 mean_y_gender_m = median(posterior_y_gender_m),
                                 bias_y_gender_m = mean(df_popn$y[df_popn$gender=="m"])- median(posterior_y_gender_m),
                                 ci_width_y_gender_m = quantile(posterior_y_gender_m,.975)-quantile(posterior_y_gender_m,.025),
                                 
                                 mean_y_gender_f = median(posterior_y_gender_f),
                                 bias_y_gender_f = mean(df_popn$y[df_popn$gender=="f"])- median(posterior_y_gender_f),
                                 ci_width_y_gender_f = quantile(posterior_y_gender_f,.975)-quantile(posterior_y_gender_f,.025),
                                 
                                 mean_y_gender_nb = NA,
                                 bias_y_gender_nb = NA,
                                 ci_width_y_gender_nb = NA,
                                 conditions_df[i,],
                                 cover_y = median(posterior_y)> quantile(posterior_y,.025) & median(posterior_y) < quantile(posterior_y,.975),
                                 cover_m_sex = mean(df_popn$y[df_popn$true_sex_answer=="m"]) > quantile(posterior_y_sex_m,.025) &  mean(df_popn$y[df_popn$true_sex_answer=="m"]) < quantile(posterior_y_sex_m,.975),
                                 cover_f_sex = mean(df_popn$y[df_popn$true_sex_answer=="f"]) > quantile(posterior_y_sex_f,.025) &  mean(df_popn$y[df_popn$true_sex_answer=="f"]) < quantile(posterior_y_sex_f,.975),
                                 cover_m_gender = mean(df_popn$y[df_popn$gender=="m"]) > quantile(posterior_y_gender_m,.025) &  mean(df_popn$y[df_popn$gender=="m"]) < (mean_y_gender[1,2]+1.96*mean_y_gender[1,3]),
                                 cover_f_gender = mean(df_popn$y[df_popn$gender=="f"]) > quantile(posterior_y_gender_f,.025)&  mean(df_popn$y[df_popn$gender=="f"]) < quantile(posterior_y_gender_f,.975),
                                 cover_nb_gender = mean(df_popn$y[df_popn$gender=="nb"]) > quantile(posterior_y_gender_nb,.025) &  mean(df_popn$y[df_popn$gender=="nb"]) < quantile(posterior_y_gender_nb,.975)
                                 ))
  }
  final_df <- rbind(final_df,store_df)

}


saveRDS(final_df,paste0("results/sim_results_biasvar_",iter,".rds"))

