library(tidyverse)
library(survey)
library(ggplot2)

impute_sex <- function (x,method,p = NA){
  if(method == "random"){
    sex = ifelse(x %in% c('m','f'),c('f','m')[x],
                 sample(c('m','f'),size = sum(x %in% 'o'),replace=TRUE,prob=c(.5,.5)))
  } else if(method == "impute female"){
    sex = ifelse(x %in% c('m','f'),c('f','m')[x],'f')
  } else if(method == "worst model estimate"){
    sex = ifelse(x %in% c('m','f'),c('f','m')[x],
                 sample(c('f','m'),size = sum(x %in% 'o'),replace=TRUE,prob=p))
  } else if(method == "best model estimate"){
    sex = ifelse(x %in% c('m','f'),c('f','m')[x],
                 sample(c('f','m'),size = sum(x %in% 'o'),replace=TRUE,prob=c(p[2],p[1])))
  }
  sex <- factor(sex, level = c('m','f'))
  return(sex)
}

mat_condition = as.matrix(data.frame(cond = 1:4,
                          y_mean_m = c(0,50,50,50),
                          y_mean_f = c(0,50, 0, -50),
                          y_mean_o = c(0, 0, 0, 0)))
rownames(mat_condition) <- c("none","m,f same","f,o same", "all diff")

popn_demog <- c(.4,.4,.2)
nrep = 100
nsamp = 5000

df_samp <- data.frame(gender = sample(c('m','f','o'), size=nsamp,replace = TRUE, prob = popn_demog))
df_samp$gender <- factor(df_samp$gender, level = c('m','f','o'))



store_df <- expand.grid(cond = 1:4, reps=1:nrep, method = c("random","impute female","best model estimate","worst model estimate"), prop_resp_male = seq(0,1,.01),bias=NA)
for(i in 1:nrow(store_df)){
    cond <- store_df$cond[i]
    df_samp$y <- rnorm(nsamp,mat_condition[cond,as.numeric(df_samp$gender)+1],1)
    response_pattern_census <- c(popn_demog[1] + (1-store_df$prop_resp_male[i])*popn_demog[3],popn_demog[2] + store_df$prop_resp_male[i]*popn_demog[3])
    df_samp$imp_sex <- impute_sex(df_samp$gender, method = store_df$method[i], p = response_pattern_census)
    df_samp$wts <- matrix(prop.table(table(df_samp$imp_sex))/response_pattern_census)[as.numeric(df_samp$imp_sex),1]
    bias <- (sum(df_samp$wts*df_samp$y)/sum(df_samp$wts) - sum(mat_condition[cond,2:4]*popn_demog))^2
    store_df$bias_male[i] <- (sum(df_samp$wts[df_samp$imp_sex=='m']*df_samp$y[df_samp$imp_sex=='m'])/sum(df_samp$wts[df_samp$imp_sex=='m']) - mat_condition[cond,2])^2
    store_df$bias_female[i] <- (sum(df_samp$wts[df_samp$imp_sex=='f']*df_samp$y[df_samp$imp_sex=='f'])/sum(df_samp$wts[df_samp$imp_sex=='f']) - mat_condition[cond,3])^2
    store_df$bias[i] <- bias
    print(i)
}

summary_df <- store_df %>% 
  group_by(prop_resp_male, cond, method)%>%
  summarise(bias_med = mean(bias),bias_low = quantile(bias,.10),bias_up = quantile(bias,.90),
            bias_male_med = mean(bias_male),bias_male_low = quantile(bias_male,.10),bias_male_up = quantile(bias_male,.90),
            bias_female_med = mean(bias_female),bias_female_low = quantile(bias_female,.10),bias_female_up = quantile(bias_female,.90))

summary_df$cond <- factor(summary_df$cond, labels = c("No gender differences","Male, female same, other different","Female, other same, male different", "All different"))
summary_df$method <- factor(summary_df$method, labels = c("Random","Impute Female","Best case model estimate","Worst case model estimate"))

ggplot(summary_df, aes(x=prop_resp_male,y=bias_med, colour = method, fill = method))+
  geom_ribbon(aes(ymin=bias_low,ymax=bias_up), alpha = .3, colour =NA)+
  geom_line()+
  ylab("Mean square difference")+ xlab("Proportion of non-binary respondents who respond male on census binary question")+
  facet_wrap(.~cond,nrow=2)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())


ggplot(summary_df, aes(x=prop_resp_male,y=bias_female_med, colour = method, fill = method))+
  geom_ribbon(aes(ymin=bias_female_low,ymax=bias_female_up), alpha = .3, colour =NA)+
  geom_line()+ylab("Mean square difference")+xlab("Proportion of non-binary respondents who respond male on census binary question")+
  facet_wrap(.~cond,nrow=2)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggplot(summary_df, aes(x=prop_resp_male,y=bias_male_med, colour = method, fill = method))+
  geom_ribbon(aes(ymin=bias_male_low,ymax=bias_male_up), alpha = .3, colour =NA)+
  geom_line()+ylab("Mean square difference")+xlab("Proportion of non-binary respondents who respond male on census binary question")+
  facet_wrap(.~cond,nrow=2)+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())

