library(tidyverse)
library(ggplot2)
library(ggthemes)
library(here)
library(viridis)

results_biasvar <- readRDS(here(paste0("results/sim_results_biasvar_",1,".rds"))) %>%
  mutate(iteration = 1)

for(iteration in c(2:100)) {
  tmp <- readRDS(here(paste0("results/sim_results_biasvar_",iteration,".rds"))) %>%
    mutate(iteration = iteration)
  results_biasvar <- rbind(tmp,results_biasvar)
}

results_biasvar %>%
  mutate(cond =  factor(cond, labels = c("No gender differences","Male, female same, non-binary different","Female, non-binary same, male different", "All different"))) %>%
  filter(estimate == "mrp")%>%
  select(bias_y, ci_width_y, cond, reps, prop_resp_male, nb_representation,method)%>%
  group_by(cond, prop_resp_male,nb_representation,method)%>%
  summarise(mean_bias = mean(bias_y^2),mean_ci_width = mean(ci_width_y))%>%
  ggplot(., aes(x=prop_resp_male, y = mean_bias, fill = method))+
  geom_col(position = position_dodge())+
  facet_grid(cond~nb_representation)+
  scale_fill_colorblind()+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  ggtitle("MRP")


results_biasvar %>%
  mutate(cond =  factor(cond, labels = c("No gender differences","Male, female same, non-binary different","Female, non-binary same, male different", "All different"))) %>%
  filter(estimate == "mrp", cond =="All different")%>%
  select(bias_y, ci_width_y, cond, reps, prop_resp_male, nb_representation,method)%>%
  group_by(cond, prop_resp_male,nb_representation,method)%>%
  summarise(mean_bias = mean(bias_y^2),mean_ci_width = mean(ci_width_y))%>%
  ggplot(., aes(x=method, y = mean_bias, fill = method))+
  geom_col(position = position_dodge())+
  facet_grid(prop_resp_male~nb_representation)+
  scale_fill_colorblind()+
  theme_bw()+
  coord_flip()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  ggtitle("MRP")


results_biasvar %>%
  mutate(cond =  factor(cond, labels = c("No gender differences","Male, female same, non-binary different","Female, non-binary same, male different", "All different"))) %>%
  filter(estimate == "wtd")%>%
  select(bias_y, ci_width_y, cond, reps, prop_resp_male, nb_representation,method)%>%
  group_by(cond, prop_resp_male,nb_representation,method)%>%
  summarise(mean_bias = mean(bias_y^2),mean_ci_width = mean(ci_width_y))%>%
  ggplot(., aes(x=prop_resp_male, y = mean_bias, fill = method))+
  geom_col(position = position_dodge())+
  facet_grid(cond~nb_representation)+
  scale_fill_colorblind()+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  ggtitle("Weighted")


results_biasvar %>%
  mutate(cond =  factor(cond, labels = c("No gender differences","Male, female same, non-binary different","Female, non-binary same, male different", "All different"))) %>%
  filter(estimate == "wtd", cond =="All different")%>%
  select(bias_y, ci_width_y, cond, reps, prop_resp_male, nb_representation,method)%>%
  group_by(cond, prop_resp_male,nb_representation,method)%>%
  summarise(mean_bias = mean(bias_y^2),mean_ci_width = mean(ci_width_y))%>%
  ggplot(., aes(x=method, y = mean_bias, fill = method))+
  geom_col(position = position_dodge())+
  facet_grid(prop_resp_male~nb_representation)+
  scale_fill_colorblind()+
  theme_bw()+
  coord_flip()+
  theme(legend.position = "bottom",
        legend.title = element_blank())+
  ggtitle("Weighted")


results_biasvar %>%
  mutate(cond =  factor(cond, labels = c("No gender differences","Male, female same, non-binary different","Female, non-binary same, male different", "All different"))) %>%
  filter(nb_representation =="under", cond =="All different")%>%
  select(bias_y, ci_width_y, cond, estimate,reps, prop_resp_male, nb_representation,method)%>%
  ggplot(., aes(x=method, y = bias_y, fill = method))+
  geom_boxplot()+
  facet_grid(prop_resp_male~estimate)+
  scale_fill_colorblind()+
  theme_bw()+
  coord_flip()+
  theme(legend.position = "bottom",
        legend.title = element_blank())



results_biasvar %>%
  mutate(cond =  factor(cond, labels = c("No gender differences","Male, female same, non-binary different","Female, non-binary same, male different", "All different"))) %>%
  filter(nb_representation =="under", cond =="All different")%>%
  select(bias_y, ci_width_y, cond, estimate,reps, prop_resp_male, nb_representation,method)%>%
  ggplot(., aes(x=method, y = ci_width_y, fill = method))+
  geom_boxplot()+
  facet_grid(prop_resp_male~estimate)+
  scale_fill_colorblind()+
  theme_bw()+
  coord_flip()+
  theme(legend.position = "bottom",
        legend.title = element_blank())
