# setup ----------------------------------------------------------------
library(survey)
library(foreign)
library(dplyr)
library(RHhelpers)

rm(list = ls())
set.seed(210)
source("trim_wts.R")

# raise limit on the number of nested expressions that can be
# evaluated (default is 5000) 
options(expressions = 10000)

# function to produce file paths for saving the weights later
save_weights_path <- function(filename, extension = ".rda") {
  file.path("RH4/weight/", paste0(filename, "-", Sys.Date(), extension))
}


# Data ---------------------------------------------------------------

# prepared RH4 survey data
RH4 <- readRDS("RH4/data/RH4-data-for-weights.rds")
# drop Other from Poverty Tracker data
RH4 <- subset(RH4, female != 3)
RH4_samp_size <- nrow(RH4)

# Baseline weights --------------------------------------------------------

# Adjustment for number of adults ------------------------------------------
# 
# We adjust the SRBI sample for selection bias due to the number of adults in
# the household and the family. The larger the household, the smaller the
# selection probability is for each individual, so we need to upweight them.
# However, at the same time, the larger the family, the larger the response
# probability, so we need to downweight larger families.
# 
# We calculate the square root of the number of adults in the household 
# (adultxhh) divided by the number of adults in the family (adultx). Gelman and 
# Little (1998) recommend square roots for this weighting adjustment because 
# inverse probability weights for household sizes tend to overcorrect in 
# telephone surveys.

# weights adjusting for household selection (srbi sample only)
hh_adj <- with(RH4, sqrt(adultxhh / adultx))

# Adjustment for dual phone service in SRBI sample  ------------------------
#
# Due to the overlap in the landline and cellular RDD frames, we construct a 
# frame integration weight (Lohr, 2009) to combine the landline and cellular 
# sample components. The dual service respondents from the two frames are 
# integrated in proportion to their effective sample sizes.
# 
# We first filter on the dual service cases in the landline RDD sample and 
# compute the coefficient of variation (cv) of the final screener base weight. 
# The design effect for these cases is approximated as 1 + cv^2 and then the 
# effective sample size is computed as the unweighted sample size divided by the
# design effect. The effective sample size for the dual service cases in the 
# cellular RDD sample is computed in an analogous way.
# 
# The frame integration weights for the landline phone samples are defined as 
# the ratio of the effective number of dual service landline cases to the total 
# effective number of dual service landline and cellular cases; then the frame 
# integration weights for the cell phone samples are defined as the ratio of the
# effective number of dual service cell cases to the total effective number of 
# dual service landline and cellphone cases. Landline-only and cell-phone-only 
# cases will be assigned a frame integration weight of 1. This adjustment 
# assumes that the dual service households from each of the two samples are 
# random samples from the population of dual service households. So far the 
# weights are treated as household weights.

# if landline and interrupted service give weight of weight of 2 (code as
# 1/2 and divide later)
interrupted_landline <- with(RH4, cellphone == 0 & utilities_cut_off == 1)
cp_l <- ones(RH4_samp_size)
cp_l[interrupted_landline] <- 1/2

# for respondents with multiple phones give weight of 1/2 (code as 2 and divide
# later)
cell_phone_sample <- with(RH4, cellphone == 1)
cp_c <- ones(RH4_samp_size)
cp_c[cell_phone_sample] <- RH4$num_cellphones[cell_phone_sample]
cp_c <- cap(cp_c, at = 2)

# weights adjusting for the higher or lower probability of telephone
# availability.
basewt_1 <- hh_adj * (1 / cp_l) * (1 / cp_c)

# design effect for landline sample
also_has_cell <- with(RH4, landline_has_cellphone == 1 & !is.na(landline_has_cellphone))
cv_l <- cv(basewt_1[also_has_cell])
d_eff_l <- 1 + cv_l^2

#	design effect for cellular sample
also_has_landline <- with(RH4, cellphone_has_landline == 1 & !is.na(cellphone_has_landline))
cv_c <- cv(basewt_1[also_has_landline])
d_eff_c <- 1 + cv_c^2

# effective sample sizes
ESS_landline <- sum(also_has_cell, na.rm = TRUE) / d_eff_l
ESS_cell <- sum(also_has_landline, na.rm = TRUE) / d_eff_c
ESS_total <- ESS_landline + ESS_cell

# frame integration weights
# for respondents in landline sample who also have cell phone in hh
# and respondents in cellphone sample who also have landline in hh
fintw <- ones(RH4_samp_size)
fintw[also_has_cell] <- ESS_landline / ESS_total
fintw[also_has_landline] <- ESS_cell / ESS_total
basewt_1a <- basewt_1 * fintw

cat("summary of weights adjusting for selection for SRBI")
print(summary(basewt_1a))
hist(basewt_1a)

# Normalization to mean 1 for  srbi -----------------------------
basewt_2 <- basewt_1a
basewt_2 <- normalize(basewt_2)
basewt_hh <- as.vector(basewt_2)

cat('Summary of normalized weights')
print(summary(basewt_hh))
par(mfrow = c(1, 1))
hist(basewt_hh, main = "HH weights")

# Person weights ----------------------------------------------------------
# Multiply household weights by the number of adults in the household (capped at
# 4 due to sparseness at larger values) to obtain the personal weights.
n_adults_hh <- cap(RH4$adultxhh, at = 4)
basewt_p <- basewt_hh * n_adults_hh
cat('summary of personal weights')
print(summary(basewt_p))

#Save intermediate design weights
designwts_path <- save_weights_path(paste0("design-weights-RH4"), extension = ".dta")
write.dta(data.frame(analytic_ID = RH4$analytic_ID,i_qweight_p = basewt_p,i_qweight_pu=basewt_hh),file = designwts_path)

RH4$basewt_p <- basewt_p
RH4$basewt_hh <- basewt_hh

# Raking -----------------------------------------------------------------
#
# We use iterative post-stratification to match marginal distributions of the 
# sample to population margins (from the ACS-NYC weighted totals) on key
# variables. Although the joint distribution by cross tabulation is available,
# we use raking under an independence assumption to control the variability due
# to small post-stratification cell sizes.

# ACS-NYC 2014/2015 survey data for 18+ individuals
acs_ad <- readRDS("RH4/data/acs_adults.rds")
acs_ad <- as.data.frame(acs_ad)

# First need to do some recoding
RH4 <- RH4 %>% mutate(
  age_group = cut_numeric_leftclosed(age, breaks = c(18, 35, 65, Inf)),
  gender = female, 
  wkswork2 = cut_numeric_leftclosed(52/12 * wkmoshd, c(0, 1, 14, 27, 40, 48, Inf)),
  workedx = wkswork2, #wkswork2 - 1, # makes first level 0 instead of 1
  eldx_c = cap(eldx, 2),
  immigrant=immigrant - 1, #need recode, sample[1,2], acs[0,1].
  childx_c = cap(childx, 3),
  wax_c = cap(wax, 2),
  personx_c = cap(personx, 4)
)

acs_ad <- acs_ad %>% mutate(
  age_group = cut_numeric_leftclosed(age, breaks = c(18, 35, 65, Inf)), 
  gender = sex,
  race = racex,
  eldx_c = cap(eldx, 2),
  childx_c = cap(childx, 3),
  immigrant=pimmigrant,
  wax_c = cap(wax, 2),
  personx_c = cap(personx, 4),
  workedx = recode(as.numeric(wkswork2), '7'= 6), #`6` = 5
  boro = qbor
)

# create the sample.margins argument to rake() (list to be filled in with
# formulas) and the population.margins argument for rake() (list to be filled in
# with data.frames)

# main effects
main_effects <- c("age_group", "gender", "race", "educat","immigrant","tenure","workedx", "eldx_c", "childx_c", "wax_c")

# use the weights provided by ACS for person level vars
acs_ds_ad_person <- svydesign(id = ~1, weights = ~perwt, data = acs_ad)
margins <-
  margins_for_rake(main_effects,
                   int_vars = NULL,
                   interact_with = NULL,
                   design = acs_ds_ad_person)

RH4_rake <- rake(
  design = svydesign(id = ~1, data = RH4, weights = ~basewt_p),
  sample.margins = margins$samp,
  population.margins = margins$pop,
  control = list(maxit = 5000)
)

RH4_trimmed <- trim_wts(weights(RH4_rake),.975)
RH4_basewt_p <- round(RH4_trimmed,0) #round(weights(RH4_trimmed), 0)
RH4_basewt_pu <- round(RH4_basewt_p / cap(RH4$adultx, at = 4), 0)

colnames(RH4)


# Replicate weights --------------------------------------------------------
#
# Because the weights adjust for unequal selection, undercoverage and 
# nonresponse, it is hard to define a single formula for the variance estimate. 
# We use Rao and Wu's n-1 bootstrap method implemented in the survey package 
# to obtain 50 replicate weights for each set of sampling weights, which can be 
# used to calculate the variance if desired.

# for person weights
reps_p <-
  svydesign(
    ids = ~ 1,
    weights = ~ RH4_basewt_p,
    data = RH4
  ) %>%
  as.svrepdesign(type = "subbootstrap") %>%
  weights(type = "analysis") %>%
  round() %>%
  as.data.frame()
colnames(reps_p) <- paste0("qweight_p_rep", 1:ncol(reps_p)) 

# for poverty unit weights
reps_pu <-
  svydesign(
    ids = ~ 1,
    weights = ~ RH4_basewt_pu,
    data = RH4
  ) %>%
  as.svrepdesign(type = "subbootstrap") %>%
  weights(type = "analysis") %>%
  round() %>%
  as.data.frame()
colnames(reps_pu) <- paste0("qweight_pu_rep", 1:ncol(reps_pu)) 

RH4 <-
  cbind(RH4,
        qweight_p = RH4_basewt_p,
        reps_p,
        qweight_pu = RH4_basewt_pu,
        reps_pu)

# Save weights in rds (R) and/or dta (Stata) format ---------------------------
dta_path <- save_weights_path(paste0("weights-RH4-nopovgap-drop-other-gender"), extension = ".dta")
rds_path <- save_weights_path(paste0("weights-RH4-nopovgap-drop-other-gender"), extension = ".rds")

save_weights <- function(data, format = c("rds","dta"),
                         file_name = paste0("weights.",format),
                         include_reps = TRUE) {
  
  ext <- match.arg(format)
  L <- nchar(file_name)
  file_end <- substr(file_name, L - nchar(ext) + 1, L)
  if (file_end != ext) {
    file_name <- paste0(file_name, ".", ext)
  }
  
  cnms <- colnames(data)
  weight_cols <- cnms[grep("weight_", cnms)]
  if (!include_reps) {
    rep_cols <- grep("rep", weight_cols)
    if (!identical(rep_cols, integer(0)))
      weight_cols <- weight_cols[-rep_cols]
  }
  weights <- subset(data, sel = c("analytic_ID", weight_cols))
  weights <- dplyr::arrange(weights, analytic_ID)
  cat(paste0("Writing file '", file_name, "'"))
  
  if (format == "rds") {
    attr(weights, "date") <- Sys.Date()
    saveRDS(weights, file = file_name)
  } else {
    foreign::write.dta(weights, file = file_name)
  }
}

save_weights(data = RH4, format = "dta", include_reps = TRUE,
             file_name = dta_path)
save_weights(data = RH4, format = "rds", include_reps = TRUE,
             file_name = rds_path)

acs_ad_2020 <- readRDS("RH4/data/acs_2020_adults.rds")

# (Optionally) rescale weights so they sum to population
NYC_adult_pop_2020 <- sum(acs_ad_2020$perwt[acs_ad_2020$age>17]) # from ACS
NYC_num_pu_2020 <- sum(acs_ad_2020$hhwt[acs_ad_2020$relate2%in% 1]) # from ACS

wts <- 
  readRDS(rds_path) %>%
  mutate_at(
    .vars = vars(ends_with("weight_p")), 
    .funs = funs(rescale_weights_to_pop(., pop_size = NYC_adult_pop_2020))
  ) %>% 
  mutate_at(
    .vars = vars(ends_with("weight_pu")), 
    .funs = funs(rescale_weights_to_pop(., pop_size = NYC_num_pu_2020))
  )

save_weights(wts, format = "dta", include_reps = TRUE,
             file_name = dta_path)


# Generate weighted estimates
library(readstata13)
RH4 <- read.dta13("RH4/data/RH4_Data_2020.dta",convert.factor = FALSE)
RH4_wts <- read.dta13("RH4/weight/weights-RH4-nopovgap-drop-other-gender-2022-03-07.dta", convert.factors = FALSE)
RH4_file <- merge(RH4, RH4_wts, by = "analytic_ID")
RH4_file$imp_plimit[RH4_file$imp_plimit == 2] <- 0
design <- svydesign(id = ~1, weights = ~qweight_p, data = RH4_file)
svymean(~qspmpov, design)
svyby(~qspmpov, ~imp_pfemale, design, svymean)
svymean(~qsevhard, design)
svyby(~qsevhard, ~imp_pfemale, design, svymean)
svymean(~imp_plimit, design)
svyby(~imp_plimit, ~imp_pfemale, design, svymean)