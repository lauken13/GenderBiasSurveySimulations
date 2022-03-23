# setup
library(haven)
library(dplyr)
library(data.table)
library(RHhelpers)
library(foreign)
library(readstata13)

# Load 2020 ACS data and exclude people living in group housing (if there are 
# any still in the data). Then save data as separate adult and child datasets. 
clean_acs <- function(acs_data,name,wave) {
  acs_data <- mutate(acs_data,
                     age_group = cut_numeric_leftclosed(age, breaks = c(18, 35, 65, Inf)), 
                     race = racex,
                     opmres_x = lt_eq_gt(povgap, 3, c(0,3,4)),
                     eldx_c = cap(eldx, 2),
                     childx_c = cap(childx, 3),
                     immigrant=pimmigrant,
                     wax_c = cap(wax, 2),
                     personx_c = cap(personx, 4),
                     workedx = recode(as.numeric(wkswork2), `6` = 5),
                     immigrant = pimmigrant,
                     tenure = tenure,
                     spworkedx = recode(as.numeric(spwkswork2), `6` = 5)
  )
  col_names<-c('age_group','workedx','personx_c','wax_c','childx_c','eldx_c','povgap','opmres_x','spworkedx')
  acs_data$opmres_x[acs_data$opmres_x==0] <- acs_data$povgap[acs_data$opmres_x==0]
  acs_data$spworkedx[acs_data$sppart==0] <- 0
  if(wave !=''){
    setnames(acs_data, old=col_names, new=paste('q',wave,col_names,sep=''))
  }
  acs_pop <- subset(acs_data, !(gq %in% 3:4))
  acs_adults <- subset(acs_pop, age >= 18)
  acs_children <- subset(acs_pop, age < 18)
  saveRDS(acs_adults, file = paste0("RH4/data/",name,"_adults.rds"))
  saveRDS(acs_children, file = paste0("RH4/data/",name,"_children.rds"))
  return(acs_adults)
}

acs_2020 <- clean_acs(read_dta("acs data/acs_nyc_weights_prepared_2020.dta"),"acs_2020", "")

acs2020 <-
  read_dta("acs data/acs_nyc_weights_prepared_2020.dta") %>%
  filter(!(gq %in% 3:4))

# save adult and child datasets separately
acs2020 %>%
  filter(age >= 18) %>%
  saveRDS(file = "RH4/data/acs_adults.rds")
acs2020 %>%
  filter(age < 18) %>%
  saveRDS(file = "RH4/data/acs_children.rds")

# import Poverty Tracker survey data
dat <- read.dta13("RH4/data/RH4_Data_2020.dta",convert.factor = FALSE)
sum(is.na(dat$analytic_ID)) #If this is not zero double check.
dat <- dat[!is.na(dat$analytic_ID),]

# recoding and dropping what what we don't need
dat <- dat %>%
  arrange(analytic_ID) %>%
  transmute(
    # rename imputed variables
    educat = imp_peducat,
    age = imp_page,
    race = imp_prace,
    female = imp_pfemale,
    wkmoshd = imp_pmoswork,  # number of months worked
    spouse_wkmoshd = imp_psmoswork,
    immigrant = imp_pimmigrant,
    govhous = imp_pgovhous,
    ladder = imp_pladder, # life satisfaction
    limit = imp_plimit, # facing a work limiting health condition
    # k6 scale health variables
    health = imp_phealth,
    
    # phone variables
    cellphone = ifelse(imp_pcellphone >= 1, 1, 0), #in case coded as 1/2 instead of 0/1
    num_cellphones = imp_qi13n,
    num_landlines = imp_qi12n,
    landline_has_cellphone = imp_qi11, # part of landline sample but also has cellphone
    cellphone_has_landline = imp_qi10, # part of cellphone sample but also has landline
    
    # frequency of social services use
    service_freq = imp_pservfreq - 1, #recodes to 0:5
    # other variables we need
    analytic_ID = analytic_ID,
    log_incneeds = recode(log(qincneeds), `-Inf` = 0),
    utilities_cut_off = qf8,
    sevhard = qsevhard, # presence of a severe hardship
    adultxhh = qadultxhh,
    adultx = qadultx,
    childx = qchildx,
    wax = qwax, # number of working age adults
    eldx = qeldx,
    personx = qpersonx,
    opmpov = qopmpov,
    povgap = povgap,
    sppart = qsppart,
    tenure = qtenure #house situation
  )

saveRDS(dat, file = "RH4/data/RH4-data-for-weights.rds")
