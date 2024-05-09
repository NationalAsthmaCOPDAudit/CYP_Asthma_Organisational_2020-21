#---------------------------------------------------------------#
#                                                               #
#     C Y P   o r g a n i s a t i o n a l   a n a l y s i s     #
#                                                               #
#     Author: A l e x                                           #
#     Date created: 2 0 2 1 - 1 1 - 1 7                         #
#                                                               #
#---------------------------------------------------------------#


library(tidyverse)


dat <- read.csv("Z:/Group_work/PS_AA/CYP Asthma Organisational 2020-21/Data/rawData/NACAP-CYPAA---Org-Audit-2021---Imperial-extract-20211108-160229-281.csv")




hosp_dat <- read.csv("Z:/Group_work/PS_AA/General UK data/Hospital_codes_CYP_asthma_R_format.csv")

dat <- dat %>% rename(hosp_code = OrgUnit)

dat <- left_join(dat, hosp_dat, by = "hosp_code")
colnames(dat)

dat <- dat %>% select(-Hospital, -region) %>% relocate(hosp_name:region_alt, .after = hosp_code) %>% rename(region = region_alt)

# Now we order them how they want in the spreadsheet

dat <- dat %>% arrange(country, region, integrated_care_system, trust_name, hosp_name)

dat %>% select(country, region, integrated_care_system, trust_name, hosp_name)

table(dat$region)



# Get rid of those who aren't complete:

dat <- dat %>% filter(Complete == "Yes")


# And then get rid of useless columns:

colnames(dat)

dat <- dat %>% select(-Complete, -EDeadline, -X3.3.On.call.paediatric.respiratory.consultants, -X3.8.Smoking.cessation.service,
                      -X3.10.Diag.tools.for.paediatric.asthma.patients, -X4.1.Senior.PAU.round.days.and.times, -X4.2.Senior.ward.round.days.and.times,
                      -X4.3.Resp.nurse.s..available.to.review, -X5.2a.Early.warn.detect.system.records, -X7.1.Transition.process, -Form)




dat$X2.1.Asthma.specialist.unfilled[dat$X2.1.Asthma.specialist.unfilled == 99] <- NA
dat$X2.1.Paediatric.consultants.unfilled[dat$X2.1.Paediatric.consultants.unfilled == 99] <- NA
dat$X2.1.Physician.posts.ST3.and.above.unfilled[dat$X2.1.Physician.posts.ST3.and.above.unfilled == 99] <- NA
dat$X2.1.Nurse.consultant.other.specialist.nurse.unfilled[dat$X2.1.Nurse.consultant.other.specialist.nurse.unfilled == 99] <- NA
dat$X2.1.Paediatric.respiratory.consultants.unfilled[dat$X2.1.Paediatric.respiratory.consultants.unfilled == 99] <- NA




# We need to actually do some calculations: 

dat <- dat %>% mutate(paed_resp_per_bed = round(X1.2.Paediatric.respiratory.coded.emergencies/X1.4.Paediatric.medical.beds, 1),
                      paed_asthma_per_paed_admission = round((X1.3.Paediatric.asthma.coded.emergencies/X1.1.Paediatric.emergencies.hospital.admit)*1000, 1)) %>%
  relocate(paed_resp_per_bed, .after = X1.2.Paediatric.respiratory.coded.emergencies) %>%
  relocate(paed_asthma_per_paed_admission, .after = X1.3.Paediatric.asthma.coded.emergencies)


# Create some functions to use with mutate for the WTE stuff


all_paed_WTE <- function(x) {round((x/dat$X1.2.Paediatric.respiratory.coded.emergencies)*1000, 1)}
just_asthma_WTE <- function(x) {round((x/dat$X1.3.Paediatric.asthma.coded.emergencies)*1000, 1)}



# Create all the extra columns that need to be created

dat %>% select(contains("filled")) %>% colnames()

dat <- dat %>% mutate(X2.1.Physician.posts.ST3.and.above.filled_WTE_just_asthma = just_asthma_WTE(X2.1.Physician.posts.ST3.and.above.filled), 
                      .after = X2.1.Physician.posts.ST3.and.above.unfilled) %>% 
       mutate(X2.1.Physician.posts.ST3.and.above.filled_WTE_all_paed = all_paed_WTE(X2.1.Physician.posts.ST3.and.above.filled), 
              .after = X2.1.Physician.posts.ST3.and.above.unfilled)

dat <- dat %>% mutate(X2.1.Paediatric.consultants.filled_WTE_just_asthma = just_asthma_WTE(X2.1.Paediatric.consultants.filled), 
                      .after = X2.1.Paediatric.consultants.unfilled) %>% 
  mutate(X2.1.Paediatric.consultants.filled_WTE_all_paed = all_paed_WTE(X2.1.Paediatric.consultants.filled), 
         .after = X2.1.Paediatric.consultants.unfilled)

dat <- dat %>% mutate(X2.1.Paediatric.respiratory.consultants.filled_WTE_just_asthma = just_asthma_WTE(X2.1.Paediatric.respiratory.consultants.filled), 
                      .after = X2.1.Paediatric.respiratory.consultants.unfilled) %>% 
  mutate(X2.1.Paediatric.respiratory.consultants.filled_WTE_all_paed = all_paed_WTE(X2.1.Paediatric.respiratory.consultants.filled), 
         .after = X2.1.Paediatric.respiratory.consultants.unfilled)

dat <- dat %>% mutate(X2.1.Asthma.nurse.specialist.filled_WTE_just_asthma = just_asthma_WTE(X2.1.Asthma.nurse.specialist.filled), 
                      .after = X2.1.Asthma.specialist.unfilled) %>% 
  mutate(X2.1.Asthma.nurse.specialist.filled_WTE_all_paed = all_paed_WTE(X2.1.Asthma.nurse.specialist.filled), 
         .after = X2.1.Asthma.specialist.unfilled)

dat <- dat %>% mutate(X2.1.Nurse.consultant.other.specialist.nurse.filled_WTE_just_asthma = just_asthma_WTE(X2.1.Nurse.consultant.other.specialist.nurse.filled),
                      .after = X2.1.Nurse.consultant.other.specialist.nurse.unfilled) %>% 
  mutate(X2.1.Nurse.consultant.other.specialist.nurse.filled_WTE_all_paed = all_paed_WTE(X2.1.Nurse.consultant.other.specialist.nurse.filled), 
         .after = X2.1.Nurse.consultant.other.specialist.nurse.unfilled)



# # # # # #


# And just relevel the factors so the order is correct:

dat$X3.1.Paediatric.admissions.routinely.review.weekdays <- factor(dat$X3.1.Paediatric.admissions.routinely.review.weekdays,
                                                                   levels = c("Twice daily", "Once daily",  "Other"))      

dat$X3.1.Paediatric.admissions.routinely.review.weekends <- factor(dat$X3.1.Paediatric.admissions.routinely.review.weekends,
                                                                   levels = c("Twice daily", "Once daily",  "Other"))      


dat$X3.2.Access.to.a.paediatric.respiratory.nurse <- factor(dat$X3.2.Access.to.a.paediatric.respiratory.nurse,
  levels = c("None", "All paediatric asthma patients", "Those under the care of a paediatric respiratory consultant", "Other"))


dat$X3.4.Regional.paediatric.asthma.network <- factor(dat$X3.4.Regional.paediatric.asthma.network,
                                                      levels = c("Yes", "No", "Not known"))


dat$X3.5.Designated.named.clinical.lead.for.asthma.services <- factor(dat$X3.5.Designated.named.clinical.lead.for.asthma.services,
  levels = c("Paediatric lead only", "Adult lead only", "Single lead for both adults and paediatrics", "No lead"))


dat$X3.5a.Is.this.role.currently.filled <- factor(dat$X3.5a.Is.this.role.currently.filled,
                                                      levels = c("Yes", "No", "Not known"))

dat$X3.5b.Asthma.lead.responsible.for.formal.training <- factor(dat$X3.5b.Asthma.lead.responsible.for.formal.training,
                                                                levels = c("Yes - paediatric only", "Yes - paediatric and adult", "No"))


dat$X3.6.Specific.service.for.paediatric.asthma <- factor(dat$X3.6.Specific.service.for.paediatric.asthma,
                                                          levels = c("Yes", "No", "Not known"))


dat$X3.7.Asthma.lead.review.the.child.prior.to.referral <- factor(dat$X3.7.Asthma.lead.review.the.child.prior.to.referral,
                                            levels = c("Yes", "No", "Not applicable - we have specialist advice on site", "Not known"))


dat$X5.2b.Measure.of.how.worried <- factor(dat$X5.2b.Measure.of.how.worried, levels = c("Yes", "No", "Not known"))


dat$X6.1.Formal.patient.parent.carer.surveys.undertaken <- factor(dat$X6.1.Formal.patient.parent.carer.surveys.undertaken,
     levels = c("Continuous (every patient)", "More than 4 times a year", "3-4 times a year", " 1-2 times a year", "Less than once a year", "Never"))


dat$X6.2.Does.your.trust.have.a.strategic.group.for.asthma.services <- factor(dat$X6.2.Does.your.trust.have.a.strategic.group.for.asthma.services,
                                                                              levels = c("Yes", "No", "Not known"))



# # # # # # # 


# It's at this point, because I didn't take out the columns with 3 dots... I need to do it here. By creating this vector for use at the end.
# Have to do it here because they are in numeric format here

inspectvect <- dat %>% select(where(is.numeric)) %>% select(contains("...")) %>% colnames()






dat_hosp_level <- dat
colnames(dat_hosp_level)
dat_hosp_level <- dat_hosp_level %>% arrange(region, integrated_care_system, trust_code, trust_name, hosp_code, hosp_name) %>% select(-country) %>%
  relocate(region, integrated_care_system, trust_code, trust_name, hosp_code, hosp_name)

dat_hosp_level %>% select(where(is.numeric)) %>% select(contains("...")) %>% colnames()





# Need to do this because of dplyr.
# find the columns that are numeric or logical, and combine with grepl,
# grepl is finding all the columns that contain '...', which are the the ones with multiple options. 
# For the hospital-level table all of these need to be converted from '1's and missing to 'yes' and 'no'


for (i in 1:ncol(dat_hosp_level)) {
  if (grepl("\\.\\.\\.", colnames(dat_hosp_level))[i] == TRUE & 
      (is.numeric(dat_hosp_level[ , i]) == TRUE | is.logical(dat_hosp_level[ , i]) == TRUE)) {

    dat_hosp_level[[i]] <- as.character(dat_hosp_level[[i]])
    dat_hosp_level[[i]] <- recode(dat_hosp_level[[i]], "1" = "Yes", .missing = "No")
  
    }
}



# Need to do some rounding.

dat_hosp_level$paed_asthma_per_paed_admission <- round(dat_hosp_level$paed_asthma_per_paed_admission, 1)
dat_hosp_level$paed_asthma_per_paed_admission <- sprintf("%.1f", dat_hosp_level$paed_asthma_per_paed_admission)

dat_hosp_level$paed_resp_per_bed <- round(dat_hosp_level$paed_resp_per_bed, 1)
dat_hosp_level$paed_resp_per_bed <- sprintf("%.1f", dat_hosp_level$paed_resp_per_bed)

dat_hosp_level <- dat_hosp_level %>% mutate(across(starts_with("X2.1"), ~sprintf("%.1f", round(., 1))))

# write.csv(dat_hosp_level, "Z:/Group_work/PS_AA/CYP Asthma Organisational 2020-21/Data/tidyData/Hospital_level_data_CYP_org_2020-21.csv",
#         row.names = FALSE, na = "")


# # # # # # that is it for the hospital-level data!!! # # # # # # # # #







# For the summary statistics, we need to do some more cleaning





for (i in 1:ncol(dat)) {
  if (grepl("\\.\\.\\.", colnames(dat))[i] == TRUE & 
      (is.numeric(dat[ , i]) == TRUE | is.logical(dat[ , i]) == TRUE)) {
    
    dat[[i]] <- as.character(dat[[i]])
    dat[[i]] <- recode(dat[[i]], "1" = "Yes", .missing = "No")
    dat[[i]] <- factor(dat[[i]])
    
  }
}


#  for some reason the function that you're doing on the column
# goes within the 'across' bracket. The squiggle on fct_recode is essential. "is.factor" can't have brackets.

dat <- dat %>% mutate(across(where(is.factor), ~fct_recode(., NULL = "")))


summary(dat)

# Also, we need to recode the ones that are just yes and no to 1s and 0s
# again, more inconsistency - if we are selecting specific columns, we now use 'c' to select them

# Or... we try it without doing this?


# Now.... we create a new function.


sum_stats <- function(x, varname, roundno = 0, total = TRUE, mean = TRUE, sd = TRUE) {
  
  # Get the summary statistics for each country:
  
    
  # [!is.ininite(x[[varname]]) & !is.nan(x[[varname]]) #
  
  tabby_all <- data.frame(Total = sum(x[[varname]], na.rm = TRUE),
                      Mean = mean(x[[varname]], na.rm = TRUE),
                      SD = sd(x[[varname]], na.rm = TRUE),
                      Median = median(x[[varname]], na.rm = TRUE),
                      Low_quart = as.numeric(unname(summary(x[[varname]]))[2]),
                      High_quart = as.numeric(unname(summary(x[[varname]]))[5]))

  eng <- x %>% filter(country == "England")
  wal <- x %>% filter(country == "Wales")
  
  tabby_eng <- data.frame(Total = sum(eng[[varname]], na.rm = TRUE),
                          Mean = mean(eng[[varname]], na.rm = TRUE),
                          SD = sd(eng[[varname]], na.rm = TRUE),
                          Median = median(eng[[varname]], na.rm = TRUE),
                          Low_quart = as.numeric(unname(summary(eng[[varname]]))[2]),
                          High_quart = as.numeric(unname(summary(eng[[varname]]))[5]))
  
  
  tabby_wal <- data.frame(Total = sum(wal[[varname]], na.rm = TRUE),
                          Mean = mean(wal[[varname]], na.rm = TRUE),
                          SD = sd(wal[[varname]], na.rm = TRUE),
                          Median = median(wal[[varname]], na.rm = TRUE),
                          Low_quart = as.numeric(unname(summary(wal[[varname]]))[2]),
                          High_quart = as.numeric(unname(summary(wal[[varname]]))[5]))
  
  # Bind them all together
  
  tabby_all <- rbind(tabby_all, tabby_eng, tabby_wal)

  # round them to what you want
  
  
  tabby_all <- round(tabby_all, roundno)
  
  
  # Format them how they need to be formatted
  
  tabby_all$Mean <- sprintf(paste0("%.", roundno, "f"), tabby_all$Mean)
  tabby_all$SD <- sprintf(paste0("%.", roundno, "f"), tabby_all$SD)
  tabby_all$Median <- sprintf(paste0("%.", roundno, "f"), tabby_all$Median)
  tabby_all$Low_quart <- sprintf(paste0("%.", roundno, "f"), tabby_all$Low_quart)
  tabby_all$High_quart <- sprintf(paste0("%.", roundno, "f"), tabby_all$High_quart)
  

  # Remove the unnecessary statistics for that variable
                          
  if (total == FALSE) {
    tabby_all$Total <- NULL
  }
  
  if (mean == FALSE) {
    tabby_all$Mean <- NULL
  }
  
  if (sd == FALSE) {
    tabby_all$SD <- NULL
  }

  
  colnames(tabby_all) <- paste0(varname, "_", colnames(tabby_all))

  
      
  return(tabby_all)
}





# And another one.


freq_stats <- function(x, varname, remove_nos = FALSE, KPI = FALSE) {
  
  all <- x
  eng <- x %>% filter(country == "England")
  wal <- x %>% filter(country == "Wales")

  # for all:
    
    gen <- all %>% dplyr::select(!!varname) %>% drop_na()
    var_N <- data.frame(nrow(gen))
    tabby_all <- data.frame(delete_me = 0)
    
    #   if(nrow(gen) == 0) {return(var_N)}
    
    #  else {
    
    gen0 <- as.data.frame(table(gen[[1]]))
    gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
      dplyr::rename(perc = Freq)
    gen2 <- inner_join(gen0, gen1, by = "Var1")
    gen2$perc <- sprintf("%.1f", gen2$perc)
    
    if (remove_nos == TRUE) {
      
      gen2 <- gen2 %>% filter(Var1 != "No")

    }
    
    # gen.E2$England <- paste(gen.E2$Freq, " (", gen.E2$perc, ")", sep = "")
    # gen.E2 <- select(gen.E2, Var1, England)
    for (i in 1:nrow(gen2)) {
      gen3 <- gen2
      gen3$Var1 <- as.character(gen3$Var1)
      gen3 <- gen3[i, ]
      colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                          paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
      gen4 <- cbind(var_N, gen3[ ,2:3])
      colnames(gen4)[1] <- paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_Total_N")
      tabby_all <- cbind(tabby_all, gen4)
    }
    
    
    # for England:
    
    gen <- eng %>% dplyr::select(!!varname) %>% drop_na()
    var_N <- data.frame(nrow(gen))
    tabby_eng <- data.frame(delete_me = 0)
    
    gen0 <- as.data.frame(table(gen[[1]]))
    gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
      dplyr::rename(perc = Freq)
    gen2 <- inner_join(gen0, gen1, by = "Var1")
    gen2$perc <- sprintf("%.1f", gen2$perc)
    
    if (remove_nos == TRUE) {
      
      gen2 <- gen2 %>% filter(Var1 != "No")
      
    }
    
   
    for (i in 1:nrow(gen2)) {
      gen3 <- gen2
      gen3$Var1 <- as.character(gen3$Var1)
      gen3 <- gen3[i, ]
      colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                          paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
      gen4 <- cbind(var_N, gen3[ ,2:3])
      colnames(gen4)[1] <- paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_Total_N")
      tabby_eng <- cbind(tabby_eng, gen4)
    }
    
    # for Wales:
    
    gen <- wal %>% dplyr::select(!!varname) %>% drop_na()
    var_N <- data.frame(nrow(gen))
    tabby_wal <- data.frame(delete_me = 0)
    
    gen0 <- as.data.frame(table(gen[[1]]))
    gen1 <- as.data.frame(round(prop.table(table(gen[[1]]))*100, 1), nsmall = 1) %>% 
      dplyr::rename(perc = Freq)
    gen2 <- inner_join(gen0, gen1, by = "Var1")
    gen2$perc <- sprintf("%.1f", gen2$perc)
    
    if (remove_nos == TRUE) {
      
      gen2 <- gen2 %>% filter(Var1 != "No")
      
    }
    

    for (i in 1:nrow(gen2)) {
      gen3 <- gen2
      gen3$Var1 <- as.character(gen3$Var1)
      gen3 <- gen3[i, ]
      colnames(gen3) <- c("Var1", paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_n"),
                          paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_perc")) 
      gen4 <- cbind(var_N, gen3[ ,2:3])
      colnames(gen4)[1] <- paste0(varname, "_", gsub(" ", "_", gen3[1,1]), "_Total_N")
      tabby_wal <- cbind(tabby_wal, gen4)
    }
    
    
    tabby <- rbind(tabby_all, tabby_eng, tabby_wal)
    
    tabby$delete_me <- NULL
    
    if (KPI == TRUE) {   # If it's a KPI variable the order is slightly different
    
        tabby <- tabby[ , c(2, 1, 3)]
    
    }
    
    return(tabby)
    

}









# # # # Start the data frame # # # # 

all <- data.frame(Country = c("National (All)", "England", "Wales"))

# # # # # 


# Create the first load of column names

first.1 <- dat %>% select(X1.1.Paediatric.emergencies.hospital.admit:X1.2.Paediatric.respiratory.coded.emergencies) %>% colnames()
                            
                            # X1.4.Paediatric.medical.beds) %>% colnames()

for (i in first.1) {
  all <- cbind(all, sum_stats(dat, i, roundno = 0))
}


all <- cbind(all, sum_stats(dat, "paed_resp_per_bed", total = FALSE, roundno = 1))

all <- cbind(all, sum_stats(dat, "X1.3.Paediatric.asthma.coded.emergencies", roundno = 0))

all <- cbind(all, sum_stats(dat, "paed_asthma_per_paed_admission", total = FALSE, roundno = 1))

all <- cbind(all, sum_stats(dat, "X1.4.Paediatric.medical.beds", roundno = 0))


# then we carry on

all <- cbind(all, freq_stats(dat, "X1.5.Paediatric.HDU", remove_nos = TRUE))

all <- cbind(all, sum_stats(dat, "X1.5a..How.many.beds.paediatric.in.HDU"))

all <- cbind(all, freq_stats(dat, "X1.6.Paediatric.Intensive.Care.Unit", remove_nos = TRUE))

all <- cbind(all, sum_stats(dat, "X1.6a.How.many.beds.does.in.PICU"))


# all$X2.1.Physician.posts.ST3.and.above.filled_WTE_all_paed_Total <- all$X2.1.Physician.posts.ST3.and.above.filled_Total/all$X1.2.Paediatric.respiratory.coded.emergencies_Total

head(dat$X2.1.Physician.posts.ST3.and.above.filled)

second <- dat %>% select(X2.1.Physician.posts.ST3.and.above.filled:X2.1.Nurse.consultant.other.specialist.nurse.filled_WTE_just_asthma) %>% colnames()


# Have to do this because slightly different things are wanted for each variable (some want totals, etc)

for (i in 1:5) {
  all <- cbind(all, sum_stats(dat, second[(i*4) - 3], mean = FALSE, sd = FALSE, total = TRUE, roundno = 1))
  all <- cbind(all, sum_stats(dat, second[(i*4) - 2], mean = FALSE, sd = FALSE, total = TRUE, roundno = 1))
  all <- cbind(all, sum_stats(dat, second[(i*4) - 1], mean = FALSE, sd = FALSE, total = FALSE, roundno = 1))
  all <- cbind(all, sum_stats(dat, second[(i*4) - 0], mean = FALSE, sd = FALSE, total = FALSE, roundno = 1))
  }



third <- dat %>% select(X3.1.Paediatric.admissions.routinely.review.weekdays:X3.6.Specific.service.for.paediatric.asthma) %>% colnames()

for (i in third) {
  all <- cbind(all, freq_stats(dat, i))
}

all <- cbind(all, freq_stats(dat, "X3.6a.If.not...Set.criteria.for.referral.offsite", remove_nos = TRUE))


fourth <- dat %>% select(X3.7.Asthma.lead.review.the.child.prior.to.referral:X4.3.Resp.nurse.s..available.to.review....No.paediatric.respiratory.nurse.available) %>% colnames()

for (i in fourth) {
  all <- cbind(all, freq_stats(dat, i))
}

all <- cbind(all, freq_stats(dat, "X5.1.Electronic.Patient.Record..EPR..system", remove_nos = TRUE))

colnames(dat)

all$BLANK_COLUMN <- NA

all <- cbind(all, freq_stats(dat, "X5.2.Early.warning.detection", remove_nos = TRUE))

fifth <- dat %>% select(X5.2a.Early.warn.detect.system.records...Target.oxygen.saturation:X7.1.Transition.process...We.do.not.have.any.formal.transition.arrangements) %>% colnames()

for (i in fifth) {
  all <- cbind(all, freq_stats(dat, i))
}


# # # # The end!!!!!



all %>% select(contains(inspectvect)) %>% colnames() # & ends_with(c("No_Total_N", "No_)) 

# Use the vector we created earlier of the columns we have to drop

ncol(all)

all <- all %>% select(!(contains(inspectvect) & ends_with(c("No_Total_N", "No_n", "No_perc")))) # %>% colnames()

ncol(all)


# write.csv(all, "Z:/Group_work/PS_AA/CYP Asthma Organisational 2020-21/Data/tidyData/National_level_data_CYP_org_2020-21.csv", row.names = FALSE)



# # # # # # # Benchmarking

# Make the benchmarking variables

dat$KPI_1_spiro_feno <- "No"
dat$KPI_1_spiro_feno[dat$X3.10.Diag.tools.for.paediatric.asthma.patients...Spirometry == "Yes" &
                       dat$X3.10.Diag.tools.for.paediatric.asthma.patients...Exhaled.nitric.oxide..FeNO. == "Yes"] <- "Yes"
dat$KPI_1_spiro_feno <- factor(dat$KPI_1_spiro_feno, levels = c("Yes", "No"))


dat$KPI_2_paed_resp_nurse <- "No"
dat$KPI_2_paed_resp_nurse[dat$X3.2.Access.to.a.paediatric.respiratory.nurse == "All paediatric asthma patients"] <- "Yes"
dat$KPI_2_paed_resp_nurse <- factor(dat$KPI_2_paed_resp_nurse, levels = c("Yes", "No"))


dat$KPI_3_clinical_lead_asthma <- "No"
dat$KPI_3_clinical_lead_asthma[dat$X3.5.Designated.named.clinical.lead.for.asthma.services != "No lead"] <- "Yes"
dat$KPI_3_clinical_lead_asthma <- factor(dat$KPI_3_clinical_lead_asthma, levels = c("Yes", "No"))


dat$KPI_4_smoking_cess <- "No"
dat$KPI_4_smoking_cess[dat$X3.8.Smoking.cessation.service....Parents.carers.of.paediatric.asthma.patients == "Yes" &
                         dat$X3.8.Smoking.cessation.service....Paediatric.asthma.patients == "Yes"] <- "Yes"
dat$KPI_4_smoking_cess <- factor(dat$KPI_4_smoking_cess, levels = c("Yes", "No"))


dat$KPI_5_transition <- "No"
dat$KPI_5_transition[dat$X7.1.Transition.process...The.GP.is.sent.the.same.record == "Yes"] <- "Yes"
dat$KPI_5_transition[dat$X7.1.Transition.process...The.young.person.has.a.full.record.of.their.condition == "Yes"] <- "Yes"
dat$KPI_5_transition[dat$X7.1.Transition.process...The.young.person.has.a.transition.plan... == "Yes"] <- "Yes"
dat$KPI_5_transition[dat$X7.1.Transition.process...The.young.person.is.allocated.a.coordinator... == "Yes"] <- "Yes"
dat$KPI_5_transition[dat$X7.1.Transition.process...The.young.person.is.given.the.opportunity.to.be.seen... == "Yes"] <- "Yes"
dat$KPI_5_transition <- factor(dat$KPI_5_transition, levels = c("Yes", "No"))


bmk_hosp_level <- dat %>% select(region, integrated_care_system, trust_name, hosp_name, starts_with("KPI_"))

bmk_hosp_level

# write.csv(bmk_hosp_level, "Z:/Group_work/PS_AA/CYP Asthma Organisational 2020-21/Data/tidyData/Hospital_level_benchmarking_CYP_org_2020-21.csv", row.names = FALSE)


bmk <- data.frame(Country = c("National (All)", "England", "Wales"))

KPI_vars <- dat %>% select(starts_with("KPI_")) %>% colnames()

bmk <- cbind(bmk, 
             freq_stats(dat, KPI_vars[1], remove_nos = TRUE, KPI = TRUE),
             freq_stats(dat, KPI_vars[2], remove_nos = TRUE, KPI = TRUE),
             freq_stats(dat, KPI_vars[3], remove_nos = TRUE, KPI = TRUE),
             freq_stats(dat, KPI_vars[4], remove_nos = TRUE, KPI = TRUE),
             freq_stats(dat, KPI_vars[5], remove_nos = TRUE, KPI = TRUE))



# write.csv(bmk, "Z:/Group_work/PS_AA/CYP Asthma Organisational 2020-21/Data/tidyData/National_level_benchmarking_CYP_org_2020-21.csv", row.names = FALSE)

