### Nicholas Judd 
# Dep. Neuro
# Karolinska Institute
# 2021-07-11 
# nickjudd@gmail.com

##################################################################
########### Main schooling analysis script: Notes ########### 

# NDA 3.0 Release Notes ABCD README FIRST
# this document has all the relevant info

# Due to data sharing of ABCD, researchers must ask them for access (INSERT DOI of relevant release)
# Most data is from ABCD's release 3.0 only two things are inhouse
# 1. A list of randomally selected single siblings (this is on the github)
# 2. A multitrait cog-PGS score computed by SciLife Lab ('abcd_pgs.txt')
# After getting access to ABCD's genetic data you can ask us for this data to replicate the analysis. 
# You must prove that you have current and approved access to ABCD's genetic data.

# variables
# abcd_tbss01 is the two timepoint cognition file
# 8. NDA 3.0 Neurocognition.docx has info on tasks used for fIQ & cIQ in results dir

# demo_ed_v2: What grade is the child in? If it is summer, indicate grade starting in the fall.
# demo_race_a_p___10: Is your child white?
# demo_prnt_ed_v2: What is the highest grade or level of school you have completed or the highest degree you have received?
# demo_prtnr_ed_v2: What is the highest grade or level of school your partner completed or highest degree they received?
# demo_comb_income_v2


# Consider only including children in regular public or private schooling
## dibf01.txt # kbi_p_c_school_setting
# Select your child's current school setting
# 1 = Not in School; 2 = Regular Public School; 3 = Regular Private School 
# 4 = Vocational-Technical School; 9 = Charter School; 5 = Cyber School; 
# 6 = Home School; 7 = Specialized School for Youth with Emotional/Behavioral Problems; 8 = Other/ Otra

## pdem02.txt # demographics & SES


# demo_comb_income_v2; yearly total combined income?
# 1= Less than $5,000; 2=$5,000 through $11,999; 
# 3=$12,000 through $15,999; 4=$16,000 through $24,999; 
# 5=$25,000 through $34,999; 6=$35,000 through $49,999; 
# 7=$50,000 through $74,999; 8= $75,000 through $99,999; 
# 9=$100,000 through $199,999; 10=$200,000 and greater. 
# 999 = Don't know No; 777 = Refuse to answer

# demo_prnt_ed_v2 what is the highest grade you atteneded?
# demo_prtnr_ed_v2What is the highest grade or level of school your partner completed or highest degree they received? 
# 0 = Never attended/Kindergarten only; 
# 1 = 1st grade ; 2 = 2nd grade, 3 = 3rd grade; 4 = 4th grade
# 5 = 5th grade ; 6 = 6th grade ; 7 = 7th grade 7.Âº grado ; 8 = 8th grade 8.Âº grado ; 
# 9 = 9th grade 9.Âº grado ; 10 = 10th grade 10.Âº grado ; 
# 11 = 11th grade 11.Âº grado ; 12 = 12th grade; 
# 13 = High school graduate ; 14 = GED or equivalent ; 
# 15 = Some college; 16 = Associate degree: Occupational; 17 = Associate degree:  ; 
# 18 = Bachelor's degree 
# (ex. BA; 19 = Master's degree (ex. MA; 20 = Professional School degree (ex. MD; 21 = Doctoral degree (ex. PhD; 
# 777 = Refused to answer

# recoding to...
# 1: middel school or less
# 2: some high school
# 3: high school graudate or GED
# 4: Some college, Associate degree: Occupational or Associate degree
# 5: Bachlors degree
# 6: Master's degree
# 7: Professional School degree or Doctoral degree

##################################################################
########### Start of script: loading & cleaning data ########### 

setwd("~/Projects/R_projects/ABCDschooling/")
library(data.table); library(ggplot2); library(patchwork); library(effectsize); library(lubridate); 
library(patchwork); library(lme4); library(sjPlot); library(performance); library(mice); library(broom.mixed) # need for pool in mice
source('funcs/vec_to_fence.R')
source('funcs/imp_3way.R')

options(scipen = 999); set.seed(42)

# there are also children that are not in normal public or private schools
schools <- fread("sed -e '2d' data/dibf01.txt")[, .(subjectkey, kbi_p_c_school_setting)] # 2 & 3 represent normal public and private schools respectively

# here's a list from Bruno of the children that are included (they have another family member that's excluded)
siblings_to_include <- fread("data/abcd_included_sibling_fromBruno.csv")
siblings_to_include$sibs_included <- rep(1, length(siblings_to_include$subjectkey))

incl <- schools[siblings_to_include, on = 'subjectkey'# making a dt of subs to include
                 ][kbi_p_c_school_setting %in% c(2,3)]

# loading relevant data
cog <- fread("sed -e '2d' data/abcd_tbss01.txt")
site <- fread("sed -e '2d' data/StudySiteBLgrade/abcd_lt01.txt")
site <- site[eventname =='baseline_year_1_arm_1'][, .(subjectkey, site_id_l)]
grade <- fread("sed -e '2d' data/StudySiteBLgrade/pdem02.txt")
grade <- grade[, .(subjectkey, demo_ed_v2, demo_race_a_p___11, demo_prnt_ed_v2, demo_prtnr_ed_v2, demo_comb_income_v2)]
grade <- grade[site, on= "subjectkey"]

# demo_prnt_race_a_v2___10 # parental race white
# demo_race_a_p___10 # child race white

# recoding demo_prnt_ed_v2, demo_prtnr_ed_v2 to get the max between them for the parent
grade$demo_prnt_ed_v2[grade$demo_prnt_ed_v2 %in% c(0,1,2,3,4,5,6,7,8)] <- 1 # middle school or less
grade$demo_prnt_ed_v2[grade$demo_prnt_ed_v2 %in% c(9,10,11,12)] <- 2 # some highschool
grade$demo_prnt_ed_v2[grade$demo_prnt_ed_v2 %in% c(13,14)] <- 3 # high school graudate or GED
grade$demo_prnt_ed_v2[grade$demo_prnt_ed_v2 %in% c(15,16,17)] <- 4 # Some college, Associate degree: Occupational or Associate degree
grade$demo_prnt_ed_v2[grade$demo_prnt_ed_v2 %in% c(18)] <- 5 # Bachlors degree
grade$demo_prnt_ed_v2[grade$demo_prnt_ed_v2 %in% c(19)] <- 6 # MSc degree
grade$demo_prnt_ed_v2[grade$demo_prnt_ed_v2 %in% c(20,21)] <- 7 # MD, PhD
grade$demo_prnt_ed_v2[grade$demo_prnt_ed_v2 %in% c(777, 999)] <- NA # refused to answer or don't know was coded as missing

# for the care giver
grade$demo_prtnr_ed_v2[grade$demo_prtnr_ed_v2 %in% c(0,1,2,3,4,5,6,7,8)] <- 1 # middle school or less
grade$demo_prtnr_ed_v2[grade$demo_prtnr_ed_v2 %in% c(9,10,11,12)] <- 2 # some highschool
grade$demo_prtnr_ed_v2[grade$demo_prtnr_ed_v2 %in% c(13,14)] <- 3 # high school graudate or GED
grade$demo_prtnr_ed_v2[grade$demo_prtnr_ed_v2 %in% c(15,16,17)] <- 4 # Some college, Associate degree: Occupational or Associate degree
grade$demo_prtnr_ed_v2[grade$demo_prtnr_ed_v2 %in% c(18)] <- 5 # Bachlors degree
grade$demo_prtnr_ed_v2[grade$demo_prtnr_ed_v2 %in% c(19)] <- 6 # MSc degree
grade$demo_prtnr_ed_v2[grade$demo_prtnr_ed_v2 %in% c(20,21)] <- 7 # MD, PhD
grade$demo_prtnr_ed_v2[grade$demo_prtnr_ed_v2 %in% c(777, 999)] <- NA # refused to answer was coded as missing

grade[, ParEd_max := pmax(demo_prtnr_ed_v2, demo_prnt_ed_v2, na.rm=TRUE)][ # getting the max parental education
  , c("demo_prtnr_ed_v2","demo_prnt_ed_v2"):=NULL
]
# DT[, col_min:= do.call(pmin, c(.SD, list(na.rm=TRUE))), .SDcols= col_names] # how to do it when you have a lot of cols

grade$demo_comb_income_v2[grade$demo_comb_income_v2 %in% c(777, 999)] <- NA # recoding refusing to answer & don't know as NA's
sum(is.na(grade$demo_comb_income_v2)) # 1018 subs missing

# excluding subjects based on my exclusion criteria: Randomly sampled sibling, in normal private or public schooling
cog <- cog[subjectkey %in% incl$subjectkey]
grade <- grade[subjectkey %in% incl$subjectkey]

cog <- cog[eventname == "baseline_year_1_arm_1"][
  , .(subjectkey, interview_date, interview_age, sex, 
      nihtbx_list_uncorrected, nihtbx_fluidcomp_uncorrected, nihtbx_cryst_uncorrected)][
        ][
          grade, on = "subjectkey"#, nomatch = 0 # joining grade on subjectkey
          ][
            demo_ed_v2 %in% c(3,4,5) # selecting children in grades 3, 4 or 5
            ][
              , interview_mnth := month(as.POSIXlt(interview_date, format="%m/%d/%Y"))
              # using the interview date to our advantage
              # we have children with varying amounts of schooling in months as well
              ][
                , schooling_mnth := interview_mnth-7 # making August month 1
                ]

# adding neighborhood SES
cog <- fread("sed -e '2d' data/abcd_rhds01.txt")[
  eventname == "baseline_year_1_arm_1" ][
  , .(subjectkey, reshist_addr1_adi_wsum) ][
    cog, on = "subjectkey"]

# fixing subjects recruited in the summer as having the months of schooling equal to before summer break
cog$summer_logical_fix <- (day(mdy(cog$interview_date)) >= 15 & month(mdy(cog$interview_date)) == 6) | (day(mdy(cog$interview_date)) <= 15 & month(mdy(cog$interview_date)) == 8 | month(mdy(cog$interview_date)) == 7)
cog$schooling_mnth <- as.numeric(dplyr::recode(as.character(cog$schooling_mnth),
                                               "0" = "10", "-1" = "10", # setting June & July to June
                                             "-2" = "10", "-3" = "9", "-4" = "8", "-5" = "7", "-6" = "6"))

cog$schooling_mnth[cog$summer_logical_fix == TRUE] <- 10 # setting the first half of August children to June

# recoding starting grade 3 at 1, so it can be multiplied with schooling_mnths
cog$grade <- as.numeric(dplyr::recode(as.character(cog$demo_ed_v2), "3" = "1", "4" = "2", "5" = "3"))

# "If it is summer, indicate grade starting in the fall.". Therefore we need summer children to have a grade subtracted.
cog$grade[cog$summer_logical_fix==T] <- cog$grade[cog$summer_logical_fix==T] -1
cog <- cog[grade %in% c(1,2,3)] # getting rid of 2nd graders taken during the summer (n = 64)

# adding 10 months per grade
cog[grade ==2]$schooling_mnth <- cog[grade ==2]$schooling_mnth +10
cog[grade ==3]$schooling_mnth <- cog[grade ==3]$schooling_mnth +20
# table(cog$schooling_mnth)

# making vars in the unit of years
cog$schooling_yrs <- cog$schooling_mnth/10 #this means that one year is not equal to one year of schooling!*
cog$age_yrs <- cog$interview_age/12

# sampling it only perfect around age 10 *
# ggplot(cog, aes(age_yrs, group = schooling_yrs, color = schooling_yrs)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(cog, aes(interview_age, group = schooling_mnth, color = schooling_mnth)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(cog, aes(schooling_mnth, group = interview_age, color = interview_age)) +
#   geom_density() +
#   theme_minimal()

# The measure if the children have repeated a grade is within the file abcd_yksad01 with the name kbi_y_grade_repeat
# Have you ever repeated a grade? # 7.5% of children repeated a grade!

cog <- fread("sed -e '2d' data/abcd_yksad01.txt")[
  eventname == "baseline_year_1_arm_1"][
    , .(subjectkey, kbi_y_grade_repeat)][ 
      cog, on = 'subjectkey']

# PGS from Bruno (there is only 10,000 of them)
cog <- fread("data/abcd_pgs.txt")[cog, on = "subjectkey"]



# psych::describe(cog)

# making a PCA for SES
# cog$ses_pca <- psych::pca(cog[, .(ParEd_max, demo_comb_income_v2, reshist_addr1_adi_wsum)])$scores
# there's n = 1043 NA's (only 663 when you get rid of neighborhood)
# * imputation might be a good SI analysis
# https://stats.stackexchange.com/questions/35561/imputation-of-missing-values-for-pca


cog <- cog[kbi_y_grade_repeat ==0] # 9% of subjects gone

cog <- fread('data/pca_data_ethnicity_PC20.txt', fill = T)[,1:22][cog, on = "subjectkey"][name == "White"]


# Bringing outliers to the fence for cryst, fluid and list sorting
cog$nihtbx_fluidcomp_uncorrected <- vec_to_fence(cog$nihtbx_fluidcomp_uncorrected)
cog$nihtbx_cryst_uncorrected <- vec_to_fence(cog$nihtbx_cryst_uncorrected)
cog$nihtbx_list_uncorrected <- vec_to_fence(cog$nihtbx_list_uncorrected)

# gonna do probabilitics PCA to get the SES PCA scores and than MICE for the other values
cog$ParEd_max.s <- as.numeric(scale(cog$ParEd_max))
cog$demo_comb_income_v2.s <- as.numeric(scale(cog$demo_comb_income_v2))
cog$reshist_addr1_adi_wsum.s <- as.numeric(scale(cog$reshist_addr1_adi_wsum))

# mice::md.pattern(cog[, .(ParEd_max.s, demo_comb_income_v2.s, reshist_addr1_adi_wsum.s)], plot = F)
cog$ses_ppca <- as.numeric(pcaMethods::ppca(BiocGenerics::t(cog[, .(ParEd_max.s, demo_comb_income_v2.s, reshist_addr1_adi_wsum.s)]), nPcs = 1, seed = 42)@loadings)
# ppca has a .999 correlation for the non-missing values with normal pca

# I am now finding subject that were missing more than 1 value for the 3 SES categories
cog$twoormore <- rep(0, length(cog$subjectkey))
cog$twoormore[is.na(cog$ParEd_max)] <- cog$twoormore[is.na(cog$ParEd_max)] +1
cog$twoormore[is.na(cog$demo_comb_income_v2)] <- cog$twoormore[is.na(cog$demo_comb_income_v2)] +1 
cog$twoormore[is.na(cog$reshist_addr1_adi_wsum)] <- cog$twoormore[is.na(cog$reshist_addr1_adi_wsum)] +1 
cog$twoormore[cog$twoormore==1] <- 0
cog$twoormore[cog$twoormore>1] <- 1

# sum(cog$twoormore) # 46 subjects, matches md pattern above
cog$ses_ppca[cog$twoormore==1] <- NA # making them NA

# standardizing vars
cog$nihtbx_fluidcomp_uncorrected.s <- as.numeric(scale(cog$nihtbx_fluidcomp_uncorrected))
cog$nihtbx_cryst_uncorrected.s <- as.numeric(scale(cog$nihtbx_cryst_uncorrected))
cog$nihtbx_list_uncorrected.s <- as.numeric(scale(cog$nihtbx_list_uncorrected))

cog$pgs.s <- as.numeric(scale(cog$pgs))
cog$ses_ppca.s <- -as.numeric(scale(cog$ses_ppca)) # sometimes you need to flip this sign, see cor with parental edu to see

# adding PCs Aug 4th 2021



# 
# cog$ses_ppca_whites <- as.numeric(pcaMethods::ppca(BiocGenerics::t(cog[, .(ParEd_max.s, demo_comb_income_v2.s, reshist_addr1_adi_wsum.s)]), nPcs = 1, seed = 42)@loadings)
# cog$ses_ppca_whites[cog$twoormore==1] <- NA
# cog$ses_ppca_whites.s <- -as.numeric(scale(cog$ses_ppca_whites)) 

##################################################################
########### plotting descript (not essential to run) ###########  ###### 

# # should age be inverse...?
# fi_age <- ggplot(x, aes(age_yrs, nihtbx_fluidcomp_uncorrected)) +
#   geom_jitter(alpha = .2, color = "blue") +
#   theme_minimal() +
#   geom_smooth(method = "lm", color = "black")
# cy_age <- ggplot(x, aes(age_yrs, nihtbx_cryst_uncorrected)) +
#   geom_jitter(alpha = .2, color = "blue") +
#   theme_minimal() +
#   geom_smooth(method = "lm", color = "black")
# 
# # checking the inverse, smaller better (i.e., negative numbers)
# AIC(lm(EF_fa ~ 1/age_yrs, data = x)) - AIC(lm(EF_fa ~ age_yrs, data = x)); 
# AIC(lm(nihtbx_fluidcomp_uncorrected ~ 1/age_yrs, data = x)) - AIC(lm(nihtbx_fluidcomp_uncorrected ~ age_yrs, data = x)); 
# AIC(lm(nihtbx_cryst_uncorrected ~ 1/age_yrs, data = x)) - AIC(lm(nihtbx_cryst_uncorrected ~ age_yrs, data = x)); 
# 
# 
# ef_school <- ggplot(x, aes(schooling_yrs, EF_fa)) +
#   geom_jitter(alpha = .2, color = "blue") +
#   theme_minimal() +
#   geom_smooth(method = "lm", color = "black")
# 
# cor_ageschool <- ggplot(x, aes(schooling_yrs, age_yrs)) +
#   geom_jitter(alpha = .2, color = "darkred") +
#   theme_minimal() +
#   geom_smooth(method = "lm", color = "black")
# cor(x$age_yrs, x$schooling_yrs, use = "pairwise.complete.obs")
# 
# # this is important as sampling it only perfect around age 10 *
# ggplot(x, aes(age_yrs, group = schooling_yrs, color = schooling_yrs)) +
#   geom_density() +
#   theme_minimal()


# PGS europeans only prediction
# summary(lm(scale(nihtbx_fluidcomp_uncorrected.s) ~ scale(pgs), data = cog[demo_race_a_p___10==1]))
# summary(lm(scale(nihtbx_fluidcomp_uncorrected.s) ~ scale(pgs), data = cog[demo_race_a_p___10==0]))
# 
# summary(lm(scale(nihtbx_cryst_uncorrected.s) ~ scale(pgs), data = cog[demo_race_a_p___10==1]))
# summary(lm(scale(nihtbx_cryst_uncorrected.s) ~ scale(pgs), data = cog[demo_race_a_p___10==0]))

# Bruno has done this with PCA and showed the expected results (altho it still predicts pretty well)

# apa table
# apaTables::apa.cor.table(cog[, .(nihtbx_cryst_uncorrected.s, nihtbx_fluidcomp_uncorrected.s, nihtbx_list_uncorrected.s,
#                                  pgs.s, ses_ppca.s,
#                                  ParEd_max.s, demo_comb_income_v2.s, reshist_addr1_adi_wsum.s)],
#                          show.conf.interval = FALSE,
#                          filename = "~/Projects/R_projects/gxe_ABCD/cortable.doc"
#                          )

##################################################################
########### model datasets ########### 
fluid_data_pca <- cog[kbi_y_grade_repeat == 0 # this has already been done
                    ][
                      , .(nihtbx_fluidcomp_uncorrected.s,
                          schooling_yrs, age_yrs, site_id_l, sex, #subjectkey,
                          ses_ppca.s, pgs.s, subjectkey,
                          C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20)]#[
                        #    !is.na(nihtbx_fluidcomp_uncorrected.s)] # removes 147 (>2%)
                          
cryst_data_pca <- cog[kbi_y_grade_repeat == 0 # this has already been done
                      ][
                        , .(nihtbx_cryst_uncorrected.s,
                            schooling_yrs, age_yrs, site_id_l, sex, #subjectkey,
                            ses_ppca.s, pgs.s, subjectkey,
                            C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20)]#[
                         #     !is.na(nihtbx_cryst_uncorrected.s)] # removes 115 (~1.5%)
                            
list_data_pca <- cog[kbi_y_grade_repeat == 0 # this has already been done
                      ][
                        , .(nihtbx_list_uncorrected.s,
                            schooling_yrs, age_yrs, site_id_l, sex, #subjectkey,
                            ses_ppca.s, pgs.s, subjectkey)]#[
                    #          !is.na(nihtbx_list_uncorrected.s)] # removes 115 (~1.5%)
                             
# checking patterns of missingness
# mice::md.pattern(fluid_data_pca, plot = F)
# mice::md.pattern(cryst_data_pca, plot = F)
# mice::md.pattern(list_data_pca, plot = F)

# making dfs with complete cases
fluid_data_pca.complete <- na.omit(fluid_data_pca)
cryst_data_pca.complete <- na.omit(cryst_data_pca)
list_data_pca.complete <- na.omit(list_data_pca)

# making dfs with imputed data (uncomment to get imputation results)
# cryst_imp <- imp_3way(cryst_data_pca[, c("site_id_l", "nihtbx_cryst_uncorrected.s", "ses_ppca.s", "pgs.s", "age_yrs", "schooling_yrs", "sex")])
# fluid_imp <- imp_3way(fluid_data_pca[, c("site_id_l", "nihtbx_fluidcomp_uncorrected.s", "ses_ppca.s", "pgs.s", "age_yrs", "schooling_yrs", "sex")])
# list_imp <- imp_3way(list_data_pca[, c("site_id_l", "nihtbx_list_uncorrected.s", "ses_ppca.s", "pgs.s", "age_yrs", "schooling_yrs", "sex")])

#MAR assumption, showing that those without PGS's happen to be a different population
cog$missing_PGS <- is.na(cog$pgs)

effectsize::cohens_d(demo_comb_income_v2.s ~ missing_PGS, data = cog)
summary(lm(demo_comb_income_v2.s ~ missing_PGS, data = cog))

effectsize::cohens_d(ParEd_max.s ~ missing_PGS, data = cog)
summary(lm(ParEd_max.s ~ missing_PGS, data = cog))

effectsize::cohens_d(reshist_addr1_adi_wsum.s ~ missing_PGS, data = cog)
summary(lm(reshist_addr1_adi_wsum.s ~ missing_PGS, data = cog))

##################################################################
########### Analysis: fitting models ########### 
# linear mixed-eff model with site coded
# https://stats.stackexchange.com/questions/116770/reml-or-ml-to-compare-two-mixed-effects-models-with-differing-fixed-effects-but

### regression discounity models

# crystalized IQ

cy_mm1.1_age <- lmerTest::lmer(nihtbx_cryst_uncorrected.s ~ age_yrs + (1 | site_id_l), data = cryst_data_pca.complete, REML = F)

cy_mm1.1 <- lmerTest::lmer(nihtbx_cryst_uncorrected.s ~ age_yrs + schooling_yrs + (1 | site_id_l),
                       data = cryst_data_pca.complete, REML = F)


cy_mm1.2 <- lme4::lmer(nihtbx_cryst_uncorrected.s ~ age_yrs + schooling_yrs + sex + (1 | site_id_l),
                        data = cryst_data_pca.complete, REML = F) # adding sex

cy_mm1.2 <- lme4::lmer(nihtbx_cryst_uncorrected.s ~ age_yrs + schooling_yrs + pgs.s + ses_ppca.s + (1 | site_id_l),
                       data = cryst_data_pca.complete, REML = F) # adding sex


# 
# cy_mm2.1 <- lme4::lmer(nihtbx_cryst_uncorrected.s ~ age_yrs + sex + schooling_yrs + pgs.s + (1 | site_id_l), 
#                        data = cryst_data_pca.complete, REML = F)  # adding pgs

cy_mm2.2 <- lmerTest::lmer(nihtbx_cryst_uncorrected.s ~ age_yrs + schooling_yrs + sex  + pgs.s + ses_ppca.s + 
                             C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l), # controling interactions Keller 2014
                           data = cryst_data_pca.complete, REML = F)
summary(cy_mm2.2)
cy_mm2.2_imp <- with(cryst_imp, lmer(dv ~ 1 + age + school + sex + pgs + ses + (1 | site), REML = FALSE))
summary(pool(cy_mm2.2_imp))

# cy_mm3.1 <- lme4::lmer(nihtbx_cryst_uncorrected.s ~ age_yrs + sex + schooling_yrs*pgs.s + ses_ppca.s + (1 | site_id_l), 
#                        data = cryst_data_pca.complete, REML = F) # adding interaction PGS
# cy_mm3.2 <- lme4::lmer(nihtbx_cryst_uncorrected.s ~ age_yrs + sex + schooling_yrs*ses_ppca.s + pgs.s + (1 | site_id_l), 
#                        data = cryst_data_pca.complete, REML = F) # adding interaction SES

# two way interactions, controlling for two-way age (this brought the B schooling*SES and schooling*PGS down; classic Keller 2014 potential false positive)
# cy_mm4_no3way_noagecontrol <- lme4::lmer(nihtbx_cryst_uncorrected.s ~ age_yrs + sex + schooling_yrs + ses_ppca.s + pgs.s + 
#                               schooling_yrs:ses_ppca.s + schooling_yrs:pgs.s + pgs.s:ses_ppca.s + (1 | site_id_l), # controling interactions Keller 2014
#                             data = cryst_data_pca.complete, REML = F) # adding interaction SES

cy_mm4_no3way <- lme4::lmer(nihtbx_cryst_uncorrected.s ~ age_yrs + schooling_yrs + sex + pgs.s + ses_ppca.s + 
                              pgs.s:ses_ppca.s + schooling_yrs:pgs.s + schooling_yrs:ses_ppca.s +
                              age_yrs:pgs.s + age_yrs:ses_ppca.s + (1 | site_id_l), # controling interactions Keller 2014
                            data = cryst_data_pca.complete, REML = F) # adding interaction SES


cy_mm4_no3way <- lmerTest::lmer(nihtbx_cryst_uncorrected.s ~ age_yrs + schooling_yrs + sex + pgs.s + ses_ppca.s + 
                              pgs.s:ses_ppca.s + schooling_yrs:pgs.s + schooling_yrs:ses_ppca.s +
                              age_yrs:pgs.s + age_yrs:ses_ppca.s + 
                              C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l), # controling interactions Keller 2014
                            data = cryst_data_pca.complete, REML = F)

cy_mm4 <- lme4::lmer(nihtbx_cryst_uncorrected.s ~ age_yrs + schooling_yrs + sex + pgs.s + ses_ppca.s + 
                       pgs.s:ses_ppca.s + schooling_yrs:pgs.s + schooling_yrs:ses_ppca.s +
                       age_yrs:pgs.s + age_yrs:ses_ppca.s + # controling interactions Keller 2014
                       schooling_yrs:ses_ppca.s:pgs.s + (1 | site_id_l), 
                     data = cryst_data_pca.complete, REML = F) # adding interaction SES

anova(cy_mm4, cy_mm4_no3way)

cy_mm4_no3way_imp <- with(cryst_imp, lmer(dv ~ 1 + age + school + sex + pgs + ses + pgs:ses + pgs:school + ses:school + age:pgs + age:ses + 
                                            (1 | site), REML = FALSE))

cy_mm4_imp <- with(cryst_imp, lmer(dv ~ 1 + age + school + sex + pgs + ses + pgs:ses + pgs:school + ses:school + age:pgs + age:ses + 
                                     ses:pgs:school + (1 | site), REML = FALSE))

D3(cy_mm4_imp, cy_mm4_no3way_imp) # summary(pool(cy_mm4_imp))

# fluid IQ
# fi_mm1.1 <- lme4::lmer(nihtbx_fluidcomp_uncorrected.s ~ age_yrs + schooling_yrs + (1 | site_id_l), 
#                        data = fluid_data_pca.complete, REML = F)
 fi_mm1.2 <- lme4::lmer(nihtbx_fluidcomp_uncorrected.s ~ age_yrs + schooling_yrs + sex + (1 | site_id_l),
                        data = fluid_data_pca.complete, REML = F) # adding sex
# fi_mm2.1 <- lme4::lmer(nihtbx_fluidcomp_uncorrected.s ~ age_yrs + sex + schooling_yrs + pgs.s + (1 | site_id_l), 
#                        data = fluid_data_pca.complete, REML = F) # adding pgs 

fi_mm2.2 <- lmerTest::lmer(nihtbx_fluidcomp_uncorrected.s ~ age_yrs + schooling_yrs + sex + pgs.s + ses_ppca.s + 
                             C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l), # controling interactions Keller 2014
                       data = fluid_data_pca.complete, REML = F) # adding SES 
summary(fi_mm2.2)
fi_mm2.2_imp <- with(fluid_imp, lmer(dv ~ 1 + age + school + sex + pgs + ses + (1 | site), REML = FALSE))
summary(pool(fi_mm2.2_imp))

# fi_mm3.1 <- lme4::lmer(nihtbx_fluidcomp_uncorrected.s ~ age_yrs + sex + schooling_yrs*pgs.s + ses_ppca.s + (1 | site_id_l), 
#                        data = fluid_data_pca.complete, REML = F) # adding schooling pgs INTERACTION
# fi_mm3.2 <- lme4::lmer(nihtbx_fluidcomp_uncorrected.s ~ age_yrs + sex + schooling_yrs*ses_ppca.s + pgs.s + (1 | site_id_l), 
#                        data = fluid_data_pca.complete, REML = F) # adding schooling SES INTERACTION
# fi_mm4_no3way_noagecontrol <- lme4::lmer(nihtbx_fluidcomp_uncorrected.s ~ age_yrs + schooling_yrs + sex + pgs.s + ses_ppca.s + 
#                                            pgs.s:ses_ppca.s + schooling_yrs:pgs.s + schooling_yrs:ses_ppca.s + 
#                                            (1 | site_id_l), data = fluid_data_pca.complete, REML = F)


fi_mm4_no3way <- lme4::lmer(nihtbx_fluidcomp_uncorrected.s ~ age_yrs + schooling_yrs + sex + pgs.s + ses_ppca.s + 
                              pgs.s:ses_ppca.s + schooling_yrs:pgs.s + schooling_yrs:ses_ppca.s + 
                              age_yrs:pgs.s + age_yrs:ses_ppca.s + (1 | site_id_l), 
                            data = fluid_data_pca.complete, REML = F)

fi_mm4 <- lme4::lmer(nihtbx_fluidcomp_uncorrected.s ~ age_yrs + schooling_yrs + sex + pgs.s + ses_ppca.s + 
                       pgs.s:ses_ppca.s + schooling_yrs:pgs.s + schooling_yrs:ses_ppca.s + 
                       age_yrs:pgs.s + age_yrs:ses_ppca.s + 
                       schooling_yrs:ses_ppca.s:pgs.s + (1 | site_id_l), 
                     data = fluid_data_pca.complete, REML = F)

anova(fi_mm4, fi_mm4_no3way)

fi_mm4_no3way_imp <- with(fluid_imp, lmer(dv ~ 1 + age + school + sex + pgs + ses + pgs:ses + pgs:school + ses:school + age:pgs + age:ses + 
                                            (1 | site), REML = FALSE))

fi_mm4_imp <- with(fluid_imp, lmer(dv ~ 1 + age + school + sex + pgs + ses + pgs:ses + pgs:school + ses:school + age:pgs + age:ses + 
                                     ses:pgs:school + (1 | site), REML = FALSE))

D3(fi_mm4_imp, fi_mm4_no3way_imp) # summary(pool(fi_mm4_imp))


# for list sorting
# list_mm1.1 <- lme4::lmer(nihtbx_list_uncorrected.s ~ age_yrs + schooling_yrs + (1 | site_id_l), 
#                          data = list_data_pca.complete, REML = F)
list_mm1.2 <- lme4::lmer(nihtbx_list_uncorrected.s ~ age_yrs + schooling_yrs + sex + (1 | site_id_l),
                          data = list_data_pca.complete, REML = F)
# 
# list_mm2.1 <- lme4::lmer(nihtbx_list_uncorrected.s ~ age_yrs + sex + schooling_yrs + pgs.s + (1 | site_id_l), 
#                          data = list_data_pca.complete, REML = F)

list_mm2.2 <- lmerTest::lmer(nihtbx_list_uncorrected.s ~ age_yrs + schooling_yrs + sex + pgs.s + ses_ppca.s + (1 | site_id_l), 
                         data = list_data_pca.complete, REML = F)
summary(list_mm2.2)
list_mm2.2_imp <- with(list_imp, lmer(dv ~ 1 + age + school + sex + pgs + ses + (1 | site), REML = FALSE))
summary(pool(list_mm2.2_imp))


# list_mm3.1 <- lme4::lmer(nihtbx_list_uncorrected.s ~ age_yrs + sex + schooling_yrs*pgs.s + ses_ppca.s + (1 | site_id_l), 
#                          data = list_data_pca.complete, REML = F)
# list_mm3.2 <- lme4::lmer(nihtbx_list_uncorrected.s ~ age_yrs + sex + schooling_yrs*ses_ppca.s + pgs.s + (1 | site_id_l), 
#                          data = list_data_pca.complete, REML = F)

# list_mm4_no3way_noagecontrol <- lme4::lmer(nihtbx_list_uncorrected.s ~ age_yrs + sex + schooling_yrs + ses_ppca.s + pgs.s + 
#                                 schooling_yrs:ses_ppca.s + schooling_yrs:pgs.s + pgs.s:ses_ppca.s + (1 | site_id_l), 
#                               data = list_data_pca.complete, REML = F) # adding interaction SES


list_mm4_no3way <- lme4::lmer(nihtbx_list_uncorrected.s ~ age_yrs + schooling_yrs + sex + pgs.s + ses_ppca.s + 
                                pgs.s:ses_ppca.s + schooling_yrs:pgs.s + schooling_yrs:ses_ppca.s +  
                                age_yrs:pgs.s + age_yrs:ses_ppca.s + (1 | site_id_l), 
                            data = list_data_pca.complete, REML = F) # adding interaction SES

list_mm4 <- lme4::lmer(nihtbx_list_uncorrected.s ~ age_yrs + schooling_yrs + sex + pgs.s + ses_ppca.s + 
                         pgs.s:ses_ppca.s + schooling_yrs:pgs.s + schooling_yrs:ses_ppca.s +  
                         age_yrs:pgs.s + age_yrs:ses_ppca.s + 
                         schooling_yrs:ses_ppca.s:pgs.s + (1 | site_id_l), 
                       data = list_data_pca.complete, REML = F) # adding interaction SES
anova(list_mm4, list_mm4_no3way)


list_mm4_no3way_imp <- with(list_imp, lmer(dv ~ 1 + age + school + sex + pgs + ses + pgs:ses + pgs:school + ses:school + age:pgs + age:ses + (1 | site), REML = FALSE))


list_mm4_imp <- with(list_imp, lmer(dv ~ 1 + age + school + sex + pgs + ses + pgs:ses + pgs:school + ses:school + age:pgs + age:ses + 
                                      ses:pgs:school + (1 | site), REML = FALSE))
D3(list_mm4_imp, list_mm4_no3way_imp) # summary(pool(list_mm4_imp))

##################################################################
########### SES subcomponent analysis ########### 

# imputations are commented off since they take forever...
# #imp cryst
# cryst_imp_par <- imp_3way(cog[, c("site_id_l", "nihtbx_cryst_uncorrected.s", "ParEd_max.s", "pgs.s", "age_yrs", "schooling_yrs", "sex")])
# cryst_imp_income <- imp_3way(cog[, c("site_id_l", "nihtbx_cryst_uncorrected.s", "demo_comb_income_v2.s", "pgs.s", "age_yrs", "schooling_yrs", "sex")])
# cryst_imp_neigh <- imp_3way(cog[, c("site_id_l", "nihtbx_cryst_uncorrected.s", "reshist_addr1_adi_wsum.s", "pgs.s", "age_yrs", "schooling_yrs", "sex")])
# #imp fluid
# fluid_imp_par <- imp_3way(cog[, c("site_id_l", "nihtbx_fluidcomp_uncorrected.s", "ParEd_max.s", "pgs.s", "age_yrs", "schooling_yrs", "sex")])
# fluid_imp_income <- imp_3way(cog[, c("site_id_l", "nihtbx_fluidcomp_uncorrected.s", "demo_comb_income_v2.s", "pgs.s", "age_yrs", "schooling_yrs", "sex")])
# fluid_imp_neigh <- imp_3way(cog[, c("site_id_l", "nihtbx_fluidcomp_uncorrected.s", "reshist_addr1_adi_wsum.s", "pgs.s", "age_yrs", "schooling_yrs", "sex")])
# #imp list
# list_imp_par <- imp_3way(cog[, c("site_id_l", "nihtbx_list_uncorrected.s", "ParEd_max.s", "pgs.s", "age_yrs", "schooling_yrs", "sex")])
# list_imp_income <- imp_3way(cog[, c("site_id_l", "nihtbx_list_uncorrected.s", "demo_comb_income_v2.s", "pgs.s", "age_yrs", "schooling_yrs", "sex")])
# list_imp_neigh <- imp_3way(cog[, c("site_id_l", "nihtbx_list_uncorrected.s", "reshist_addr1_adi_wsum.s", "pgs.s", "age_yrs", "schooling_yrs", "sex")])

# analysis cryst subcomp
cy_mm2.2_imp_par <- with(cryst_imp_par, lmer(dv ~ 1 + age + sex + school + pgs + ses + (1 | site), REML = FALSE))
summary(pool(cy_mm2.2_imp_par))[6,]
cy_mm2.2_imp_income <- with(cryst_imp_income, lmer(dv ~ 1 + age + sex + school + pgs + ses + (1 | site), REML = FALSE))
summary(pool(cy_mm2.2_imp_income))[6,]
cy_mm2.2_imp_neigh <- with(cryst_imp_neigh, lmer(dv ~ 1 + age + sex + school + pgs + ses + (1 | site), REML = FALSE))
summary(pool(cy_mm2.2_imp_neigh))[6,]

# analysis fluid subcomp
fi_mm2.2_imp_par <- with(fluid_imp_par, lmer(dv ~ 1 + age + sex + school + pgs + ses + (1 | site), REML = FALSE))
summary(pool(fi_mm2.2_imp_par))[6,]
fi_mm2.2_imp_income <- with(fluid_imp_income, lmer(dv ~ 1 + age + sex + school + pgs + ses + (1 | site), REML = FALSE))
summary(pool(fi_mm2.2_imp_income))[6,]
fi_mm2.2_imp_neigh <- with(fluid_imp_neigh, lmer(dv ~ 1 + age + sex + school + pgs + ses + (1 | site), REML = FALSE))
summary(pool(fi_mm2.2_imp_neigh))[6,]

# analysis list subcomp
list_mm2.2_imp_par <- with(list_imp_par, lmer(dv ~ 1 + age + sex + school + pgs + ses + (1 | site), REML = FALSE))
summary(pool(list_mm2.2_imp_par))[6,]
list_mm2.2_imp_income <- with(list_imp_income, lmer(dv ~ 1 + age + sex + school + pgs + ses + (1 | site), REML = FALSE))
summary(pool(list_mm2.2_imp_income))[6,]
list_mm2.2_imp_neigh <- with(list_imp_neigh, lmer(dv ~ 1 + age + sex + school + pgs + ses + (1 | site), REML = FALSE))
summary(pool(list_mm2.2_imp_neigh))[6,]

# for the interaction, when sig only looking at the term that shows for the composite (yet ofc still controlling for everything else)

# cryst
# cy_fit_fixed_par <- with(cryst_imp_par, lmer(dv ~ 1 + age + school + sex + ses + pgs + (1 | site), REML = FALSE))
cy_fit_inter_par <- with(cryst_imp_par, lmer(dv ~ 1 + age + school + sex + ses*pgs + school*ses + school*pgs + age*ses + age*pgs + (1 | site), REML = FALSE))
summary(pool(cy_fit_inter_par))

# cy_fit_fixed_income <- with(cryst_imp_income, lmer(dv ~ 1 + age + school + sex + ses + pgs + (1 | site), REML = FALSE))
cy_fit_inter_income <- with(cryst_imp_income, lmer(dv ~ 1 + age + school + sex + ses*pgs + school*ses + school*pgs + age*ses + age*pgs + (1 | site), REML = FALSE))
summary(pool(cy_fit_inter_income))

# cy_fit_fixed_neigh <- with(cryst_imp_neigh, lmer(dv ~ 1 + age + school + sex + ses + pgs + (1 | site), REML = FALSE))
cy_fit_inter_neigh <- with(cryst_imp_neigh, lmer(dv ~ 1 + age + school + sex + ses*pgs + school*ses + school*pgs + age*ses + age*pgs + (1 | site), REML = FALSE))
summary(pool(cy_fit_inter_neigh))

# fluid
# fi_fit_fixed_par <- with(fluid_imp_par, lmer(dv ~ 1 + age + school + sex + ses + pgs + (1 | site), REML = FALSE))
fi_fit_inter_par <- with(fluid_imp_par, lmer(dv ~ 1 + age + school + sex + ses*pgs + school*ses + school*pgs + age*ses + age*pgs + (1 | site), REML = FALSE))
summary(pool(fi_fit_inter_par))

# fi_fit_fixed_income <- with(fluid_imp_income, lmer(dv ~ 1 + age + school + sex + ses + pgs + (1 | site), REML = FALSE))
fi_fit_inter_income <- with(fluid_imp_income, lmer(dv ~ 1 + age + school + sex + ses*pgs + school*ses + school*pgs + age*ses + age*pgs + (1 | site), REML = FALSE))
summary(pool(fi_fit_inter_income))


# fi_fit_fixed_neigh <- with(fluid_imp_neigh, lmer(dv ~ 1 + age + school + sex + ses + pgs + (1 | site), REML = FALSE))
fi_fit_inter_neigh <- with(fluid_imp_neigh, lmer(dv ~ 1 + age + school + sex + ses*pgs + school*ses + school*pgs + age*ses + age*pgs + (1 | site), REML = FALSE))
summary(pool(fi_fit_inter_neigh))

##################################################################
########### SI tables & results ########### 
# Results
# comparing and constrating mixed-model vs no grouping
# fi_m1;fi_mm1
# cy_m1;cy_mm1 # pretty much the exact same result, yet effects from aging go up and schooling goes down with a random intercept

# seeing the results from Model 2 (with only a fixed effect of PGS)
# summary(lmerTest::as_lmerModLmerTest(fi_mm2))
# summary(lmerTest::as_lmerModLmerTest(cy_mm2))
# summary(lmerTest::as_lmerModLmerTest(list_mm2))

# Plotting of results
# plot_model(fi_mm2, type = "est") + ylim(0, .5) + theme_minimal()
# plot_model(cy_mm2, type = "est") + ylim(0, .5) + theme_minimal()
# plot_model(list_mm2, type = "est") + ylim(0, .5) + theme_minimal()

# Cryst
# sjPlot::tab_model(cy_mm2.2, cy_mm4_no3way, cy_mm4,
#                   show.loglik = T, show.aic = T, digits = 3, # p.val = "kr"
#                   p.style = "numeric",show.se = T, show.ci = NULL,
#                   file = "~/Projects/R_projects/ABCDschooling/tabs/cryst_results.html")
# 
# sjPlot::tab_model(fi_mm2.2, fi_mm4_no3way, fi_mm4,
#                   show.loglik = T, show.aic = T, digits = 3, # p.val = "kr"
#                   p.style = "numeric",show.se = T, show.ci = NULL,
#                   file = "~/Projects/R_projects/ABCDschooling/tabs/fluid_results.html")
# sjPlot::tab_model(list_mm2.2, list_mm4_no3way, list_mm4,
#                   show.loglik = T, show.aic = T, digits = 3, # p.val = "kr"
#                   p.style = "numeric",show.se = T, show.ci = NULL,
#                   file = "~/Projects/R_projects/ABCDschooling/tabs/wm_results.html")

# base model tables
# sjPlot::tab_model(cy_mm2.2, fi_mm2.2, list_mm2.2,
#                   show.loglik = T, show.aic = T, digits = 3, # p.val = "kr"
#                   p.style = "numeric",show.se = T, show.ci = NULL,
#                   file = "~/Projects/R_projects/ABCDschooling/tabs/base_mods.html")
# 
# sjPlot::tab_model(cy_mm4, fi_mm4, list_mm4,
#                   show.loglik = T, show.aic = T, digits = 3, # p.val = "kr"
#                   p.style = "numeric",show.se = T, show.ci = NULL,
#                   file = "~/Projects/R_projects/ABCDschooling/tabs/schooling_intermods.html")

# making imputation tables... the nice fucntion no longer works...

cy_IMPresults <- setDT(summary(pool(cy_mm2.2_imp)))[
  setDT(summary(pool(cy_mm4_no3way_imp))), on = 'term'][
    setDT(summary(pool(cy_mm4_imp))), on = 'term'][
      , c("statistic", "df", "i.statistic", "i.df", "i.statistic.1", "i.df.1") := NULL][
        , lapply(.SD, round, 3), term # rounding all values to 3
      ] #joining full model
colnames(cy_IMPresults) <- c("term", "est_mm2.2", "se_mm2.2", "p_mm2.2", "est_mm4_no3way", "se_mm4_no3way", "p_mm4_no3way", "est_mm4", "se_mm4", "p_mm4")

# now for fluid
fi_IMPresults <- setDT(summary(pool(fi_mm2.2_imp)))[
  setDT(summary(pool(fi_mm4_no3way_imp))), on = 'term'][
    setDT(summary(pool(fi_mm4_imp))), on = 'term'][
      , c("statistic", "df", "i.statistic", "i.df", "i.statistic.1", "i.df.1") := NULL][
        , lapply(.SD, round, 3), term # rounding all values to 3
        ] #joining full model
colnames(fi_IMPresults) <- c("term", "est_mm2.2", "se_mm2.2", "p_mm2.2", "est_mm4_no3way", "se_mm4_no3way", "p_mm4_no3way", "est_mm4", "se_mm4", "p_mm4")
# now for WM
wm_IMPresults <- setDT(summary(pool(list_mm2.2_imp)))[
  setDT(summary(pool(list_mm4_no3way_imp))), on = 'term'][
    setDT(summary(pool(list_mm4_imp))), on = 'term'][
      , c("statistic", "df", "i.statistic", "i.df", "i.statistic.1", "i.df.1") := NULL][
        , lapply(.SD, round, 3), term # rounding all values to 3
        ] #joining full model
colnames(wm_IMPresults) <- c("term", "est_mm2.2", "se_mm2.2", "p_mm2.2", "est_mm4_no3way", "se_mm4_no3way", "p_mm4_no3way", "est_mm4", "se_mm4", "p_mm4")

# saving the tables
# library(kableExtra); library(magrittr)
# kbl(cy_IMPresults) %>%
#   kable_styling() %>%
#   save_kable(file = "~/Projects/R_projects/ABCDschooling/tabs/cryst_results_imp.html", self_contained = T)
# kbl(fi_IMPresults) %>%
#   kable_styling() %>%
#   save_kable(file = "~/Projects/R_projects/ABCDschooling/tabs/fluid_results_imp.html", self_contained = T)
# kbl(wm_IMPresults) %>%
#   kable_styling() %>%
#   save_kable(file = "~/Projects/R_projects/ABCDschooling/tabs/wm_results_imp.html", self_contained = T)


kableExtra::kable_styling(kableExtra::kable(cy_IMPresults))


# I kept the most important imputation checks here
# xyplot(imp, pgs ~ ses, na.groups = miss
#        , cex = c(.3, .2), pch = c(1, 20),
#        ylab = "ses", xlab = "pgs calculated")
# 
# 
# xyplot(imp, pgs ~ fluid, na.groups = miss
#        , cex = c(.3, .2), pch = c(1, 20),
#        ylab = "fluid", xlab = "pgs calculated")
# 
# densityplot(imp, ~pgs) # observed vs imputed
# lattice::histogram(~nihtbx_cryst_uncorrected.s | is.na(pgs.s), data=cryst_data_pca)
# lattice::histogram(~ses_ppca.s | is.na(pgs.s), data=cryst_data_pca)

##################################################################
########### Main manuscript plotting ########### 
# figure 1; making a plot of the main effects
eff_plt <- data.table(DV = c(rep("fIQ", 4), rep("cIQ", 4), rep("WM", 4)),
                      var = c(rep(c("Age 1yr", "School 1yr", "cog-PGS", "SES"), 3)),
           beta = c(
             as.numeric(summary(fi_mm2.2)$coefficients[,1][c(2,3,5,6)]), # grabing the beta terms for age, schooling, pgs & ses (in the same order as the var I made)
             as.numeric(summary(cy_mm2.2)$coefficients[,1][c(2,3,5,6)]), 
             as.numeric(summary(list_mm2.2)$coefficients[,1][c(2,3,5,6)]) # gender is last in the model so it works
           ), 
           ci_low = c( # getting the lower confidence intervals for the 4 terms for each 3 models
             as.numeric(confint(fi_mm2.2)[c(4,5,7,8), 1]),as.numeric(confint(cy_mm2.2)[c(4,5,7,8), 1]), as.numeric(confint(list_mm2.2)[c(4,5,7,8), 1])
           ), 
           ci_high= c( # getting the higher confidence intervals for the 4 terms for each 3 models
             as.numeric(confint(fi_mm2.2)[c(4,5,7,8), 2]),as.numeric(confint(cy_mm2.2)[c(4,5,7,8), 2]), as.numeric(confint(list_mm2.2)[c(4,5,7,8), 2])
           ))

barCOLS <- c('#DE3163', '#6495ED', '#9FE2BF') # a vector of colors

eff_plt$var <- factor(eff_plt$var, levels = c("Age 1yr", "School 1yr", "cog-PGS", "SES")) # making var a factor for ggplot, ordering them as I wish as well
eff_plt$DV <- factor(eff_plt$DV) # making the DV as a factor

main_eff_Dplt <- ggplot(eff_plt, aes(var, beta, ymin = ci_low, ymax = ci_high, col = DV, fill = DV)) +
  geom_linerange(size=1.5,position=position_dodge(width = 0.5)) +
  #geom_hline(yintercept=0, lty=2) +
  geom_point(size=2.5, shape=21, colour="black", stroke = 0.5, position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS) +
  scale_color_manual(values=barCOLS) +
  scale_x_discrete(name="") + # , breaks = NULL
  scale_y_continuous(name="Effect in SD", limits = c(0, .4)) +
  theme_minimal(base_size = 20) +
  labs(color = " ", fill = " ") +
  ggExtra::removeGridX()

ggsave("figs/main_eff_Dplt.png", main_eff_Dplt, width = 10, height = 4)

# Figure 2; results of the cog-PGS*SES interaction
# plot_model(cy_mm_inter, type = "pred", terms = c("pgs.s [-2, 0, 2]", "ses_ppca.s [-2, 0, 2]")) + theme_minimal()
# plot_model(fi_mm_inter, type = "pred", terms = c("pgs.s [-2, 0, 2]", "ses_ppca.s [-2, 0, 2]")) + theme_minimal()


library(ggeffects)

mydf_cy <- ggpredict(cy_mm4_no3way, terms = c("pgs.s [-2, 0, 2]", "ses_ppca.s [-2, 0, 2]"))
mydf_fi <- ggpredict(fi_mm4_no3way, terms = c("pgs.s [-2, 0, 2]", "ses_ppca.s [-2, 0, 2]"))

cy_plt <- plot(mydf_cy) + 
  scale_color_manual(values=c("#64CEED", "#6495ED", "#8164ED")) +
  scale_fill_manual(values=c("#64CEED", "#6495ED", "#8164ED")) +
  theme_minimal(base_size = 25) +
  labs(title = "", x = "cog-PGS (SD)", y = "cIQ")+
  guides(color = guide_legend(reverse = T, "SES (SD)", override.aes=list(shape=15, size = 5, fill=NA))) +
  theme(legend.position= "none") +
  ggExtra::removeGridX()

fi_plt <- plot(mydf_fi) + 
  scale_color_manual(values=c("#64CEED", "#6495ED", "#8164ED"), name = "SES (SD)") +
  scale_fill_manual(values = c("#64CEED", "#6495ED", "#8164ED")) + # c('gray', 'gray', 'gray')
  theme_minimal(base_size = 25) +
  labs(title = "", x = "cog-PGS (SD)", y = "fIQ") +
  guides(color = guide_legend(reverse = T, "SES (SD)", override.aes=list(shape=15, size = 5, fill=NA))) +
  ggExtra::removeGridX() 

  # scale_y_continuous(breaks = c(-1, -.5, 0 , .5, 1))
  # coord_cartesian(ylim = c(-1, -.5, 0 , .5, 1))
  
library(patchwork)
gxe_inter_plt <- cy_plt + fi_plt + plot_annotation(tag_levels = 'A')
ggsave("figs/gxe_inter_plt.png", gxe_inter_plt, width = 20, height = 10)

# Ploting the 3-way interaction trend: nothing to plot, effect does not survive
# plot_model(cy_mm4, type = "pred", terms = c("schooling_yrs", "pgs.s [-1.5, 1.5]", "ses_ppca.s [-1.5, 1.5]"))
# plot_model(cy_mm4, type = "pred", terms = c("schooling_yrs", "pgs.s [-1.5, 0, 1.5]", "ses_ppca.s [-1.5, 0, 1.5]")) + theme_minimal()

##################################################################
########## Model Diagnostic code; may be out of date ###########

# checking model diagnostic plots (only care about plots 2 & 4; 3 is vaguely intersting; ignoring plot 1 see Regression and other stories p. 162)
# fluid IQ
# plot_model(fi_mm1, type = "diag") 
# plot_model(fi_mm2, type = "diag")

# one site is fucked in both models, will update mm2 & mm3 without it
# coef(fi_mm1) # scale(coef(fi_mm1)$site_id_l[,1]) 
# another way to find the bastard... 
# troublesome_site <- lme4::ranef(fi_mm1)[[1]]
# troublesome_site$out <- troublesome_site$`(Intercept)` %in% boxplot(troublesome_site$`(Intercept)`)$out # it is site 15
# 
# fi_mm2_site15out <- update(fi_mm2, data = mod_data[site_id_l != "site15"])
# plot_model(fi_mm2_site15out, type = "diag") #looking good
# 
# fi_mm3_site15out <- update(fi_mm3, data = mod_data[site_id_l != "site15"])
# 
# AIC(fi_mm2_site15out) - AIC(fi_mm3_site15out) # still worse with the interaction

# did the betas change?
# summary(lmerTest::as_lmerModLmerTest(fi_mm2))
# summary(lmerTest::as_lmerModLmerTest(fi_mm2_site15out))
# 
# # crystalized
# plot_model(cy_mm1, type = "diag")
# plot_model(cy_mm2, type = "diag")
# # the residuals vs fit isn't great, look at the distribution of DV
# car::qqPlot(mod_data$nihtbx_cryst_uncorrected) # its just the way it is...
# 
# # list sorting
# plot_model(list_mm1, type = "diag")
# plot_model(list_mm1.2, type = "diag")
# plot_model(list_mm2, type = "diag")
# 
# # influential cases?
# car::influencePlot(fi_mm2)
# car::influencePlot(cy_mm2)
# car::influencePlot(list_mm2)
# 
# # this beatiful function in easystats parameters does everything!
# check_model(fi_mm2)
# check_model(fi_mm3)
# 
# check_model(cy_mm2)
# check_model(cy_mm3)
# 
# check_model(list_mm2)
# check_model(list_mm3)
# 
# compare_performance(fi_mm1, fi_mm1.2, fi_mm2, fi_mm3, rank = TRUE)
# compare_performance(cy_mm1, cy_mm1.2, cy_mm2, cy_mm3, rank = TRUE)
# compare_performance(list_mm1, list_mm1.2, list_mm2, list_mm3, rank = TRUE)



# PNAS rev suggestions (some are good and make sense, some dont)


# could put this table in the SI
performance::check_collinearity(cy_mm2.2)



# R squared with MuMIn package
MuMIn::r.squaredGLMM()




# Regression disc plot 

library(visreg)
visreg_partial_residi_cy <- visreg(cy_mm2.2, "schooling_yrs")
partial_residi_cy <- ggplot(visreg_partial_residi_cy$res, aes(schooling_yrs, visregRes)) + geom_point(size=1, alpha= 0.5, color = "black") + geom_smooth(method = "lm", color = "black") +
  labs(y = "Partial Residuals of cIQ", x = "Years of schooling") + theme_minimal(base_size = 15) +
  #  scale_x_continuous(breaks=c(-3, -1.5, 0, 1.5, 3), labels = c('-3', '-1.5', '0', '1.5', '3'), limits = c(-2.5, 2)) +
  scale_y_continuous(breaks=c(-2, -1, 0, 1, 2), labels = c('-3', '-1.5', '0', '1.5', '3'), limits = c(-2, 2)) 


partial_residi_cy <- ggplot(visreg_partial_residi_cy$res, aes(schooling_yrs, visregRes)) + 
  geom_jitter(size=1, alpha= 0.5, color = "black") + geom_smooth(method = "lm", color = "black") +
  labs(y = "Partial Residuals of cIQ", x = "Years of schooling") + theme_minimal(base_size = 15) +
  #  scale_x_continuous(breaks=c(-3, -1.5, 0, 1.5, 3), labels = c('-3', '-1.5', '0', '1.5', '3'), limits = c(-2.5, 2)) +
  scale_y_continuous(breaks=c(-2, -1, 0, 1, 2), labels = c('-3', '-1.5', '0', '1.5', '3'), limits = c(-2, 2)) 






visreg_partial_residi_fi <- visreg(fi_mm2.2, "schooling_yrs")
partial_residi_fi <- ggplot(visreg_partial_residi_fi$res, aes(schooling_yrs, visregRes)) + geom_point(size=1, alpha= 0.5, color = "black") + geom_smooth(method = "lm", color = "black") +
  labs(y = "Partial Residuals of cIQ", x = "Years of schooling") + theme_minimal(base_size = 15) +
  #  scale_x_continuous(breaks=c(-3, -1.5, 0, 1.5, 3), labels = c('-3', '-1.5', '0', '1.5', '3'), limits = c(-2.5, 2)) +
  scale_y_continuous(breaks=c(-2, -1, 0, 1, 2), labels = c('-3', '-1.5', '0', '1.5', '3'), limits = c(-2, 2)) 

visreg_partial_residi_list <- visreg(list_mm2.2, "schooling_yrs")
partial_residi_list <- ggplot(visreg_partial_residi_list$res, aes(schooling_yrs, visregRes)) + geom_point(size=1, alpha= 0.5, color = "black") + geom_smooth(method = "lm", color = "black") +
  labs(y = "Partial Residuals of cIQ", x = "Years of schooling") + theme_minimal(base_size = 15) +
  #  scale_x_continuous(breaks=c(-3, -1.5, 0, 1.5, 3), labels = c('-3', '-1.5', '0', '1.5', '3'), limits = c(-2.5, 2)) +
  scale_y_continuous(breaks=c(-2, -1, 0, 1, 2), labels = c('-2', '-1', '0', '2', '2'), limits = c(-2, 2)) 



partial_residi_cy + partial_residi_fi + partial_residi_list



######### plotting with density


### some small things, you could add the main effect of schooling to all the residual values so ability starts at zero,
### also the regression line should be the estimated slope, not just estimated for the graph.

get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

visreg_partial_residi_cy <- visreg(cy_mm2.2, "schooling_yrs")
visreg_partial_residi_fi <- visreg(fi_mm2.2, "schooling_yrs")
visreg_partial_residi_list <- visreg(list_mm2.2, "schooling_yrs")

visreg_partial_residi_cy <- visreg_partial_residi_cy$res
visreg_partial_residi_fi <- visreg_partial_residi_fi$res
visreg_partial_residi_list <- visreg_partial_residi_list$res


visreg_partial_residi_cy$dens <- get_density(visreg_partial_residi_cy$schooling_yrs, visreg_partial_residi_cy$visregRes, n =100)
visreg_partial_residi_fi$dens <- get_density(visreg_partial_residi_fi$schooling_yrs, visreg_partial_residi_fi$visregRes, n =100)
visreg_partial_residi_list$dens <- get_density(visreg_partial_residi_list$schooling_yrs, visreg_partial_residi_list$visregRes, n =100)

visreg_partial_residi_cy$dens <- -as.numeric(scale(visreg_partial_residi_cy$dens))
visreg_partial_residi_fi$dens <- -as.numeric(scale(visreg_partial_residi_fi$dens))
visreg_partial_residi_list$dens <- -as.numeric(scale(visreg_partial_residi_list$dens))


cy_school_plt <- ggplot(visreg_partial_residi_cy, aes(x = schooling_yrs, y = visregRes + 0.20166)) + 
  geom_jitter(aes(color = dens), alpha = 0.2, width = .01) + # width = .5)
  geom_smooth(method = "lm", color = "black") + # you should put the actual model estimated slope
  theme_minimal() +
  theme(legend.position = "none", panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylim(-1.5,1.5) +
  labs(title = "", x = "Years of Schooling", y = "Partial Residuals of cIQ") +
  geom_abline(slope = 0.20166, intercept = 0, color = 'black', size = 2)
  # theme_minimal(base_size = 35)
  #viridis::scale_color_viridis(direction = 1, option = "A")




ggplot(visreg_partial_residi_cy, aes(x = schooling_yrs, y = visregRes)) + 
  geom_jitter(aes(color = dens), alpha = 0.2, width = .01) + # width = .5)
  geom_smooth(method = "lm", color = "black") + # you should put the actual model estimated slope
  theme_minimal() +
  theme(legend.position = "none", panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylim(-1.5,1.5) +
  labs(title = "", x = "Years of Schooling", y = "Partial Residuals of cIQ") +
  geom_abline(slope = 0.20166, intercept = -0.20166, color = 'black', size = 2, color = "red", alpha = .3)
# theme_minimal(base_size = 35)
#viridis::scale_color_viridis(direction = 1, option = "A")


# https://stackoverflow.com/questions/44960410/how-to-limit-the-length-of-abline-in-ggplot2-using-slope-and-intercept
# https://stackoverflow.com/questions/37641831/constraining-an-abline-in-ggplot2

# *********************
# SES looks like it works, you should be careful with fucking with it!!!!*********
# *********************

visreg(cy_mm2.2, "ses_ppca.s")
visreg_partial_residi_SES <- visreg(cy_mm2.2, "ses_ppca.s")
visreg_partial_residi_SES <- visreg_partial_residi_SES$res
visreg_partial_residi_SES$dens <- get_density(visreg_partial_residi_SES$ses_ppca.s, visreg_partial_residi_SES$visregRes, n =100)
visreg_partial_residi_SES$dens <- -as.numeric(scale(visreg_partial_residi_SES$dens))
ggplot(visreg_partial_residi_SES, aes(x = ses_ppca.s, y = visregRes)) + 
  geom_jitter(aes(color = dens), alpha = 0.2, width = .01) + # width = .5)
  geom_smooth(method = "lm", color = "black") + # you should put the actual model estimated slope
  theme_minimal() 

# why are these two lines so different...?



  
fi_school_plt <- ggplot(visreg_partial_residi_fi, aes(x = schooling_yrs, y = visregRes)) + 
  geom_jitter(aes(color = dens), alpha = 0.2, width = .01) + # width = .5)
  geom_smooth(method = "lm", color = "black") +
  theme_minimal() +
  theme(legend.position = "none", panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylim(-2,2) +
  labs(title = "", x = "Years of Schooling", y = "Partial Residuals of fIQ")
# theme_minimal(base_size = 35)
#viridis::scale_color_viridis(direction = 1, option = "A")

list_school_plt <- ggplot(visreg_partial_residi_list, aes(x = schooling_yrs, y = visregRes)) + 
  geom_jitter(aes(color = dens), alpha = 0.2, width = .01) + # width = .5)
  geom_smooth(method = "lm", color = "black") +
  theme_minimal() +
  theme(legend.position = "none", panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylim(-2,2) +
  labs(title = "", x = "Years of Schooling", y = "Partial Residuals of WM")
# theme_minimal(base_size = 35)
#viridis::scale_color_viridis(direction = 1, option = "A")



cy_school_plt / fi_school_plt / list_school_plt





#### 

# you need to residualize age by interview date,
# y axis can be IQ, x is age and than you group by the factor grade


# it will look super bad because of heterogeniety between schools.

# just subtract interview month from interview age

cog$age_sub <- cog$interview_age - cog$interview_mnth

# yet this is a problem since you haven't subtracted interview_mnt from cognition

# Say something that we intially wanted to base our analysis on the unit of grade rather than montsh of schooling.
# while both are identical for regression disc. assumptions since recruitment was yearly we cannot break student by grade.
# if we broke them by grade we would be conflating schooling and age.

# cognition includes age + school, since students were testing on a year around basis

# making a partial residual plto for IQ

cog$grade <- as.factor(cog$grade)
cog$demo_ed_v2 <- as.factor(cog$demo_ed_v2)

ggplot(cog[demo_ed_v2 != 3], aes(age_sub, nihtbx_cryst_uncorrected.s, color = demo_ed_v2)) +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  ylim(-2,2)

ggplot(cog, aes(age_sub, nihtbx_cryst_uncorrected.s, color = demo_ed_v2)) +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm") +
  theme_minimal()





