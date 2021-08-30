### Nicholas Judd 
# Dep. Neuro
# Karolinska Institute
# 2021-07-11 
# nickjudd@gmail.com

##################################################################
########### Main schooling analysis script: Notes ########### 



# make an SI table with psych::describe() add VIFs?

# maybe distribution plots in SI?

# read the paper Rev2 gave and look into sibling analysis

# could do Raw cor with Cy, Fi, list with PGS & than show the amount of genetic nurture?

# need to figure out how to do it...

# There are reasons to expect the correlation of PGS with IQ to be over estimated due to indirect genetic effects.
# While previous studies have found these indirect genetic effects to be mediated via SES? (check this)
# We used XXX siblings to estimate genetic nuture ***, we found the effect to decrease *, *, *.
# Yet sibling analysis **** limitations. 










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
library(data.table); library(ggplot2); library(effectsize); library(lubridate); 
library(lme4); library(sjPlot); library(performance); library(mice); library(broom.mixed) # need for pool in mice
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
cog <- fread('data/pca_data_ethnicity_PC20.txt', fill = T)[,1:22][cog, on = "subjectkey"] #[name == "White"]

cog.complete <- cog[kbi_y_grade_repeat ==0] # 9% of subjects gone


# IMPUTATION NOTES
# very sad; it is impossible to impute with PCs. They are orthogonal, therefore loads of singularity warnings
# and eventually stops working with higher PCs.
# There is no perfect sollution to this so I will sadly just list this as a limitation
# They're alternatives to controlling for PCs, we could just residualize the PGS for them (Judd et al., 2020 PNAS). 
# Yet this is unsatisfactory as we should control IQ and SES for population stratification. Another alternative is to just 
# residualize the DV (IQ in this case), yet this erronously leads to over conservative PGS & SES effect sizes
# The original PC paper (Price 2006 Nat Gen), residualizes the phenotype(IQ or SES in this case) & genotype (cog-PGS)
# This method is the most similar to just adding the covariates, yet we would have to listwise delete anyways to do that.
# This is a shitty situation that effects SEM approaches with FIML as well.
# cog <- umx::umx_residualize(c("nihtbx_cryst_uncorrected", "nihtbx_fluidcomp_uncorrected", "nihtbx_list_uncorrected", "ses", "pgs", "age_yrs", "schooling_yrs"), c("C1" ,"C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20"), data = cog)

# making dfs with imputed data (uncomment to get imputation results)
# cryst_imp <- imp_3way(cryst_data_pca[, c("site_id_l", "nihtbx_cryst_uncorrected", "ses", "pgs", "age_yrs", "schooling_yrs", "sex")])
# fluid_imp <- imp_3way(fluid_data_pca[, c("site_id_l", "nihtbx_fluidcomp_uncorrected", "ses", "pgs", "age_yrs", "schooling_yrs", "sex")])
# list_imp <- imp_3way(list_data_pca[, c("site_id_l", "nihtbx_list_uncorrected", "ses", "pgs", "age_yrs", "schooling_yrs", "sex")])


# data tidy, making one complete dataset
dvs <- c("nihtbx_cryst_uncorrected", "nihtbx_fluidcomp_uncorrected", "nihtbx_list_uncorrected")
dvs_plus <- c(dvs, "schooling_yrs", "age_yrs")
all_cols <- c(dvs_plus, "pgs",
                 "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")
# subseting the main df

cog.complete <- cog.complete[cog.complete[, complete.cases(.SD), .SDcols = dvs_plus]][
  , (dvs) := lapply(.SD, vec_to_fence), .SDcols=dvs # bringing the dvs to the fence
][
  , c("schooling_yrs.unscaled", "age_yrs.unscaled") := .(schooling_yrs, age_yrs) # making new holding cols that are unscaled for age & school
  ][
    , (all_cols) := lapply(.SD, function(x) as.numeric(scale(x))), .SDcols=all_cols # standardizing all the relevant info
][]

# rescaling the SES components for the ppca
cols_ses <- c("ParEd_max", "demo_comb_income_v2", "reshist_addr1_adi_wsum")
cog.complete[, (cols_ses) := lapply(.SD, function(x) as.numeric(scale(x))), .SDcols=cols_ses]

cog.complete$reshist_addr1_adi_wsum <- -cog.complete$reshist_addr1_adi_wsum # neighborhood deprivation is now called neighborhood quality
# making a PCA for SES
# cog$ses_pca <- psych::pca(cog[, .(ParEd_max, demo_comb_income_v2, reshist_addr1_adi_wsum)])$scores
# https://stats.stackexchange.com/questions/35561/imputation-of-missing-values-for-pca

# gonna do probabilitics PCA to get the SES PCA scores and than MICE for the other values
# mice::md.pattern(cog[, .(ParEd_max.s, demo_comb_income_v2.s, reshist_addr1_adi_wsum.s)], plot = F)
cog.complete$ses <- as.numeric(pcaMethods::ppca(BiocGenerics::t(cog.complete[, .(ParEd_max, demo_comb_income_v2, reshist_addr1_adi_wsum)]), nPcs = 1, seed = 42)@loadings)
# ppca has a .999 correlation for the non-missing values with normal pca

# I am now finding subject that were missing more than 1 value for the 3 SES categories
cog.complete$twoormore <- rep(0, length(cog.complete$subjectkey))
cog.complete$twoormore[is.na(cog.complete$ParEd_max)] <- cog.complete$twoormore[is.na(cog.complete$ParEd_max)] +1
cog.complete$twoormore[is.na(cog.complete$demo_comb_income_v2)] <- cog.complete$twoormore[is.na(cog.complete$demo_comb_income_v2)] +1 
cog.complete$twoormore[is.na(cog.complete$reshist_addr1_adi_wsum)] <- cog.complete$twoormore[is.na(cog.complete$reshist_addr1_adi_wsum)] +1 
cog.complete$twoormore[cog.complete$twoormore==1] <- 0
cog.complete$twoormore[cog.complete$twoormore>1] <- 1

# sum(cog.complete$twoormore) # 46 subjects, matches md pattern above
cog.complete$ses[cog.complete$twoormore==1] <- NA # making them NA

cog.complete$ses <- as.numeric(scale(cog.complete$ses))
# dim(cog.complete)[1] - dim(cog.complete[!is.na(cog.complete$ses),])[1] # n = 45
cog.complete <- cog.complete[!is.na(cog.complete$ses),]

# this SES is done with the DNA missing people!!!
cog.complete$SES_all <- cog.complete$ses
cog.complete$ses <- rep(NA, length(cog.complete$ses))

# so to figure out exactly how many are excluded due to dna I need here to find a group of all subs had DNA not been excluded ONLY
# but I also need to do the ppca on only the included subjects... (yet redo it to show the bias...)

#MAR assumption, showing that those without PGS's happen to be a different population
cog.complete$missing_PGS <- is.na(cog.complete$pgs)
sum(cog.complete$missing_PGS)
summary(lm(SES_all ~ missing_PGS, data = cog.complete))

# I don't think that this MAR missingness will substantially change the data, because I think its site DNA only missingness
# sites are not SES representative. Also I did the whole imputation accidently without genetic PCs and the main effect results
# where almost the exact same...


cog.complete <- cog.complete[cog.complete[, complete.cases(.SD), .SDcols = all_cols]]

# now making the actually SES ppca on the listwise deleted sample, I had to do this to report the sample is higher SES
cols_ses <- c("ParEd_max", "demo_comb_income_v2", "reshist_addr1_adi_wsum")
cog.complete[, (cols_ses) := lapply(.SD, function(x) as.numeric(scale(x))), .SDcols=cols_ses]
cog.complete$ses <- as.numeric(pcaMethods::ppca(BiocGenerics::t(cog.complete[, .(ParEd_max, demo_comb_income_v2, reshist_addr1_adi_wsum)]), nPcs = 1, seed = 42)@loadings)
cog.complete$ses <- -cog.complete$ses # sign flipping

# rescalling them all
cog.complete <- cog.complete[, (c(all_cols, "ses")) := lapply(.SD, function(x) as.numeric(scale(x))), .SDcols=c(all_cols, "ses")]

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
# summary(lm(scale(nihtbx_fluidcomp_uncorrected) ~ scale(pgs), data = cog.complete[demo_race_a_p___10==1]))
# summary(lm(scale(nihtbx_fluidcomp_uncorrected) ~ scale(pgs), data = cog.complete[demo_race_a_p___10==0]))
# 
# summary(lm(scale(nihtbx_cryst_uncorrected) ~ scale(pgs), data = cog.complete[demo_race_a_p___10==1]))
# summary(lm(scale(nihtbx_cryst_uncorrected) ~ scale(pgs), data = cog.complete[demo_race_a_p___10==0]))

# Bruno has done this with PCA and showed the expected results (altho it still predicts pretty well)

#########################  #########################
# making corelation plots
#########################  #########################
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

if('corplt' == 'off'){
  # functions
  # Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  # making a corplot
  checking_p <- Hmisc::rcorr(as.matrix(cog.complete[, .(schooling_yrs, age_yrs, pgs, ses,
                                                        nihtbx_list_uncorrected, nihtbx_fluidcomp_uncorrected, nihtbx_cryst_uncorrected)]))
  cormat <- round(cor(cog.complete[, .(schooling_yrs, age_yrs, pgs, ses,
                                       nihtbx_list_uncorrected, nihtbx_fluidcomp_uncorrected, nihtbx_cryst_uncorrected)], use = "pairwise.complete.obs"),2) # for upper
  
  # renaming vars
  rownames(cormat) <- c("Schooling", "Age", "cogPGS", "SES", "WM", "fIQ", "cIQ")
  colnames(cormat) <- c("Schooling", "Age", "cogPGS", "SES", "WM", "fIQ", "cIQ")

  
  cormat_metled <- data.table::melt(cormat)
  
  upper_tri <- get_upper_tri(cormat)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  
  melted_cormat_ns <- melted_cormat
  # showing values with p < .001
  melted_cormat_ns[4,3] <- NA 
  melted_cormat_ns[5,3] <- NA
  melted_cormat_ns[7,3] <- NA 
  melted_cormat_ns[8,3] <- NA 
  
  p1 <- ggplot(data = melted_cormat_ns, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+ 
    viridis::scale_fill_viridis(option="rocket",direction=-1,limits=c(0,1), na.value="lightgrey", begin = .2,
                                name = "Pearson's \nCorrelation") + coord_fixed() +
    #scale_color_brewer(name="Pearson\nCorrelation", direction = -1, limit = c(0,1)) +
    #scale_fill_continuous(name="Pearson\nCorrelation", limit = c(0,1), low = "yellow", high = "red",  na.value = "white") +
    #scale_fill_viridis(name="Pearson\nCorrelation", limit = c(.25,.86), na.value = "white", option = "D", direction= 1) +
    theme(axis.text.x = element_text(angle = 65, vjust = 1,
                                     size = 25, hjust = 1),
          axis.text.y = element_text(size = 25))

  
  # scale_fill_viridis_c(limits=c(0,1),option="plasma")
  
  p1 <- p1 +
   geom_text(data = melted_cormat, aes(Var2, Var1, label = sprintf("%0.2f", value)), color = "black", size = 10) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # axis.text.y = element_blank(), # removing y axis text
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.title = element_text(size = 25),
      legend.text = element_text(size = 20),
      legend.key.size = unit(1.7, "cm"))
  
  png("~/Projects/R_projects/ABCDschooling/figs/corplt.png", 1000, 900)
  p1
  dev.off()
  
}

# apa table
# apaTables::apa.cor.table(cog.complete[, .(nihtbx_cryst_uncorrected, nihtbx_fluidcomp_uncorrected, nihtbx_list_uncorrected,
#                                  pgs, ses,
#                                  ParEd_max.s, demo_comb_income_v2.s, reshist_addr1_adi_wsum.s)],
#                          show.conf.interval = FALSE,
#                          filename = "~/Projects/R_projects/gxe_ABCD/cortable.doc"
#                          )


##########################
### descriptives

library(kableExtra)


# psych::describe(cog.complete[, .(nihtbx_cryst_uncorrected, nihtbx_fluidcomp_uncorrected, nihtbx_list_uncorrected,
#                                  age_yrs, age_yrs.unscaled, schooling_yrs, schooling_yrs.unscaled,
#                                  ses, pgs,
#                                  demo_comb_income_v2, ParEd_max, reshist_addr1_adi_wsum)]) %>%
#   kbl(digits = 2) %>%
#   save_kable("~/Projects/R_projects/ABCDschooling/tables/descrip.html")

##################################################################
########### Analysis: fitting models ########### 
# linear mixed-eff model with site coded
# https://stats.stackexchange.com/questions/116770/reml-or-ml-to-compare-two-mixed-effects-models-with-differing-fixed-effects-but

### regression discounity models

# crystalized IQ

cy_1 <- lmerTest::lmer(nihtbx_cryst_uncorrected ~ age_yrs + schooling_yrs + (1 | site_id_l),
                       data = cog.complete, REML = F)

cy_2 <- lmerTest::lmer(nihtbx_cryst_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
                     C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                       data = cog.complete, REML = F) 


cy_2_unscaled <- lmerTest::lmer(nihtbx_cryst_uncorrected ~ age_yrs.unscaled + schooling_yrs.unscaled + sex + pgs + ses + 
                         C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                       data = cog.complete, REML = F) 

# https://en.wikipedia.org/wiki/Frisch%E2%80%93Waugh%E2%80%93Lovell_theorem
# hold <- cog.complete
# hold <- umx::umx_residualize(c("nihtbx_cryst_uncorrected", "ses", "pgs", "age_yrs", "schooling_yrs"), c("C1" ,"C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20"), 
#                                                                                                  data = hold)
#                                               
# cy_2_hold <- lmerTest::lmer(nihtbx_cryst_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + (1 | site_id_l),
#                             data = hold, REML = F) 



# adding the two way interactions of interest schoolingXpgs, schoolingXses & pgsXses
cy_3 <- lmerTest::lmer(nihtbx_cryst_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
                     pgs:ses + schooling_yrs:pgs + schooling_yrs:ses + age_yrs:pgs + age_yrs:ses +
                     C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                   data = cog.complete, REML = F) 

# adding the 3way interaction
cy_4 <- lmerTest::lmer(nihtbx_cryst_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
                     pgs:ses + schooling_yrs:pgs + schooling_yrs:ses + age_yrs:pgs + age_yrs:ses +
                     schooling_yrs:ses:pgs + 
                     C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                   data = cog.complete, REML = F) 

# fluid IQ

fi_1 <- lmerTest::lmer(nihtbx_fluidcomp_uncorrected ~ age_yrs + schooling_yrs + (1 | site_id_l),
                       data = cog.complete, REML = F)

fi_2 <- lmerTest::lmer(nihtbx_fluidcomp_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
                     C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                   data = cog.complete, REML = F) 

fi_2_unscaled <- lmerTest::lmer(nihtbx_fluidcomp_uncorrected ~ age_yrs.unscaled + schooling_yrs.unscaled + sex + pgs + ses + 
                         C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                       data = cog.complete, REML = F) 

# adding the two way interactions of interest schoolingXpgs, schoolingXses & pgsXses
fi_3 <- lmerTest::lmer(nihtbx_fluidcomp_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
                     pgs:ses + schooling_yrs:pgs + schooling_yrs:ses + age_yrs:pgs + age_yrs:ses +
                     C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                   data = cog.complete, REML = F) 

# adding the 3way interaction
fi_4 <- lmerTest::lmer(nihtbx_fluidcomp_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
                     pgs:ses + schooling_yrs:pgs + schooling_yrs:ses + age_yrs:pgs + age_yrs:ses +
                     schooling_yrs:ses:pgs + 
                     C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                   data = cog.complete, REML = F) 

# for list sorting

list_1 <- lmerTest::lmer(nihtbx_list_uncorrected ~ age_yrs + schooling_yrs + (1 | site_id_l),
                       data = cog.complete, REML = F)

list_2 <- lmerTest::lmer(nihtbx_list_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
                         C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                       data = cog.complete, REML = F) 

list_2_unscaled <- lmerTest::lmer(nihtbx_list_uncorrected ~ age_yrs.unscaled + schooling_yrs.unscaled + sex + pgs + ses + 
                           C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                         data = cog.complete, REML = F) 


# adding the two way interactions of interest schoolingXpgs, schoolingXses & pgsXses
list_3 <- lmerTest::lmer(nihtbx_list_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
                         pgs:ses + schooling_yrs:pgs + schooling_yrs:ses + age_yrs:pgs + age_yrs:ses +
                         C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                       data = cog.complete, REML = F) 

# adding the 3way interaction
list_4 <- lmerTest::lmer(nihtbx_list_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
                         pgs:ses + schooling_yrs:pgs + schooling_yrs:ses + age_yrs:pgs + age_yrs:ses +
                         schooling_yrs:ses:pgs + 
                         C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                       data = cog.complete, REML = F) 

########### SES subcomponent analysis ###########

cy_2_income <- lmerTest::lmer(nihtbx_cryst_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + demo_comb_income_v2 + 
                         C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                       data = cog.complete, REML = F) 
cy_2_ParEd <- lmerTest::lmer(nihtbx_cryst_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ParEd_max + 
                         C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                       data = cog.complete, REML = F) 
cy_2_neigh <- lmerTest::lmer(nihtbx_cryst_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + reshist_addr1_adi_wsum + 
                         C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                       data = cog.complete, REML = F) 


fi_2_income <- lmerTest::lmer(nihtbx_fluidcomp_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + demo_comb_income_v2 + 
                         C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                       data = cog.complete, REML = F) 
fi_2_ParEd <- lmerTest::lmer(nihtbx_fluidcomp_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ParEd_max + 
                         C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                       data = cog.complete, REML = F) 
fi_2_neigh <- lmerTest::lmer(nihtbx_fluidcomp_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + reshist_addr1_adi_wsum + 
                         C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                       data = cog.complete, REML = F) 

list_2_income <- lmerTest::lmer(nihtbx_list_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + demo_comb_income_v2 + 
                           C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                         data = cog.complete, REML = F) 
list_2_ParEd <- lmerTest::lmer(nihtbx_list_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ParEd_max + 
                           C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                         data = cog.complete, REML = F) 
list_2_neigh <- lmerTest::lmer(nihtbx_list_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + reshist_addr1_adi_wsum + 
                           C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                         data = cog.complete, REML = F) 

########### Bayesian analysis for 2way interactions ###########

library(brms); library(bayestestR); options(buildtools.check = function(action) TRUE )
library(tidybayes); library(tidyverse)

# EVERYTHING, including schooling and age must be standardized for these comparisons to make sense.


# http://mjskay.github.io/tidybayes/articles/tidy-brms.html


# the goals is to make your own ROPE graph
# https://easystats.github.io/bayestestR/articles/credible_interval.html claims you need to update the number of draws


# cy_3_bayes <- brm(nihtbx_cryst_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
#                          pgs:ses + schooling_yrs:pgs + schooling_yrs:ses + age_yrs:pgs + age_yrs:ses +
#                          C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
#                        data = cog.complete) 

# prior = prior(student_t(1, -0.01, 0.001), coef = year)

# schooling & age (for cryst)
# curve(dnorm(x, .15, .1), from = -1, to =1, #fluid/wm .15
#       main = "Relative Plausibility a priori for the model mean",
#       xlab = "mean",
#       ylab = "Probability of mean")



cy_prior <- 
  prior(normal(.2,15), coef = age_yrs) + 
  prior(normal(.2,15), coef = schooling_yrs) + 
  prior(normal(.15,.1), coef = pgs) +
  prior(normal(.25,.2), coef = ses) +
  prior(normal(0,.1), coef = pgs:ses) +
  prior(normal(0,.1), coef = age_yrs:ses) +
  prior(normal(0,.1), coef = schooling_yrs:ses) +
  prior(normal(0,.1), coef = age_yrs:pgs) +
  prior(normal(0,.1), coef = schooling_yrs:pgs)
  

cy_3_bayes_pri <- brm(nihtbx_cryst_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
                    pgs:ses + schooling_yrs:pgs + schooling_yrs:ses + age_yrs:pgs + age_yrs:ses +
                    C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                    prior = cy_prior,
                  data = cog.complete) 

# prior_summary(cy_3_bayes_pri)

# cy_3_bayes_rope.08 <- rope(cy_3_bayes, range =  c(-0.08, 0.08))
# cy_3_bayes_rope.05 <- rope(cy_3_bayes, range =  c(-0.05, 0.05))
# cy_3_bayes_rope.02 <- rope(cy_3_bayes, range =  c(-0.02, 0.02))

cy_3_bayes_rope.08_pri <- rope(cy_3_bayes_pri, range =  c(-0.08, 0.08))
cy_3_bayes_rope.05_pri <- rope(cy_3_bayes_pri, range =  c(-0.05, 0.05))
cy_3_bayes_rope.02_pri <- rope(cy_3_bayes_pri, range =  c(-0.02, 0.02))

# making a table of the ROPE estimates
# cy_rope <- as.data.table(cy_3_bayes_rope.08)[27:29,c(1,2,5)][
#   as.data.table(cy_3_bayes_rope.08_pri)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
#     as.data.table(cy_3_bayes_rope.05)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
#       as.data.table(cy_3_bayes_rope.05_pri)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
#         as.data.table(cy_3_bayes_rope.02)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
#           as.data.table(cy_3_bayes_rope.02_pri)[27:29,c(1,2,5)], on = c("Parameter", "CI")]
# colnames(cy_rope)[3:8] <- c("ROPE .08","ROPE .08 priors", "ROPE .05","ROPE .05 priors", "ROPE .02", "ROPE .02 priors")

fIQWM_prior <- 
  prior(normal(.15,15), coef = age_yrs) + 
  prior(normal(.15,15), coef = schooling_yrs) + 
  prior(normal(.1,.1), coef = pgs) +
  prior(normal(.2,.2), coef = ses) +
  prior(normal(0,.1), coef = pgs:ses) +
  prior(normal(0,.1), coef = age_yrs:ses) +
  prior(normal(0,.1), coef = schooling_yrs:ses) +
  prior(normal(0,.1), coef = age_yrs:pgs) +
  prior(normal(0,.1), coef = schooling_yrs:pgs)


# fi_3_bayes <- brm(nihtbx_fluidcomp_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
#                     pgs:ses + schooling_yrs:pgs + schooling_yrs:ses + age_yrs:pgs + age_yrs:ses +
#                     C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
#                   data = cog.complete) 

fi_3_bayes_pri <- brm(nihtbx_fluidcomp_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
                    pgs:ses + schooling_yrs:pgs + schooling_yrs:ses + age_yrs:pgs + age_yrs:ses +
                    C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                  prior = fIQWM_prior,
                  data = cog.complete) 

# fi_3_bayes_rope.08 <- rope(fi_3_bayes, range =  c(-0.08, 0.08))
# fi_3_bayes_rope.05 <- rope(fi_3_bayes, range =  c(-0.05, 0.05))
# fi_3_bayes_rope.02 <- rope(fi_3_bayes, range =  c(-0.02, 0.02))

fi_3_bayes_rope.08_pri <- rope(fi_3_bayes_pri, range =  c(-0.08, 0.08))
fi_3_bayes_rope.05_pri <- rope(fi_3_bayes_pri, range =  c(-0.05, 0.05))
fi_3_bayes_rope.02_pri <- rope(fi_3_bayes_pri, range =  c(-0.02, 0.02))

# making a table of the ROPE estimates
# fi_rope <- as.data.table(fi_3_bayes_rope.08)[27:29,c(1,2,5)][
#   as.data.table(fi_3_bayes_rope.08_pri)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
#     as.data.table(fi_3_bayes_rope.05)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
#       as.data.table(fi_3_bayes_rope.05_pri)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
#         as.data.table(fi_3_bayes_rope.02)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
#           as.data.table(fi_3_bayes_rope.02_pri)[27:29,c(1,2,5)], on = c("Parameter", "CI")]
# colnames(fi_rope)[3:8] <- c("ROPE .08","ROPE .08 priors", "ROPE .05","ROPE .05 priors", "ROPE .02", "ROPE .02 priors")



# list_3_bayes <- brm(nihtbx_list_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
#                       pgs:ses + schooling_yrs:pgs + schooling_yrs:ses + age_yrs:pgs + age_yrs:ses +
#                       C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
#                     data = cog.complete) 

list_3_bayes_pri <- brm(nihtbx_list_uncorrected ~ age_yrs + schooling_yrs + sex + pgs + ses + 
                      pgs:ses + schooling_yrs:pgs + schooling_yrs:ses + age_yrs:pgs + age_yrs:ses +
                      C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15 + C16 + C17 + C18 + C19 + C20 + (1 | site_id_l),
                    prior = fIQWM_prior,
                    data = cog.complete) 

# list_3_bayes_rope.08 <- rope(list_3_bayes, range =  c(-0.08, 0.08))
# list_3_bayes_rope.05 <- rope(list_3_bayes, range =  c(-0.05, 0.05))
# list_3_bayes_rope.02 <- rope(list_3_bayes, range =  c(-0.02, 0.02))

list_3_bayes_rope.08_pri <- rope(list_3_bayes_pri, range =  c(-0.08, 0.08))
list_3_bayes_rope.05_pri <- rope(list_3_bayes_pri, range =  c(-0.05, 0.05))
list_3_bayes_rope.02_pri <- rope(list_3_bayes_pri, range =  c(-0.02, 0.02))

# making a table of the ROPE estimates
# list_rope <- as.data.table(list_3_bayes_rope.08)[27:29,c(1,2,5)][
#   as.data.table(list_3_bayes_rope.08_pri)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
#     as.data.table(list_3_bayes_rope.05)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
#       as.data.table(list_3_bayes_rope.05_pri)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
#         as.data.table(list_3_bayes_rope.02)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
#           as.data.table(list_3_bayes_rope.02_pri)[27:29,c(1,2,5)], on = c("Parameter", "CI")]
# colnames(list_rope)[3:8] <- c("ROPE .08","ROPE .08 priors", "ROPE .05","ROPE .05 priors", "ROPE .02", "ROPE .02 priors")


# one table with the ROPES (.05, .02) from the model with priors 

ROPE_tab <- as.data.table(cy_3_bayes_rope.05_pri)[27:29,c(1,2,5)][
  as.data.table(cy_3_bayes_rope.02_pri)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
    as.data.table(fi_3_bayes_rope.05_pri)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
      as.data.table(fi_3_bayes_rope.02_pri)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
        as.data.table(list_3_bayes_rope.05_pri)[27:29,c(1,2,5)], on = c("Parameter", "CI")][
          as.data.table(list_3_bayes_rope.02_pri)[27:29,c(1,2,5)], on = c("Parameter", "CI")]
colnames(ROPE_tab)[3:8] <- c("cIQ ROPE .05","cIQ ROPE .02", "fIQ ROPE .05","fIQ ROPE .02", "WM ROPE .05", "WM ROPE .02")

ROPE_tab %>% 
  kbl(digits = 2) %>% 
  kable_styling()

# now plotting the null Hx
# plot(cy_3_bayes_rope.05_pri, rope_color = "red") +
#   scale_fill_brewer(palette = "Greens", direction = -1)

# plot cIQ
cy_Bayes_plt <- cy_3_bayes_pri %>%
  gather_draws(`b_schooling_yrs:pgs`, `b_schooling_yrs:ses`, `b_pgs:ses`) %>%
  ggplot(aes(y = .variable, x = .value)) +
  geom_vline(xintercept = c(0)) +
  geom_rect(aes(xmin=-.02,xmax=.02,ymin=-Inf,ymax=Inf),fill="gray", color = NA,alpha=0.05) + # weird fill actually make the color change... 
  stat_slab(aes(fill = stat(cut_cdf_qi(cdf, .width = .95,labels = scales::percent_format()))), alpha = .5) +
  scale_fill_manual(values = "blue", na.value = "lightblue") + #, na.translate = FALSE
  geom_vline(xintercept = c(-.05, .05), linetype = "dashed") +
  labs(y = "", x = "cIQ posterior distribution (s.d.)") +
  scale_x_continuous(limits = c(-.1, .1), breaks = c(-.1, -.05, 0, .05, .1)) +
  scale_y_discrete(labels = c("b_schooling_yrs:pgs" = "", "b_schooling_yrs:ses" = "", "b_pgs:ses" = "")) +
  theme_minimal(base_size = 15) +
  theme(legend.position="none",
        axis.title=element_text(face="bold"),
        axis.text.x = element_text(),
        axis.text.y = element_text(angle = 45))+
  coord_cartesian(expand = FALSE)
# plot fIQ
fi_Bayes_plt <- fi_3_bayes_pri %>%
  gather_draws(`b_schooling_yrs:pgs`, `b_schooling_yrs:ses`, `b_pgs:ses`) %>%
  ggplot(aes(y = .variable, x = .value)) +
  geom_vline(xintercept = c(0)) +
  geom_rect(aes(xmin=-.02,xmax=.02,ymin=-Inf,ymax=Inf),fill="gray", color = NA,alpha=0.05) + # weird fill actually make the color change... 
  stat_slab(aes(fill = stat(cut_cdf_qi(cdf, .width = .95,labels = scales::percent_format()))), alpha = .5) +
  scale_fill_manual(values = c("blue"), na.value = "lightblue") +
  geom_vline(xintercept = c(-.05, .05), linetype = "dashed") +
  labs(y = "", x = "fIQ posterior distribution (s.d.)") +
  scale_x_continuous(limits = c(-.1, .1), breaks = c(-.1, -.05, 0, .05, .1)) +
  scale_y_discrete(labels = c("b_schooling_yrs:pgs" = "", "b_schooling_yrs:ses" = "", "b_pgs:ses" = "")) +
  theme_minimal(base_size = 15) +
  theme(legend.position="none",
        axis.title=element_text(face="bold"),
        axis.text.x = element_text(),
        axis.text.y = element_text(angle = 45))+
  coord_cartesian(expand = FALSE)
# plot WM
list_Bayes_plt <- list_3_bayes_pri %>%
  gather_draws(`b_schooling_yrs:pgs`, `b_schooling_yrs:ses`, `b_pgs:ses`) %>%
  ggplot(aes(y = .variable, x = .value)) +
  geom_vline(xintercept = c(0)) +
  geom_rect(aes(xmin=-.02,xmax=.02,ymin=-Inf,ymax=Inf),fill="gray", color = NA,alpha=0.05) + # weird fill actually make the color change... 
  stat_slab(aes(fill = stat(cut_cdf_qi(cdf, .width = .95,labels = scales::percent_format()))), alpha = .5) +
  scale_fill_manual(values = c("blue"), na.value = "lightblue") +
  geom_vline(xintercept = c(-.05, .05), linetype = "dashed") +
  labs(y = "", x = "WM posterior distribution (s.d.)") +
  scale_x_continuous(limits = c(-.1, .1), breaks = c(-.1, -.05, 0, .05, .1)) +
  scale_y_discrete(labels = c("b_schooling_yrs:pgs" = "", "b_schooling_yrs:ses" = "", "b_pgs:ses" = "")) +
  theme_minimal(base_size = 15) +
  theme(legend.position="none",
        axis.title=element_text(face="bold"),
        axis.text.x = element_text(),
        axis.text.y = element_text(angle = 45))+
  coord_cartesian(expand = FALSE)


ROPE_plt <- cy_Bayes_plt + fi_Bayes_plt + list_Bayes_plt + plot_annotation(tag_levels = 'a')
# ggsave("figs/ROPE_plt.png", ROPE_plt, width = 15.4, height = 9.65)





# a first step to check the assumptions of this hypothesis testing is to look at different pair plots. 
# An even more sophisticated check is the projection predictive variable selection (Piironen and Vehtari 2017).


##################################
### Instrument variable rohbustness checks

# see how older they are in comparison
hold <- cog

table(cog$kbi_y_grade_repeat)

# data tidy, making one complete dataset
dvs <- c("nihtbx_cryst_uncorrected", "nihtbx_fluidcomp_uncorrected", "nihtbx_list_uncorrected")
common_cols <- c(dvs, "pgs", "schooling_yrs", "age_yrs",
                 "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")
# subseting the main df
cog <- cog[cog[, complete.cases(.SD), .SDcols = common_cols]][
  , (dvs) := lapply(.SD, vec_to_fence), .SDcols=dvs # bringing the dvs to the fence
][
  , c("schooling_yrs.unscaled", "age_yrs.unscaled") := .(schooling_yrs, age_yrs) # making new holding cols that are unscaled for age & school
][
  , (common_cols) := lapply(.SD, function(x) as.numeric(scale(x))), .SDcols=common_cols # standardizing all the relevant info
]


# rescaling the SES components for the ppca
cols_ses <- c("ParEd_max", "demo_comb_income_v2", "reshist_addr1_adi_wsum")
cog[, (cols_ses) := lapply(.SD, function(x) as.numeric(scale(x))), .SDcols=cols_ses]

cog$reshist_addr1_adi_wsum <- -cog$reshist_addr1_adi_wsum # neighborhood deprivation is now called neighborhood quality
# making a PCA for SES
# cog$ses_pca <- psych::pca(cog[, .(ParEd_max, demo_comb_income_v2, reshist_addr1_adi_wsum)])$scores
# https://stats.stackexchange.com/questions/35561/imputation-of-missing-values-for-pca

# gonna do probabilitics PCA to get the SES PCA scores and than MICE for the other values
# mice::md.pattern(cog[, .(ParEd_max.s, demo_comb_income_v2.s, reshist_addr1_adi_wsum.s)], plot = F)
cog$ses <- as.numeric(pcaMethods::ppca(BiocGenerics::t(cog[, .(ParEd_max, demo_comb_income_v2, reshist_addr1_adi_wsum)]), nPcs = 1, seed = 42)@loadings)
# ppca has a .999 correlation for the non-missing values with normal pca

# I am now finding subject that were missing more than 1 value for the 3 SES categories
cog$twoormore <- rep(0, length(cog$subjectkey))
cog$twoormore[is.na(cog$ParEd_max)] <- cog$twoormore[is.na(cog$ParEd_max)] +1
cog$twoormore[is.na(cog$demo_comb_income_v2)] <- cog$twoormore[is.na(cog$demo_comb_income_v2)] +1 
cog$twoormore[is.na(cog$reshist_addr1_adi_wsum)] <- cog$twoormore[is.na(cog$reshist_addr1_adi_wsum)] +1 
cog$twoormore[cog$twoormore==1] <- 0
cog$twoormore[cog$twoormore>1] <- 1

# sum(cog$twoormore) 
cog$ses[cog$twoormore==1] <- NA # making them NA

cog$ses <- as.numeric(scale(cog$ses))
# dim(cog)[1] - dim(cog[!is.na(cog$ses),])[1] # n = 34
cog <- cog[!is.na(cog$ses),]

table(cog$kbi_y_grade_repeat)
cog$logi_repeat <- cog$kbi_y_grade_repeat == 1

summary(lm(age_yrs.unscaled ~ logi_repeat + schooling_yrs.unscaled, data = cog))

cy_1_reps <- lmerTest::lmer(nihtbx_cryst_uncorrected ~ age_yrs + schooling_yrs + (1 | site_id_l),
                       data = cog, REML = F)
fi_1_reps <- lmerTest::lmer(nihtbx_fluidcomp_uncorrected ~ age_yrs + schooling_yrs + (1 | site_id_l),
                       data = cog, REML = F)
list_1_reps <- lmerTest::lmer(nihtbx_list_uncorrected ~ age_yrs + schooling_yrs + (1 | site_id_l),
                       data = cog, REML = F)

cy_1; cy_1_reps
fi_1; fi_1_reps
list_1; list_1_reps

table(cog.complete$demo_ed_v2)
cog.complete[demo_ed_v2 == 5, fifth_grade := TRUE][demo_ed_v2 == 4, fifth_grade := FALSE]

cy_1_g <- lmerTest::lmer(nihtbx_cryst_uncorrected ~ age_yrs + demo_ed_v2 + (1 | site_id_l),
                            data = cog.complete, REML = F)
fi_1_g <- lmerTest::lmer(nihtbx_fluidcomp_uncorrected ~ age_yrs + demo_ed_v2 + (1 | site_id_l),
                            data = cog.complete, REML = F)
list_1_g <- lmerTest::lmer(nihtbx_list_uncorrected ~ age_yrs + demo_ed_v2 + (1 | site_id_l),
                              data = cog.complete, REML = F)

cy_1_5v4 <- lmerTest::lmer(nihtbx_cryst_uncorrected ~ age_yrs + fifth_grade + (1 | site_id_l),
                         data = cog.complete, REML = F)
fi_1_5v4 <- lmerTest::lmer(nihtbx_fluidcomp_uncorrected ~ age_yrs + fifth_grade + (1 | site_id_l),
                         data = cog.complete, REML = F)
list_1_5v4 <- lmerTest::lmer(nihtbx_list_uncorrected ~ age_yrs + fifth_grade + (1 | site_id_l),
                           data = cog.complete, REML = F)


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

performance::check_collinearity(cy_4) %>% 
  kbl(digits = 2) %>% 
  save_kable("~/Projects/R_projects/ABCDschooling/tables/vif_cy.html")
  
  
  # psych::describe(cog.complete[, .(nihtbx_cryst_uncorrected, nihtbx_fluidcomp_uncorrected, nihtbx_list_uncorrected,
  #                                  age_yrs, age_yrs.unscaled, schooling_yrs, schooling_yrs.unscaled,
  #                                  ses, pgs,
  #                                  demo_comb_income_v2, ParEd_max, reshist_addr1_adi_wsum)]) %>%
  #   kbl(digits = 2) %>%
  #   save_kable("~/Projects/R_projects/ABCDschooling/tables/descrip.html")
  

# Cryst
# sjPlot::tab_model(cy_1, cy_2, cy_3, cy_4,
#                   show.loglik = T, show.aic = T, digits = 3, # p.val = "kr"
#                   p.style = "numeric",show.se = T, show.ci = NULL,
#                   file = "~/Projects/R_projects/ABCDschooling/tables/cryst_results.html")
# sjPlot::tab_model(fi_1, fi_2, fi_3, fi_4,
#                   show.loglik = T, show.aic = T, digits = 3, # p.val = "kr"
#                   p.style = "numeric",show.se = T, show.ci = NULL,
#                   file = "~/Projects/R_projects/ABCDschooling/tables/fluid_results.html")
# sjPlot::tab_model(list_1, list_2, list_3, list_4,
#                   show.loglik = T, show.aic = T, digits = 3, # p.val = "kr"
#                   p.style = "numeric",show.se = T, show.ci = NULL,
#                   file = "~/Projects/R_projects/ABCDschooling/tables/wm_results.html")

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
# lattice::histogram(~nihtbx_cryst_uncorrected | is.na(pgs), data=cryst_data_pca)
# lattice::histogram(~ses | is.na(pgs), data=cryst_data_pca)

##################################################################
########### Main manuscript plotting ########### 
# figure 1; making a plot of the main effects
eff_plt <- data.table(DV = c(rep("fIQ", 4), rep("cIQ", 4), rep("WM", 4)),
                      var = c(rep(c("Age 1yr", "School 1yr", "cogPGS", "SES"), 3)),
           beta = c(
             as.numeric(summary(fi_2_unscaled)$coefficients[,1][c(2,3,5,6)]), # grabing the beta terms for age, schooling, pgs & ses (in the same order as the var I made)
             as.numeric(summary(cy_2_unscaled)$coefficients[,1][c(2,3,5,6)]), 
             as.numeric(summary(list_2_unscaled)$coefficients[,1][c(2,3,5,6)]) # gender is last in the model so it works
           ), 
           ci_low = c( # getting the lower confidence intervals for the 4 terms for each 3 models
             as.numeric(confint(fi_2_unscaled)[c(4,5,7,8), 1]),as.numeric(confint(cy_2_unscaled)[c(4,5,7,8), 1]), as.numeric(confint(list_2_unscaled)[c(4,5,7,8), 1])
           ), 
           ci_high= c( # getting the higher confidence intervals for the 4 terms for each 3 models
             as.numeric(confint(fi_2_unscaled)[c(4,5,7,8), 2]),as.numeric(confint(cy_2_unscaled)[c(4,5,7,8), 2]), as.numeric(confint(list_2_unscaled)[c(4,5,7,8), 2])
           ))

barCOLS <- c('#DE3163', '#6495ED', '#9FE2BF') # a vector of colors

eff_plt$var <- factor(eff_plt$var, levels = c("Age 1yr", "School 1yr", "cogPGS", "SES")) # making var a factor for ggplot, ordering them as I wish as well
eff_plt$DV <- factor(eff_plt$DV) # making the DV as a factor

main_eff_Dplt <- ggplot(eff_plt, aes(var, beta, ymin = ci_low, ymax = ci_high, col = DV, fill = DV)) +
  geom_linerange(size=1.5,position=position_dodge(width = 0.5)) +
  #geom_hline(yintercept=0, lty=2) +
  geom_point(size=2.5, shape=21, colour="black", stroke = 0.5, position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS) +
  scale_color_manual(values=barCOLS) +
  scale_x_discrete(name="") + # , breaks = NULL
  scale_y_continuous(name="Effect in SD", limits = c(-.05, .4), breaks = c(0, .1, .2, .3, .4)) +
  theme_minimal(base_size = 20) +
  labs(color = " ", fill = " ") +
  ggExtra::removeGridX() +
  coord_cartesian(expand = FALSE)

ggsave("figs/main_eff_Dplt.png", main_eff_Dplt, width = 10, height = 4)

# Figure 2; results of the cog-PGS*SES interaction
# plot_model(cy_mm_inter, type = "pred", terms = c("pgs [-2, 0, 2]", "ses [-2, 0, 2]")) + theme_minimal()
# plot_model(fi_mm_inter, type = "pred", terms = c("pgs [-2, 0, 2]", "ses [-2, 0, 2]")) + theme_minimal()


library(ggeffects)

mydf_cy <- ggpredict(cy_mm4_no3way, terms = c("pgs [-2, 0, 2]", "ses [-2, 0, 2]"))
mydf_fi <- ggpredict(fi_mm4_no3way, terms = c("pgs [-2, 0, 2]", "ses [-2, 0, 2]"))

cy_plt <- plot(mydf_cy) + 
  scale_color_manual(values=c("#64CEED", "#6495ED", "#8164ED")) +
  scale_fill_manual(values=c("#64CEED", "#6495ED", "#8164ED")) +
  theme_minimal(base_size = 25) +
  labs(title = "", x = "cogPGS (SD)", y = "cIQ")+
  guides(color = guide_legend(reverse = T, "SES (SD)", override.aes=list(shape=15, size = 5, fill=NA))) +
  theme(legend.position= "none") +
  ggExtra::removeGridX()

fi_plt <- plot(mydf_fi) + 
  scale_color_manual(values=c("#64CEED", "#6495ED", "#8164ED"), name = "SES (SD)") +
  scale_fill_manual(values = c("#64CEED", "#6495ED", "#8164ED")) + # c('gray', 'gray', 'gray')
  theme_minimal(base_size = 25) +
  labs(title = "", x = "cogPGS (SD)", y = "fIQ") +
  guides(color = guide_legend(reverse = T, "SES (SD)", override.aes=list(shape=15, size = 5, fill=NA))) +
  ggExtra::removeGridX() 

  # scale_y_continuous(breaks = c(-1, -.5, 0 , .5, 1))
  # coord_cartesian(ylim = c(-1, -.5, 0 , .5, 1))
  
library(patchwork)
gxe_inter_plt <- cy_plt + fi_plt + plot_annotation(tag_levels = 'A')
ggsave("figs/gxe_inter_plt.png", gxe_inter_plt, width = 20, height = 10)

# Ploting the 3-way interaction trend: nothing to plot, effect does not survive
# plot_model(cy_mm4, type = "pred", terms = c("schooling_yrs", "pgs [-1.5, 1.5]", "ses [-1.5, 1.5]"))
# plot_model(cy_mm4, type = "pred", terms = c("schooling_yrs", "pgs [-1.5, 0, 1.5]", "ses [-1.5, 0, 1.5]")) + theme_minimal()

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

visreg(cy_mm2.2, "ses")
visreg_partial_residi_SES <- visreg(cy_mm2.2, "ses")
visreg_partial_residi_SES <- visreg_partial_residi_SES$res
visreg_partial_residi_SES$dens <- get_density(visreg_partial_residi_SES$ses, visreg_partial_residi_SES$visregRes, n =100)
visreg_partial_residi_SES$dens <- -as.numeric(scale(visreg_partial_residi_SES$dens))
ggplot(visreg_partial_residi_SES, aes(x = ses, y = visregRes)) + 
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

ggplot(cog[demo_ed_v2 != 3], aes(age_sub, nihtbx_cryst_uncorrected, color = demo_ed_v2)) +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  ylim(-2,2)

ggplot(cog, aes(age_sub, nihtbx_cryst_uncorrected, color = demo_ed_v2)) +
  geom_jitter(alpha = .3) +
  geom_smooth(method = "lm") +
  theme_minimal()





