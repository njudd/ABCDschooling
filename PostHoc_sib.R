# twin analysis
### Nicholas Judd 
# Dep. Neuro
# Karolinska Institute
# 2021-08-18
# nickjudd@gmail.com

# the book behavioural genetics of cognition across the lifespan
# has a good stats intro by turkhiemer

setwd("~/Projects/R_projects/ABCDschooling/")
source("~/Projects/R_projects/ABCDschooling/funcs/vec_to_fence.R")
library(data.table); 
options(scipen = 999); set.seed(42)

# twins
# https://www.biorxiv.org/content/10.1101/2020.02.10.942011v1.full.pdf

# from acspsw03 on NDA data archive
# 1= monozygotic ; 2= dizygotic ; 3=siblings ;-1= not available (twins/sibs, genetic_pi_hat not calculated)

# rel_group_id: Group ID (twins and triplets in the same family share a group ID)	
# rel_ingroup_order: In-Group Order ID (twins and triplets in the same family and in the same group have different values)

# genetic_paired_subjectid_1 show the pairing for genetic_zygosity_status_1

# 1= monozygotic ; 2= dizygotic
sibs <- fread("sed -e '2d' ~/ABCDStudyNDA/acspsw03.txt")[
  , .(subjectkey, eventname, interview_age, sex, rel_family_id, rel_group_id, rel_ingroup_order, 
      genetic_zygosity_status_1, genetic_zygosity_status_2, genetic_zygosity_status_3, genetic_zygosity_status_4)][ 
    eventname == "baseline_year_1_arm_1"
  ]


# hit list for the mono's 


# group by family you dumbass
subs_genetic_zygosity_status_1 <- sibs[
  genetic_zygosity_status_1 == 1
][, by = "rel_family_id",
  .SD[1]]$subjectkey

subs_genetic_zygosity_status_2 <- sibs[
  genetic_zygosity_status_2 == 1
][,  by = "rel_family_id",
  .SD[1]]$subjectkey

subs_genetic_zygosity_status_3 <- sibs[
  genetic_zygosity_status_3 == 1
][,  by = "rel_family_id",
  .SD[1]]$subjectkey

# there are no quads
# subs_genetic_zygosity_status_4 <- sibs[
#   genetic_zygosity_status_4 == 1
# ][, .SD[1]]$subjectkey

hitlist <- c(subs_genetic_zygosity_status_1, subs_genetic_zygosity_status_2, subs_genetic_zygosity_status_3)

# only having siblings!
sibs <- sibs[!is.na(genetic_zygosity_status_1)][
  !subjectkey %in% hitlist
]

iq <- fread("sed -e '2d' data/abcd_tbss01.txt")[
  eventname == "baseline_year_1_arm_1"][
    , .(subjectkey, interview_date, interview_age, sex, 
        nihtbx_list_uncorrected, nihtbx_fluidcomp_uncorrected, nihtbx_cryst_uncorrected)][
          ]

sibs <- iq[sibs, on = "subjectkey"]

ses <- fread("sed -e '2d' data/StudySiteBLgrade/pdem02.txt")[
  , .(subjectkey, demo_comb_income_v2, demo_prnt_ed_v2, demo_prtnr_ed_v2)] 

# recoding demo_prnt_ed_v2, demo_prtnr_ed_v2 to get the max between them
# for the parent
ses$demo_prnt_ed_v2[ses$demo_prnt_ed_v2 %in% c(0,1,2,3,4,5,6,7,8)] <- 1 # middle school or less
ses$demo_prnt_ed_v2[ses$demo_prnt_ed_v2 %in% c(9,10,11,12)] <- 2 # some highschool
ses$demo_prnt_ed_v2[ses$demo_prnt_ed_v2 %in% c(13,14)] <- 3 # high school graudate or GED
ses$demo_prnt_ed_v2[ses$demo_prnt_ed_v2 %in% c(15,16,17)] <- 4 # Some college, Associate degree: Occupational or Associate degree
ses$demo_prnt_ed_v2[ses$demo_prnt_ed_v2 %in% c(18)] <- 5 # Bachlors degree
ses$demo_prnt_ed_v2[ses$demo_prnt_ed_v2 %in% c(19)] <- 6 # MSc degree
ses$demo_prnt_ed_v2[ses$demo_prnt_ed_v2 %in% c(20,21)] <- 7 # MD, PhD
ses$demo_prnt_ed_v2[ses$demo_prnt_ed_v2 %in% c(777, 999)] <- NA # refused to answer or don't know was coded as missing
# for the care giver

ses$demo_prtnr_ed_v2[ses$demo_prtnr_ed_v2 %in% c(0,1,2,3,4,5,6,7,8)] <- 1 # middle school or less
ses$demo_prtnr_ed_v2[ses$demo_prtnr_ed_v2 %in% c(9,10,11,12)] <- 2 # some highschool
ses$demo_prtnr_ed_v2[ses$demo_prtnr_ed_v2 %in% c(13,14)] <- 3 # high school graudate or GED
ses$demo_prtnr_ed_v2[ses$demo_prtnr_ed_v2 %in% c(15,16,17)] <- 4 # Some college, Associate degree: Occupational or Associate degree
ses$demo_prtnr_ed_v2[ses$demo_prtnr_ed_v2 %in% c(18)] <- 5 # Bachlors degree
ses$demo_prtnr_ed_v2[ses$demo_prtnr_ed_v2 %in% c(19)] <- 6 # MSc degree
ses$demo_prtnr_ed_v2[ses$demo_prtnr_ed_v2 %in% c(20,21)] <- 7 # MD, PhD
ses$demo_prtnr_ed_v2[ses$demo_prtnr_ed_v2 %in% c(777, 999)] <- NA # refused to answer was coded as missing


ses$demo_comb_income_v2[ses$demo_comb_income_v2 %in% c(777, 999)] <- NA 

ses[, ParEd_max := pmax(demo_prtnr_ed_v2, demo_prnt_ed_v2, na.rm=TRUE)][
  , c("demo_prtnr_ed_v2","demo_prnt_ed_v2"):=NULL
  ]

# adding neighborhood SES
ses <- fread("sed -e '2d' data/abcd_rhds01.txt")[
  eventname == "baseline_year_1_arm_1" ][
    , .(subjectkey, reshist_addr1_adi_wsum) ][
      ses, on = "subjectkey"]

# scale
# ses$ParEd_max.s <- as.numeric(scale(ses$ParEd_max))
# ses$demo_comb_income_v2.s <- as.numeric(scale(ses$demo_comb_income_v2))
# ses$reshist_addr1_adi_wsum.s <- as.numeric(scale(ses$reshist_addr1_adi_wsum))

# making pPCA

sibs <- ses[sibs, on = "subjectkey"]

sibs$ses <- as.numeric(pcaMethods::ppca(BiocGenerics::t(sibs[, .(ParEd_max, demo_comb_income_v2, reshist_addr1_adi_wsum)]), nPcs = 1, seed = 42)@loadings)

sibs <- fread("data/abcd_pgs.txt")[sibs, on = "subjectkey"]
sibs <- fread('data/pca_data_ethnicity_PC20.txt', fill = T)[,1:22][sibs, on = "subjectkey"] #[name == "White"]

# so now I need to regress DVs and PGS from PC's
# important: you do not rescale!
dvs <- c("nihtbx_cryst_uncorrected", "nihtbx_fluidcomp_uncorrected", "nihtbx_list_uncorrected")
common_cols <- c(dvs, "pgs", "ses",
                 "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")
# subseting the main df
sibs <- sibs[sibs[, complete.cases(.SD), .SDcols = common_cols,]][
  , (dvs) := lapply(.SD, vec_to_fence), .SDcols=dvs # bringing the dvs to the fence
]


# sibs <- sibs[, pgs := pgs*10000 # scalling by 100 and adding 10 for subtractions
#                  ][
#                    , pgs := pgs+10
#                  ]
# # ********

# the beta for between families mean
pgs_b <- sibs[, .(pgs_b = mean(pgs)), by = "rel_family_id"]

scale_cols <- c(dvs, "pgs_w", "pgs_b", "ses")

sibs <- pgs_b[sibs, on = "rel_family_id"][
  , pgs_w := pgs - pgs_b
  ][
    , (scale_cols) := lapply(.SD, function(x) as.numeric(scale(x))), .SDcols=scale_cols # standardizing all the relevant info
  ]



# ********
### check the code above the last two subs have identical pgs_d scaled vals!!!

# sibs <- umx::umx_residualize(c(dvs, "ses", "pgs_w", "pgs_b"), c("C1" ,"C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20"),
#                              data = sibs)

sibs <- sibs[, .(nihtbx_cryst_uncorrected, nihtbx_fluidcomp_uncorrected, nihtbx_list_uncorrected,
         pgs_w, pgs_b, ses, rel_family_id,
         C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C15, C16, C17, C18, C19, C20)]



# put them in as covariates

cy_mod <- lmerTest::lmer(nihtbx_cryst_uncorrected ~ pgs_w + pgs_b +
                         C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + 
                           C11 + C12 + C13 + C14 + C15 + C15 + C16 + C17 + C18 + C19 + C20 + (1|rel_family_id), data = sibs)
cy_mod_ses <- lmerTest::lmer(nihtbx_cryst_uncorrected ~ pgs_w + pgs_b + ses +
                               C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + 
                               C11 + C12 + C13 + C14 + C15 + C15 + C16 + C17 + C18 + C19 + C20 + (1|rel_family_id), data = sibs)
summary(cy_mod); summary(cy_mod_ses); 

fi_mod <- lmerTest::lmer(nihtbx_fluidcomp_uncorrected ~ pgs_w + pgs_b +
                           C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + 
                           C11 + C12 + C13 + C14 + C15 + C15 + C16 + C17 + C18 + C19 + C20 + (1|rel_family_id), data = sibs)
fi_mod_ses <- lmerTest::lmer(nihtbx_fluidcomp_uncorrected ~ pgs_w + pgs_b + ses +
                               C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + 
                               C11 + C12 + C13 + C14 + C15 + C15 + C16 + C17 + C18 + C19 + C20 + (1|rel_family_id), data = sibs)

summary(fi_mod); summary(fi_mod_ses)
# Because there is the between SES is non-sig in line with Selzem
# they are overlaping in their effect on Fluid
# check how they reached their conclusion
# they did something else because it's a mixed effects model
# is there something else I need to put in...?
# SES is already correcting for this to some extent
# a lot of genetic indirect effects on cognition is because of SES (write to reviewer)




list_mod <- lmerTest::lmer(nihtbx_list_uncorrected ~ pgs_w + pgs_b +
                             C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + 
                             C11 + C12 + C13 + C14 + C15 + C15 + C16 + C17 + C18 + C19 + C20 + (1|rel_family_id), data = sibs)
list_mod_ses <- lmerTest::lmer(nihtbx_list_uncorrected ~ pgs_w + pgs_b + ses +
                                 C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + 
                                 C11 + C12 + C13 + C14 + C15 + C15 + C16 + C17 + C18 + C19 + C20 + (1|rel_family_id), data = sibs)
summary(list_mod); summary(list_mod_ses)

















