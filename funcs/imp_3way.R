# imp_3way, this fucntion is meant to impute a 3way interaction regression discountiy model
# lmer(dv ~ 1 + age + ses*pgs*school + (1 | site), REML = FALSE))

# May 5th, 2021 Keller, 2014 Biol Psychiatry need age*PGS & age*SES (these terms we aren't interested in yet still need to control for)
# age_d, age_d.pgs_d, age_d.ses_d
# school*age is intentionally left out as this is an assumption of regression discounity

imp_3way <- function(d){
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print("this function is hard coded!!!!!")
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print("make sure cols are in this order: site_id_l, DV (coded as dv), ses_ppca.s, pgs.s, age_yrs, schooling_yrs, sex, plus 20PC")
  
  require(mice); require(miceadds)
  

  
  colnames(d) <- c("site", "dv", "ses", "pgs", "age", "school", "sex",
                   "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")
  d$site <- as.numeric(as.factor(d$site))
  
  d <- data.frame(d, dv_m = NA, pgs_m = NA, ses_m = NA, age_m = NA, school_m = NA, #sex_m = NA, # holds the cluster means; you don't need sex since there is no interaction
               #   C1_m = NA, C2_m = NA, C3_m = NA, C4_m = NA, C5_m = NA, C6_m = NA, C7_m = NA, C8_m = NA, C9_m = NA, C10_m = NA,  # they don't interact!
              #    C11_m = NA, C12_m = NA, C13_m = NA, C14_m = NA, C15_m = NA, C16_m = NA, C17_m = NA, C18_m = NA, C19_m = NA, C20_m = NA, #20PCs
                  dv_d = NA, pgs_d = NA, ses_d = NA, school_d = NA, age_d = NA, # will hold the deviations from their cluster means
                  dv_d.ses_d = NA, dv_d.pgs_d = NA, dv_d.school_d = NA, dv_d.age_d = NA, # are two-way interactions of level-1 variables scaled as deviations from the cluster means
                  pgs_d.ses_d = NA, pgs_d.school_d = NA, ses_d.school_d = NA, age_d.pgs_d = NA, age_d.ses_d = NA,
                  pgs_d.ses_d.school_d = NA)  # three way interaction modeled
  
  # level-1 variables
  meth <- make.method(d)
  meth[c("dv", "pgs", "ses", "age", "school", "sex",
         "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", 
         "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- "2l.pmm"
  
  #?miceadds::mice.impute.2l.pmm()
  
  pred <- make.predictorMatrix(d)
  pred[,] <- 0
  pred[, "site"] <- -2
  codes <- c(3, 3, 3, 3, 3 , 1, 1, 1, 1, 1)
  
  pred["dv", c("pgs", "ses", "age", "school", "sex", "pgs_d.ses_d", "pgs_d.school_d", "ses_d.school_d", "pgs_d.ses_d.school_d", "age_d.pgs_d", "age_d.ses_d",
               "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(codes, 1, rep(3, 20)) # 3 way added
  
  pred["pgs", c("dv", "ses", "age", "school", "sex", "dv_d.ses_d", "dv_d.school_d", "dv_d.age_d", "ses_d.school_d", "age_d.ses_d", 
                "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(codes, rep(3, 20))
  pred["ses", c("dv", "pgs", "age", "school", "sex", "dv_d.pgs_d", "pgs_d.school_d", "dv_d.school_d", "dv_d.age_d", "age_d.pgs_d",
                "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(codes, rep(3, 20))
  
  pred["school", c("dv", "pgs", "ses", "age", "sex", "dv_d.pgs_d", "dv_d.ses_d", "pgs_d.ses_d", "age_d.pgs_d", "age_d.ses_d", "dv_d.age_d", # one more because there is no school*age interaction
                   "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(codes, 1, rep(3, 20))
  
  # beacuse of the 3 way there's one less
  pred["age", c("dv", "pgs", "ses", "school", "sex", "dv_d.pgs_d", "dv_d.ses_d", "pgs_d.ses_d", "pgs_d.school_d", "dv_d.school_d", "ses_d.school_d", "pgs_d.ses_d.school_d",
                "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(codes,1,1, rep(3, 20))
  
  
  
  
  # not included in interactions...
  pred["sex", c("dv", "pgs", "ses", "school", "age",
                "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  
  # making the others predict off the PCs
  
  pred["C1", c("dv", "pgs", "ses", "school", "age", "sex",
                "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C2", c("dv", "pgs", "ses", "school", "age", "sex",
               "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C3", c("dv", "pgs", "ses", "school", "age", "sex",
               "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C4", c("dv", "pgs", "ses", "school", "age", "sex",
               "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C5", c("dv", "pgs", "ses", "school", "age", "sex",
               "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  
  pred["C6", c("dv", "pgs", "ses", "school", "age", "sex",
               "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C7", c("dv", "pgs", "ses", "school", "age", "sex",
               "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C8", c("dv", "pgs", "ses", "school", "age", "sex",
               "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C9", c("dv", "pgs", "ses", "school", "age", "sex",
               "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C10", c("dv", "pgs", "ses", "school", "age", "sex",
               "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C1", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  
  pred["C11", c("dv", "pgs", "ses", "school", "age", "sex",
                "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C12", c("dv", "pgs", "ses", "school", "age", "sex",
                "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C13", c("dv", "pgs", "ses", "school", "age", "sex",
                "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C14", c("dv", "pgs", "ses", "school", "age", "sex",
                "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C15", c("dv", "pgs", "ses", "school", "age", "sex",
               "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C1", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  
  pred["C16", c("dv", "pgs", "ses", "school", "age", "sex",
                "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C17", c("dv", "pgs", "ses", "school", "age", "sex",
                "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C18", c("dv", "pgs", "ses", "school", "age", "sex",
                "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C19", c("dv", "pgs", "ses", "school", "age", "sex",
                "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  pred["C20", c("dv", "pgs", "ses", "school", "age", "sex",
                "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- c(rep(3, 25))
  
  
  
  # starting to do things for the interactions, we need to get the means so we can subtract them so we can get there deviances, which than we take the product of
  
  # derive group means
  meth[c("pgs_m", "ses_m", "dv_m", "age_m", "school_m")] <- "2l.groupmean"
         # "C1_m", "C2_m", "C3_m", "C4_m", "C5_m", "C6_m", "C7_m", "C8_m", "C9_m", "C10_m", 
         # "C11_m", "C12_m", "C13_m", "C14_m", "C15_m", "C16_m", "C17_m", "C18_m", "C19_m", "C20_m")] <- "2l.groupmean"
  
  pred[c("pgs_m", "ses_m", "dv_m", "age_m", "school_m"), 
         # "sex_m", "C1_m", "C2_m", "C3_m", "C4_m", "C5_m", "C6_m", "C7_m", "C8_m", "C9_m", "C10_m", 
         # "C11_m", "C12_m", "C13_m", "C14_m", "C15_m", "C16_m", "C17_m", "C18_m", "C19_m", "C20_m"), 
       c("pgs", "ses", "dv", "age", "school")] <- diag(5)
         # "sex", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", 
         # "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")] <- diag(26)
         # 
  
  
  
  # derive deviations from cluster mean, within each cluster the deviation from the grad mean is coded
  meth["pgs_d"] <- "~ I(pgs - pgs_m)"
  meth["dv_d"] <- "~ I(dv - dv_m)"
  meth["ses_d"] <- "~ I(ses - ses_m)"
  meth["school_d"] <- "~ I(school - school_m)"
  meth["age_d"] <- "~ I(age - age_m)"
  
  
  # derive interactions, by means of passive imputation.
  
  # making interaction for the dependent as recommended
  meth["dv_d.ses_d"] <- "~ I(dv_d * ses_d)"
  meth["dv_d.pgs_d"] <- "~ I(dv_d * pgs_d)"
  meth["dv_d.school_d"] <- "~ I(dv_d * school_d)"
  meth["dv_d.age_d"] <- "~ I(dv_d * age_d)"
  # now the other terms
  meth["pgs_d.ses_d"] <- "~ I(pgs_d * ses_d)"
  meth["pgs_d.school_d"] <- "~ I(pgs_d * school_d)"
  meth["ses_d.school_d"] <- "~ I(ses_d * school_d)"
  meth["age_d.pgs_d"] <- "~ I(age_d * pgs_d)"
  meth["age_d.ses_d"] <- "~ I(age_d * ses_d)"
  # now the 3way
  meth["pgs_d.ses_d.school_d"] <- "~ I(ses_d * school_d * pgs_d)"
  
  
  visit <- c("dv", "dv_m", "dv_d", "dv_d.ses_d", "dv_d.pgs_d", "dv_d.school_d",
             "pgs", "pgs_m", "pgs_d", "pgs_d.ses_d", "dv_d.pgs_d", "pgs_d.school_d", "pgs_d.ses_d.school_d",
             "ses", "ses_m", "ses_d", "pgs_d.ses_d", "dv_d.ses_d", "ses_d.school_d", "pgs_d.ses_d.school_d",
             "age", "age_m", "age_d", "age_d.pgs_d", "age_d.ses_d",
             "school", "school_m", "school_d", "dv_d.school_d", "pgs_d.school_d", "ses_d.school_d", "pgs_d.ses_d.school_d",
             "sex", 
             "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20")
  
  imp <- mice(d, pred = pred, meth = meth, seed = 188,
              visit = visit, m = 20, print = FALSE,
              allow.na = TRUE)
  
  long <- mice::complete(imp, "long", include = TRUE)
  imp <- as.mids(long)
  
  return(imp)
}

















