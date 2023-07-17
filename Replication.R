library(rstudioapi)
# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

## Packages
library(lfe)             # Support multiple group fixed-effect regression
library(stargazer)
#library(AER)
library(tidyverse)
library(broom)
library(CLmisc) # For reducing felm object size

df <- read_dta("D:\\My workshop\\PhD\\Teaching\\2021_Workflow of statistical analysis in R\\Laouenan_Rathelot_2020\\data\\base_airbnb_AEJ.dta")
df <- df %>% filter(active_listing=="1")
df <- df %>% mutate(sqrev100 = rev100^2)
df <- df %>% mutate(lastrat78 = lastrat7 + lastrat8)
df <- df %>% mutate(sample_black = ifelse(mino == 4 | mino == 5, 1, 0))
df <- df %>% mutate(sample_arabic = ifelse(mino == 2 | mino == 3, 1, 0))
df <- df %>% mutate(uscan = us + can)

### REPLICATION-----
# Properties characteristics: lesX
# lesX = sharedflat $descrip_flat $loueur $count
# $descrip_flat = $size $descrip_gen $descrip_spe $equip $rules $missing
# $loueur =  more_1_flat year2009 year2010 year2011 year2012 year2013 year2014
#           year2015 superhost verified_email verified_offline verified_phone facebook
# $count = count_descrip count_about count_languages count_rules picture_count noccur_pro_true change_pics
# $size = person_capacity345 bedrooms bathrooms
# $descrip_gen = appart house_loft
# $descrip_spe = couch airbed sofa futon
# $equip = cabletv wireless heating ac elevator handiaccess doorman 
#          fireplace washer dryer parking gym pool buzzer hottub breakfast family events
# $rules = people extrapeople cancel_policy smoking_allowed pets_allowed
# $missing = missingyear missingcabletv missingwireless missingheating 
#            missingac missingelevator missinghandiaccess missingdoorman missingfireplace
#            missingwasher missingdryer missingparking missinggym missingpool missingbuzzer
#            missinghottub missingbreakfast missingfamily missingevents missingcancel_policy
#            missingnoccur_pro_true missingverified_email missingverified_phone missingfacebook
#            missingverified_offline missingsuperhost
# $ratinglist = i.accuracy_rating i.cleanliness_rating i.checkin_rating
#            i.communication_rating i.location_rating i.value_rating i.rating_visible
#            accuracy_ratingNA cleanliness_ratingNA checkin_ratingNA
#            communication_ratingNA location_ratingNA value_ratingNA rating_visibleNA
missing <- paste0("missingyear + missingcabletv + missingwireless + missingheating + missingac + missingelevator + missinghandiaccess +",
                  "missingdoorman + missingfireplace + missingwasher + missingdryer + missingparking + missinggym + missingpool +",
                  "missingbuzzer + missinghottub + missingbreakfast + missingfamily + missingevents + missingcancel_policy +",
                  "missingnoccur_pro_true + missingverified_email + missingverified_phone + missingfacebook + missingverified_offline +",
                  "missingsuperhost")
rules <- paste0("people + extrapeople + cancel_policy + smoking_allowed + pets_allowed") 
equip <- paste0("cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + washer + dryer + parking +",
                "gym + pool + buzzer + hottub + breakfast + family + events")
descrip_spe <- paste0("couch + airbed + sofa + futon")
descrip_gen <- paste0("appart + house_loft")
size <- paste0("person_capacity345 + bedrooms + bathrooms")
count <- paste0("count_descrip + count_about + count_languages + count_rules + picture_count + noccur_pro_true + change_pics")
loueur <- paste0("more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 +",
                 "superhost + verified_email + verified_offline + verified_phone + facebook")
descrip_flat <- paste0("size $descrip_gen $descrip_spe $equip $rules $missing")
lesX <- paste0("sharedflat +",size,"+",descrip_gen,"+",descrip_spe,"+",equip,"+",rules,"+",missing,"+",loueur,"+",count)
ratinglist <- paste0("accuracy_rating + cleanliness_rating + checkin_rating + communication_rating + location_rating +",
                     "value_rating + rating_visible + accuracy_ratingNA + cleanliness_ratingNA + checkin_ratingNA +",
                     "communication_ratingNA + location_ratingNA + value_ratingNA + rating_visibleNA")

### VERIFICATION: TABLE 1-----
tab1 <- felm(log_price ~ factor(mino) | citywaveID,  data = df)
stargazer(tab1, type = "text", digits = 3, keep = c(1:4)) 
stargazer(tab1, type = "latex", digits = 3, keep = c(1:4))
gc()
#rm(tab1) #for space

### VERIFICATION: TABLE 2-----
tab2.1 <- felm( log_price ~ minodummy | citywaveID | 0 | newid, data = df)
tab2.2 <- felm( formula = log_price ~ minodummy + sharedflat + person_capacity345 + bedrooms + 
                  bathrooms+appart + house_loft+couch + airbed + sofa + futon+cabletv + wireless + 
                  heating + ac + elevator + handiaccess + doorman + fireplace + washer + dryer + 
                  parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                  extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + 
                  missingcabletv + missingwireless + missingheating + missingac + missingelevator + 
                  missinghandiaccess +missingdoorman + missingfireplace + missingwasher + missingdryer + 
                  missingparking + missinggym + missingpool +missingbuzzer + missinghottub + missingbreakfast + 
                  missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                  missingverified_email + missingverified_phone + missingfacebook + 
                  missingverified_offline +missingsuperhost+more_1_flat + year2009 + 
                  year2010 + year2011 + year2012 + year2013 + year2014 + year2015 +
                  superhost + verified_email + verified_offline + verified_phone + facebook+count_descrip + 
                  count_about + count_languages + count_rules + picture_count + noccur_pro_true + 
                  change_pics | citywaveID | 0 | newid   , data=df )            
tab2.3 <- felm( log_price ~ minodummy | citywaveID + hoodcityID + blockID |  0   |  newid  , data=df )
# ALERT: Long run-time (...81 vectors centred in 4218 seconds)
tab2.4 <- felm( formula = log_price ~ minodummy + sharedflat + person_capacity345 + bedrooms + 
                  bathrooms+appart + house_loft+couch + airbed + sofa + futon+cabletv + wireless + 
                  heating + ac + elevator + handiaccess + doorman + fireplace + washer + dryer + 
                  parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                  extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + 
                  missingcabletv + missingwireless + missingheating + missingac + missingelevator + 
                  missinghandiaccess +missingdoorman + missingfireplace + missingwasher + missingdryer + 
                  missingparking + missinggym + missingpool +missingbuzzer + missinghottub + missingbreakfast + 
                  missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                  missingverified_email + missingverified_phone + missingfacebook + 
                  missingverified_offline +missingsuperhost+more_1_flat + year2009 + 
                  year2010 + year2011 + year2012 + year2013 + year2014 + year2015 +
                  superhost + verified_email + verified_offline + verified_phone + facebook+count_descrip + 
                  count_about + count_languages + count_rules + picture_count + noccur_pro_true + 
                  change_pics | citywaveID + hoodcityID + blockID | 0 | newid   , data=df )            

stargazer(tab2.1,tab2.2,tab2.3,tab2.4, type = "text", digits = 3, keep = 1)
stargazer(tab2.1,tab2.2,tab2.3,tab2.4, type = "latex", digits = 3, keep = 1)
gc()
#rm(tab2.1,tab2.2,tab2.3,tab2.4) #for space

### VERIFICATION: TABLE 3-----
tab3.1 <- felm( formula = log_price ~ minodummy + sharedflat + person_capacity345 + bedrooms +  ### ERROR WITH as.factor() TRANSFORMATION
                  bathrooms+appart + house_loft+couch + airbed + sofa + futon+cabletv + wireless + 
                  heating + ac + elevator + handiaccess + doorman + fireplace + washer + dryer + 
                  parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                  extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + 
                  missingcabletv + missingwireless + missingheating + missingac + missingelevator + 
                  missinghandiaccess +missingdoorman + missingfireplace + missingwasher + missingdryer + 
                  missingparking + missinggym + missingpool +missingbuzzer + missinghottub + missingbreakfast + 
                  missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                  missingverified_email + missingverified_phone + missingfacebook + 
                  missingverified_offline +missingsuperhost+more_1_flat + year2009 + 
                  year2010 + year2011 + year2012 + year2013 + year2014 + year2015 +
                  superhost + verified_email + verified_offline + verified_phone + facebook+count_descrip + 
                  count_about + count_languages + count_rules + picture_count + 
                  noccur_pro_true + change_pics| citywaveID + hoodcityID + blockID | 0 | blockID  , data=df[df$review == 0,])
gc(); model3.1s <- reduce_felm_object_size(tab3.1)
#rm(tab3.1) #for space

tab3.2 <- felm( formula = log_price ~ minodummy + sharedflat + person_capacity345 + bedrooms + 
                  bathrooms+appart + house_loft+couch + airbed + sofa + futon+cabletv + wireless + 
                  heating + ac + elevator + handiaccess + doorman + fireplace + washer + dryer + 
                  parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                  extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + 
                  missingcabletv + missingwireless + missingheating + missingac + missingelevator + 
                  missinghandiaccess +missingdoorman + missingfireplace + missingwasher + missingdryer + 
                  missingparking + missinggym + missingpool +missingbuzzer + missinghottub + missingbreakfast + 
                  missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                  missingverified_email + missingverified_phone + missingfacebook + 
                  missingverified_offline +missingsuperhost+more_1_flat + year2009 + 
                  year2010 + year2011 + year2012 + year2013 + year2014 + year2015 +
                  superhost + verified_email + verified_offline + verified_phone + facebook+count_descrip + 
                  count_about + count_languages + count_rules + picture_count + noccur_pro_true + change_pics + 
                  accuracy_rating + cleanliness_rating + checkin_rating + communication_rating + 
                  location_rating + value_rating + rating_visible + accuracy_ratingNA + cleanliness_ratingNA + 
                  checkin_ratingNA + communication_ratingNA + location_ratingNA + value_ratingNA + 
                  rating_visibleNA | citywaveID + hoodcityID + blockID | 0 | blockID   , data=df[df$review > 0 & df$review < 5,] )
gc(); model3.2s <- reduce_felm_object_size(tab3.2)
#rm(tab3.2) # for space

tab3.3 <- felm( formula = log_price ~ minodummy + sharedflat + person_capacity345 + bedrooms + 
                  bathrooms+appart + house_loft+couch + airbed + sofa + futon+cabletv + wireless + 
                  heating + ac + elevator + handiaccess + doorman + fireplace + washer + dryer + 
                  parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                  extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + 
                  missingcabletv + missingwireless + missingheating + missingac + missingelevator + 
                  missinghandiaccess +missingdoorman + missingfireplace + missingwasher + missingdryer + 
                  missingparking + missinggym + missingpool +missingbuzzer + missinghottub + missingbreakfast + 
                  missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                  missingverified_email + missingverified_phone + missingfacebook + 
                  missingverified_offline +missingsuperhost+more_1_flat + year2009 + 
                  year2010 + year2011 + year2012 + year2013 + year2014 + year2015 +
                  superhost + verified_email + verified_offline + verified_phone + facebook+count_descrip + 
                  count_about + count_languages + count_rules + picture_count + noccur_pro_true + change_pics + 
                  accuracy_rating + cleanliness_rating + checkin_rating + communication_rating + 
                  location_rating + value_rating + rating_visible + accuracy_ratingNA + cleanliness_ratingNA + 
                  checkin_ratingNA + communication_ratingNA + location_ratingNA + value_ratingNA + 
                  rating_visibleNA | citywaveID + hoodcityID + blockID | 0 | blockID   , data=df[df$review > 4 & df$review < 20,] )
gc(); model3.3s <- reduce_felm_object_size(tab3.3)
#rm(tab3.3) #for space

tab3.4 <- felm( formula = log_price ~ minodummy + sharedflat + person_capacity345 + bedrooms + 
                  bathrooms+appart + house_loft+couch + airbed + sofa + futon+cabletv + wireless + 
                  heating + ac + elevator + handiaccess + doorman + fireplace + washer + dryer + 
                  parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                  extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + 
                  missingcabletv + missingwireless + missingheating + missingac + missingelevator + 
                  missinghandiaccess +missingdoorman + missingfireplace + missingwasher + missingdryer + 
                  missingparking + missinggym + missingpool +missingbuzzer + missinghottub + missingbreakfast + 
                  missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                  missingverified_email + missingverified_phone + missingfacebook + 
                  missingverified_offline +missingsuperhost+more_1_flat + year2009 + 
                  year2010 + year2011 + year2012 + year2013 + year2014 + year2015 +
                  superhost + verified_email + verified_offline + verified_phone + facebook+count_descrip + 
                  count_about + count_languages + count_rules + picture_count + noccur_pro_true + change_pics + 
                  accuracy_rating + cleanliness_rating + checkin_rating + communication_rating + 
                  location_rating + value_rating + rating_visible + accuracy_ratingNA + cleanliness_ratingNA + 
                  checkin_ratingNA + communication_ratingNA + location_ratingNA + value_ratingNA + 
                  rating_visibleNA | citywaveID + hoodcityID + blockID | 0 | blockID   , data=df[df$review >19 & df$review < 50,] )
gc(); model3.4s <- reduce_felm_object_size(tab3.4)
# rm(tab3.4) # for space

tab3.5 <- felm( formula = log_price ~ minodummy + sharedflat + person_capacity345 + bedrooms + 
                  bathrooms+appart + house_loft+couch + airbed + sofa + futon+cabletv + wireless + 
                  heating + ac + elevator + handiaccess + doorman + fireplace + washer + dryer + 
                  parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                  extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + 
                  missingcabletv + missingwireless + missingheating + missingac + missingelevator + 
                  missinghandiaccess +missingdoorman + missingfireplace + missingwasher + missingdryer + 
                  missingparking + missinggym + missingpool +missingbuzzer + missinghottub + missingbreakfast + 
                  missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                  missingverified_email + missingverified_phone + missingfacebook + 
                  missingverified_offline +missingsuperhost+more_1_flat + year2009 + 
                  year2010 + year2011 + year2012 + year2013 + year2014 + year2015 +
                  superhost + verified_email + verified_offline + verified_phone + facebook+count_descrip + 
                  count_about + count_languages + count_rules + picture_count + noccur_pro_true + change_pics + 
                  accuracy_rating + cleanliness_rating + checkin_rating + communication_rating + 
                  location_rating + value_rating + rating_visible + accuracy_ratingNA + cleanliness_ratingNA + 
                  checkin_ratingNA + communication_ratingNA + location_ratingNA + value_ratingNA + 
                  rating_visibleNA | citywaveID + hoodcityID + blockID | 0 | blockID   , data=df[df$review >49 ,] )
gc(); model3.5s <- reduce_felm_object_size(tab3.5)
# rm(tab3.5) # for space
stargazer(model3.1s,model3.2s,model3.3s,model3.4s,model3.5s,type = "text", digits = 3, keep = 1)
stargazer(model3.1s,model3.2s,model3.3s,model3.4s,model3.5s,type = "latex", digits = 3, keep = 1)

### VERIFICATION: TABLE 4-----
### TABLE 4
# Generate variables
rho = 0.137
diff = 500
iter = 1
df <- df %>% mutate(KKrho = (rev100)/(rho+rev100))
df <- df %>% mutate(lastrat10_KKrho = lastrat10*KKrho,
                    lastrat9_KKrho=lastrat9*KKrho,
                    lastrat8_KKrho=lastrat8*KKrho,
                    lastrat7_KKrho=lastrat7*KKrho,
                    minodummy_KKrho=-minodummy*KKrho)

model4.1 <- felm( formula = log_price ~ lastrat10_KKrho + lastrat9_KKrho + lastrat8_KKrho + lastrat7_KKrho + minodummy_KKrho + 
                    minodummy:relevel(factor(wave), ref = "10") +
                    sharedflat +person_capacity345 + bedrooms + bathrooms+appart + house_loft+couch + airbed + 
                    sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                    washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                    extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                    missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                    missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                    missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                    missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                    more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                    verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                    noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df)

model4.1s <- reduce_felm_object_size(model4.1)
rm(model4.1);gc()

stargazer(model4.1s, type="latex")

model4.2 <- felm( formula = log_price ~ lastrat7 + lastrat8 + lastrat9 + minodummy + minodummy_KKrho + KKrho + 
                    minodummy:relevel(factor(wave), ref = "10") + 
                    sharedflat +person_capacity345 + bedrooms + bathrooms+appart + house_loft+couch + airbed + 
                    sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                    washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                    extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                    missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                    missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                    missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                    missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                    more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                    verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                    noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df)

model4.2s <- reduce_felm_object_size(model4.2)
rm(model4.2);gc()

stargazer(model4.2s, type="latex")

### VERIFICATION: TABLE 5 -----
## Regression 1 Table 5
model5.1 <- felm( formula = log_price ~ lastrat7:rev100 + lastrat8:rev100 + lastrat9:rev100 + lastrat10:rev100 + minodummy:rev100 + 
                              minodummy:relevel(factor(wave), ref = "10") +  # Note: The base level is set for wave (ib10.wave). I don't know what this is for.
                              sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                              sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                              washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                              extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                              missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                              missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                              missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                              missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                              more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                              verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                              noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df[df$review >0 & df$review <40,])
model5.1s <- reduce_felm_object_size(model5.1)
rm(model5.1);gc()

## Regression 2 Table 5
model5.2 <- felm( formula = log_price ~ lastrat7:rev100 + lastrat8:rev100 + lastrat9:rev100 + lastrat10:rev100 + minodummy:rev100 + 
                    minodummy:relevel(factor(wave), ref = "10") +  # Note: The base level is set for wave (ib10.wave). I don't know what this is for.
                    sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                    sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                    washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                    extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                    missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                    missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                    missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                    missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                    more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                    verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                    noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df[df$review >0 & df$review <60,])

model5.2s <- reduce_felm_object_size(model5.2)
rm(model5.2);gc()

## Regression 3 Table 5
model5.4 <- felm( formula = log_price ~ lastrat7:rev100 + lastrat8:rev100 + lastrat9:rev100 + lastrat10:rev100 + minodummy:rev100 + 
                    lastrat7:sqrev100 + lastrat8:sqrev100 + lastrat9:sqrev100 + lastrat10:sqrev100 + minodummy:sqrev100 +
                    minodummy:relevel(factor(wave), ref = "10") +  # Note: The base level is set for wave (ib10.wave). I don't know what this is for.
                    sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                    sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                    washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                    extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                    missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                    missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                    missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                    missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                    more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                    verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                    noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df[df$review >0 & df$review <80,])

model5.4s <- reduce_felm_object_size(model5.4)
rm(model5.4);gc()

### EXTENSION: TABLE 5 LINEAR-----
## K < 100 linear
model5.34 <- felm( formula = log_price ~ lastrat7:rev100 + lastrat8:rev100 + lastrat9:rev100 + lastrat10:rev100 + minodummy:rev100 + 
                     minodummy:relevel(factor(wave), ref = "10") +  # Note: The base level is set for wave (ib10.wave). I don't know what this is for.
                     sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                     sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                     washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                     extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                     missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                     missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                     missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                     missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                     more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                     verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                     noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df[df$review >0 & df$review <100,])

model5.34s <- reduce_felm_object_size(model5.34)
rm(model5.34);gc()

## K < 120 linear
model5.35 <- felm( formula = log_price ~ lastrat7:rev100 + lastrat8:rev100 + lastrat9:rev100 + lastrat10:rev100 + minodummy:rev100 + 
                     minodummy:relevel(factor(wave), ref = "10") +  # Note: The base level is set for wave (ib10.wave). I don't know what this is for.
                     sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                     sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                     washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                     extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                     missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                     missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                     missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                     missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                     more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                     verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                     noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df[df$review >0 & df$review <120,])

model5.35s <- reduce_felm_object_size(model5.35)
rm(model5.35);gc()

## K < 140 linear
model5.36 <- felm( formula = log_price ~ lastrat7:rev100 + lastrat8:rev100 + lastrat9:rev100 + lastrat10:rev100 + minodummy:rev100 + 
                     minodummy:relevel(factor(wave), ref = "10") +  # Note: The base level is set for wave (ib10.wave). I don't know what this is for.
                     sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                     sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                     washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                     extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                     missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                     missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                     missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                     missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                     more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                     verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                     noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df[df$review >0 & df$review <140,])

model5.36s <- reduce_felm_object_size(model5.36)
rm(model5.36);gc()

## Full sample linear
model5.37 <- felm( formula = log_price ~ lastrat7:rev100 + lastrat8:rev100 + lastrat9:rev100 + lastrat10:rev100 + minodummy:rev100 + 
                     minodummy:relevel(factor(wave), ref = "10") +  # Note: The base level is set for wave (ib10.wave). I don't know what this is for.
                     sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                     sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                     washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                     extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                     missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                     missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                     missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                     missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                     more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                     verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                     noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df)

model5.37s <- reduce_felm_object_size(model5.37)
rm(model5.37);gc()

### EXTENSION: TABLE 5 QUADRATIC-----
## K < 100 quadratic
model5.314 <- felm( formula = log_price ~ lastrat7:rev100 + lastrat8:rev100 + lastrat9:rev100 + lastrat10:rev100 + minodummy:rev100 + 
                      lastrat7:sqrev100 + lastrat8:sqrev100 + lastrat9:sqrev100 + lastrat10:sqrev100 + minodummy:sqrev100 +
                      minodummy:relevel(factor(wave), ref = "10") +  # Note: The base level is set for wave (ib10.wave). I don't know what this is for.
                      sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                      sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                      washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                      extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                      missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                      missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                      missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                      missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                      more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                      verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                      noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df[df$review >0 & df$review <100,])
model5.314s <- reduce_felm_object_size(model5.314)
rm(model5.314);gc()
## K < 120 quadratic
model5.315 <- felm( formula = log_price ~ lastrat7:rev100 + lastrat8:rev100 + lastrat9:rev100 + lastrat10:rev100 + minodummy:rev100 + 
                      lastrat7:sqrev100 + lastrat8:sqrev100 + lastrat9:sqrev100 + lastrat10:sqrev100 + minodummy:sqrev100 +
                      minodummy:relevel(factor(wave), ref = "10") +  # Note: The base level is set for wave (ib10.wave). I don't know what this is for.
                      sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                      sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                      washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                      extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                      missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                      missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                      missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                      missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                      more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                      verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                      noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df[df$review >0 & df$review <120,])

model5.315s <- reduce_felm_object_size(model5.315)
rm(model5.315);gc()
## K < 140 quadratic
model5.316 <- felm( formula = log_price ~ lastrat7:rev100 + lastrat8:rev100 + lastrat9:rev100 + lastrat10:rev100 + minodummy:rev100 + 
                      lastrat7:sqrev100 + lastrat8:sqrev100 + lastrat9:sqrev100 + lastrat10:sqrev100 + minodummy:sqrev100 +
                      minodummy:relevel(factor(wave), ref = "10") +  # Note: The base level is set for wave (ib10.wave). I don't know what this is for.
                      sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                      sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                      washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                      extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                      missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                      missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                      missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                      missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                      more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                      verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                      noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df[df$review >0 & df$review <140,])

model5.316s <- reduce_felm_object_size(model5.316)
rm(model5.316);gc()
## Full sample quadratic
model5.317 <- felm( formula = log_price ~ lastrat7:rev100 + lastrat8:rev100 + lastrat9:rev100 + lastrat10:rev100 + minodummy:rev100 + 
                      lastrat7:sqrev100 + lastrat8:sqrev100 + lastrat9:sqrev100 + lastrat10:sqrev100 + minodummy:sqrev100 +
                      minodummy:relevel(factor(wave), ref = "10") +  # Note: The base level is set for wave (ib10.wave). I don't know what this is for.
                      sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                      sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                      washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                      extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                      missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                      missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                      missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                      missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                      more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                      verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                      noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df)

model5.317s <- reduce_felm_object_size(model5.317)
rm(model5.317);gc()

models4 <- list(model5.34s,model5.35s,model5.36s,model5.37s)
models5 <- list(model5.314s,model5.315s,model5.316s,model5.317s)
K <- c("Number of Reviews","$K<100$","$K<120$","$K<140$","Full sample")
s7 <- c("N obs. (3.5 stars)", "66,256","66,291","66,301","89,709")
s8 <- c("N obs. (4 stars)","225,523","226,352","226,783","282,335")
s9 <- c("N obs. (4.5 stars)","918,180","926,405","931,323","1,039,134")
s10 <- c("N obs. (5 stars)","859,453","866,405","870,527","1,040,653")
stargazer(models4, type = "latex", keep = c("lastrat7:rev100","rev100:lastrat8","lastrat78:rev100","rev100:lastrat9","rev100:lastrat10","rev100:minodummy"),
          add.lines = list(K,s7,s8,s9,s10))
stargazer(models5, type = "latex", keep = c("lastrat7:rev100","rev100:lastrat8","lastrat78:rev100","rev100:lastrat9","rev100:lastrat10","rev100:minodummy",
                                            "lastrat7:sqrev100","lastrat8:sqrev100","lastrat78:sqrev100","lastrat9:sqrev100","lastrat10:sqrev100","minodummy:sqrev100"),
          add.lines = list(K,s7,s8,s9,s10))

### VERIFICATION: FIGURE 2-----
model_fig2.1 <- felm( formula = log_price ~ (lastrat7 + lastrat8 + lastrat9 + lastrat10):(revc1 + revc2 + revc3 + revc4 + revc5 + revc6) +
                    sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                    sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                    washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                    extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                    missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                    missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                    missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                    missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                    more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                    verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                    noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df)

model_fig2.1s <- reduce_felm_object_size(model_fig2.1);rm(model_fig2.1);gc()
stargazer(model_fig2.1s, type = "latex", keep = c(478:501))

model_fig2.2 <- felm( formula = log_price ~ (lastrat7 + lastrat8 + lastrat9 + lastrat10):(revc1 + revc2 + revc3 + revc4 + revc5 + revc6) +
                        sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                        sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                        washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                        extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                        missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                        missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                        missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                        missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                        more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                        verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                        noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df[df$minodummy==1,])

model_fig2.2s <- reduce_felm_object_size(model_fig2.2);rm(model_fig2.2);gc()
stargazer(model_fig2.1s,model_fig2.2s, type = "latex", keep = c(478:501))

## Create grid and spline
dd <- 1:60
knots=c(5,10,20,30,50)
lins = dd
for (ki in knots){lins = cbind(lins,pmax(0,dd-ki))}

## Import data
coef <- model_fig2.1s$coefficients[c(478:501)]
coef2 <- model_fig2.2s$coefficients[c(478:501)]

## Build predicted values
p7 <- coef[1:6]
p8 <- coef[6+(1:6)]
p9 <- coef[12+(1:6)]
p10 <- coef[18+(1:6)]
p11 <- coef2[1:6]
p12 <- coef2[6+(1:6)]
p13 <- coef2[12+(1:6)]
p14 <- coef2[18+(1:6)]

ff7 = (lins%*%p7)
#ff7[31:60] = NA
ff8 = (lins%*%p8)
ff9 = (lins%*%p9)
ff10 = (lins%*%p10)
ff11 = (lins%*%p11)
#ff7[31:60] = NA
ff12 = (lins%*%p12)
ff13 = (lins%*%p13)
ff14 = (lins%*%p14)


fmat <- rbind(ff7,ff8,ff9,ff10,ff11,ff12,ff13,ff14)

## Sort in dataset for ggplot
dat = data.frame(dd=rep(dd,times=8),ffe=fmat,
                 Rating =factor(rep(c("3.5 or less","4","4.5","5"),each=length(dd)),
                              level=rev(c("3.5 or less","4","4.5","5")),ordered=TRUE),
                 Group = factor(rep(c("Full sample","Minority"),each = 4*length(dd))))

## ggplot and export
ggplot(dat,aes(x=dd,y=ffe,linetype=Rating, colour=Group)) +
  geom_line() + 
  labs(x="Number of reviews",y="Relative price") + 
  theme_bw(base_size=15)

### EXTENSION: FIGURE 2 TO K < 200----
df <- df %>% mutate(rev1 = review,
                    rev2 = review -5,
                    rev3 = review -10,
                    rev4 = review -15,
                    rev5 = review -20,
                    rev6 = review -30,
                    rev7 = review -40,
                    rev8 = review -50,
                    rev9 = review -60,
                    rev10 = review -80,
                    rev11 = review - 100,
                    rev12 = review -120,
                    rev13 = review - 150,
                    rev14 = review - 300,
                    rev15 = review - 450,
                    rev16 = review - 600)

df$rev2 <- ifelse(df$rev2>0,df$rev2,0)
df$rev3 <- ifelse(df$rev3>0,df$rev3,0)
df$rev4 <- ifelse(df$rev4>0,df$rev4,0)
df$rev5 <- ifelse(df$rev5>0,df$rev5,0)
df$rev6 <- ifelse(df$rev6>0,df$rev6,0)
df$rev7 <- ifelse(df$rev7>0,df$rev7,0)
df$rev8 <- ifelse(df$rev8>0,df$rev8,0)
df$rev9 <- ifelse(df$rev9>0,df$rev9,0)
df$rev10 <- ifelse(df$rev10>0,df$rev10,0)
df$rev11 <- ifelse(df$rev11>0,df$rev11,0)
df$rev12 <- ifelse(df$rev12>0,df$rev12,0)
df$rev13 <- ifelse(df$rev13>0,df$rev13,0)
df$rev14 <- ifelse(df$rev14>0,df$rev14,0)
df$rev15 <- ifelse(df$rev15>0,df$rev15,0)
df$rev16 <- ifelse(df$rev16>0,df$rev16,0)

model_fig2.3 <- felm( formula = log_price ~ (lastrat7 + lastrat8 + lastrat9 + lastrat10):(rev1 + rev2 + rev3 + rev4 + rev5 + rev6 + rev7 + rev8 +
                        rev9 + rev10 + rev11 + rev12 + rev13) +
                        sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                        sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                        washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                        extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                        missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                        missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                        missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                        missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                        more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                        verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                        noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df)

model_fig2.3s <- reduce_felm_object_size(model_fig2.3);rm(model_fig2.3);gc()
stargazer(model_fig2.3s, type = "text", keep = c(478:529))

model_fig2.4 <- felm( formula = log_price ~ (lastrat7 + lastrat8 + lastrat9 + lastrat10):(rev1 + rev2 + rev3 + rev4 + rev5 + rev6 + rev7 + rev8 +
                        rev9 + rev10 + rev11 + rev12 + rev13) +
                        sharedflat + person_capacity345 + bedrooms + bathrooms + appart + house_loft+couch + airbed + 
                        sofa + futon+cabletv + wireless + heating + ac + elevator + handiaccess + doorman + fireplace + 
                        washer + dryer + parking +gym + pool + buzzer + hottub + breakfast + family + events+people + 
                        extrapeople + cancel_policy + smoking_allowed + pets_allowed+missingyear + missingcabletv + missingwireless + 
                        missingheating + missingac + missingelevator + missinghandiaccess +missingdoorman + missingfireplace + 
                        missingwasher + missingdryer + missingparking + missinggym + missingpool +missingbuzzer + missinghottub + 
                        missingbreakfast + missingfamily + missingevents + missingcancel_policy +missingnoccur_pro_true + 
                        missingverified_email + missingverified_phone + missingfacebook + missingverified_offline + missingsuperhost + 
                        more_1_flat + year2009 + year2010 + year2011 + year2012 + year2013 + year2014 + year2015 + superhost + verified_email + 
                        verified_offline + verified_phone + facebook + count_descrip + count_about + count_languages + count_rules + picture_count + 
                        noccur_pro_true + change_pics + as.factor(citywaveID) | newid | 0 | newid , data=df[df$minodummy==1,])

model_fig2.4s <- reduce_felm_object_size(model_fig2.4);rm(model_fig2.4);gc()
stargazer(model_fig2.3s,model_fig2.4s, type = "latex", keep = c(478:529))


## Create grid and spline
dd <- 1:200
knots=c(5,10,15,20,30,40,50,60,80,100,120,150)
lins = dd
for (ki in knots){lins = cbind(lins,pmax(0,dd-ki))}

## Import data
coef <- model_fig2.3s$coefficients[c(478:529)]
coef2 <- model_fig2.4s$coefficients[c(478:529)]

## Build predicted values
p7 <- coef[1:13]
p8 <- coef[13+(1:13)]
p9 <- coef[26+(1:13)]
p10 <- coef[39+(1:13)]
p11 <- coef2[1:13]
p11[12] <- 0; p11[13] <- 0
p12 <- coef2[13+(1:13)]
p13 <- coef2[26+(1:13)]
p14 <- coef2[39+(1:13)]

ff7 = (lins%*%p7)
#ff7[31:60] = NA
ff8 = (lins%*%p8)
ff9 = (lins%*%p9)
ff10 = (lins%*%p10)
ff11 = (lins%*%p11)
#ff7[31:60] = NA
ff12 = (lins%*%p12)
ff13 = (lins%*%p13)
ff14 = (lins%*%p14)


fmat <- rbind(ff7,ff8,ff9,ff10,ff11,ff12,ff13,ff14)

## Sort in dataset for ggplot
dat = data.frame(dd=rep(dd,times=8),ffe=fmat,
                 Rating =factor(rep(c("3.5 or less","4","4.5","5"),each=length(dd)),
                              level=rev(c("3.5 or less","4","4.5","5")),ordered=TRUE),
                 Group = factor(rep(c("Full sample","Minority"),each = 4*length(dd))))

## ggplot and export
ggplot(dat,aes(x=dd,y=ffe,linetype=Rating, colour=Group)) +
  geom_line() + 
  labs(x="Number of reviews",y="Relative price") + 
  theme_bw(base_size=15) + facet_wrap(~Group,nrow = 2) + theme_bw()
