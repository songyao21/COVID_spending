
#load in the data needed
fit_data_1_demean_final = read.csv("fit_data_1_demean_final.csv")
prefer_spec_data_sub_final = read.csv("prefer_spec_data_sub_final.csv")
fit_state_data1_demean_final = read.csv("fit_state_data1_demean_final.csv")



#model 1: Social Distancing Model
fit_sdis = felm(sd~per_case_total7+national_per_case_total7+new_case_total7_1000+national_new_case_total7_1000+rain+avg_temp
                |dow+week+fips|(closing_of_public_venues|non_essential|school_closure|shelter_in_place|
                                  gathering|religious_gatherings|max_mask|
                                  max_mask_logpop|max_mask_black|max_mask_trump2020|
                                  closing_of_public_venues_logpop|closing_of_public_venues_black|closing_of_public_venues_trump2020|
                                  non_essential_logpop|non_essential_black|non_essential_trump2020|
                                  school_closure_logpop|school_closure_black|school_closure_trump2020|
                                  shelter_in_place_logpop|shelter_in_place_black|shelter_in_place_trump2020|
                                  gathering_logpop|gathering_black|gathering_trump2020|
                                  religious_gatherings_logpop|religious_gatherings_black|religious_gatherings_trump2020
                                ~closing_of_public_venues_iv+non_essential_iv+school_closure_iv+shelter_in_place_iv+
                                  gathering_iv+religious_gatherings_iv+max_mask_iv+max_mask_iv_logpop+max_mask_iv_black+max_mask_iv_trump2020
                                +closing_of_public_venues_iv_logpop+closing_of_public_venues_iv_black+closing_of_public_venues_iv_trump2020
                                +non_essential_iv_logpop+non_essential_iv_black+non_essential_iv_trump2020
                                +school_closure_iv_logpop+school_closure_iv_black+school_closure_iv_trump2020
                                +shelter_in_place_iv_logpop+shelter_in_place_iv_black+shelter_in_place_iv_trump2020
                                +gathering_iv_logpop+gathering_iv_black+gathering_iv_trump2020
                                +religious_gatherings_iv_logpop+religious_gatherings_iv_black+religious_gatherings_iv_trump2020)|fips, data=fit_data_1_demean_final)
summary(fit_sdis)



#model 2: Case model
fit_case= felm(new_log_case~avg_temp+humidity|fips+date|
                 (sd|max_mask|sd_mask|close_pub_venues|non_essential|school_closure+shelter_in_place|gathering|religious_gather|
                    log_popden_sd|trump_sd|black_sd|
                    log_popden_max_mask|trump_max_mask|black_max_mask|
                    log_popden_close_pub_venues|trump_close_pub_venues|black_close_pub_venues|
                    log_popden_non_essential|trump_non_essential|black_non_essential|
                    log_popden_school_closure|trump_school_closure|black_school_closure|
                    log_popden_shelter_in_place|trump_shelter_in_place|black_shelter_in_place|
                    log_popden_gathering|trump_gathering|black_gathering|
                    log_popden_religious_gather|trump_religious_gather|black_religious_gather~
                    sd_lag+max_mask_lag+sd_mask_lag+close_pub_venues_iv+non_essential_iv+school_closure_iv+shelter_in_place_iv+gathering_iv+religious_gather_iv+
                    log_popden_sd_lag+trump_sd_lag+black_sd_lag+
                    log_popden_max_mask_lag+trump_max_mask_lag+black_max_mask_lag+
                    log_popden_close_pub_venues_iv+trump_close_pub_venues_iv+black_close_pub_venues_iv+
                    log_popden_non_essential_iv+trump_non_essential_iv+black_non_essential_iv+
                    log_popden_school_closure_iv+trump_school_closure_iv+black_school_closure_iv+
                    log_popden_shelter_in_place_iv+trump_shelter_in_place_iv+black_shelter_in_place_iv+
                    log_popden_gathering_iv+trump_gathering_iv+black_gathering_iv+
                    log_popden_religious_gather_iv+trump_religious_gather_iv+black_religious_gather_iv)|fips, data=prefer_spec_data_sub_final)
summary(fit_case)



#model 3: spending model
#Spending model measures spending in 7 categories:
#Overall
fit_all = felm(spend_all~avg_prcp+avg_temp+log_popden+trump_percent2020+black|0|
                 (state_sd|closing_of_public_venues|non_essential|school_closure|shelter_in_place|
                    gathering|religious_gatherings|max_mask|
                    max_mask_logpop|max_mask_black|max_mask_trump2020|
                    state_sd_logpop|state_sd_black|state_sd_trump2020|
                    closing_of_public_venues_logpop|closing_of_public_venues_black|closing_of_public_venues_trump2020|
                    non_essential_logpop|non_essential_black|non_essential_trump2020|
                    school_closure_logpop|school_closure_black|school_closure_trump2020|
                    shelter_in_place_logpop|shelter_in_place_black|shelter_in_place_trump2020|
                    gathering_logpop|gathering_black|gathering_trump2020|
                    religious_gatherings_logpop|religious_gatherings_black|religious_gatherings_trump2020+maxmask_sd
                  ~state_sd_iv+closing_of_public_venues_iv+non_essential_iv+school_closure_iv+shelter_in_place_iv+
                    gathering_iv+religious_gatherings_iv+max_mask_iv+max_mask_iv_logpop+max_mask_iv_black+max_mask_iv_trump2020
                  +state_sd_iv_logpop+state_sd_iv_black+state_sd_iv_trump2020
                  +closing_of_public_venues_iv_logpop+closing_of_public_venues_iv_black+closing_of_public_venues_iv_trump2020
                  +non_essential_iv_logpop+non_essential_iv_black+non_essential_iv_trump2020
                  +school_closure_iv_logpop+school_closure_iv_black+school_closure_iv_trump2020
                  +shelter_in_place_iv_logpop+shelter_in_place_iv_black+shelter_in_place_iv_trump2020
                  +gathering_iv_logpop+gathering_iv_black+gathering_iv_trump2020
                  +religious_gatherings_iv_logpop+religious_gatherings_iv_black+religious_gatherings_iv_trump2020+maxmask_sd_iv)|statefips, data=fit_state_data1_demean_final)
summary(fit_all)
#Accomodation and food service
fit_acf = felm(spend_acf~avg_prcp+avg_temp+log_popden+trump_percent2020+black|0|
                 (state_sd|closing_of_public_venues|non_essential|school_closure|shelter_in_place|
                    gathering|religious_gatherings|max_mask|
                    max_mask_logpop|max_mask_black|max_mask_trump2020|
                    state_sd_logpop|state_sd_black|state_sd_trump2020|
                    closing_of_public_venues_logpop|closing_of_public_venues_black|closing_of_public_venues_trump2020|
                    non_essential_logpop|non_essential_black|non_essential_trump2020|
                    school_closure_logpop|school_closure_black|school_closure_trump2020|
                    shelter_in_place_logpop|shelter_in_place_black|shelter_in_place_trump2020|
                    gathering_logpop|gathering_black|gathering_trump2020|
                    religious_gatherings_logpop|religious_gatherings_black|religious_gatherings_trump2020+maxmask_sd
                  ~state_sd_iv+closing_of_public_venues_iv+non_essential_iv+school_closure_iv+shelter_in_place_iv+
                    gathering_iv+religious_gatherings_iv+max_mask_iv+max_mask_iv_logpop+max_mask_iv_black+max_mask_iv_trump2020
                  +state_sd_iv_logpop+state_sd_iv_black+state_sd_iv_trump2020
                  +closing_of_public_venues_iv_logpop+closing_of_public_venues_iv_black+closing_of_public_venues_iv_trump2020
                  +non_essential_iv_logpop+non_essential_iv_black+non_essential_iv_trump2020
                  +school_closure_iv_logpop+school_closure_iv_black+school_closure_iv_trump2020
                  +shelter_in_place_iv_logpop+shelter_in_place_iv_black+shelter_in_place_iv_trump2020
                  +gathering_iv_logpop+gathering_iv_black+gathering_iv_trump2020
                  +religious_gatherings_iv_logpop+religious_gatherings_iv_black+religious_gatherings_iv_trump2020+maxmask_sd_iv)|statefips, data=fit_state_data1_demean_final)
summary(fit_acf)
#Arts entertainment and recreation
fit_aer = felm(spend_aer~avg_prcp+avg_temp+log_popden+trump_percent2020+black|0|
                 (state_sd|closing_of_public_venues|non_essential|school_closure|shelter_in_place|
                    gathering|religious_gatherings|max_mask|
                    max_mask_logpop|max_mask_black|max_mask_trump2020|
                    state_sd_logpop|state_sd_black|state_sd_trump2020|
                    closing_of_public_venues_logpop|closing_of_public_venues_black|closing_of_public_venues_trump2020|
                    non_essential_logpop|non_essential_black|non_essential_trump2020|
                    school_closure_logpop|school_closure_black|school_closure_trump2020|
                    shelter_in_place_logpop|shelter_in_place_black|shelter_in_place_trump2020|
                    gathering_logpop|gathering_black|gathering_trump2020|
                    religious_gatherings_logpop|religious_gatherings_black|religious_gatherings_trump2020+maxmask_sd
                  ~state_sd_iv+closing_of_public_venues_iv+non_essential_iv+school_closure_iv+shelter_in_place_iv+
                    gathering_iv+religious_gatherings_iv+max_mask_iv+max_mask_iv_logpop+max_mask_iv_black+max_mask_iv_trump2020
                  +state_sd_iv_logpop+state_sd_iv_black+state_sd_iv_trump2020
                  +closing_of_public_venues_iv_logpop+closing_of_public_venues_iv_black+closing_of_public_venues_iv_trump2020
                  +non_essential_iv_logpop+non_essential_iv_black+non_essential_iv_trump2020
                  +school_closure_iv_logpop+school_closure_iv_black+school_closure_iv_trump2020
                  +shelter_in_place_iv_logpop+shelter_in_place_iv_black+shelter_in_place_iv_trump2020
                  +gathering_iv_logpop+gathering_iv_black+gathering_iv_trump2020
                  +religious_gatherings_iv_logpop+religious_gatherings_iv_black+religious_gatherings_iv_trump2020+maxmask_sd_iv)|statefips, data=fit_state_data1_demean_final)
summary(fit_aer)
#General merchandise stroes and apparel and accessories
fit_apg = felm(spend_apg~avg_prcp+avg_temp+log_popden+trump_percent2020+black|0|
                 (state_sd|closing_of_public_venues|non_essential|school_closure|shelter_in_place|
                    gathering|religious_gatherings|max_mask|
                    max_mask_logpop|max_mask_black|max_mask_trump2020|
                    state_sd_logpop|state_sd_black|state_sd_trump2020|
                    closing_of_public_venues_logpop|closing_of_public_venues_black|closing_of_public_venues_trump2020|
                    non_essential_logpop|non_essential_black|non_essential_trump2020|
                    school_closure_logpop|school_closure_black|school_closure_trump2020|
                    shelter_in_place_logpop|shelter_in_place_black|shelter_in_place_trump2020|
                    gathering_logpop|gathering_black|gathering_trump2020|
                    religious_gatherings_logpop|religious_gatherings_black|religious_gatherings_trump2020+maxmask_sd
                  ~state_sd_iv+closing_of_public_venues_iv+non_essential_iv+school_closure_iv+shelter_in_place_iv+
                    gathering_iv+religious_gatherings_iv+max_mask_iv+max_mask_iv_logpop+max_mask_iv_black+max_mask_iv_trump2020
                  +state_sd_iv_logpop+state_sd_iv_black+state_sd_iv_trump2020
                  +closing_of_public_venues_iv_logpop+closing_of_public_venues_iv_black+closing_of_public_venues_iv_trump2020
                  +non_essential_iv_logpop+non_essential_iv_black+non_essential_iv_trump2020
                  +school_closure_iv_logpop+school_closure_iv_black+school_closure_iv_trump2020
                  +shelter_in_place_iv_logpop+shelter_in_place_iv_black+shelter_in_place_iv_trump2020
                  +gathering_iv_logpop+gathering_iv_black+gathering_iv_trump2020
                  +religious_gatherings_iv_logpop+religious_gatherings_iv_black+religious_gatherings_iv_trump2020+maxmask_sd_iv)|statefips, data=fit_state_data1_demean_final)
summary(fit_apg)spend_aerspend_apg
#Grocery and food store
fit_grf = felm(spend_grf~avg_prcp+avg_temp+log_popden+trump_percent2020+black|0|
                 (state_sd|closing_of_public_venues|non_essential|school_closure|shelter_in_place|
                    gathering|religious_gatherings|max_mask|
                    max_mask_logpop|max_mask_black|max_mask_trump2020|
                    state_sd_logpop|state_sd_black|state_sd_trump2020|
                    closing_of_public_venues_logpop|closing_of_public_venues_black|closing_of_public_venues_trump2020|
                    non_essential_logpop|non_essential_black|non_essential_trump2020|
                    school_closure_logpop|school_closure_black|school_closure_trump2020|
                    shelter_in_place_logpop|shelter_in_place_black|shelter_in_place_trump2020|
                    gathering_logpop|gathering_black|gathering_trump2020|
                    religious_gatherings_logpop|religious_gatherings_black|religious_gatherings_trump2020+maxmask_sd
                  ~state_sd_iv+closing_of_public_venues_iv+non_essential_iv+school_closure_iv+shelter_in_place_iv+
                    gathering_iv+religious_gatherings_iv+max_mask_iv+max_mask_iv_logpop+max_mask_iv_black+max_mask_iv_trump2020
                  +state_sd_iv_logpop+state_sd_iv_black+state_sd_iv_trump2020
                  +closing_of_public_venues_iv_logpop+closing_of_public_venues_iv_black+closing_of_public_venues_iv_trump2020
                  +non_essential_iv_logpop+non_essential_iv_black+non_essential_iv_trump2020
                  +school_closure_iv_logpop+school_closure_iv_black+school_closure_iv_trump2020
                  +shelter_in_place_iv_logpop+shelter_in_place_iv_black+shelter_in_place_iv_trump2020
                  +gathering_iv_logpop+gathering_iv_black+gathering_iv_trump2020
                  +religious_gatherings_iv_logpop+religious_gatherings_iv_black+religious_gatherings_iv_trump2020+maxmask_sd_iv)|statefips, data=fit_state_data1_demean_final)
summary(fit_grf)
#Health care and social assistance
fit_hcs = felm(spend_hcs~avg_prcp+avg_temp+log_popden+trump_percent2020+black|0|
                 (state_sd|closing_of_public_venues|non_essential|school_closure|shelter_in_place|
                    gathering|religious_gatherings|max_mask|
                    max_mask_logpop|max_mask_black|max_mask_trump2020|
                    state_sd_logpop|state_sd_black|state_sd_trump2020|
                    closing_of_public_venues_logpop|closing_of_public_venues_black|closing_of_public_venues_trump2020|
                    non_essential_logpop|non_essential_black|non_essential_trump2020|
                    school_closure_logpop|school_closure_black|school_closure_trump2020|
                    shelter_in_place_logpop|shelter_in_place_black|shelter_in_place_trump2020|
                    gathering_logpop|gathering_black|gathering_trump2020|
                    religious_gatherings_logpop|religious_gatherings_black|religious_gatherings_trump2020+maxmask_sd
                  ~state_sd_iv+closing_of_public_venues_iv+non_essential_iv+school_closure_iv+shelter_in_place_iv+
                    gathering_iv+religious_gatherings_iv+max_mask_iv+max_mask_iv_logpop+max_mask_iv_black+max_mask_iv_trump2020
                  +state_sd_iv_logpop+state_sd_iv_black+state_sd_iv_trump2020
                  +closing_of_public_venues_iv_logpop+closing_of_public_venues_iv_black+closing_of_public_venues_iv_trump2020
                  +non_essential_iv_logpop+non_essential_iv_black+non_essential_iv_trump2020
                  +school_closure_iv_logpop+school_closure_iv_black+school_closure_iv_trump2020
                  +shelter_in_place_iv_logpop+shelter_in_place_iv_black+shelter_in_place_iv_trump2020
                  +gathering_iv_logpop+gathering_iv_black+gathering_iv_trump2020
                  +religious_gatherings_iv_logpop+religious_gatherings_iv_black+religious_gatherings_iv_trump2020+maxmask_sd_iv)|statefips, data=fit_state_data1_demean_final)
summary(fit_hcs)
#Transportation and warehousing
fit_tws = felm(spend_tws~avg_prcp+avg_temp+log_popden+trump_percent2020+black|0|
                 (state_sd|closing_of_public_venues|non_essential|school_closure|shelter_in_place|
                    gathering|religious_gatherings|max_mask|
                    max_mask_logpop|max_mask_black|max_mask_trump2020|
                    state_sd_logpop|state_sd_black|state_sd_trump2020|
                    closing_of_public_venues_logpop|closing_of_public_venues_black|closing_of_public_venues_trump2020|
                    non_essential_logpop|non_essential_black|non_essential_trump2020|
                    school_closure_logpop|school_closure_black|school_closure_trump2020|
                    shelter_in_place_logpop|shelter_in_place_black|shelter_in_place_trump2020|
                    gathering_logpop|gathering_black|gathering_trump2020|
                    religious_gatherings_logpop|religious_gatherings_black|religious_gatherings_trump2020+maxmask_sd
                  ~state_sd_iv+closing_of_public_venues_iv+non_essential_iv+school_closure_iv+shelter_in_place_iv+
                    gathering_iv+religious_gatherings_iv+max_mask_iv+max_mask_iv_logpop+max_mask_iv_black+max_mask_iv_trump2020
                  +state_sd_iv_logpop+state_sd_iv_black+state_sd_iv_trump2020
                  +closing_of_public_venues_iv_logpop+closing_of_public_venues_iv_black+closing_of_public_venues_iv_trump2020
                  +non_essential_iv_logpop+non_essential_iv_black+non_essential_iv_trump2020
                  +school_closure_iv_logpop+school_closure_iv_black+school_closure_iv_trump2020
                  +shelter_in_place_iv_logpop+shelter_in_place_iv_black+shelter_in_place_iv_trump2020
                  +gathering_iv_logpop+gathering_iv_black+gathering_iv_trump2020
                  +religious_gatherings_iv_logpop+religious_gatherings_iv_black+religious_gatherings_iv_trump2020+maxmask_sd_iv)|statefips, data=fit_state_data1_demean_final)
summary(fit_tws)




