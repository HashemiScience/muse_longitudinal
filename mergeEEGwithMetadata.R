load('./data_20180930/cleanSubjMetadata.Rdata')

load("~/Github/muse_longitudinal/data_20180930/rdata/psd_cal_ch1.Rdata", verbose = TRUE)
clean_psd_cal_ch1 = inner_join(cleanSubj.df, psd_cal_ch1, by = "session_id")
save(clean_psd_cal_ch1, file = './data_20180930/clean_psd_cal_ch1.Rdata')


load("~/Github/muse_longitudinal/data_20180930/rdata/psd_med_ch1.Rdata", verbose = TRUE)
clean_psd_med_ch1 = inner_join(cleanSubj.df, psd_med_ch1, by = "session_id")
save(clean_psd_med_ch1, file = './data_20180930/clean_psd_med_ch1.Rdata')
rm(psd_med_ch1)

load("~/Github/muse_longitudinal/data_20180930/rdata/psd_med_ch4.Rdata", verbose = TRUE)
clean_psd_med_ch4 = inner_join(cleanSubj.df, psd_med_ch4, by = "session_id")
save(clean_psd_med_ch4, file = './data_20180930/clean_psd_med_ch4.Rdata')
rm(psd_med_ch4)

load("~/Github/muse_longitudinal/data_20180930/rdata/psd_cal_ch4.Rdata", verbose = TRUE)
clean_psd_cal_ch4 = inner_join(cleanSubj.df, psd_cal_ch4, by = "session_id")
save(clean_psd_cal_ch4, file = './data_20180930/clean_psd_cal_ch4.Rdata')
rm(psd_cal_ch4)

length(unique(cleanSubj.df$session_id))

length(unique(clean_psd_med_ch1$session_id)) 
length(unique(clean_psd_med_ch4$session_id))
length(unique(clean_psd_cal_ch1$session_id))
length(unique(clean_psd_cal_ch4$session_id))



length(unique(clean_psd_med_ch1$user_id))
length(unique(clean_psd_med_ch4$user_id))
length(unique(clean_psd_cal_ch1$user_id))
length(unique(clean_psd_cal_ch4$user_id))

load("~/Github/muse_longitudinal/data_20180930/rdata/fooof_cal_ch1.Rdata", verbose = TRUE)
clean_fooof_cal_ch1 = inner_join(cleanSubj.df, all_fooof_cal_ch1$fooof_tibble, by = "session_id")
save(clean_fooof_cal_ch1, file = './data_20180930/clean_fooof_cal_ch1.Rdata')
rm(all_fooof_cal_ch1)


load("~/Github/muse_longitudinal/data_20180930/rdata/fooof_cal_ch4.Rdata", verbose = TRUE)
clean_fooof_cal_ch4 = inner_join(cleanSubj.df, all_fooof_cal_ch4$fooof_tibble, by = "session_id")
save(clean_fooof_cal_ch4, file = './data_20180930/clean_fooof_cal_ch4.Rdata')
rm(all_fooof_cal_ch4)


load("~/Github/muse_longitudinal/data_20180930/rdata/fooof_med_ch1.Rdata", verbose = TRUE)
clean_fooof_med_ch1 = inner_join(cleanSubj.df, all_fooof_med_ch1$fooof_tibble, by = "session_id")
save(clean_fooof_med_ch1, file = './data_20180930/clean_fooof_med_ch1.Rdata')
rm(all_fooof_med_ch1)


load("~/Github/muse_longitudinal/data_20180930/rdata/fooof_med_ch4.Rdata", verbose = TRUE)
clean_fooof_med_ch4 = inner_join(cleanSubj.df, all_fooof_med_ch4$fooof_tibble, by = "session_id")
save(clean_fooof_med_ch4, file = './data_20180930/clean_fooof_med_ch4.Rdata')
rm(all_fooof_med_ch4)
