# install.packages("rjson")
library(rjson)
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(purrr) # required for unlisting fooof to find best peak
library(pracma)

# Get a list of filenames (psd only)
getwd()
datapath = "~/Documents/MATLAB/muse_longitudinal/psd_data_oct1/"
fooofpath = "~/Documents/MATLAB/muse_longitudinal/fooof_data_ch1/"
files_psd <- dir(datapath, pattern="*psds.json")
files_raw = dir(datapath, pattern="*raw_data.json")
files_fooofch1med <- dir(fooofpath, pattern="*fg_meditation_ch1.json")
file_subjdetails <- dir(datapath, pattern="*.csv")
subjdets <- as_tibble(read.csv(paste0(datapath,file_subjdetails)))
with(subjdets, tapply(session_id, list(year_of_birth, gender), length))


######
loadPSD=0
if(loadPSD==1){
  
  #meditate_psd = tibble(session_id = NA, channel = NA, epoch = NA, power = NA, freq = NA)
  meditate_psd = tibble(session_id = NA, epoch = NA, psd=NA)
  meditate_psd = meditate_psd[-1,]
  
  start_time <- Sys.time()
  for(curfile in files_psd){ # loop per session file
  
    # Give the input file name to the function.
    raw_psd <- fromJSON(file = paste0(datapath, curfile))
    # raw_eeg <- fromJSON(file = paste0(datapath, files_raw[1]))
    # data_ch1_med <- fromJSON(file = paste0(datapath, files_ch1med[1]))
    
    for(ee in 1:length(raw_psd$psd_mean_meditation)){ # loop through number of epochs per subj
  
      #freq_num <- as.numeric(raw_psd$freqs_meditation)
      
      #for(cc in 1:4){ # loop through 4 channels
        
        # Convert JSON file to a data frame.
        #psd_num <- as.numeric(raw_psd$psd_mean_meditation[[ss]][[cc]])
        
        tmp <- data.frame(Reduce(cbind, raw_psd$psd_mean_meditation[[ee]], init=raw_psd$freqs_meditation))
        names(tmp) <- c("freq","ch1","ch2","ch3","ch4")
        
        curmat <- tibble(session_id = substr(curfile,1,nchar(curfile)-10), epoch = ee, psd=list(tmp))
        #curmat <- tibble(session_id = substr(curfile,1,nchar(curfile)-10), channel = cc, epoch = ss, power = psd_num, freq = freq_num)
        
        meditate_psd <- bind_rows(meditate_psd, curmat)
        
    }
  }
  end_time <- Sys.time()
  (total_time = end_time-start_time)
}


######
loadFOOOF=0
if(loadFOOOF==1){
  
  meditate_fooof = tibble(session_id = NA, 
                                 bg_int = NA,
                                 bg_slope = NA,
                                 pk_freq = NA,
                                 pk_amp = NA,
                                 pk_bw = NA,
                                 r2 = NA,
                                 err = NA)
  meditate_fooof = meditate_fooof[-1,]
  
  start_time <- Sys.time()
  for(curfile in files_fooofch1med){ # loop per session file
    
    # Give the input file name to the function.
    curfooof <- fromJSON(file = paste0(fooofpath, curfile))
    # raw_eeg <- fromJSON(file = paste0(datapath, files_raw[1]))
    # data_ch1_med <- fromJSON(file = paste0(datapath, files_ch1med[1]))
    
      #curfoof
    
    curpeaks <- map_df(curfooof$peak_params_, ~as.data.frame(t(.)))
    names(curpeaks) <- c("freq","amp","bw")
    pkIX <- which.max(curpeaks[curpeaks$freq<14&curpeaks$freq>7,2])

      curmat <- tibble(session_id = substr(curfile,1,nchar(curfile)-23), 
                       bg_int=curfooof$background_params_[1],
                       bg_slope=curfooof$background_params_[2],
                       pk_freq = curpeaks[pkIX, 1],
                       pk_amp = curpeaks[pkIX, 2],
                       pk_bw = curpeaks[pkIX, 3],
                       r2 = curfooof$r_squared_,
                       err = curfooof$error_)
      #curmat <- tibble(session_id = substr(curfile,1,nchar(curfile)-10), channel = cc, epoch = ss, power = psd_num, freq = freq_num)
      
      meditate_fooof <- bind_rows(meditate_fooof, curmat)
      
  }
  end_time <- Sys.time()
  (total_time = end_time-start_time)
  
  merged_fooof = inner_join(meditate_fooof, subjdets, by="session_id")
  setDT(merged_fooof)
  merged_fooof <- merged_fooof[, sessOrder := rank(session_timestamp), by=.(user_id)]
  merged_fooof <- as_tibble(merged_fooof)
  
  ## clean up the data frame to the following criteria:
  #- all subjects have at least 25 sessions
  #- select only first 25 sessions
  goodSubjs <- unique(merged_fooof[merged_fooof$sessOrder==25,]$user_id)
  merged_fooof <- merged_fooof[merged_fooof$user_id %in% goodSubjs,]
  merged_fooof <- merged_fooof[merged_fooof$sessOrder<26,]
  merged_fooof <- merged_fooof[merged_fooof$gender %in% c("male","female"),]
  merged_fooof$age <- 2018 - as.integer(merged_fooof$year_of_birth) # add age as of 2018
  merged_fooof <- droplevels.data.frame(merged_fooof)
  
  #ageFreqs <- melt(with(merged_fooof[merged_fooof$sessOrder==1,], tapply(user_id,list(age,gender),length)))
  
  ggplot(data=merged_fooof, aes(x=age, y=pk_freq, col=gender)) +
    #geom_point(aes(x=age, y=ch1, col=sessOrder), na.rm=T) +
    #geom_point(data=merged_fooof, aes(x=age, y=pk_freq, col=gender), na.rm=T) +
    stat_summary_bin(aes(), fun.y=mean, geom="point",binwidth=1,na.rm=T) +
    stat_smooth(se=TRUE, alpha=0.3, method="lm", formula=y~poly(x,2)) +
    facet_wrap(~sessOrder) +
    #scale_y_log10() +
    scale_colour_manual(values=c("blue","red"))
  
}


# convert channel an epochs to factors
#meditate_psd$channel = as.factor(meditate_psd$channel)
meditate_psd$epoch = as.factor(meditate_psd$epoch)

subjdets$session_id = as.character(subjdets$session_id)

# merge actual PSD with user/session info dataset according to matching "session_id" value
merged_df <- inner_join(meditate_psd, subjdets, by="session_id")
#merged_df <- merge(meditate_psd[50000:550000,], subjdets, by="session_id")

#merged_df %>% group_by(user_id) %>% ave(merged_df$session_timestamp, merged_df$user_id, FUN = function(x) rank(x, na.last=TRUE, ties.method="average"))
setDT(merged_df)
merged_df <- merged_df[, sessOrder := rank(session_timestamp), by=.(user_id,epoch)]
merged_df <- as_tibble(merged_df)

# # check that the merge worked
# length(unique(merged_df$session_id))
# length(unique(meditate_psd$session_id))
# length(unique(subjdets$session_id)) 

## clean up the data frame to the following criteria:
#- all subjects have at least 25 sessions
#- select only first 25 sessions
goodSubjs <- unique(merged_df[merged_df$sessOrder==25,]$user_id)
merged_df <- merged_df[merged_df$user_id %in% goodSubjs,]
merged_df <- merged_df[merged_df$sessOrder<26,]
merged_df <- merged_df[merged_df$gender %in% c("male","female"),]
merged_df$age <- 2018 - as.integer(merged_df$year_of_birth) # add age as of 2018

merged_df$sessOrder <- as.integer(merged_df$sessOrder)
merged_df <- droplevels.data.frame(merged_df)

(ageFreqs <- with(merged_df[merged_df$epoch==1&merged_df$sessOrder==1,], tapply(user_id,list(age,gender),length)))

merged_df$ageGroup <- cut(merged_df$age,c(20,30,40,50,60,70,80),right=FALSE)

# merged_df$PAF = list(data.frame(NA,NA,NA,NA))
# merged_df$PAA = list(data.frame(NA,NA,NA,NA))
# # find peak alpha freq before unnesting
# start_time <- Sys.time()
# for(abc in 1:dim(merged_df)[1]){
#   tmp <- apply(subset(data.frame(merged_df[abc,3][[1]]), freq>6.5&freq<13.5)[,2:5], 2, findpeaks, nups=1, ndowns=1, npeaks=1, sortstr=FALSE)
#     #findpeaks(mergedata[abc,c("7","8","9","10","11","12","13","14")]),nups=1, ndowns=1, npeaks=1, sortstr=FALSE)
#   if(any(unlist(lapply(tmp,is.null)))){
#     tmp[which(unlist(lapply(tmp,is.null)))] = list(array(NA, dim=c(1,4)))
#     tmp = data.frame(cbind(unlist(tmp[1]), unlist(tmp[2]), unlist(tmp[3]), unlist(tmp[4])))
#     names(tmp) = c("ch1","ch2","ch3","ch4")
#   }
#   peakAmps = tmp[1,]
#   peakFreqs = tmp[2,]
#   peakFreqs = subset(data.frame(merged_df[abc,3][[1]]), freq>6.5&freq<13.5)[0,1]
#   names(peakFreqs) <- c("ch1","ch2","ch3","ch4")
#   #peakFreqs[which(is.na(peakFreqs))]=0
#   merged_df[abc,]$PAF <- list(peakFreqs)
#   merged_df[abc,]$PAA <- list(peakAmps)
#   
# }
# end_time <- Sys.time()
# (total_time = end_time-start_time)

# unnest the data.frames/lists containing the actual data
merged_df <- unnest(merged_df)

# the FFT was not percent, off by 0.01 Hz often, so round to nearest 1 decimal
merged_df$freq <- round(merged_df$freq,1)



#################################
## workspace saved at this point on Sept. 28, 2018 and Oct. 1, 2018
#################################

merged_df$gender <- factor(merged_df$gender, levels=c("male","female"))
merged_df$gender <- revalue(merged_df$gender, c("male"="men", "female"="women"))

ggplot(data=subset(merged_df, freq==3&sessOrder==1&epoch==1), aes(x=age, fill=gender), na.rm=T) +
  geom_histogram(aes(y = ..count..), binwidth=1, position="identity", alpha=0.5) +
  geom_density(aes(y = ..count..), position="identity", alpha=0) +
  scale_fill_manual(values=c("#0072B2", "#D55E00"))+
  theme_bw() +
  theme(text = element_text(size=25)) +
  ggsave("FIG_longitudinal_histogram.png")


# setting up red-blue colour ramp for when levels are discrete... unlikely to need
RdBuCols <- c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac')
cc <- colorRampPalette(RdBuCols)(length(levels(merged_df$sessOrder)))

psd_ageSessionMeans <- merged_df %>% group_by(ageGroup,sessOrder,freq) %>% summarize(ch1 = mean(ch1), ch2=mean(ch2), ch3=mean(ch3), ch4=mean(ch4))
# plot power per 1 channel at each session (colour), separated for each age group (facet)
ggplot(data=psd_ageSessionMeans, aes(x=freq, y=log10(ch1),col=sessOrder,group=sessOrder), na.rm=T) +
  #geom_line(aes(), alpha=0.4) +
  stat_summary(aes(), fun.y=mean, geom="line",na.rm=T) +
  facet_wrap(~ageGroup, nrow=2) +
  xlim(c(0,30)) +
  #scale_y_log10() +
  scale_colour_gradient2(low="#B2182B", mid="#F7F7F7", high="#2166AC",midpoint=13) +
  #theme_bw() +
  theme(text = element_text(size=15)) +
  labs(colour = "Session #") +
  ylab("log10(power)") +
  xlab("frequency") +
  ggsave("FIG_psd_ageSessionInteraction.png", width =10, height=5)

RdBuCols <- c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac')
cc <- colorRampPalette(RdBuCols)(length(levels(merged_df$age_group)))

psd_ageMeans <- merged_df %>% group_by(age_group,freq) %>% summarize(ch1 = mean(ch1), ch2=mean(ch2), ch3=mean(ch3), ch4=mean(ch4))
# plot power per 1 channel at each age group (colour) averaged across all sessions
ggplot(data=psd_ageMeans, aes(x=freq, y=log10(ch1),col=age_group,group=age_group), na.rm=T) +
  stat_summary(aes(), fun.y=mean, geom="line",na.rm=T) +
  #facet_wrap(~ageGroup) +
  xlim(c(0,30)) +
  #scale_y_log10() +
  scale_colour_manual(values = cc)+
  #theme_bw() +
  theme(text = element_text(size=20)) +
  labs(colour = "Age Group") +
  ylab("log10(power)") +
  xlab("frequency") +
  ggsave("FIG_psd_ageEffect.png", width =8, height=5)
#  scale_colour_gradient2(low="#B2182B", mid="#F7F7F7", high="#2166AC",midpoint=13)


psd_sessMeans <- merged_df %>% group_by(sessOrder,freq) %>% summarize(ch1 = mean(ch1), ch2=mean(ch2), ch3=mean(ch3), ch4=mean(ch4))
# plot power per 1 channel at each age group (colour) averaged across all sessions
ggplot(data=psd_sessMeans, aes(x=freq, y=log10(ch1), col=sessOrder, group=sessOrder), na.rm=T) +
  #geom_line(aes(), alpha=0.4) +
  stat_summary(aes(), fun.y=mean, geom="line",na.rm=T) +
  #facet_wrap(~ageGroup) +
  xlim(c(0,30)) +
  #scale_y_log10() +
  scale_colour_gradient2(low="#B2182B", mid="#F7F7F7", high="#2166AC",midpoint=13) +
  #theme_bw() +
  theme(text = element_text(size=20)) +
  labs(colour = "Session #") +
  ylab("power") +
  xlab("frequency") +
  ggsave("FIG_psd_sessionEffect.png",width =8, height=5)

## extract parameters (e.g., alpha amp) to compare across sessions:

## create separate data.frame for each parameter
#alphaAmp <- subset(merged_df, freq>7&freq<13)
#alphaAmp <- alphaAmp %>% group_by(user_id,session_id,channel,epoch) %>% summarize(alphaAmp = mean(power))

# alternatively, and better, label each freq with band label, then categorize by them
merged_df$band <- NA
merged_df[merged_df$freq<30.2,"band"] = "beta"
merged_df[merged_df$freq<13.5,"band"] = "alpha"
merged_df[merged_df$freq<8,"band"] = "theta"
merged_df[merged_df$freq<3,"band"] = "delta"

# calculate band power for each channel, epoch, session
bandpower_df <- merged_df[merged_df$freq<30.1&xor(merged_df$gender=="men",merged_df$gender=="women"),] %>% group_by(user_id, age, gender, handedness, session_id, sessOrder, band) %>% summarize(ch1 = mean(ch1), ch2 = mean(ch2), ch3 = mean(ch3), ch4 = mean(ch4))

bandpower_df$asymmetryTP <- log10(bandpower_df$ch4) - log10(bandpower_df$ch1)
bandpower_df$asymmetryAF <- log10(bandpower_df$ch3) - log10(bandpower_df$ch2)

peakAlpha_df <- merged_df[merged_df$freq<13.5&merged_df$freq>6.5&xor(merged_df$gender=="men",merged_df$gender=="women"),] %>% group_by(user_id, age, gender, handedness, session_id, sessOrder, epoch) %>% summarize(PAAch1 = findpeaks(ch1,nups=1, ndowns=1, npeaks=1, sortstr=TRUE)[1],
                                                          PAFch1 = freq[findpeaks(ch1,nups=1, ndowns=1, npeaks=1, sortstr=TRUE)[2]])
#peakAlpha_df$alphaAsym <- peakAlpha_df$

bandpower_means <- bandpower_df %>% group_by(age,gender,band) %>% summarize(ch1 = mean(ch1), ch2 = mean(ch2), ch3 = mean(ch3), ch4 = mean(ch4))
(ageFreqs <- with(merged_df[merged_df$epoch==1&merged_df$sessOrder==1&merged_df$freq==3,], tapply(user_id,list(age,gender),length)))
ageFreqs <- melt(ageFreqs)
names(ageFreqs) <- c("age","gender","weight")
bandpower_means <- inner_join(bandpower_means, ageFreqs)

#summary(aov(ch1 ~ age*gender*sessOrder, data=bandpower_df[bandpower_df$band=="alpha",]))

cc <- scales::seq_gradient_pal("#2166AC", "#B2182B", "Lab")(seq(0,1,length.out=length(unique(peakAlpha_df$sessOrder))))
cc <- scales::seq_gradient_pal("#2166AC", "#B2182B", "Lab")(seq(0,1,length.out=length(unique(peakAlpha_df$gender))))

ggplot(data=peakAlpha_df) +
  #geom_point(aes(x=age, y=PAFch1, col=gender), na.rm=T) +
  stat_summary(aes(x=sessOrder, y=PAFch1, colour=gender), fun.y=mean, geom="point",na.rm=T) +
  stat_smooth(aes(x=sessOrder, y=PAFch1, colour=gender, linetype=gender), se=TRUE, alpha=0.3, method="lm", formula=y~poly(x,2)) +
  #facet_wrap(~gender) +
  #scale_y_log10() +
  scale_colour_manual(values=cc) +
  theme(text = element_text(size=25)) +
  #labs(colour = "Session #") +
  ylab("frequency") +
  xlab("session #") +
  #coord_cartesian(ylim=c(7.5,10)) +
  ggsave("FIG_PAF_sessionEffect.png",width =8, height=5)

summary(aov(PAFch1 ~ gender*sessOrder, data=peakAlpha_df))
peakAlpha_df$ageGroup <- cut(peakAlpha_df$age,c(20,25,30,35,40,45,50,55,60,65,70,75,80),right=FALSE)
peakAlpha_df$sessionF <- as.factor(peakAlpha_df$sessOrder)
ezANOVA(data=peakAlpha_df, dv=PAFch1, wid=user_id, within=sessionF, between=.(ageGroup,gender), type=3)

ggplot(data=peakAlpha_df) +
  #geom_point(aes(x=age, y=PAFch1, col=gender), na.rm=T) +
  stat_summary(aes(x=age, y=PAFch1, colour=gender), fun.y=mean, geom="point",na.rm=T) +
  stat_smooth(aes(x=age, y=PAFch1, colour=gender, linetype=gender), se=TRUE, alpha=0.3, method="lm", formula=y~poly(x,2)) +
  #facet_wrap(~gender) +
  #scale_y_log10() +
  scale_colour_manual(values=cc) +
  theme(text = element_text(size=25)) +
  ylab("frequency") +
  xlab("age") +
  coord_cartesian(ylim=c(7.5,10)) +
  ggsave("FIG_PAF_ageEffect.png",width =8, height=5)

ggplot(data=peakAlpha_df) +
  #geom_point(aes(x=age, y=PAFch1, col=gender), na.rm=T) +
  stat_summary(aes(x=age, y=PAFch1, colour=sessOrder, group=as.factor(sessOrder)), fun.y=mean, geom="point",na.rm=T) +
  stat_smooth(aes(x=age, y=PAFch1, colour=sessOrder, group=as.factor(sessOrder)), se=FALSE, alpha=0.3, method="lm", formula=y~poly(x,2)) +
  #facet_wrap(~sessOrder) +
  #scale_y_log10() +
  scale_colour_gradient2(low="#B2182B", mid="#F7F7F7", high="#2166AC",midpoint=13) +
  #scale_colour_manual(values=cc) +
  theme(text = element_text(size=25)) +
  ylab("frequency") +
  xlab("age") +
  coord_cartesian(ylim=c(7.5,10)) +
  ggsave("FIG_PAF_ageSessionEffect.png",width =8, height=5)


summary(lm(PAFch1 ~ age+sessOrder+gender, data=peakAlpha_df))
summary(lm(PAFch1 ~ sessOrder, data=peakAlpha_df))
summary(lm(PAFch1 ~ gender, data=peakAlpha_df))

PAFepochSD <- peakAlpha_df %>% group_by(user_id,sessOrder) %>% summarize(PAFch1SD=sd(PAFch1))

ggplot()+
  stat_summary(aes(x=sessOrder,y=PAFch1SD), data=PAFepochSD, fun.data=mean_se, na.rm=T)


ggplot(data=peakAlpha_df) +
  #geom_point(aes(x=age, y=PAFch1, col=gender), na.rm=T) +
  stat_summary(aes(x=epoch, y=PAFch1, colour=as.numeric(sessOrder), group=sessOrder), fun.y=mean, geom="point",na.rm=T) +
  stat_smooth(aes(x=as.integer(epoch), y=PAFch1), se=TRUE, alpha=0.3, method="lm", formula=y~poly(x,2), col="black") +
  #facet_wrap(~gender) +
  #scale_y_log10() +
  scale_colour_gradient2(low="#B2182B", mid="#F7F7F7", high="#2166AC",midpoint=13) +
  theme(text = element_text(size=25)) +
  labs(colour = "Session #") +
  ylab("frequency") +
  xlab("epoch") #+
  #coord_cartesian(ylim=c(7.5,10)) +
  ggsave("FIG_PAF_epochEffect.png",width =8, height=5)



ggplot(data=peakAlpha_df) +
  #geom_point(aes(x=age, y=PAFch1, col=gender), na.rm=T) +
  stat_summary(aes(x=sessOrder, y=log10(PAAch1), colour=gender), fun.y=mean, geom="point",na.rm=T) +
  stat_smooth(aes(x=sessOrder, y=log10(PAAch1), colour=gender), se=TRUE, alpha=0.3, method="lm", formula=y~poly(x,2)) +
  #facet_wrap(~gender) +
  #coord_trans(y="log10")+
  scale_colour_manual(values=cc) +
  theme(text = element_text(size=25)) +
  #labs(colour = "Session #") +
  ylab("log10(power)") +
  xlab("session #") +
  #coord_cartesian(ylim=c(7.5,10)) +
  ggsave("FIG_PAA_sessionEffect.png",width =8, height=5)

ggplot(data=peakAlpha_df) +
  #geom_point(aes(x=age, y=PAFch1, col=gender), na.rm=T) +
  stat_summary(aes(x=age, y=log10(PAAch1), colour=gender), fun.y=mean, geom="point",na.rm=T) +
  stat_smooth(aes(x=age, y=log10(PAAch1), colour=gender), se=TRUE, alpha=0.3, method="lm", formula=y~poly(x,2)) +
  #facet_wrap(~gender) +
  #coord_trans(y="log10")+
  scale_colour_manual(values=cc) +
  theme(text = element_text(size=25)) +
  ylab("log10(power)") +
  xlab("age") +
  #coord_cartesian(ylim=c(7.5,10)) +
  ggsave("FIG_PAA_ageEffect.png",width =8, height=5)

ggplot(data=peakAlpha_df) +
  #geom_point(aes(x=age, y=PAFch1, col=gender), na.rm=T) +
  stat_summary(aes(x=age, y=log10(PAAch1), colour=sessOrder, group=(sessOrder)), alpha=0.3, fun.y=mean, geom="point",na.rm=T) +
  stat_smooth(aes(x=age, y=log10(PAAch1), colour=sessOrder,group=(sessOrder)), se=FALSE, alpha=0.3, method="lm", formula=y~poly(x,2)) +
  facet_wrap(~gender) +
  #scale_y_continuous(trans="log10")+
  scale_colour_gradient2(low="#B2182B", mid="#F7F7F7", high="#2166AC",midpoint=13) +
  theme(text = element_text(size=25)) +
  ylab("log1(power)") +
  xlab("age") +
  #coord_cartesian(ylim=c(7.5,10)) +
  ggsave("FIG_PAA_ageSessionEffect.png",width =12, height=5)

summary(aov((PAAch1) ~ age*gender, data=peakAlpha_df))

cc <- scales::seq_gradient_pal("#2166AC", "#B2182B", "Lab")(seq(0,1,length.out=length(levels(bandpower_df$age))))
#cc <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=length(levels(bandpower_df$epoch))))

ggplot(data=bandpower_df[bandpower_df$band=="alpha",]) +
  #geom_point(aes(x=age, y=ch1, col=sessOrder), na.rm=T) +
  stat_summary(aes(x=age, y=asymmetryAF, colour=gender), fun.y=mean, geom="point",na.rm=T) +
  stat_smooth(aes(x=age, y=asymmetryAF, colour=gender), se=FALSE, alpha=0.3, method="lm", formula=y~poly(x,2)) +
  #facet_wrap(~band) +
  #scale_y_log10() +
  scale_colour_manual(values=c("#2166AC", "#B2182B"))

options(contrasts=c("contr.sum","contr.poly"))
summary(lm(asymmetryAF ~ as.ordered(age)*gender, data=bandpower_df[bandpower_df$band=="alpha",]))
summary(aov(asymmetryAF ~ as.ordered(age)*gender, data=bandpower_df[bandpower_df$band=="alpha",]))


ggplot(data=bandpower_df[bandpower_df$band=="beta"&(bandpower_df$sessOrder==1|bandpower_df$sessOrder==25),]) +
  #geom_point(aes(x=age, y=ch1, col=sessOrder), na.rm=T) +
  stat_summary(aes(x=sessOrder, y=log10(ch1)), fun.y=mean, geom="point",na.rm=T) +
  stat_smooth(aes(x=sessOrder, y=log10(ch1)), se=FALSE, alpha=0.3, method="lm", formula=y~poly(x,2)) +
  #facet_wrap(~band) +
  #scale_y_log10() +
  scale_colour_manual(values=cc)

summary(aov((ch1) ~ sessOrder+Error(user_id/sessOrder), data=bandpower_df[bandpower_df$band=="beta"&(bandpower_df$sessOrder==1|bandpower_df$sessOrder==25),]))

ggplot() +
  #geom_point(aes(x=age, y=ch1, col=sessOrder), na.rm=T) +
  geom_point(data=bandpower_means[bandpower_means$band=="alpha",], aes(x=age, y=ch1, size=weight, col=gender), na.rm=T) +
  stat_smooth(data=bandpower_means[bandpower_means$band=="alpha",], aes(x=age, y=ch1, col=gender), se=TRUE, alpha=0.3, method="lm", formula=y~x) +
  #facet_wrap(~sessOrder) +
  scale_y_log10() +
  scale_colour_manual(values=c("blue","red"))



# library(lme4)
# library(lmerTest)
# library(emmeans)

#peakAlpha_df$sessOrder <- as.ordered(peakAlpha_df$sessOrder)
# full = lmer(PAFch1 ~ gender+sessOrder+age+ 
#               age:gender+age:sessOrder+
#               (1+sessOrder|user_id), 
#             data=peakAlpha_df)

#anova(full)
#emmeans(full, "sessOrder", by = c("age", "gender"))