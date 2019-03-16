
# install.packages("rjson")
#library(rjson)
#library(data.table) # does not work for ER's macbook. 
library(ggplot2)
#library(plyr)
library(dplyr)
#library(tidyr)



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
```

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
```
