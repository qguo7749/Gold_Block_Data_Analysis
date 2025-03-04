library(tidyverse)
library(car)
library(psych)
library(effectsize)
library(easystats)
library(lme4)

setwd('G:\\My Drive\\UCB\\Research\\Cultural Evolution\\QG-Analysis')
data<-read.csv("ptcp_gnometrans.csv")

s_demo_round<-as.factor(ceiling(data$s_demo/0.1)*0.1)
s_trial_round<-as.factor(ceiling(data$mean_trial_score/0.1)*0.1)
n_algokids<-data$n_algokids
pp<-data$p_trans
n_nece_compare<-data$n_nece_compare
s_demo<-data$s_demo
s_trial<-data$mean_trial_score

scatterplotMatrix(~s_demo + pp + n_nece_compare+n_algokids+s_trial, data=data)

# n_nece_compare<-as.factor(n_nece_compare)
plot(n_nece_compare,pp)
ggplot(data, aes(x=n_nece_compare, y=pp)) + labs(x = "Score of Demonstration",y="Probability of successful transmission",title="Effect of demonstration to the transmission fidelity")+
  geom_violin()+stat_summary(fun.y=mean, geom="point", shape=23, size=2)


s_demo_round<-as.factor(s_demo_round)
plot(s_demo,pp)
ggplot(data, aes(x=s_demo_round, y=pp)) + labs(x = "Score of Demonstration",y="Probability of successful transmission",title="Effect of demonstration to the transmission fidelity")+
  geom_violin()+stat_summary(fun.y=mean, geom="point", shape=23, size=2)


s_demo_round<-as.factor(s_trial_round)
plot(s_trial,pp)
ggplot(data, aes(x=s_trial_round, y=pp)) + labs(x = "Mean Trial Score",y="Probability of successful transmission",title="Effect of demonstration to the transmission fidelity")+ 
  geom_violin()+stat_summary(fun.y=mean, geom="point", shape=23, size=2)

summary(lm(pp ~ s_demo))
summary(lm(n_algokids ~ s_demo))

data2<-read.csv("transmission_pair_for_mixed_effect.csv")
pid<-data2$Parent.ID
cid<-data2$Child.ID
t<-data2$Transmitted
s_demo2<-data2$S_demo
g<-data2$Generation

m.standard<-lm(t~s_demo2+g,data=data2)

m.mix<-lmer(t~s_demo2+g+(1|pid),data=data2,REML=FALSE)
summary(m.standard)
summary(m.mix)
