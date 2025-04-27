# by Sophia-Helen Sass
# statistical analysis of the Space Adventure Task (2025)
# using pre-processed and cleaned data 

#clear workspace
rm(list = ls())

# load packages
library(pwr)
library(car)
library(ggplot2)
library(psych)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)
library(reshape)
library(plyr)

#set working directory and load data
setwd("")
B3_SAT<- read.csv("SATUP_sociodemo_quest_all_data_144trials_public.csv", header = TRUE)

# design jitter for data points in plots
jitter = position_jitterdodge(
  jitter.width = 0.09,
  dodge.width = 0.7,
  seed = NA
)

# initial sample descriptives ####
# sex (male=1,female=2) of recruited sample
B3_SAT %>%
  group_by(sex) %>%
  dplyr::summarize(Freq=n())

# age of recruited sample
descr_age <- describe(B3_SAT$age)
sd(B3_SAT$age)
descr_age

#exclude data sets that cannot be fitted based on pseudo-Rho2
B3_SAT <- subset(B3_SAT, participant_ID!=1019)
B3_SAT <- subset(B3_SAT, participant_ID!=1023)
B3_SAT <- subset(B3_SAT, participant_ID!=43)
             
# descriptives of cleaned sample ####
# age 
descr_age <- describe(B3_SAT$age)
descr_age

# education (yes=1,no=2)
B3_SAT %>%
  group_by(a_levels) %>%
  dplyr::summarize(Freq=n())

# gaming
# categorize regular and irregular gamers
# (regular gaming yes=1, no=0)
B3_SAT %>% add_column(game_high = NA)
for(k in 1:nrow(B3_SAT)){
  if (B3_SAT$gaming[k] == "5" | B3_SAT$gaming[k] == "4"){
    B3_SAT$game_high[k] = 1}
  else {
    B3_SAT$game_high[k] = 0}}

B3_SAT %>% 
  group_by(game_high) %>%
  dplyr::summarize(Freq=n())

# performance x replan cost overall ####
SAT <- subset(B3_SAT,select=c("participant_ID",
                               "UP1_2_rel_gain",
                               "UP3_4_rel_gain",
                               "rel_gain_UP5"))
SAT <-melt.data.frame(SAT, id=c("participant_ID"), variable_name = "condition")
levels(SAT$condition)[1] <- "high"
levels(SAT$condition)[2] <- "medium"
levels(SAT$condition)[3] <- "none"

SAT[ , c(2)]<- as.factor(SAT[ , c(2)])
SAT$condition  <- relevel(SAT$condition , "high")

names(SAT)[names(SAT) == 'value'] <- 'acc'

# boxplot for data overview
bxp_noise <- ggboxplot(
  SAT, x = "condition", y = "acc",palette = "jco",
  add = "jitter")+
  xlab("age group") +
  ylab("mean acc") +
  theme_bw()
bxp_noise

# descriptives
descr_acc <- describeBy(SAT$acc,SAT$condition)
descr_acc <- ldply (descr_acc, data.frame)
names(descr_acc)[names(descr_acc) == '.id'] <- 'condition'

# checking for assumptions
# normality: Shapiro-Test
SAT %>%
  group_by(condition) %>%
  shapiro_test(acc) 

# homoscedasticity: Levene-Test
SAT %>%
  levene_test(acc ~ condition) 

# plot bar plot with error bars (standard error of the mean)
relative_performance_plot <- ggplot(descr_acc, aes(x=condition, y=mean, fill=condition)) +
  geom_point(data = SAT, position = jitter, size = 0.9, aes(y = acc, x = condition))+
  scale_color_identity()+
  geom_bar(stat="identity", color="black", width=0.7,
           position=position_dodge()) +
  scale_fill_manual("transition uncertainty", values=c("#3399FF99","#3399FF99","#3399FF99"))+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.7))+
  xlab("repl") +
  ylab("relative performance (%)") +
  theme(plot.title=element_text(hjust=0.5) ,
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=15),
        title=element_text(face = "bold", size = 15),
        legend.position = 'none') +
  theme(legend.text = element_text(size = 15)) +
  scale_y_continuous(limits = c(0, 1500), breaks = seq(0,110,20))+
  coord_cartesian(ylim=c(10,120))
relative_performance_plot

# save image 
# output_path <- ""
# ggsave(file = output_path, plot = relative_performance_plot, width = 5, height = 4)

# compute condition comparison
res.fried <- SAT %>% friedman_test(acc ~ condition | participant_ID)
res.fried
SAT %>% friedman_effsize(acc ~ condition | participant_ID)
pwc <- SAT %>%
  wilcox_test(acc ~ condition, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# planning depth x replan cost overall ####
SAT <- subset(B3_SAT,select=c("participant_ID",
                               "PD_UP1_2",
                               "PD_UP3_4", 
                               "PD_UP5"))
SAT <-melt.data.frame(SAT, id=c("participant_ID"), variable_name = "condition")
levels(SAT$condition)[1] <- "high"
levels(SAT$condition)[2] <- "medium"
levels(SAT$condition)[3] <- "none"

SAT[ , c(2)]<- as.factor(SAT[ , c(2)])
SAT$condition  <- relevel(SAT$condition , "high")

names(SAT)[names(SAT) == 'value'] <- 'PD'

# boxplot for data overview
bxp_noise <- ggboxplot(
  SAT, x = "condition", y = "PD",palette = "jco", 
  add = "jitter")+
  xlab("age group") +
  ylab("mean PD") +
  theme_bw() 
bxp_noise

# descriptives 
descr_PD <- describeBy(SAT$PD,SAT$condition)

descr_PD <- ldply (descr_PD, data.frame)
names(descr_PD)[names(descr_PD) == '.id'] <- 'condition'

# checking for assumptions
# normality: Shapiro-Test
SAT %>%
  group_by(condition) %>%
  shapiro_test(PD) 

# homoscedasticity: Levene-Test
SAT %>%
  levene_test(PD ~ condition) 

# plot bar plot with error bars (standard error of the mean)
planning_depth_plot <- ggplot(descr_PD, aes(x=condition, y=mean, fill=condition)) + 
  geom_point(data = SAT, position = jitter, size = 0.9, aes(y = PD, x = condition))+
  scale_color_identity()+
  geom_bar(stat="identity", color="black", width=0.7, 
           position=position_dodge()) +
  scale_fill_manual("transition uncertainty", values=c("#3399FF99","#3399FF99","#3399FF99"))+
  theme_light()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,  
                position=position_dodge(.7))+
  xlab("replanning costs") +
  ylab("planning depth (steps)") +
  theme(plot.title=element_text(hjust=0.5) ,
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=15),
        title=element_text(face = "bold", size = 15),
        legend.position = 'none') +
  theme(legend.text = element_text(size = 15)) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(1,2.25,0.25))+
  coord_cartesian(ylim=c(1,2.25))

planning_depth_plot

# compute comparison between replanning cost conditions
res.fried <- SAT %>% friedman_test(PD ~ condition |participant_ID)
res.fried
SAT %>% friedman_effsize(PD ~ condition |participant_ID)
pwc <- SAT %>%
  wilcox_test(PD ~ condition, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# output_path <- ""
# ggsave(file = output_path, plot = planning_depth_plot, width = 5, height = 4)

# correlational analyses ####
# planning measures 
cor.test(B3_SAT$PD_overall, B3_SAT$mean_RT1_total,method = "spearman") 

cor.test(B3_SAT$PD_overall, B3_SAT$rel_gain_total,method = "spearman") 

cor.test(B3_SAT$rel_gain_total, B3_SAT$mean_RT1_total,method = "spearman") 

# BIS ####
descr_BIS_motor <- describe(B3_SAT$bis15_sum_motor)
descr_BIS_non_planning <- describe(B3_SAT$bis15_sum_non_planning)
descr_BIS_attention <- describe(B3_SAT$bis15_sum_attentional)

shapiro_test(B3_SAT$bis15_sum_motor)
shapiro_test(B3_SAT$bis15_sum_non_planning)
shapiro_test(B3_SAT$bis15_sum_attentional)

cor.test(B3_SAT$rel_gain_total, B3_SAT$bis15_sum_motor,method = "spearman") 
cor.test(B3_SAT$rel_gain_total, B3_SAT$bis15_sum_non_planning,method = "spearman") 
cor.test(B3_SAT$rel_gain_total, B3_SAT$bis15_sum_attentional,method = "spearman") 
cor.test(B3_SAT$rel_gain_total, B3_SAT$bis15_sum,method = "spearman") 

cor.test(B3_SAT$PD_overall, B3_SAT$bis15_sum_motor,method = "spearman") 
cor.test(B3_SAT$PD_overall, B3_SAT$bis15_sum_non_planning,method = "spearman") 
cor.test(B3_SAT$PD_overall, B3_SAT$bis15_sum_attentional,method = "spearman") 
cor.test(B3_SAT$PD_overall, B3_SAT$bis15_sum,method = "spearman") 

# planning time x replanning costs overall #####
SAT <- subset(B3_SAT,select=c("participant_ID","UP1_2_RT1","UP3_4_RT1", "RT1_UP5"))
SAT <-melt.data.frame(SAT, id=c("participant_ID"), variable_name = "condition")
levels(SAT$condition)[1] <- "RT1_UP1_2"
levels(SAT$condition)[2] <- "RT1_UP3_4"
levels(SAT$condition)[3] <- "RT1_UP5"
SAT[ , c(2)]<- as.factor(SAT[ , c(2)])
SAT$condition  <- relevel(SAT$condition , "RT1_UP1_2")

names(SAT)[names(SAT) == 'value'] <- 'RT1'
SAT$RT1 <- SAT$RT1/1000

# boxplot for data overview
bxp_noise <- ggboxplot(
  SAT, x = "condition", y = "RT1",palette = "jco", 
  add = "jitter")+
  xlab("age group") +
  ylab("mean RT1") +
  ggtitle("mean relative gain per condition ") +
  theme_bw() 
bxp_noise

# descriptives 
descr_RT1 <- describeBy(SAT$RT1,SAT$condition)

descr_RT1 <- ldply (descr_RT1, data.frame)
names(descr_RT1)[names(descr_RT1) == '.id'] <- 'condition'

# checking for assumptions 
# normality: Shapiro-Test 
SAT %>%
  group_by(condition) %>%
  shapiro_test(RT1) 

# homoscedasticity: Levene-Test 
SAT %>%
  levene_test(RT1 ~ condition)

# plot bar plot with error bars (standard error of the mean)
planning_time_plot <- ggplot(descr_RT1, aes(x=condition, y=mean, fill=condition)) + 
  geom_point(data = SAT, position = jitter, size = 0.9, aes(y = RT1, x = condition))+
  scale_color_identity()+
  geom_bar(stat="identity", color="black", width=0.7, 
           position=position_dodge()) +
  scale_fill_manual("transition uncertainty", values=c("#3399FF99","#3399FF99","#3399FF99"))+
  theme_light()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,  linewidth = 0.4, 
                position=position_dodge(.7))+
  xlab("mini-block type") +
  ylab("planning time (s)") +
  theme(plot.title=element_text(hjust=0.5) ,
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=15),
        title=element_text(face = "bold", size = 15),
        legend.position = 'none') +
  theme(legend.text = element_text(size = 15)) +
  scale_y_continuous(limits = c(0, 20000))+
  coord_cartesian(ylim=c(1,17))

planning_time_plot

# compute comparison between replanning cost conditions 
res.fried <- SAT %>% friedman_test(RT1 ~ condition |participant_ID)
res.fried
SAT %>% friedman_effsize(RT1 ~ condition |participant_ID)
pwc <- SAT %>%
  wilcox_test(RT1 ~ condition, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# save image
# output_path <- ""
# ggsave(file = output_path, plot = planning_time_plot, width = 5, height = 4)

# RT replan and plan comparison ####
## UP1 ####
SAT<- subset(B3_SAT,n_UP1_noreplan>4 & 
                   n_UP1_replan>4 & 
                   n_UP1_transsuccess>4 & 
                   n_UP1_RTdeterm_1action >4)

SAT <- subset(SAT,select=c("participant_ID",
                              "UP1_RTnoreplan",
                              "UP1_RTreplan",
                              "UP1_RTtranssuccess",
                              "UP1_RTdeterm_1action"))

SAT <-melt.data.frame(SAT, id=c("participant_ID"), variable_name = "condition")
levels(SAT$condition)[1] <- "noreplan"
levels(SAT$condition)[2] <- "replan"
levels(SAT$condition)[3] <- "success"
levels(SAT$condition)[4] <- "deterministic"

SAT[ , c(2)]<- as.factor(SAT[ , c(2)])
SAT$condition  <- relevel(SAT$condition , "success")

names(SAT)[names(SAT) == 'value'] <- 'planning_time'

SAT$planning_time <- SAT$planning_time/1000

# descriptives 
descr_RT <- describeBy(SAT$planning_time,SAT$condition)

descr_RT <- ldply (descr_RT, data.frame)
names(descr_RT)[names(descr_RT) == '.id'] <- 'condition'

# boxplot for data overview
bxp_noise <- ggboxplot(
  SAT, x = "condition", y = "planning_time",palette = "jco", 
  add = "jitter")+
  xlab("action x condition") +
  ylab("mean RT") +
  ggtitle("mean RT per action x condition ") +
  theme_bw() 
bxp_noise

# plot bar plot with error bars (standard error of the mean)
replanning_effect_plot_UP1 <- ggplot(descr_RT, aes(x=condition, y=mean, fill=condition)) + 
  scale_color_identity()+
  geom_point(data = SAT, position = jitter, size = 0.9, aes(y = planning_time, x = condition))+
  geom_bar(stat="identity", color="black", width=0.7, alpha=.5,
           position=position_dodge()) +
  theme_light()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,  linewidth = 0.4, 
                position=position_dodge(.7))+
  xlab("mini-block type") +
  ylab("RT after probabilistic transition (s) ") +
  theme(plot.title=element_text(hjust=0.5) ,
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=15),
        title=element_text(face = "bold", size = 15),
        legend.position = 'none') +
  theme(legend.text = element_text(size = 15)) +
  scale_y_continuous(limits = c(0, 10000))+
  coord_cartesian(ylim=c(0.3,6.5))

replanning_effect_plot_UP1

# # save image
# output_path <- ""
# ggsave(file = output_path, plot = replanning_effect_plot_UP1, width = 5, height = 4)

# compare statistically
res.fried <- SAT %>% friedman_test(planning_time ~ condition |participant_ID)
res.fried
SAT %>% friedman_effsize(planning_time ~ condition |participant_ID)

# pairwise comparisons
pwc <- SAT %>%
  wilcox_test(planning_time ~ condition, paired = TRUE, p.adjust.method = "bonferroni")
pwc

## UP2 #### 
SAT<- subset(B3_SAT,n_UP2_noreplan_1action>4 & 
                   n_UP2_replan_1action>4 & 
                   n_UP2_transsuccess>4 & 
                   n_UP2_RTdeterm_2actions >4)

SAT <- subset(SAT,select=c("participant_ID",
                              "UP2_RTnoreplan_1action",
                              "UP2_RTreplan_1action",
                              "UP2_RTtranssuccess",
                              "UP2_RTdeterm_2actions"))

SAT <-melt.data.frame(SAT, id=c("participant_ID"), variable_name = "condition")
levels(SAT$condition)[1] <- "noreplan"
levels(SAT$condition)[2] <- "replan"
levels(SAT$condition)[3] <- "success"
levels(SAT$condition)[4] <- "deterministic"

SAT[ , c(2)]<- as.factor(SAT[ , c(2)])
SAT$condition  <- relevel(SAT$condition , "success")

names(SAT)[names(SAT) == 'value'] <- 'planning_time'
SAT$planning_time <- SAT$planning_time/1000

# descriptives 
descr_RT <- describeBy(SAT$planning_time,SAT$condition)

descr_RT <- ldply (descr_RT, data.frame)
names(descr_RT)[names(descr_RT) == '.id'] <- 'condition'

# boxplot for data overview
bxp_noise <- ggboxplot(
  SAT, x = "condition", y = "planning_time",palette = "jco", 
  add = "jitter")+
  xlab("action x condition") +
  ylab("mean RT") +
  ggtitle("mean RT per action x condition ") +
  theme_bw() 
bxp_noise


# plot bar plot with error bars (standard error of the mean)
replanning_effect_plot_UP2 <- ggplot(descr_RT, aes(x=condition, y=mean, fill=condition)) + 
  scale_color_identity()+
  geom_point(data = SAT, position = jitter, size = 0.9, aes(y = planning_time, x = condition))+
  geom_bar(stat="identity", color="black", width=0.7, alpha=.5,
           position=position_dodge()) +
  theme_light()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(.7))+
  xlab("mini-block type") +
  ylab("RT after probabilistic transition (s) ") +
  theme(plot.title=element_text(hjust=0.5) ,
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=15),
        title=element_text(face = "bold", size = 15),
        legend.position = 'none') +
  theme(legend.text = element_text(size = 15)) +
  scale_y_continuous(limits = c(0, 10000))+
  coord_cartesian(ylim=c(0.3,6.5))

replanning_effect_plot_UP2

# # save image
# output_path <- ""
# ggsave(file = output_path, plot = replanning_effect_plot_UP2, width = 5, height = 4)

# compare statistically
res.fried <- SAT %>% friedman_test(planning_time ~ condition |participant_ID)
res.fried
SAT %>% friedman_effsize(planning_time ~ condition |participant_ID)

# pairwise comparisons
pwc <- SAT %>%
  wilcox_test(planning_time ~ condition, paired = TRUE, p.adjust.method = "bonferroni")
pwc


## RT plan replan simplified ####
# keep the subset the same
SAT<- subset(B3_SAT,n_UP1_noreplan>4 & 
               n_UP1_replan>4 & 
               n_UP1_transsuccess>4 & 
               n_UP1_RTdeterm_1action >4)


SAT <- subset(SAT,select=c("participant_ID",
                           "UP1_RTdeterm_1action",
                           "UP1_RTtranssuccess",
                           "UP1_RTtransfail"))

SAT <-melt.data.frame(SAT, id=c("participant_ID"), variable_name = "condition")
levels(SAT$condition)[1] <- "deterministic"
levels(SAT$condition)[2] <- "success"
levels(SAT$condition)[3] <- "replan"

SAT[ , c(2)]<- as.factor(SAT[ , c(2)])
SAT$condition  <- relevel(SAT$condition , "deterministic")

names(SAT)[names(SAT) == 'value'] <- 'planning_time'

SAT$planning_time <- SAT$planning_time/1000

# descriptives 
descr_RT <- describeBy(SAT$planning_time,SAT$condition)

descr_RT <- ldply (descr_RT, data.frame)
names(descr_RT)[names(descr_RT) == '.id'] <- 'condition'

#boxplot for data overview
bxp_noise <- ggboxplot(
  SAT, x = "condition", y = "planning_time",palette = "jco", 
  add = "jitter")+
  xlab("action x condition") +
  ylab("mean RT") +
  ggtitle("mean RT per action x condition ") +
  theme_bw() 
bxp_noise


# plot bar plot with error bars (standard error of the mean)
replanning_effect_plot_UP1 <- ggplot(descr_RT, aes(x=condition, y=mean, fill=condition)) + 
  scale_color_identity()+
  geom_point(data = SAT, position = jitter, size = 0.9, aes(y = planning_time, x = condition))+
  geom_bar(stat="identity", color="black", width=0.7, alpha=.5,
           position=position_dodge()) +
  theme_light()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, 
                position=position_dodge(.7))+
  xlab("mini-block type") +
  ylab("RT after probabilistic transition (s) ") +
  theme(plot.title=element_text(hjust=0.5) ,
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=15),
        title=element_text(face = "bold", size = 15),
        legend.position = 'none') +
  theme(legend.text = element_text(size = 15)) +
  scale_y_continuous(limits = c(0, 10000))+
  coord_cartesian(ylim=c(0.3,5.5))

replanning_effect_plot_UP1

# # save image
# output_path <- ""
# ggsave(file = output_path, plot = replanning_effect_plot_UP1, width = 5, height = 4)

# test statistically
res.fried <- SAT %>% friedman_test(planning_time ~ condition |participant_ID)
res.fried
SAT %>% friedman_effsize(planning_time ~ condition |participant_ID)

# pairwise comparisons
pwc <- SAT %>%
  wilcox_test(planning_time ~ condition, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# UP2 
SAT<- subset(B3_SAT,n_UP2_noreplan_1action>4 & 
               n_UP2_replan_1action>4 & 
               n_UP2_transsuccess>4 & 
               n_UP2_RTdeterm_2actions >4)

SAT <- subset(SAT,select=c("participant_ID",
                           "UP2_RTdeterm_2actions",
                           "UP2_RTtranssuccess",
                           "UP2_RTtransfail"))

SAT <-melt.data.frame(SAT, id=c("participant_ID"), variable_name = "condition")
levels(SAT$condition)[1] <- "deterministic"
levels(SAT$condition)[2] <- "success"
levels(SAT$condition)[3] <- "replan"


SAT[ , c(2)]<- as.factor(SAT[ , c(2)])
SAT$condition  <- relevel(SAT$condition , "deterministic")

names(SAT)[names(SAT) == 'value'] <- 'planning_time'
SAT$planning_time <- SAT$planning_time/1000

# descriptives 
descr_RT <- describeBy(SAT$planning_time,SAT$condition)

descr_RT <- ldply (descr_RT, data.frame)
names(descr_RT)[names(descr_RT) == '.id'] <- 'condition'


# plot bar plot with error bars (standard error of the mean)
replanning_effect_plot_UP2 <- ggplot(descr_RT, aes(x=condition, y=mean, fill=condition)) + 
  scale_color_identity()+
  geom_point(data = SAT, position = jitter, size = 0.9, aes(y = planning_time, x = condition))+
  geom_bar(stat="identity", color="black", width=0.7, alpha=.5,
           position=position_dodge()) +
  theme_light()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, 
                position=position_dodge(.7))+
  xlab("mini-block type") +
  ylab("RT after probabilistic transition (s) ") +
  theme(plot.title=element_text(hjust=0.5) ,
        axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face="bold", size=15),
        title=element_text(face = "bold", size = 15),
        legend.position = 'none') +
  theme(legend.text = element_text(size = 15)) +
  scale_y_continuous(limits = c(0, 10000))+
  coord_cartesian(ylim=c(0.3,5.5))

replanning_effect_plot_UP2

# # save image
# output_path <- ""
# ggsave(file = output_path, plot = replanning_effect_plot_UP2, width = 5, height = 4)

# test statistically
res.fried <- SAT %>% friedman_test(planning_time ~ condition |participant_ID)
res.fried
SAT %>% friedman_effsize(planning_time ~ condition |participant_ID)

# pairwise comparisons
pwc <- SAT %>%
  wilcox_test(planning_time ~ condition, paired = TRUE, p.adjust.method = "bonferroni")
pwc

# model parameters ####
descr_beta <- describe(B3_SAT$lowprob_prun_beta)
print(descr_beta)

descr_theta <- describe(B3_SAT$lowprob_prun_theta)
print(descr_theta)

# risk taking behavior #######
B3_SAT_HLL <- subset(B3_SAT, participant_ID < 1000) #as only a part of the sample filled in the questionnaire

lottery_vars <- paste0("lottery", 1:10)

# compute "switch-point": first appearance of choice B (=1)
B3_SAT_HLL$lotteryswitch <- apply(B3_SAT_HLL[, lottery_vars], 1, function(row) {
  first_one <- which(row == "1")
  if (length(first_one) == 10) {
    return(0)  
  } else if (length(first_one) > 0) {
    return(min(first_one) - 1)  
  } else {
    return(NA)
  }
})

# exclude participants that switched back to option A after switching to option B once
B3_SAT_HLL$valid_sequence <- apply(B3_SAT_HLL[, lottery_vars], 1, function(row) {
  first_one <- which(row == "1")
  if (length(first_one) == 0) {
    return(TRUE)  
  } else {
    first <- min(first_one)
    return(!("0" %in% row[(first+1):length(row)]))
  }
})

# only include valid cases
B3_SAT_HLL <- subset(B3_SAT_HLL, !is.na(lotteryswitch) & valid_sequence)

# create HLLgroups for uncertainty avoider
for (k in 1:nrow(B3_SAT_HLL)){
  if (B3_SAT_HLL$lotteryswitch[k]>4){
    B3_SAT_HLL$group[k] <- "avoider"
  }else if(B3_SAT_HLL$lotteryswitch[k]<4){
    B3_SAT_HLL$group[k] <- "taker"
  }else if(B3_SAT_HLL$lotteryswitch[k]==4){
    B3_SAT_HLL$group[k] <- "neutral"
  }
}

describe(B3_SAT_HLL$lotteryswitch)
n_taker <- sum(B3_SAT_HLL$lotteryswitch < 4, na.rm = TRUE)
n_avoider <- sum(B3_SAT_HLL$lotteryswitch > 4, na.rm = TRUE)
n_neutral <- sum(B3_SAT_HLL$lotteryswitch == 4, na.rm = TRUE)

## relative performance ####
SAT <- subset(B3_SAT_HLL,select=c("group","participant_ID","UP1_2_rel_gain","UP3_4_rel_gain", "rel_gain_UP5"))
SAT <-melt.data.frame(SAT, id=c("group", "participant_ID"), variable_name = "condition")
levels(SAT$condition)[1] <- "UP1_2_rel_gain"
levels(SAT$condition)[2] <- "UP3_4_rel_gain"
levels(SAT$condition)[3] <- "rel_gain_UP5"

SAT[ , c(3)]<- as.factor(SAT[ , c(3)])
SAT$condition  <- relevel(SAT$condition , "UP1_2_rel_gain")

names(SAT)[names(SAT) == 'value'] <- 'Gain'

# descriptives 
descr_Gain_UP12 <- describeBy(SAT$Gain[SAT$condition == 'UP1_2_rel_gain'], SAT$group[SAT$condition == 'UP1_2_rel_gain'])
descr_Gain_UP34 <- describeBy(SAT$Gain[SAT$condition == 'UP3_4_rel_gain'], SAT$group[SAT$condition == 'UP3_4_rel_gain'])
descr_Gain_UP5 <- describeBy(SAT$Gain[SAT$condition == 'rel_gain_UP5'], SAT$group[SAT$condition == 'rel_gain_UP5'])

descr_Gain_UP12 <- ldply (descr_Gain_UP12, data.frame)
descr_Gain_UP34 <-  ldply (descr_Gain_UP34, data.frame)
descr_Gain_UP5 <-  ldply (descr_Gain_UP5, data.frame)

descr_Gain <- rbind(descr_Gain_UP12, descr_Gain_UP34,descr_Gain_UP5)
descr_Gain[1,15] = "UP12"
descr_Gain[2,15] = "UP12"
descr_Gain[3,15] = "UP34"
descr_Gain[4,15] = "UP34"
descr_Gain[5,15] = "UP5"
descr_Gain[6,15] = "UP5"

names(descr_Gain)[names(descr_Gain) == 'V15'] <- 'condition'
names(descr_Gain)[names(descr_Gain) == '.id'] <- 'group'

# normality: Shapiro-Test 
SAT %>%
  group_by(condition, group) %>%
  shapiro_test(Gain) 

# homoscedasticity: Levene-Test 
SAT %>%
  group_by(group) %>%
  levene_test(Gain ~ condition) 

# plot bar plot with error bars (standard error of the mean)
ggplot(descr_Gain, aes(x=group, y=mean, fill=condition)) +
  geom_point(data = SAT, position = jitter, size = 0.9, aes(y = Gain, x = group))+
  scale_color_identity()+
  geom_bar(stat="identity", color="black", width=0.7,
           position=position_dodge()) +
  scale_fill_manual("condition",values=c("#3399FF99","#33669999","#3399FF99","#33669999","#3399FF99","#33669999"))+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,  size = 0.3,
                position=position_dodge(.7))+
  xlab("age group") +
  ylab("relative performance") +
  scale_y_continuous(limits = c(0, 1200))+
  coord_cartesian(ylim=c(0,110))


# 2-way mixed ANOVA (source: https://www.datanovia.com/en/lessons/mixed-anova-in-r/#two-way-mixed)
res <- anova_test(
  data = SAT, dv = Gain, wid = participant_ID,
  between = group, within = condition, effect.size = "pes", type = 3, detailed = TRUE)
get_anova_table(res, correction = "GG")

## planning depth ####
SAT <- subset(B3_SAT_HLL,select=c("group","participant_ID","PD_UP1_2","PD_UP3_4", "PD_UP5"))
SAT <-melt.data.frame(SAT, id=c("group", "participant_ID"), variable_name = "condition")
levels(SAT$condition)[1] <- "PD_UP1_2"
levels(SAT$condition)[2] <- "PD_UP3_4"
levels(SAT$condition)[3] <- "PD_UP5"

SAT[ , c(3)]<- as.factor(SAT[ , c(3)])
SAT$condition  <- relevel(SAT$condition , "PD_UP1_2")

names(SAT)[names(SAT) == 'value'] <- 'PD'

# descriptives 
descr_PD_UP12 <- describeBy(SAT$PD[SAT$condition == 'PD_UP1_2'], SAT$group[SAT$condition == 'PD_UP1_2'])
descr_PD_UP34 <- describeBy(SAT$PD[SAT$condition == 'PD_UP3_4'], SAT$group[SAT$condition == 'PD_UP3_4'])
descr_PD_UP5 <- describeBy(SAT$PD[SAT$condition == 'PD_UP5'], SAT$group[SAT$condition == 'PD_UP5'])

descr_PD_UP12 <- ldply (descr_PD_UP12, data.frame)
descr_PD_UP34 <-  ldply (descr_PD_UP34, data.frame)
descr_PD_UP5 <-  ldply (descr_PD_UP5, data.frame)

descr_PD <- rbind(descr_PD_UP12, descr_PD_UP34,descr_PD_UP5)
descr_PD[1,15] = "UP12"
descr_PD[2,15] = "UP12"
descr_PD[3,15] = "UP34"
descr_PD[4,15] = "UP34"
descr_PD[5,15] = "UP5"
descr_PD[6,15] = "UP5"

names(descr_PD)[names(descr_PD) == 'V15'] <- 'condition'
names(descr_PD)[names(descr_PD) == '.id'] <- 'group'

#  checking for assumptions 
# normality: Shapiro-Test 
SAT %>%
  group_by(condition, group) %>%
  shapiro_test(PD)

# homoscedasticity: Levene-Test
SAT %>%
  group_by(group) %>%
  levene_test(PD ~ condition)

# plot bar plot with error bars (standard error of the mean)
ggplot(descr_PD, aes(x=group, y=mean, fill=condition)) +
  geom_point(data = SAT, position = jitter, size = 0.9, aes(y = PD, x = group))+
  scale_color_identity()+
  geom_bar(stat="identity", color="black", width=0.7,
           position=position_dodge()) +
  scale_fill_manual("condition",values=c("#3399FF99","#33669999","#3399FF99","#33669999","#3399FF99","#33669999"))+
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,  size = 0.3,
                position=position_dodge(.7))+
  xlab("age group") +
  ylab("relative performance") +
  scale_y_continuous(limits = c(0, 1200))+
  coord_cartesian(ylim=c(0,2))


# 2-way mixed ANOVA (source: https://www.datanovia.com/en/lessons/mixed-anova-in-r/#two-way-mixed)
res <- anova_test(
  data = SAT, dv = PD, wid = participant_ID,
  between = group, within = condition, effect.size = "pes", type = 3, detailed = TRUE)
get_anova_table(res, correction = "GG") 

