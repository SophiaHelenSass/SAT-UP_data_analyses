# by Sophia-Helen Sass
# model fit analysis of the Space Adventure Task (2025)
# using preprocessed and cleaned data 

# clear workspace
rm(list = ls())

# load packages
library(ggpubr)
library(effsize)
library(ggplot2)
library(data.table)
library(dplyr)
library(emmeans)
library(afex)
library(rstatix)
library(apa)
library(car)
library(ggplot2)
library(psych)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(dplyr)
library(reshape)
library(plyr)
library(ggsignif)
library(officer)

#load data
setwd("")

# read in replanning costs condition vector 
cond=read.csv("condition.csv", header = TRUE)
cond <- cond$condition

# load data from inference
# full-breadth planning model
modelfit_rational <- read.csv('Modelfit_rational_pilot3_1runs_500iter_1000samp.csv')
modelfit_rational$model <- "rational"

# delete unnecessary columns 
modelfit_rational <- modelfit_rational[, !names(modelfit_rational) %in% c("X")]

# low-probability pruning model
modelfit_lowprob_pruning<- read.csv('Modelfit_lowprob_pruning_pilot3_1runs_500iter_1000samp.csv')
modelfit_lowprob_pruning$model <- "lowprob_pruning"
modelfit_lowprob_pruning <- modelfit_lowprob_pruning[, !names(modelfit_lowprob_pruning) %in% c("X")]

# discounted low-probability pruning model
modelfit_disc_lowprob_pruning<- read.csv('Modelfit_discounting_lowprob_pruning_pilot3_1runs_500iter_1000samp.csv')
modelfit_disc_lowprob_pruning$model <- "disc_lowprob_pruning"
modelfit_disc_lowprob_pruning <- modelfit_disc_lowprob_pruning[, !names(modelfit_disc_lowprob_pruning) %in% c("X")]


# combine all model fit data 
comp_allmodels <- rbind(modelfit_rational,
                        modelfit_lowprob_pruning,
                        modelfit_disc_lowprob_pruning
                        )

models <- c("rational", 
            "lowprob_pruning", 
            "disc_lowprob_pruning")

comp_allmodels$model <- factor(comp_allmodels$model, levels = models)

#design jitter for data points
jitter = position_jitterdodge(
  jitter.width = 0.09,
  dodge.width = -0.7,
  seed = NA
)


# pseudo-Rho2 across conditions ####
# here you can choose which models you would like to use
comp_allmodels_subset<-comp_allmodels[(comp_allmodels$model=="rational"|
                                        comp_allmodels$model=="lowprob_pruning"|
                                        comp_allmodels$model=="disc_lowprob_pruning"),]

# descriptives 
descr_rho2 <- ldply(describeBy(comp_allmodels_subset$pseudo_rsquare_120_mean,comp_allmodels_subset$model))
names(descr_rho2)[names(descr_rho2) == '.id'] <- 'model'
descr_rho2$model <- as.factor(descr_rho2$model)
descr_rho2$model <- factor(descr_rho2$model, levels = models)

# plot bar plot with error bars (standard error of the mean)
rho2plot <- ggplot(descr_rho2, aes(x=model, y=mean, group= model, fill=model)) + 
  geom_point(data = comp_allmodels_subset, position = jitter, size = 0.9, aes(y = pseudo_rsquare_120_mean, x = model))+
  geom_bar(stat="identity", color="black", width=0.7, alpha=.6, position=position_dodge(width=0.8)) + 
  scale_fill_manual(values=c("darkgrey", "darkgreen", "lightgreen")) +
  theme_bw() + 
  scale_x_discrete(breaks=NULL) + 
  theme_light() + 
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, size=0.9, position=position_dodge(width=0.8)) + 
  ylab(expression(paste( rho^2))) +  # Use expression and paste to include the Greek letter rho
  theme(
    plot.title=element_text(hjust=0.5, size=12),
    axis.text.y=element_text(face="bold", size=12),
    title=element_text(face="bold", size=16),
    axis.title.x=element_blank()  
  ) + 
  scale_y_continuous(limits=c(-1, 1.5)) + 
  coord_cartesian(ylim=c(-0.2, 0.7))


rho2plot

# save the plot as svg
# output_path <- "/modefit_rho2_incl_scatter.svg"
# ggsave(file = output_path, plot = rho2plot, width = 10, height = 8)


# pseudo Rho2 across conditions cleaned for Rho2 below 0 ####
comp_allmodels_clean<-comp_allmodels_subset[comp_allmodels_subset$pseudo_rsquare_120_mean>0,]

# descriptives
descr_rho2 <- ldply(describeBy(comp_allmodels_clean$pseudo_rsquare_120_mean,comp_allmodels_clean$model))
names(descr_rho2)[names(descr_rho2) == '.id'] <- 'model'
descr_rho2$model <- as.factor(descr_rho2$model)
descr_rho2$model <- factor(descr_rho2$model, levels = models)

# plot bar plot with error bars (standard error of the mean)
rho2plot_clean <- ggplot(descr_rho2, aes(x=model, y=mean, group= model, fill=model)) + 
  geom_point(data = comp_allmodels_clean, position = jitter, size = 0.9, aes(y = pseudo_rsquare_120_mean, x = model))+
  geom_bar(stat="identity", color="black", width=0.7, alpha=.6, position=position_dodge(width=0.8)) + 
  scale_fill_manual(values=c("darkgrey","darkgreen", "lightgreen")) +
  theme_bw() + 
  scale_x_discrete(breaks=NULL) + 
  theme_light() + 
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, size=0.9, position=position_dodge(width=0.8)) + 
  ylab(expression(paste( rho^2))) +  
  theme(
    plot.title=element_text(hjust=0.5, size=12),
    axis.text.y=element_text(face="bold", size=12),
    title=element_text(face="bold", size=16),
    axis.title.x=element_blank()  
  ) + 
  scale_y_continuous(limits=c(-1, 1.5), breaks = seq(-0.2,0.6,0.1)) + 
  coord_cartesian(ylim=c(-0.2, 0.6))

rho2plot_clean

# save the plot as svg
# output_path <- "/modefit_rho2_incl_scatter_clean.svg"
# ggsave(file = output_path, plot = rho2plot, width = 6, height = 8)


# BIC across conditions ####
# descriptives 
descr_BIC_all <- ldply(describeBy(comp_allmodels_subset$BIC_120_mean,comp_allmodels_subset$model))
names(descr_BIC_all)[names(descr_BIC_all) == '.id'] <- 'model'
descr_BIC_all$model <- as.factor(descr_BIC_all$model)
descr_BIC_all$model <- factor(descr_BIC_all$model, levels = models)

# plot bar plot with error bars (standard error of the mean)
BICplot_all <- ggplot(descr_BIC_all, aes(x=model, y=mean, group= model, fill=model)) + 
 geom_point(data = comp_allmodels_subset, position = jitter, size = 0.9, aes(y = BIC_120_mean, x = model))+
  geom_bar(stat="identity", color="black", width=0.7, alpha=.6, position=position_dodge(width=0.8)) + 
  scale_fill_manual(values=c("darkgrey", "darkgreen", "lightgreen")) +
  theme_bw() + 
  scale_x_discrete(breaks=NULL) + 
  theme_light() + 
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, size=0.9, position=position_dodge(width=0.8)) + 
  ylab(expression(paste("mean BIC"))) +  
  theme(
    plot.title=element_text(hjust=0.5, size=12),
    axis.text.y=element_text(face="bold", size=12),
    title=element_text(face="bold", size=16),
    axis.title.x=element_blank()  
  ) +
  scale_y_continuous(limits=c(0, 500)) + 
  coord_cartesian(ylim=c(90, 250))

BICplot_all

# save the plot as svg
# output_path <- "/modelfit_BIC_incl_scatter.svg"
# ggsave(file = output_path, plot = BICplot_all, width = 10, height = 8)


# BIC across conditions cleaned for rho2 < 0 (data cannot be fitted reliably) ####
# descriptives
descr_BIC_all_clean <- ldply(describeBy(comp_allmodels_clean$BIC_120_mean,comp_allmodels_clean$model))
names(descr_BIC_all_clean)[names(descr_BIC_all_clean) == '.id'] <- 'model'
descr_BIC_all_clean$model <- as.factor(descr_BIC_all_clean$model)
descr_BIC_all_clean$model <- factor(descr_BIC_all_clean$model, levels = models)

BICplot_all_clean <- ggplot(descr_BIC_all_clean, aes(x = model, y = mean, group = model, fill = model)) + 
  geom_point(data = comp_allmodels_clean, position = position_jitter(width = 0.02),  
             size = 0.9, aes(y = BIC_120_mean, x = model)) +
  geom_bar(stat = "identity", color = "black", width = 0.7, alpha = 0.6, 
           position = position_dodge(width = 0.8)) + 
  scale_fill_manual(values = c("darkgrey", "darkgreen", "lightgreen")) +
  theme_bw() + 
  scale_x_discrete(breaks = NULL) + 
  theme_light() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, size = 0.9, 
                position = position_dodge(width = 0.8)) + 
  ylab(expression(paste("mean BIC"))) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    title = element_text(face = "bold", size = 16),
    axis.title.x = element_blank()  # Remove x-axis title
  ) +
  scale_y_continuous(limits = c(0, 500), breaks = seq(90,250,20)) + 
  coord_cartesian(ylim = c(90, 250))

BICplot_all_clean

# save the plot as svg
# output_path <- "/modelfit_BIC_incl_scatter_clean.svg"
# ggsave(file = output_path, plot = BICplot_all_clean, width = 6, height = 8)


# Delta BIC #### 
# descriptives
descr_BIC_all <- ldply(describeBy(comp_allmodels$BIC_120_mean,comp_allmodels$model))
names(descr_BIC_all)[names(descr_BIC_all) == '.id'] <- 'model'
descr_BIC_all$model <- as.factor(descr_BIC_all$model)
descr_BIC_all$model <- factor(descr_BIC_all$model, levels = models)
models <- c("rational",
            "lowprob_pruning",
            "disc_lowprob_pruning")

descr_BIC_all$model <- factor(descr_BIC_all$model, levels = models)

#Create a function to compute the difference and evaluate the size
compute_difference <- function(model1, model2, bic1, bic2) {
  delta_bic <- bic2 - bic1
  abs_delta_bic <- abs(delta_bic)
  breaks <- c(-Inf, 0, 2, 6, 10, Inf)
  labels <- c("anecdotal", "bare meantion","positive", "strong", "very strong")
  evaluation <- labels[findInterval(abs_delta_bic, breaks, rightmost.closed = TRUE)]
  
  return(data.frame(
    "compared_models" = paste(model1, "vs", model2),
    "BIC_value_model_1" = round(bic1, 2),
    "BIC_value_model_2" = round(bic2, 2),
    "Delta_BIC" = round(delta_bic, 2),
    "Abs_Delta_BIC" = round(abs_delta_bic, 2),
    "Evaluation" = as.character(evaluation)
  ))
}

# Create all possible pairs of models
model_pairs <- combn(levels(descr_BIC_all$model), 2, simplify = TRUE)

# Apply the function to compute differences and evaluations
result_table <- t(apply(model_pairs, 2, function(pair) {
  model1 <- pair[1]
  model2 <- pair[2]
  bic1 <- subset(descr_BIC_all, model == model1)$mean
  bic2 <- subset(descr_BIC_all, model == model2)$mean
  compute_difference(model1, model2, bic1, bic2)
}))

# Convert result_table to a data frame
result_table <- as.data.frame(result_table)
print(result_table)

# Apply the function to compute differences and evaluations
result_table <- t(apply(model_pairs, 2, function(pair) {
  model1 <- pair[1]
  model2 <- pair[2]
  bic1 <- subset(descr_BIC_all, model == model1)$mean
  bic2 <- subset(descr_BIC_all, model == model2)$mean
  compute_difference(model1, model2, bic1, bic2)
}))

# Convert result_table to a data frame
result_table_small <- as.data.frame(result_table)
result_table <- rbind(result_table_small[[1]][[1]],
                      result_table_small[[2]][[1]],
                      result_table_small[[3]][[1]])

# plot delta BIC 
delta_BIC_plot <- ggplot(result_table, aes(x = compared_models, y = Delta_BIC)) +
  geom_bar(stat = "identity", color = "black", fill = "grey",alpha = 0.7) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12),  
    axis.title.x = element_text(face = "bold", size = 15),
    axis.title.y = element_text(face = "bold", size = 15),
    title = element_text(face = "bold", size = 16),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank()   
  ) +
  labs(y = expression(Delta * "BIC"), 
       x = "compared models", 
       title = expression("Comparison of model evidence based on " * Delta * " BIC")) +
  geom_hline(yintercept = 0, color = "black") +   
  coord_flip()  

delta_BIC_plot

# clean data and exclude participants that should not be fitted due to Rho?<0
comp_allmodels_clean<-comp_allmodels[comp_allmodels$pseudo_rsquare_120_mean>0,]

# descriptives
descr_BIC_all_clean <- ldply(describeBy(comp_allmodels_clean$BIC_120_mean,comp_allmodels_clean$model))
names(descr_BIC_all_clean)[names(descr_BIC_all_clean) == '.id'] <- 'model'
descr_BIC_all_clean$model <- as.factor(descr_BIC_all_clean$model)
descr_BIC_all_clean$model <- factor(descr_BIC_all_clean$model, levels = models)
models <- c("rational",
            "lowprob_pruning",
            "disc_lowprob_pruning")

descr_BIC_all_clean$model <- factor(descr_BIC_all_clean$model, levels = models)

#Create a function to compute the difference and evaluate the size
compute_difference <- function(model1, model2, bic1, bic2) {
  delta_bic <- bic2 - bic1
  abs_delta_bic <- abs(delta_bic)
  breaks <- c(-Inf, 0, 2, 6, 10, Inf)
  labels <- c("anecdotal", "bare meantion","positive", "strong", "very strong")
  evaluation <- labels[findInterval(abs_delta_bic, breaks, rightmost.closed = TRUE)]
  
  return(data.frame(
    "compared_models" = paste(model1, "vs", model2),
    "BIC_value_model_1" = round(bic1, 2),
    "BIC_value_model_2" = round(bic2, 2),
    "Delta_BIC" = round(delta_bic, 2),
    "Abs_Delta_BIC" = round(abs_delta_bic, 2),
    "Evaluation" = as.character(evaluation)
  ))
}

# Create all possible pairs of models
model_pairs <- combn(levels(descr_BIC_all_clean$model), 2, simplify = TRUE)

# Apply the function to compute differences and evaluations
result_table <- t(apply(model_pairs, 2, function(pair) {
  model1 <- pair[1]
  model2 <- pair[2]
  bic1 <- subset(descr_BIC_all_clean, model == model1)$mean
  bic2 <- subset(descr_BIC_all_clean, model == model2)$mean
  compute_difference(model1, model2, bic1, bic2)
}))

# Convert result_table to a data frame
result_table <- as.data.frame(result_table)
print(result_table)

# Apply the function to compute differences and evaluations
result_table <- t(apply(model_pairs, 2, function(pair) {
  model1 <- pair[1]
  model2 <- pair[2]
  bic1 <- subset(descr_BIC_all_clean, model == model1)$mean
  bic2 <- subset(descr_BIC_all_clean, model == model2)$mean
  compute_difference(model1, model2, bic1, bic2)
}))

# Convert result_table to a data frame
result_table_small <- as.data.frame(result_table)
result_table <- rbind(result_table_small[[1]][[1]],
                      result_table_small[[2]][[1]],
                      result_table_small[[3]][[1]])

# plot delta BIC 
delta_BIC_plot <- ggplot(result_table, aes(x = compared_models, y = Delta_BIC)) +
  geom_bar(stat = "identity", color = "black", fill = "grey",alpha = 0.7) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12),  
    axis.title.x = element_text(face = "bold", size = 15),
    axis.title.y = element_text(face = "bold", size = 15),
    title = element_text(face = "bold", size = 16),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank()   
  ) +
  labs(y = expression(Delta * "BIC"), 
       x = "compared models", 
       title = expression("Comparison of model evidence based on " * Delta * " BIC")) +
  geom_hline(yintercept = 0, color = "black") +   
coord_flip()  

delta_BIC_plot


# Delta BIC on subject level ####

# exclude participants with Rho2 < 0 in full-breadth and lowprob pruning model
comp_allmodels_subset <- subset(comp_allmodels_subset, ID!=1019)
comp_allmodels_subset <- subset(comp_allmodels_subset, ID!=1023)
comp_allmodels_subset <- subset(comp_allmodels_subset, ID!=43)

BIC_difference <- comp_allmodels_subset$BIC_120_mean[comp_allmodels_subset$model=="rational"]-
  comp_allmodels_subset$BIC_120_mean[comp_allmodels_subset$model=="lowprob_pruning"]

BIC_difference_df <- data.frame(
  Subject = 1:length(BIC_difference),
  Delta_BIC = BIC_difference
)

# Plot
delta_BIC_per_subject <- ggplot(BIC_difference_df, aes(x = reorder(Subject, Delta_BIC), y = Delta_BIC)) +
  geom_bar(stat = "identity", color = "black", fill = "grey", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 5),  
    axis.title.x = element_text(face = "bold", size = 15),
    axis.title.y = element_text(face = "bold", size = 15),
    title = element_text(face = "bold", size = 16),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.y = element_blank(),  
    panel.grid.minor.y = element_blank()   
  ) +
  labs(
    y = expression(Delta * "BIC"), 
    x = "Subject ID", 
    title = expression("BIC Difference: full-breadth planning - low-prob pruning")
  ) +
  coord_flip()  

delta_BIC_per_subject

# # save image as svg
# output_path <- "/delta_BIC_per_subject.svg"
# ggsave(file = output_path, plot = delta_BIC_per_subject, width = 5, height = 8)

