rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

windowsFonts(Times = windowsFont("Times New Roman"))

sim_results <- read.csv(file = "results_simulation_synth.csv")


is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))



sim_results[is.nan(sim_results)] <- 0
sim_results[is.na(sim_results)] <- 0


sim_results$p <- factor(sim_results$p,
                        labels = c("p = 10", "p = 20"),
                        levels = c("10", "20") )


##################################
######## or rule #################
##################################

sim_results_or <- sim_results %>%
  filter(measure %in% c("Sp", "Sen", "MCC"),
         model %in% c("bic_or", "boot", "aic_or", "ebic"))


summary_or <- sim_results_or %>%
  group_by(model, n, p, sparsity, size, measure) %>%
  summarise(mu = mean(score), scl = sd(score))


summary_or$model <- factor(summary_or$model,
                           levels = c("aic_and", "bic_and", "boot", "ebic"),
                           labels = c("AIC", "BIC", "Boot", "glasso_EBIC"))


summary_or$sparsity <- factor(summary_or$sparsity, levels = c(0.1, 0.2, 0.3, 0.4, 0.5),
                              labels = c(0.9, 0.8, 0.7, 0.6, 0.5))





##################################
######## and rule #################
##################################

sim_results_and <- sim_results %>%
  filter(measure %in% c("Sp", "Sen", "MCC"),
         model %in% c("bic_and", "boot", "aic_and", "ebic"))



summary_and <- sim_results_and %>%
  group_by(model, n, p, sparsity, size, measure) %>%
  summarise(mu = mean(score), scl = sd(score))


summary_and$model <- factor(summary_and$model,
                           levels = c("aic_and", "bic_and", "boot", "ebic"),
                           labels = c("AIC", "BIC", "Boot", "glasso_EBIC"))


summary_and$sparsity <- factor(summary_and$sparsity, levels = c(0.1, 0.2, 0.3, 0.4, 0.5),
                              labels = c(0.9, 0.8, 0.7, 0.6, 0.5))



########################
####### legend #########
########################
leg_plt <- summary_or %>% filter( measure == "Sp", p == "p = 20") %>%
  ggplot(aes(x = sparsity, y =  1 - mu, fill = as.factor(size))) +
  geom_bar(stat = "identity", position = position_dodge(0.85), width = .75) +
  facet_grid(p ~ model) +
  ylab("False Positive Rate (1 - SPC)") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0.005)) +
  scale_x_discrete(label = c("90 %", "80 %", "70 %", "60 %", "50 %")) +
  geom_errorbar(aes(ymax =  1 - mu + scl, ymin =  1 - mu - 0),
                position = position_dodge(0.85), width = 0) +
  theme(axis.title = element_text(size = 14),
        title =element_text(size = 14),
        text = element_text(family = "Times"),
        strip.background = element_rect(fill = "gray90"),
        legend.background = element_rect(color = "black"),
        panel.grid.minor = element_blank(),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid = element_line(color = "gray92")) +
  scale_fill_manual(values = c("#009E73", "#E69F00"), name = "Size",
                    breaks = c("0.25", "0.39"), labels = c("  0.25", "  0.40")) +
  xlab("Sparisty")
leg_get <- get_legend(leg_plt)




##############################################
############# Specificity ####################
##############################################

#########################
#### SPC 10 or rule #####
#########################
spc_plot_10_or <- summary_or %>% filter( measure == "Sp", p == "p = 10") %>%
  ggplot(aes(x = sparsity, y =  1 - mu, fill = as.factor(size))) +
  geom_bar(stat = "identity", position = position_dodge(0.85), width = .75) +
  facet_grid(p ~ model) +
  ylab("False Positive Rate (1 - SPC)") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0.005), limits = c(0, 0.65)) +
  scale_x_discrete(label = c("90 %", "80 %", "70 %", "60 %", "50 %")) +
  geom_errorbar(aes(ymax =  1 - mu + scl, ymin =  1 - mu - 0),
                position = position_dodge(0.85), width = 0) +
  theme(axis.title = element_text(size = 14),
        title =element_text(size = 14),
        text = element_text(family = "Times"),
        strip.background = element_rect(fill = "gray90"),
        legend.background = element_rect(color = "black"),
        panel.grid.minor = element_blank(),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid = element_line(color = "gray92"),
        axis.text.x = element_blank(),
        plot.margin = margin(t = .5, r = .5, b = -.1, l = .5, unit = "cm"))  +
        scale_fill_manual(values = c("#009E73", "#E69F00"), name = "Size",
        breaks = c("0.25", "0.39"), labels = c("  0.25", "  0.40")) +
  xlab("Sparisty") +
  xlab("") +
  guides(fill = FALSE)




#########################
#### SPC 20 or rule #####
#########################
spc_plot_20_or <- summary_or %>% filter(measure == "Sp", p == "p = 20") %>%
  ggplot(aes(x = sparsity, y =  1 - mu, fill = as.factor(size))) +
  geom_bar(stat = "identity", position = position_dodge(0.85), width = .75) +
  facet_grid(p ~ model) +
  ylab("False Positive Rate (1 - SPC)") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0.005),  limits = c(0, 0.65)) +
  scale_x_discrete(label = c("90 %", "80 %", "70 %", "60 %", "50 %")) +
  geom_errorbar(aes(ymax =  1 - mu + scl, ymin =  1 - mu - 0),
                position = position_dodge(0.85), width = 0) +
  theme(axis.title = element_text(size = 14),
        title =element_text(size = 14),
        text = element_text(family = "Times"),
        strip.background = element_rect(fill = "gray90"),
        legend.background = element_rect(color = "black"),
        panel.grid.minor = element_blank(),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid = element_line(color = "gray92"),
        strip.text.x = element_blank(),
        plot.margin = margin(t = -.1, r = .5, b = .1, l = .5, unit = "cm")) +
  scale_fill_manual(values = c("#009E73", "#E69F00"), name = "Size",
                   breaks = c("0.25", "0.39"), labels = c("  0.25", "  0.40")) +
  xlab("Sparisty") +
xlab("") +
  guides(fill = FALSE)



spc_combined_10_20 <- plot_grid(spc_plot_10_or, spc_plot_20_or, nrow = 2)
spc_combined_20 <- plot_grid(leg_get, spc_combined_10_20, rel_heights = c(1, 10), nrow = 2)

##############################################
############# Sensitivity ####################
##############################################

#########################
#### SPC 10 or rule #####
#########################
sn_plot_10_or <- summary_or %>% filter( measure == "Sen", p == "p = 10") %>%
  ggplot(aes(x = sparsity, y =  mu, fill = as.factor(size))) +
  geom_bar(stat = "identity", position = position_dodge(0.85), width = .75) +
  facet_grid(p ~ model) +
  ylab("Sensitivity") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0.005), limits = c(0, 1.1)) +
  scale_x_discrete(label = c("90 %", "80 %", "70 %", "60 %", "50 %")) +
  geom_errorbar(aes(ymax =   mu + scl, ymin =   mu - 0),
                position = position_dodge(0.85), width = 0) +
  theme(axis.title = element_text(size = 14),
        title =element_text(size = 14),
        text = element_text(family = "Times"),
        strip.background = element_rect(fill = "gray90"),
        legend.background = element_rect(color = "black"),
        panel.grid.minor = element_blank(),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid = element_line(color = "gray92"),
        axis.text.x = element_blank(),
        plot.margin = margin(t = .5, r = .5, b = -.1, l = .5, unit = "cm"))  +
  scale_fill_manual(values = c("#009E73", "#E69F00"), name = "Size",
                    breaks = c("0.25", "0.39"), labels = c("  0.25", "  0.40")) +
  xlab("Sparisty") +
  xlab("") +
  guides(fill = FALSE)




#########################
#### SPC 20 or rule #####
#########################
sn_plot_20_or <- summary_or %>% filter(measure == "Sen", p == "p = 20") %>%
  ggplot(aes(x = sparsity, y =   mu, fill = as.factor(size))) +
  geom_bar(stat = "identity", position = position_dodge(0.85), width = .75) +
  facet_grid(p ~ model) +
  ylab("Sensitivity") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0.005),  limits = c(0, 1)) +
  scale_x_discrete(label = c("90 %", "80 %", "70 %", "60 %", "50 %")) +
  geom_errorbar(aes(ymax =   mu + scl, ymin =   mu - 0),
                position = position_dodge(0.85), width = 0) +
  theme(axis.title = element_text(size = 14),
        title =element_text(size = 14),
        text = element_text(family = "Times"),
        strip.background = element_rect(fill = "gray90"),
        legend.background = element_rect(color = "black"),
        panel.grid.minor = element_blank(),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid = element_line(color = "gray92"),
        strip.text.x = element_blank(),
        plot.margin = margin(t = -.1, r = .5, b = .1, l = .5, unit = "cm")) +
  scale_fill_manual(values = c("#009E73", "#E69F00"), name = "Size",
                    breaks = c("0.25", "0.39"), labels = c("  0.25", "  0.40")) +
  xlab("Sparisty") +
  xlab("") +
  guides(fill = FALSE)


sn_combined_10_20 <- plot_grid(sn_plot_10_or, sn_plot_20_or, nrow = 2)
sn_combined_20 <- plot_grid(leg_get, sn_combined_10_20, rel_heights = c(1, 10), nrow = 2)












##############################################
############# Specificity ####################
##############################################

#########################
#### SPC 10 and rule #####
#########################
spc_plot_10_and <- summary_and %>% filter( measure == "Sp", p == "p = 10") %>%
  ggplot(aes(x = sparsity, y =  1 - mu, fill = as.factor(size))) +
  geom_bar(stat = "identity", position = position_dodge(0.85), width = .75) +
  facet_grid(p ~ model) +
  ylab("False Positive Rate (1 - SPC)") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0.005), limits = c(0, 0.65)) +
  scale_x_discrete(label = c("90 %", "80 %", "70 %", "60 %", "50 %")) +
  geom_errorbar(aes(ymax =  1 - mu + scl, ymin =  1 - mu - 0),
                position = position_dodge(0.85), width = 0) +
  theme(axis.title = element_text(size = 14),
        title =element_text(size = 14),
        text = element_text(family = "Times"),
        strip.background = element_rect(fill = "gray90"),
        legend.background = element_rect(color = "black"),
        panel.grid.minor = element_blank(),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid = element_line(color = "gray92"),
        axis.text.x = element_blank(),
        plot.margin = margin(t = .5, r = .5, b = -.1, l = .5, unit = "cm"))  +
  scale_fill_manual(values = c("#009E73", "#E69F00"), name = "Size",
                    breaks = c("0.25", "0.39"), labels = c("  0.25", "  0.40")) +
  xlab("Sparisty") +
  xlab("") +
  guides(fill = FALSE)




#########################
#### SPC 20 and rule #####
#########################
spc_plot_20_and <- summary_and %>% filter(measure == "Sp", p == "p = 20") %>%
  ggplot(aes(x = sparsity, y =  1 - mu, fill = as.factor(size))) +
  geom_bar(stat = "identity", position = position_dodge(0.85), width = .75) +
  facet_grid(p ~ model) +
  ylab("False Positive Rate (1 - SPC)") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0.005),  limits = c(0, 0.65)) +
  scale_x_discrete(label = c("90 %", "80 %", "70 %", "60 %", "50 %")) +
  geom_errorbar(aes(ymax =  1 - mu + scl, ymin =  1 - mu - 0),
                position = position_dodge(0.85), width = 0) +
  theme(axis.title = element_text(size = 14),
        title =element_text(size = 14),
        text = element_text(family = "Times"),
        strip.background = element_rect(fill = "gray90"),
        legend.background = element_rect(color = "black"),
        panel.grid.minor = element_blank(),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid = element_line(color = "gray92"),
        strip.text.x = element_blank(),
        plot.margin = margin(t = -.1, r = .5, b = .1, l = .5, unit = "cm")) +
  scale_fill_manual(values = c("#009E73", "#E69F00"), name = "Size",
                    breaks = c("0.25", "0.39"), labels = c("  0.25", "  0.40")) +
  xlab("Sparisty") +
  xlab("") +
  guides(fill = FALSE)



spc_combined_10_20 <- plot_grid(spc_plot_10_and, spc_plot_20_and, nrow = 2)
spc_combined_20 <- plot_grid(leg_get, spc_combined_10_20, rel_heights = c(1, 10), nrow = 2)

##############################################
############# Sensitivity ####################
##############################################

#########################
#### Sn 10 and rule #####
#########################
sn_plot_10_and <- summary_and %>% filter( measure == "Sen", p == "p = 10") %>%
  ggplot(aes(x = sparsity, y =  mu, fill = as.factor(size))) +
  geom_bar(stat = "identity", position = position_dodge(0.85), width = .75) +
  facet_grid(p ~ model) +
  ylab("Sensitivity") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0.005), limits = c(0, 1.1)) +
  scale_x_discrete(label = c("90 %", "80 %", "70 %", "60 %", "50 %")) +
  geom_errorbar(aes(ymax =   mu + scl, ymin =   mu - 0),
                position = position_dodge(0.85), width = 0) +
  theme(axis.title = element_text(size = 14),
        title =element_text(size = 14),
        text = element_text(family = "Times"),
        strip.background = element_rect(fill = "gray90"),
        legend.background = element_rect(color = "black"),
        panel.grid.minor = element_blank(),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid = element_line(color = "gray92"),
        axis.text.x = element_blank(),
        plot.margin = margin(t = .5, r = .5, b = -.1, l = .5, unit = "cm"))  +
  scale_fill_manual(values = c("#009E73", "#E69F00"), name = "Size",
                    breaks = c("0.25", "0.39"), labels = c("  0.25", "  0.40")) +
  xlab("Sparisty") +
  xlab("") +
  guides(fill = FALSE)




#########################
#### Sn 20 and rule #####
#########################
sn_plot_20_and <- summary_and %>% filter(measure == "Sen", p == "p = 20") %>%
  ggplot(aes(x = sparsity, y =   mu, fill = as.factor(size))) +
  geom_bar(stat = "identity", position = position_dodge(0.85), width = .75) +
  facet_grid(p ~ model) +
  ylab("Sensitivity") +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0.005),  limits = c(0, 1)) +
  scale_x_discrete(label = c("90 %", "80 %", "70 %", "60 %", "50 %")) +
  geom_errorbar(aes(ymax =   mu + scl, ymin =   mu - 0),
                position = position_dodge(0.85), width = 0) +
  theme(axis.title = element_text(size = 14),
        title =element_text(size = 14),
        text = element_text(family = "Times"),
        strip.background = element_rect(fill = "gray90"),
        legend.background = element_rect(color = "black"),
        panel.grid.minor = element_blank(),
        legend.direction = "horizontal",
        legend.position = "top",
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        panel.grid = element_line(color = "gray92"),
        strip.text.x = element_blank(),
        plot.margin = margin(t = -.1, r = .5, b = .1, l = .5, unit = "cm")) +
  scale_fill_manual(values = c("#009E73", "#E69F00"), name = "Size",
                    breaks = c("0.25", "0.39"), labels = c("  0.25", "  0.40")) +
  xlab("Sparisty") +
  xlab("") +
  guides(fill = FALSE)


sn_combined_10_20 <- plot_grid(sn_plot_10_and, sn_plot_20_and, nrow = 2)
sn_combined_10_20 <- plot_grid(leg_get, sn_combined_10_20, rel_heights = c(1, 10), nrow = 2)




