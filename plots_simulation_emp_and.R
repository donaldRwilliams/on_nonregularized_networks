library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

windowsFonts(Times = windowsFont("Times New Roman"))

sim_results <- read.csv(file = "results_simulation_emp.csv")





is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

sim_results[is.nan(sim_results)] <- 0
sim_results[is.na(sim_results)] <- 0



sim_results$type <- factor(sim_results$type,
                            labels = c("Continuous", "Ordinal"),
                            levels = c("cont", "ordinal") )


sim_results$p <- factor(sim_results$p,
                           labels = c("p = 10", "p = 20"),
                           levels = c("10", "20") )





####################################
######## timing results ############
####################################

sim_results_time <- sim_results %>% filter(measure == "time")
summary_time <- sim_results_time %>%
  group_by(model, n, p, type) %>%
  summarise(mu = mean(score), scl = sd(score))


summary_time$model <- factor(summary_time$model,
                             level = c("boot", "ebic", "regress"),
                             labels = c("Boot", "glasso_EBIC", "Regression"))





summary_time %>% ggplot(aes(x = n, y = mu, group =model)) +
  facet_grid(~ type + p) +
  geom_line(aes(color = model), size = 1.25) +
  theme_bw() +
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
  ylab("Elapsed Time (Seconds)") +
  xlab("Sample Size") +
  scale_color_manual(values = c("#009E73", "#0072B2", "#999999"),
                     name = "Model") +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1))




########################################
######## edge accruracy results ########
########################################

# for text p = 20 and "or-rule"

sim_results_accuracy <- sim_results %>%
  filter(measure %in% c("cor", "mse"),
         model %in% c("bic_and", "boot", "aic_and", "ebic"))
summary_accuracy <- sim_results_accuracy %>%
  group_by(model, n, p, type, measure) %>%
  summarise(mu = mean(score), scl = sd(score))


summary_accuracy$model <- factor(summary_accuracy$model,
                                 levels = c("aic_and", "bic_and", "boot", "ebic"),
                                 labels = c("AIC", "BIC", "Boot", "glasso_EBIC"))



summary_accuracy$measure <- factor(summary_accuracy$measure,
                                   levels = c("cor", "mse"),
                                   labels = c("Correlation", "MSE"))





#############################
######## get legend #########
#############################

leg_plt <- summary_accuracy %>% filter(measure == "Correlation", p == "p = 20") %>%
  ggplot(aes(x = as.factor(n), y = mu, group =model)) +
  facet_grid( ~ type + p) +


  geom_errorbar(aes(ymax = mu + scl, ymin = mu - 0),
                position = position_dodge(.85), width = 0.35) +
  #geom_point(position = position_dodge(1), aes(color = model),
   #          size = 1.5)
   geom_bar(stat = "identity",
            position = position_dodge(.85),
            aes(fill = model), width = .75) +
  theme_bw() +
  # geom_ribbon(aes(ymax = mu + scl, ymin = mu - scl, fill = model),
  #             alpha = 0.15, position = position_dodge(1))

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
  ylab("Correlation") +
  xlab("") +
  scale_fill_manual(values = c("#56B4E9","#D55E00", "#009E73", "#0072B2"),
                     name = "Model") +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 1.1), breaks = seq(0, 1, .1))


get_leg <- get_legend(leg_plt)








#############################
######## correlatioon #######
#############################
# 20
cor_plot_20 <- summary_accuracy %>% filter(measure == "Correlation", p == "p = 20") %>%
  ggplot(aes(x = as.factor(n), y = mu, group =model)) +
  facet_grid(p ~ type) +


  geom_errorbar(aes(ymax = mu + scl, ymin = mu - 0),
                position = position_dodge(.85), width = 0) +
  #geom_point(position = position_dodge(1), aes(color = model),
  #          size = 1.5)
  geom_bar(stat = "identity",
           position = position_dodge(.85),
           aes(fill = model), width = .75) +
  theme_bw() +
  # geom_ribbon(aes(ymax = mu + scl, ymin = mu - scl, fill = model),
  #             alpha = 0.15, position = position_dodge(1))

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
        plot.margin = margin(t = .5, r = .5, b = -.1, l = .5, unit = "cm")) +
  ylab("Correlation") +
  xlab("") +
  scale_fill_manual(values = c("#56B4E9","#D55E00", "#009E73", "#0072B2"),
                    name = "Model") +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 1.1), breaks = seq(0, 1, .2)) +
  guides(fill = FALSE)




cor_plot_10 <- summary_accuracy %>% filter(measure == "Correlation", p == "p = 10") %>%
  ggplot(aes(x = as.factor(n), y = mu, group =model)) +
  facet_grid(p ~ type) +


  geom_errorbar(aes(ymax = mu + scl, ymin = mu - 0),
                position = position_dodge(.85), width = 0) +
  #geom_point(position = position_dodge(1), aes(color = model),
  #          size = 1.5)
  geom_bar(stat = "identity",
           position = position_dodge(.85),
           aes(fill = model), width = .75) +
  theme_bw() +
  # geom_ribbon(aes(ymax = mu + scl, ymin = mu - scl, fill = model),
  #             alpha = 0.15, position = position_dodge(1))

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
        plot.margin = margin(t = .5, r = .5, b = -.1, l = .5, unit = "cm")) +
  ylab("Correlation") +
  xlab("") +
  scale_fill_manual(values = c("#56B4E9","#D55E00", "#009E73", "#0072B2"),
                    name = "Model") +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 1.1), breaks = seq(0, 1, .2)) +
  guides(fill = FALSE)




####################################
######### mean squared error #######
####################################
# 20 variables
mse_plot_20 <- summary_accuracy %>% filter(measure == "MSE", p == "p = 20") %>%
  ggplot(aes(x = as.factor(n), y = mu, group =model)) +
  facet_grid(p ~ type) +
  geom_errorbar(aes(ymax = mu + scl, ymin = mu - 0),
                position = position_dodge(.85), width = 0) +
  geom_bar(stat = "identity",
           position = position_dodge(.85),
           aes(fill = model), width = .75) +
  theme_bw() +

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
  ylab("Mean Squared Error") +
  xlab("Sample Size") +
  scale_fill_manual(values = c("#56B4E9","#D55E00", "#009E73", "#0072B2"),
                    name = "Model") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5)) +
  guides(fill = FALSE)


# 10 variables
mse_plot_10 <- summary_accuracy %>% filter(measure == "MSE", p == "p = 10") %>%
  ggplot(aes(x = as.factor(n), y = mu, group =model)) +
  facet_grid(p ~ type) +
  geom_errorbar(aes(ymax = mu + scl, ymin = mu - 0),
                position = position_dodge(.85), width = 0) +
  geom_bar(stat = "identity",
           position = position_dodge(.85),
           aes(fill = model), width = .75) +
  theme_bw() +

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
  ylab("Mean Squared Error") +
  xlab("Sample Size") +
  scale_fill_manual(values = c("#56B4E9","#D55E00", "#009E73", "#0072B2"),
                    name = "Model") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.8)) +
  guides(fill = FALSE)




accuracy_combined_20 <- plot_grid(cor_plot_20, mse_plot_20, nrow = 2, rel_heights = c(1.05, 1))
accuracy_combined_20 <- plot_grid(get_leg, accuracy_combined_20, nrow = 2, rel_heights = c(1, 10))

accuracy_combined_10 <- plot_grid(cor_plot_10, mse_plot_10, nrow = 2)
accuracy_combined_10 <- plot_grid(get_leg, accuracy_combined_10, nrow = 2, rel_heights = c(1, 10))






#################################################
######### edge set indentification ##############
#################################################
sim_results_edge <- sim_results %>%
  filter(measure %in% c("Sp", "Sen", "MCC"),
         model %in% c("bic_and", "boot", "aic_and", "ebic"))
summary_edge <- sim_results_edge %>%
  group_by(model, n, p, type, measure) %>%
  summarise(mu = mean(score), scl = sd(score))


summary_edge$model <- factor(summary_edge$model,
                                 levels = c("aic_and", "bic_and", "boot", "ebic"),
                                 labels = c("AIC", "BIC", "Boot", "glasso_EBIC"))



summary_edge$measure <- factor(summary_edge$measure,
                                   levels = c("Sp", "Sen", "MCC"),
                                   labels = c("Specificity", "Sensitivity", "MCC"))




#############################
######## SPC #######
#############################
# 20
spc_plot_20 <- summary_edge %>% filter(measure == "Specificity", p == "p = 20") %>%
  ggplot(aes(x = as.factor(n), y = mu, group =model)) +
  facet_grid(p ~ type) +


  geom_errorbar(aes(ymax = mu + scl, ymin = mu - 0),
                position = position_dodge(.85), width = 0) +
  #geom_point(position = position_dodge(1), aes(color = model),
  #          size = 1.5)
  geom_bar(stat = "identity",
           position = position_dodge(.85),
           aes(fill = model), width = .75) +
  theme_bw() +
  # geom_ribbon(aes(ymax = mu + scl, ymin = mu - scl, fill = model),
  #             alpha = 0.15, position = position_dodge(1))

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
        plot.margin = margin(t = .5, r = .5, b = -.1, l = .5, unit = "cm")) +
  ylab("Specificity") +
  xlab("") +
  scale_fill_manual(values = c("#56B4E9","#D55E00", "#009E73", "#0072B2"),
                    name = "Model") +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 1.1), breaks = seq(0, 1, .2)) +
  guides(fill = FALSE)


# 10
spc_plot_10 <- summary_edge %>% filter(measure == "Specificity", p == "p = 10") %>%
  ggplot(aes(x = as.factor(n), y = mu, group =model)) +
  facet_grid(p ~ type) +


  geom_errorbar(aes(ymax = mu + scl, ymin = mu - 0),
                position = position_dodge(.85), width = 0) +
  #geom_point(position = position_dodge(1), aes(color = model),
  #          size = 1.5)
  geom_bar(stat = "identity",
           position = position_dodge(.85),
           aes(fill = model), width = .75) +
  theme_bw() +
  # geom_ribbon(aes(ymax = mu + scl, ymin = mu - scl, fill = model),
  #             alpha = 0.15, position = position_dodge(1))

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
        plot.margin = margin(t = .5, r = .5, b = -.1, l = .5, unit = "cm")) +
  ylab("Specificity") +
  xlab("") +
  scale_fill_manual(values = c("#56B4E9","#D55E00", "#009E73", "#0072B2"),
                    name = "Model") +
  scale_y_continuous(expand = c(0, 0),limits = c(0, 1.1), breaks = seq(0, 1, .2)) +
  guides(fill = FALSE)



###########################
####### sensitivtiy #######
###########################
# 20
sn_plot_20 <- summary_edge %>% filter(measure == "Sensitivity", p == "p = 20") %>%
  ggplot(aes(x = as.factor(n), y = mu, group =model)) +
  facet_grid(p ~ type) +
  geom_errorbar(aes(ymax = mu + scl, ymin = mu - 0),
                position = position_dodge(.85), width = 0) +
  geom_bar(stat = "identity",
           position = position_dodge(.85),
           aes(fill = model), width = .75) +
  theme_bw() +

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
        axis.text.x = element_blank(),
        plot.margin = margin(t = -.1, r = .5, b = .1, l = .5, unit = "cm")) +
  ylab("Sensitivity") +
  xlab("") +
  scale_fill_manual(values = c("#56B4E9","#D55E00", "#009E73", "#0072B2"),
                    name = "Model") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1), breaks = seq(0, 1, .2)) +
  guides(fill = FALSE)


# 10
sn_plot_10 <- summary_edge %>% filter(measure == "Sensitivity", p == "p = 10") %>%
  ggplot(aes(x = as.factor(n), y = mu, group =model)) +
  facet_grid(p ~ type) +
  geom_errorbar(aes(ymax = mu + scl, ymin = mu - 0),
                position = position_dodge(.85), width = 0) +
  geom_bar(stat = "identity",
           position = position_dodge(.85),
           aes(fill = model), width = .75) +
  theme_bw() +

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
        axis.text.x = element_blank(),
        plot.margin = margin(t = -.1, r = .5, b = .1, l = .5, unit = "cm")) +
  ylab("Sensitivity") +
  xlab("") +
  scale_fill_manual(values = c("#56B4E9","#D55E00", "#009E73", "#0072B2"),
                    name = "Model") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1), breaks = seq(0, 1, .2)) +
  guides(fill = FALSE)


############################################
############ MCC ##########################
############################################

mcc_plot_20 <- summary_edge %>% filter(measure == "MCC", p == "p = 20") %>%
  ggplot(aes(x = as.factor(n), y = mu, group =model)) +
  facet_grid(p ~ type) +
  geom_errorbar(aes(ymax = mu + scl, ymin = mu - 0),
                position = position_dodge(.85), width = 0) +
  geom_bar(stat = "identity",
           position = position_dodge(.85),
           aes(fill = model), width = .75) +
  theme_bw() +

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
  ylab("MCC") +
  xlab("Sample Size") +
  scale_fill_manual(values = c("#56B4E9","#D55E00", "#009E73", "#0072B2"),
                    name = "Model") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1), breaks = seq(0, 1, .2)) +
  guides(fill = FALSE)




mcc_plot_10 <- summary_edge %>% filter(measure == "MCC", p == "p = 10") %>%
  ggplot(aes(x = as.factor(n), y = mu, group =model)) +
  facet_grid(p ~ type) +
  geom_errorbar(aes(ymax = mu + scl, ymin = mu - 0),
                position = position_dodge(.85), width = 0) +
  geom_bar(stat = "identity",
           position = position_dodge(.85),
           aes(fill = model), width = .75) +
  theme_bw() +

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
  ylab("MCC") +
  xlab("Sample Size") +
  scale_fill_manual(values = c("#56B4E9","#D55E00", "#009E73", "#0072B2"),
                    name = "Model") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1), breaks = seq(0, 1, .2)) +
  guides(fill = FALSE)


edge_combined_20 <- plot_grid(spc_plot_20, sn_plot_20,  mcc_plot_20, nrow = 3, rel_heights = c(1.05, 1, 1))
edge_combined_20 <- plot_grid(get_leg, edge_combined_20, nrow = 2, rel_heights = c(1, 10))
edge_combined_20




edge_combined_10 <- plot_grid(spc_plot_10, sn_plot_10,  mcc_plot_10, nrow = 3, rel_heights = c(1.05, 1, 1))
edge_combined_10 <- plot_grid(get_leg, edge_combined_10, nrow = 2, rel_heights = c(1, 10))
edge_combined_10
