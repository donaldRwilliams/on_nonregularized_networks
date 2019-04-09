rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

windowsFonts(Times = windowsFont("Times New Roman"))

cv_results <- read.csv("results_motivating_cv.csv")[,-1]
eigen_results <- read.csv("results_motivating_eigen.csv")[,-1]
n <- c(50, 100, 250, 500, 1000, 2500)

sigma <- read.csv("motivating_cov.csv")[,-1]
inv_sigma <- solve(cov2cor(as.matrix(sigma)))

cv_results <- gather(cv_results, model, outcome, gl_0:gl_05:mle)
eigen_results <- gather(eigen_results, model, outcome, gl_0:gl_05:mle)


cv_summary <- cv_results %>% 
  group_by(n, model) %>% 
  summarise(mu = mean(outcome), scl = sd(outcome)) %>% 
  as.data.frame()

eigen_summary <- eigen_results %>% 
  group_by(n, model) %>% 
  summarise(mu = mean(outcome), scl = sd(outcome)) %>% 
  as.data.frame()


true_eigen <- max(eigen(inv_sigma)$values) / min(eigen(inv_sigma)$values)


leg <- cv_summary %>% filter(model != "gl_0") %>%
  ggplot(aes(x = as.factor(n), 
             y =mu, 
             group = model, 
             color = model)) + 
  geom_line(size = 2, aes(color = model)) +
  theme_bw() +
  geom_ribbon(aes(ymax = mu + scl, 
                  ymin = mu - scl, fill = model), 
              color = NA, 
              alpha = 0.075) +
  ylab("Cross-Validated Log-Likelihood")+
  xlab("Sample Size (p = 20)") +
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_manual(name = "Model", 
                    values = c("#0072B2", "#CC79A7"), 
                    breaks=c("gl_05", "mle"),
                    labels=c("glasso_EBIC", "MLE")) +
  scale_color_manual(name = "Model", 
                     values = c("#0072B2", "#CC79A7"),
                     breaks=c("gl_05", "mle"),
                     labels=c("glasso_EBIC", "MLE")) +
  theme(legend.direction = "horizontal", 
       
        legend.position = "top", 
        legend.background = element_rect(color = "black"),
        text = element_text(family = "Times"), 
        axis.title = element_text(size = 14), 
        title =element_text(size = 14),
        plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
        panel.grid.minor = element_blank()) 
  


leg <- get_legend(leg)









cv_plot <- cv_summary %>% filter(model != "gl_0") %>%
  ggplot(aes(x = as.factor(n), 
             y =mu, 
             group = model, 
             color = model)) + 
  geom_line(size = 2, aes(color = model)) +
  theme_bw() +
  geom_ribbon(aes(ymax = mu + scl, 
                  ymin = mu - scl, fill = model), 
              color = NA, 
              alpha = 0.075) +
  ylab("Cross-Validated Log-Likelihood")+
  xlab("Sample Size (p = 20)") +
  scale_x_discrete(expand = c(0,0)) +
  guides(color = FALSE) +
  guides(fill = FALSE) +
  scale_fill_manual(name = "Model",
                    values = c("#0072B2", "#CC79A7"),
                    breaks=c("gl_05", "mle"),
                    labels=c("glasso_EBIC", "MLE")) +
  scale_color_manual(name = "Model",
                     values = c("#0072B2", "#CC79A7"),
                     breaks=c("gl_05", "mle"),
                     labels=c("glasso_EBIC", "MLE")) +
  theme(legend.direction = "horizontal", 
        legend.justification = c(1, 0), 
        legend.position = c(1, 0), 
        legend.background = element_rect(color = "black"),
        text = element_text(family = "Times"), 
        axis.title = element_text(size = 14), 
        title =element_text(size = 14),
        plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
        panel.grid.minor = element_blank()) 






eigen_plot <- eigen_summary %>% filter(model != "gl_0") %>%
  ggplot(aes(x = as.factor(n), 
             y =mu, 
             group = model, 
             color = model)) + 
  geom_line(size = 2, aes(color = model)) +
  theme_bw() +
  geom_ribbon(aes(ymax = mu + scl, 
                  ymin = mu - scl, fill = model), 
              color = NA, 
              alpha = 0.075) +
  ylab("Eigenvalue Estimation ( max / min )")+
  xlab("Sample Size (p = 20)") +
  scale_x_discrete(expand = c(0,0)) +
  guides(color = FALSE) +
  guides(fill = FALSE) +
  scale_fill_manual(name = "Model", 
                    values = c("#0072B2", "#CC79A7"), 
                    breaks=c("gl_05", "mle"),
                    labels=c("glasso_EBIC", "MLE")) +
  scale_color_manual(name = "Model", 
                     values = c("#0072B2", "#CC79A7"),
                     breaks=c("gl_05", "mle"),
                     labels=c("glasso_EBIC", "MLE")) +
  theme(legend.direction = "horizontal", 
        legend.justification = c(1, 0), 
        legend.position = c(1, 0), 
        legend.background = element_rect(color = "black"),
        text = element_text(family = "Times"), 
        axis.title = element_text(size = 14), 
        title =element_text(size = 14),
        plot.margin = margin(t = .5, r = .5, b = .5, l = .5, unit = "cm"),
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = true_eigen, size = 1, alpha = 0.5)


both_plot <- plot_grid(eigen_plot, cv_plot) 

plot_grid(leg, both_plot, rel_heights = c(1, 10), ncol = 1)


