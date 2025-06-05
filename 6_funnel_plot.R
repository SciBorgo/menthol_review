
# Funnel plots
# DN Borg
# January 2025

# Packages
library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(naniar)
library(meta)
library(metafor)
library(dmetar)

# Functions
meta_model <- function(data) {metacont(n.c = con_n,
                     mean.c = con_mu,
                     sd.c = con_sd,
                     n.e = int_n,
                     mean.e = int_mu,
                     sd.e = int_sd, 
                     data = {{data}},
                     studlab = paste(study_id),
                     common = F,
                     random = T,
                     prediction = T,
                     sm = "SMD",
                     method.random.ci = "HK",
                     method.smd = "Hedges",
                     method.tau = "REML",
                     method.tau.ci = "QP")}

# Load data
# Performance
d <- read_csv("data/data_performance.csv") %>%
  clean_names()

d_tt <- d %>%
  filter(outcome == 'time_trial') %>%
  mutate(int_mu = int_x*-1,
         con_mu = con_x*-1)

d_tte <- d %>%
  filter(outcome != 'time_trial') %>%
  mutate(int_mu = int_x,
         con_mu = con_x)

df_perf <- union(d_tt, d_tte)

# RPE
df_rpe <- read_csv("data/data_rpe.csv") %>%
  clean_names() %>%
  mutate(int_mu = int_x,
         con_mu = con_x)

# Sensation
df_sen <- read_csv("data/data_sensation.csv") %>%
  clean_names() %>%
  mutate(int_mu = int_x,
         con_mu = con_x)

# Comfort
df_com <- read_csv("data/data_comfort.csv") %>%
  clean_names() %>%
  mutate(int_mu = int_x,
         con_mu = con_x)


# Make plot
pdf("figures/funnel.pdf", width = 3.25, height = 10)

# Set up a 3x3 panel for 9 plots
par(mfrow = c(4, 1))  # 3 rows and 3 columns

# Generate funnel plots
# Performance
par(mar = c(3.5, 3.5, 2, 1))
meta::funnel(meta_model(data = df_perf), 
       contour.levels = c(0.9, 0.95, 0.99),  # Confidence contours
       col.contour = c("darkred", "coral", "lightgray"),  # Custom colors
       col = "black",  # Black points for studies
       pch = 19,        # Open circles
       bty = "n",      # Remove box around the plot
       ann = FALSE)
legend(x = 0.55, y = 0.02, 
       legend = c("p < .1", "p < .05", "p < .01"),
       fill = c("darkred", "coral", "lightgray"))
title(main = "a) Performance", adj = 0, line = 1)

# RPE
par(mar = c(3.5, 3.5, 2, 1))
meta::funnel(meta_model(data = df_rpe), 
             contour.levels = c(0.9, 0.95, 0.99),  # Confidence contours
             col.contour = c("darkred", "coral", "lightgray"),  # Custom colors
             col = "black",  # Black points for studies
             pch = 19,        # Open circles
             bty = "n",      # Remove box around the plot
             ann = FALSE)
# legend(x = 0.4, y = 0.01, 
#        legend = c("p < .1", "p < .05", "p < .01"),
#        fill = c("darkred", "coral", "lightgray"))
title(main = "b) Rating of perceived exertion", adj = 0, line = 1)

# Sensation
par(mar = c(3.5, 3.5, 2, 1))
meta::funnel(meta_model(data = df_sen), 
             contour.levels = c(0.9, 0.95, 0.99),  # Confidence contours
             col.contour = c("darkred", "coral", "lightgray"),  # Custom colors
             col = "black",  # Black points for studies
             pch = 19,        # Open circles
             bty = "n",      # Remove box around the plot
             ann = FALSE)
# legend(x = 0.55, y = 0.01, 
#        legend = c("p < .1", "p < .05", "p < .01"),
#        fill = c("darkred", "coral", "lightgray"))
title(main = "c) Thermal sensation", adj = 0, line = 1)

# Comfort
par(mar = c(3.5, 3.5, 2, 1))
meta::funnel(meta_model(data = df_com), 
             contour.levels = c(0.9, 0.95, 0.99),  # Confidence contours
             col.contour = c("darkred", "coral", "lightgray"),  # Custom colors
             col = "black",  # Black points for studies
             pch = 19,        # Open circles
             bty = "n",      # Remove box around the plot
             ann = FALSE)
# legend(x = 0.7, y = 0.01, 
#        legend = c("p < .1", "p < .05", "p < .01"),
#        fill = c("darkred", "coral", "lightgray"))
title(main = "d) Thermal comfort", adj = 0, line = 1)

dev.off()



#### End


