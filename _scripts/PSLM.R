
# ------------------------------------------------------- #
suppressMessages(library(tidyverse))
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))
suppressMessages(library(foreign))
suppressMessages(library(corrplot))
suppressMessages(library(polycor))
suppressMessages(library(psych))
suppressMessages(library(gplots))
suppressMessages(library(viridis))
suppressMessages(library(lsr))
suppressMessages(library(DescTools))
suppressMessages(library(plspm))
suppressMessages(library(reshape))
# ------------------------------------------------------- #
# Loading data
# ------------------------------------------------------- #
km_data <- read.spss(file = "../_data/Base GConocimiento PymeS  Valle_2017.sav", to.data.frame = T, use.value.labels = T) # F
names(km_data)

pilares <- km_data %>% select(P3_1:P3_8, P5_1:P9_6, P11_1:P13_16, P22_1:P25_5)
for(j in 1:ncol(pilares)){
  pilares[,j] <- factor(pilares[,j], levels = c("Totalmente en desacuerdo",
                                                "En desacuerdo",
                                                "Ni de acuerdo ni en desacuerdo",
                                                "De acuerdo",
                                                "Totalmente de acuerdo"), ordered = T)
}; rm(j)
pilares %>% glimpse
pilares %>% str

# ------------------------------------------------------- #
# Model for fitting
# ------------------------------------------------------- #
#
# Transferencia del conocimiento  -
#
#  Adquisicion Conocimiento Interno  -
#                                       -> Gestion conocimiento ---> Innovacion
#  Adquisicion Conocimiento Externo  -
#
#     Uso del Conocimiento        -
#
# ------------------------------------------------------- #

km_df <- km_data %>% dplyr::select(P5_1:P5_6,     # Adquisicion Conocimiento Externo
                                   P6_1:P6_5,     # Adquisicion Conocimiento Interno
                                   P7_1:P7_5,     # Transferencia del conocimiento
                                   P8_1:P8_5,     # Uso del conocimiento
                                   P9_1:P9_6,     # Gestion conocimiento
                                   P10_1:P10A_11) # Innovacion

# rows of the path matrix
C.Externo =      c(0,0,0,0,0,0)
C.interno =      c(0,0,0,0,0,0)
T.conocimiento = c(0,0,0,0,0,0)
U.conocimiento = c(0,0,0,0,0,0)
G.Conocimiento = c(1,1,1,1,0,0)
Innovacion =     c(0,0,0,0,1,0)


# path matrix (inner model)
foot_path = rbind(C.Externo, C.interno, T.conocimiento, U.conocimiento, G.Conocimiento, Innovacion)

# add column names
colnames(foot_path) = rownames(foot_path)

# blocks of indicators (outer model)
foot_blocks = list(1:6, 7:11, 12:16, 17:21, 22:27, 28:ncol(km_df))

# vector of modes (reflective)
foot_modes = c("A", "A", "A", "A", "A", "A")

# run plspm analysis
foot_pls = plspm(km_df, foot_path, foot_blocks, modes = foot_modes)

# path coefficients
foot_pls$path_coefs

# inner model
foot_pls$inner_model

# plotting results (inner model)
plot(foot_pls)

# plotting loadings of the outer model
plot(foot_pls, what = "loadings", arr.width = 0.1)

# Inner model results
plot(foot_pls, what = "loadings")
plot(foot_pls, what = "weights")

# unidimensionality
foot_pls$unidim

# cronbach's alpha
foot_pls$unidim[, 3, drop = FALSE]

# inner model summary R2
foot_pls$inner_summary[, "R2", drop = FALSE]

# Correlation between estimated scores
pairs(foot_pls$scores, pch = 20, lower.panel = panel.smooth)
cor(foot_pls$scores, method = "pearson")
cor(foot_pls$scores, method = "spearman")
cor(foot_pls$scores, method = "kendall")

# Estimated scores distribution
km_scores <- foot_pls$scores %>% as.data.frame
km_scores <- km_scores %>% tidyr::gather(key = Index, value = Value)
km_scores %>% ggplot(aes(x = Value, group = Index, colour = Index, fill = Index)) +
  geom_density(alpha = .5) + facet_wrap(~Index) + theme_bw()

# Crossloadings plot
xloads <- melt(foot_pls$crossloadings, id.vars = c("name", "block"), variable_name = "LV")
xloads %>% ggplot(aes(x = name, y = value, fill = block)) +
  geom_hline(yintercept = 0, color = "gray75") +
  geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(block ~ LV) +
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  ggtitle("Crossloadings") +
  xlab("Variables") + ylab("Correlation")

# running bootstrap validation
foot_val = plspm(km_df, foot_path, foot_blocks, modes = foot_modes,
                 boot.val = TRUE, br = 500)

# bootstrap results
foot_val$boot
# ------------------------------------------------------- #


# show me the first scores
head(foot_pls$scores, n = 5)

# show me the last scores
tail(foot_pls$scores, n = 5)

# load ggplot2 and reshape
library(ggplot2)
library(reshape)

# reshape crossloadings data.frame for ggplot
xloads = melt(foot_pls$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV")

# bar-charts of crossloadings by block
ggplot(data = xloads,
       aes(x = name, y = value, fill = block)) +
  # add horizontal reference lines
  geom_hline(yintercept = 0, color = "gray75") +
  geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) +
  # indicate the use of car-charts
  geom_bar(stat = 'identity', position = 'dodge') +
  # panel display (i.e. faceting)
  facet_wrap(block ~ LV) +
  # tweaking some grahical elements
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  # add title
  ggtitle("Crossloadings")



