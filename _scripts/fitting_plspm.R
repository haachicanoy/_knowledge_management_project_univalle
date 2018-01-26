# Gestion del conocimiento - Proyecto Univalle: ajustar modelos PLS-PM
# H. Achicanoy & C. Saavedra
# 2017

# R options
options(warn = -1); options(scipen = 999); g <- gc(); rm(list = ls())

# Load packages
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

# ------------------------------------------------------- #
# Model for fitting
# ------------------------------------------------------- #
# Aprendizaje -
#              -
#               -> Gestion conocimiento ---> Innovacion
#              -
# Tecnologia  -
# ------------------------------------------------------- #
km_df <- km_data %>% dplyr::select(P5_4, P5_5, P6_1, P6_2, P6_5, P7_2, P8_1, P13_13:P13_16, # Aprendizaje
                                   P3_4, P5_6, P7_4, P7_5, P11_1:P11_5, P13_5, P13_6, P13_7, P13_8, # Tecnologia
                                   P9_1:P9_6, # Gestion conocimiento
                                   P10_1:P10A_11) # Innovacion
# for(j in 1:ncol(df_m1)){
#   if(length(unique(as.character(df_m1[,j]))) == 5){
#     df_m1[,j] <- factor(df_m1[,j], levels = c("Totalmente en desacuerdo",
#                                               "En desacuerdo",
#                                               "Ni de acuerdo ni en desacuerdo",
#                                               "De acuerdo",
#                                               "Totalmente de acuerdo"), ordered = T)
#   } else {
#     if(length(unique(as.character(df_m1[,j]))) == 4){
#       df_m1[,j] <- factor(df_m1[,j], levels = c("En desacuerdo",
#                                                 "Ni de acuerdo ni en desacuerdo",
#                                                 "De acuerdo",
#                                                 "Totalmente de acuerdo"), ordered = T)
#     }
#   }
# }; rm(j)
# table(unlist(lapply(apply(X = df_m1, MARGIN = 2, FUN = table), length)))
# unlist(lapply(apply(X = df_m1, MARGIN = 2, FUN = table), length))[which(unlist(lapply(apply(X = df_m1, MARGIN = 2, FUN = table), length))==4)]
# ------------------------------------------------------- #
# Inner model
km_inner <- matrix(c(0,0,0,0,
                     0,0,0,0,
                     1,1,0,0,
                     0,0,1,0), ncol = 4, byrow = T)
rownames(km_inner) <- colnames(km_inner) <- c("Aprendizaje", "Tecnologia", "G.Conocimiento", "Innovacion")
innerplot(km_inner)

# List of variables
km_blocks <- list(1:11, 12:24, 25:30, 31:ncol(km_df))

# Modes
km_modes <- c("A", "A", "A", "A")

# Run PLS-PM
set.seed(1235)
km_pls <- plspm(Data = km_df, path_matrix = km_inner,
                blocks = km_blocks, modes = km_modes,
                scheme = "path", boot.val = TRUE, br = 500)

summary1 <- dplyr::data_frame(Response = "Innovacion",
                              Predictors = "Aprendizaje, Tecnologia, Gestion conocimiento")
summary1 <- summary1 %>%
  dplyr::mutate(Model = purrr::map(km_pls %>% list, function(x) x))

# ------------------------------------------------------- #
# PLS-PM plots
# ------------------------------------------------------- #
# Outer model results
plot(km_pls)

# Inner model results
plot(km_pls, what = "loadings")
plot(km_pls, what = "weights")

# Correlation between estimated scores
pairs(km_pls$scores, pch = 20, lower.panel = panel.smooth)
cor(km_pls$scores, method = "pearson")
cor(km_pls$scores, method = "spearman")
cor(km_pls$scores, method = "kendall")

# Estimated scores distribution
km_scores <- km_pls$scores %>% as.data.frame
km_scores <- km_scores %>% tidyr::gather(key = Index, value = Value)
km_scores %>% ggplot(aes(x = Value, group = Index, colour = Index, fill = Index)) +
  geom_density(alpha = .5) + facet_wrap(~Index) + theme_bw()

# # setting margin size
# op = par(mar = c(8, 3, 1, 0.5))
# # barplots of total effects (direct + indirect)
# barplot(t(km_pls$path_coefs), border = NA, col = c("#9E9AC8", "#DADAEB"),
#         las = 2, cex.names = 0.8, cex.axis = 0.8,
#         legend = c("Direct", "Indirect"),
#         args.legend = list(x = "top", ncol = 2, border = NA,
#                            bty = "n", title = "Effects"))
# # resetting default margins
# par(op)

# Crossloadings plot
xloads <- melt(km_pls$crossloadings, id.vars = c("name", "block"), variable_name = "LV")
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

# ------------------------------------------------------- #
# Model for fitting
# ------------------------------------------------------- #
# Organizacion -
#              -
#               -> Gestion conocimiento ---> Innovacion
#              -
# Liderazgo   -
# ------------------------------------------------------- #
km_df <- km_data %>% dplyr::select(P5_1:P5_3, P6_3, P6_4, P7_1, P7_3, P8_2, P13_9, P13_11, # Organizacion
                                   P3_1:P3_3, P3_5:P3_8, P8_3, P8_4, P8_5, P13_1, P13_2, P13_3, P13_4, # Liderazgo
                                   P9_1:P9_6, # Gestion conocimiento
                                   P10_1:P10A_11) # Innovacion
# for(j in 1:ncol(df_m1)){
#   if(length(unique(as.character(df_m1[,j]))) == 5){
#     df_m1[,j] <- factor(df_m1[,j], levels = c("Totalmente en desacuerdo",
#                                               "En desacuerdo",
#                                               "Ni de acuerdo ni en desacuerdo",
#                                               "De acuerdo",
#                                               "Totalmente de acuerdo"), ordered = T)
#   } else {
#     if(length(unique(as.character(df_m1[,j]))) == 4){
#       df_m1[,j] <- factor(df_m1[,j], levels = c("En desacuerdo",
#                                                 "Ni de acuerdo ni en desacuerdo",
#                                                 "De acuerdo",
#                                                 "Totalmente de acuerdo"), ordered = T)
#     }
#   }
# }; rm(j)
# table(unlist(lapply(apply(X = df_m1, MARGIN = 2, FUN = table), length)))
# unlist(lapply(apply(X = df_m1, MARGIN = 2, FUN = table), length))[which(unlist(lapply(apply(X = df_m1, MARGIN = 2, FUN = table), length))==4)]
# ------------------------------------------------------- #
# Inner model
km_inner <- matrix(c(0,0,0,0,
                     0,0,0,0,
                     1,1,0,0,
                     0,0,1,0), ncol = 4, byrow = T)
rownames(km_inner) <- colnames(km_inner) <- c("Organizacion", "Liderazgo", "G.Conocimiento", "Innovacion")
innerplot(km_inner)

# List of variables
km_blocks <- list(1:10, 11:24, 25:30, 31:ncol(km_df))

# Modes
km_modes <- c("A", "A", "A", "A")

# Run PLS-PM
set.seed(1235)
km_pls <- plspm(Data = km_df, path_matrix = km_inner,
                blocks = km_blocks, modes = km_modes,
                scheme = "path", boot.val = TRUE, br = 500)

summary2 <- dplyr::data_frame(Response = "Innovacion",
                              Predictors = "Organizacion, Liderazgo, Gestion conocimiento")
summary2 <- summary2 %>%
  dplyr::mutate(Model = purrr::map(km_pls %>% list, function(x) x))

# ------------------------------------------------------- #
# PLS-PM plots
# ------------------------------------------------------- #
# Outer model results
plot(km_pls)

# Inner model results
plot(km_pls, what = "loadings")
plot(km_pls, what = "weights")

# Correlation between estimated scores
pairs(km_pls$scores, pch = 20, lower.panel = panel.smooth)
cor(km_pls$scores, method = "pearson")
cor(km_pls$scores, method = "spearman")
cor(km_pls$scores, method = "kendall")

# Estimated scores distribution
km_scores <- km_pls$scores %>% as.data.frame
km_scores <- km_scores %>% tidyr::gather(key = Index, value = Value)
km_scores %>% ggplot(aes(x = Value, group = Index, colour = Index, fill = Index)) +
  geom_density(alpha = .5) + facet_wrap(~Index) + theme_bw()

# # setting margin size
# op = par(mar = c(8, 3, 1, 0.5))
# # barplots of total effects (direct + indirect)
# barplot(t(km_pls$path_coefs), border = NA, col = c("#9E9AC8", "#DADAEB"),
#         las = 2, cex.names = 0.8, cex.axis = 0.8,
#         legend = c("Direct", "Indirect"),
#         args.legend = list(x = "top", ncol = 2, border = NA,
#                            bty = "n", title = "Effects"))
# # resetting default margins
# par(op)

# Crossloadings plot
xloads <- melt(km_pls$crossloadings, id.vars = c("name", "block"), variable_name = "LV")
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

# ------------------------------------------------------- #
# Model for fitting
# ------------------------------------------------------- #
# Aprendizaje -
#
# Tecnologia   -
#                -> Gestion conocimiento ---> Innovacion
# Organizacion -
#
# Liderazgo   -
# ------------------------------------------------------- #
km_df <- km_data %>% dplyr::select(P5_4, P5_5, P6_1, P6_2, P6_5, P7_2, P8_1, P13_13:P13_16, # Aprendizaje
                                   P3_4, P5_6, P7_4, P7_5, P11_1:P11_5, P13_5, P13_6, P13_7, P13_8, # Tecnologia
                                   P5_1:P5_3, P6_3, P6_4, P7_1, P7_3, P8_2, P13_9, P13_11, # Organizacion
                                   P3_1:P3_3, P3_5:P3_8, P8_3, P8_4, P8_5, P13_1, P13_2, P13_3, P13_4, # Liderazgo
                                   P9_1:P9_6, # Gestion conocimiento
                                   P10_1:P10A_11) # Innovacion
# for(j in 1:ncol(df_m1)){
#   if(length(unique(as.character(df_m1[,j]))) == 5){
#     df_m1[,j] <- factor(df_m1[,j], levels = c("Totalmente en desacuerdo",
#                                               "En desacuerdo",
#                                               "Ni de acuerdo ni en desacuerdo",
#                                               "De acuerdo",
#                                               "Totalmente de acuerdo"), ordered = T)
#   } else {
#     if(length(unique(as.character(df_m1[,j]))) == 4){
#       df_m1[,j] <- factor(df_m1[,j], levels = c("En desacuerdo",
#                                                 "Ni de acuerdo ni en desacuerdo",
#                                                 "De acuerdo",
#                                                 "Totalmente de acuerdo"), ordered = T)
#     }
#   }
# }; rm(j)
# table(unlist(lapply(apply(X = df_m1, MARGIN = 2, FUN = table), length)))
# unlist(lapply(apply(X = df_m1, MARGIN = 2, FUN = table), length))[which(unlist(lapply(apply(X = df_m1, MARGIN = 2, FUN = table), length))==4)]
# ------------------------------------------------------- #
# Inner model
km_inner <- matrix(c(0,0,0,0,0,0,
                     0,0,0,0,0,0,
                     0,0,0,0,0,0,
                     0,0,0,0,0,0,
                     1,1,1,1,0,0,
                     0,0,0,0,1,0), ncol = 6, byrow = T)
rownames(km_inner) <- colnames(km_inner) <- c("Aprendizaje", "Tecnologia", "Organizacion", "Liderazgo", "G.Conocimiento", "Innovacion")
innerplot(km_inner)

# List of variables
km_blocks <- list(1:11, 12:24, 25:34, 35:48, 49:54, 55:ncol(km_df))

# Modes
km_modes <- c("A", "A", "A", "A", "A", "A")

# Run PLS-PM
set.seed(1235)
km_pls <- plspm(Data = km_df, path_matrix = km_inner,
                blocks = km_blocks, modes = km_modes,
                scheme = "path", boot.val = TRUE, br = 500)

summary3 <- dplyr::data_frame(Response = "Innovacion",
                              Predictors = "Aprendizaje, Tecnologia, Organizacion, Liderazgo, Gestion conocimiento")
summary3 <- summary3 %>%
  dplyr::mutate(Model = purrr::map(km_pls %>% list, function(x) x))

finalSummary <- rbind(summary1, summary2, summary3)
rm(summary1, summary2, summary3)

saveRDS(object = finalSummary, file = "../_results/plspm_results.RDS")

finalSummary <- readRDS("../_results/plspm_results.RDS")

# ------------------------------------------------------- #
# PLS-PM plots
# ------------------------------------------------------- #
# Outer model results
plot(km_pls)

# Inner model results
plot(km_pls, what = "loadings")
plot(km_pls, what = "weights")

# Correlation between estimated scores
pairs(km_pls$scores, pch = 20, lower.panel = panel.smooth)
cor(km_pls$scores, method = "pearson")
cor(km_pls$scores, method = "spearman")
cor(km_pls$scores, method = "kendall")

# Estimated scores distribution
km_scores <- km_pls$scores %>% as.data.frame
km_scores <- km_scores %>% tidyr::gather(key = Index, value = Value)
km_scores %>% ggplot(aes(x = Value, group = Index, colour = Index, fill = Index)) +
  geom_density(alpha = .5) + facet_wrap(~Index) + theme_bw()

# # setting margin size
# op = par(mar = c(8, 3, 1, 0.5))
# # barplots of total effects (direct + indirect)
# barplot(t(km_pls$path_coefs), border = NA, col = c("#9E9AC8", "#DADAEB"),
#         las = 2, cex.names = 0.8, cex.axis = 0.8,
#         legend = c("Direct", "Indirect"),
#         args.legend = list(x = "top", ncol = 2, border = NA,
#                            bty = "n", title = "Effects"))
# # resetting default margins
# par(op)

# Crossloadings plot
xloads <- melt(km_pls$crossloadings, id.vars = c("name", "block"), variable_name = "LV")
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









############################################################################
#------ Liderazgo + Orgaanizacion -> Gestion del Conocnimento ------ #
# Definir la matrix
myinner2 = matrix(c(0,0,0,0,0,0,1,1,0),
                 nrow=3, ncol=3, byrow=TRUE)
rownames(myinner2) = c("Liderazgo", "Organizacion", "G.Conocimiento")
colnames(myinner2) = c("Liderazgo", "Organizacion", "G.Conocimiento")

# Definir listado de variables
myouter2 = list(1:14, 14:24, 24:30)
# Definir modos
mymodes2 = c("A", "A", "A")
# Analisis PLS-PM 
mypls2 = plspm(pilar2, myinner2, myouter2, mymodes2)
# Graficos de analisis PLS
plot(mypls2)
plot(mypls2, what="loadings")


