# Gestion del conocimiento - Proyecto Univalle: cargar y explorar datos
# H. Achicanoy & C. Saavedra
# 2017

# R options
options(warn = -1); options(scipen = 999)

# Load packages
suppressMessages(library(tidyverse))
suppressMessages(library(modelr))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(broom))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))
suppressMessages(library(foreign))
suppressMessages(library(haven))
suppressMessages(library(corrplot))
suppressMessages(library(polycor))
suppressMessages(library(psych))
suppressMessages(library(gplots))
suppressMessages(library(viridis))

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
# Pilar aprendizaje
# ------------------------------------------------------- #
aprendizaje <- pilares %>% dplyr::select(P5_4, P5_5, P6_1, P6_2, P6_5, P7_2, P8_1, P13_13:P13_16)
aprendizaje %>% glimpse

# Analisis descriptivo
fqTable <- aprendizaje %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable) <- c("Variable", "Categoria", "Frecuencia")
fqTable <- fqTable %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(fqTable))
fqTable$Categoria <- factor(fqTable$Categoria, levels = c("Totalmente en desacuerdo",
                               "En desacuerdo",
                               "Ni de acuerdo ni en desacuerdo",
                               "De acuerdo",
                               "Totalmente de acuerdo"), ordered = T)

# Plot it (SAVE IT!!!!)
gg <- fqTable %>% ggplot(aes(x = Categoria, y = Porcentaje*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  facet_wrap(~ Variable, scales = "free") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
ggsave("../_results/_descriptive_analysis/frecuencias_aprendizaje.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable, gg)


options(warn=-1)
p.chisq = matrix(0, nrow=ncol(aprendizaje), ncol=ncol(aprendizaje), byrow=T)
for(i in 1:ncol(aprendizaje)){
  for(j in 1:ncol(aprendizaje)){
    p.chisq[i,j] = round(chisq.test(aprendizaje[,i],aprendizaje[,j])$p.value,3)
  }
}; rm(i); rm(j)

diag(p.chisq) = NA
colnames(p.chisq) = colnames(aprendizaje)
rownames(p.chisq) = colnames(aprendizaje)

color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
# png('./_results/chi_test.png', height = 7, width = 7, units = "in", res = 300)
heatmap.2(p.chisq,
          main="Independence test",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=TRUE,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(11,11))
# dev.off(); rm(catVar, p.chisq, color_scale)

