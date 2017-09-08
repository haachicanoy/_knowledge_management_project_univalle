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
suppressMessages(library(lsr))

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
x11()
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

# Chi-square test
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
png(file = '../_results/_descriptive_analysis/chi_square_test_aprendizaje.png', height = 7, width = 7, units = "in", res = 300)
heatmap.2(p.chisq,
          main="Independence test Learning",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=F,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(11,11))
dev.off()#; rm(catVar, p.chisq, color_scale)

# Cramer's V test
options(warn=-1)
p.cramer = matrix(0, nrow=ncol(aprendizaje), ncol=ncol(aprendizaje), byrow=T)
for(i in 1:ncol(aprendizaje)){
  for(j in 1:ncol(aprendizaje)){
    p.cramer[i,j] = round(lsr::cramersV(aprendizaje[,i],aprendizaje[,j]),3)
  }
}; rm(i); rm(j)

# diag(p.cramer) = NA
colnames(p.cramer) = colnames(aprendizaje)
rownames(p.cramer) = colnames(aprendizaje)

corrplot::corrplot(corr = p.cramer, method = "square")

# ------------------------------------------------------- #
# Pilar Tecnologia 
# ------------------------------------------------------- #
tecnologia <- pilares %>% dplyr::select(P3_4, P5_6, P7_4, P7_5, P11_1:P11_5, P13_5, P13_6, P13_7, P13_8)
tecnologia %>% glimpse

# Analisis descriptivo
fqTable2 <- tecnologia %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable2) <- c("Variable", "Categoria", "Frecuencia")
fqTable2 <- fqTable2 %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(fqTable2))
fqTable2$Categoria <- factor(fqTable2$Categoria, levels = c("Totalmente en desacuerdo",
                                                          "En desacuerdo",
                                                          "Ni de acuerdo ni en desacuerdo",
                                                          "De acuerdo",
                                                          "Totalmente de acuerdo"), ordered = T)

# Plot it (SAVE IT!!!!)
x11()
gg <- fqTable2 %>% ggplot(aes(x = Categoria, y = Porcentaje*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  facet_wrap(~ Variable, scales = "free") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
ggsave("../_results/_descriptive_analysis/frecuencias_tecnologia.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable2, gg)


options(warn=-1)
p.chisq2 = matrix(0, nrow=ncol(tecnologia), ncol=ncol(tecnologia), byrow=T)
for(i in 1:ncol(tecnologia)){
  for(j in 1:ncol(tecnologia)){
    p.chisq2[i,j] = round(chisq.test(tecnologia[,i],tecnologia[,j])$p.value,3)
  }
}; rm(i); rm(j)

diag(p.chisq2) = NA
colnames(p.chisq2) = colnames(tecnologia)
rownames(p.chisq2) = colnames(tecnologia)

color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
# png('./_results/chi_test.png', height = 7, width = 7, units = "in", res = 300)
heatmap.2(p.chisq2,
          main="Independence test Technology",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=F,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(11,11))

# ------------------------------------------------------- #
# Pilar Liderazgo 
# ------------------------------------------------------- #
liderazgo <- pilares %>% dplyr::select(P3_1:P3_3, P3_5:P3_8, P8_3, P8_4, P8_5, P13_1, P13_2, P13_3, P13_4)
liderazgo  %>% glimpse

# Analisis descriptivo
fqTable3 <- liderazgo  %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable3) <- c("Variable", "Categoria", "Frecuencia")
fqTable3 <- fqTable3 %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(fqTable3))
fqTable3$Categoria <- factor(fqTable3$Categoria, levels = c("Totalmente en desacuerdo",
                                                            "En desacuerdo",
                                                            "Ni de acuerdo ni en desacuerdo",
                                                            "De acuerdo",
                                                            "Totalmente de acuerdo"), ordered = T)

# Plot it (SAVE IT!!!!)
x11()
gg <- fqTable3 %>% ggplot(aes(x = Categoria, y = Porcentaje*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  facet_wrap(~ Variable, scales = "free") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
ggsave("../_results/_descriptive_analysis/frecuencias_liderazgo.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable3, gg)


options(warn=-1)
p.chisq3 = matrix(0, nrow=ncol(liderazgo), ncol=ncol(liderazgo), byrow=T)
for(i in 1:ncol(liderazgo)){
  for(j in 1:ncol(liderazgo)){
    p.chisq3[i,j] = round(chisq.test(liderazgo[,i],liderazgo[,j])$p.value,3)
  }
}; rm(i); rm(j)

diag(p.chisq3) = NA
colnames(p.chisq3) = colnames(liderazgo)
rownames(p.chisq3) = colnames(liderazgo)

color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
# png('./_results/chi_test.png', height = 7, width = 7, units = "in", res = 300)
heatmap.2(p.chisq3,
          main="Independence test Leadership",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=F,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(11,11))

# ------------------------------------------------------- #
# Pilar Organizacion 
# ------------------------------------------------------- #
organizacion <- pilares %>% dplyr::select(P5_1:P5_3, P6_3, P6_4, P7_1, P7_3, P8_2, P13_9, P13_11)
organizacion  %>% glimpse

# Analisis descriptivo
fqTable4 <- organizacion  %>%
  gather(measure, value) %>%
  count(measure, value)
names(fqTable4) <- c("Variable", "Categoria", "Frecuencia")
fqTable4 <- fqTable4 %>% dplyr::mutate(Porcentaje = Frecuencia/nrow(fqTable4))
fqTable4$Categoria <- factor(fqTable4$Categoria, levels = c("Totalmente en desacuerdo",
                                                            "En desacuerdo",
                                                            "Ni de acuerdo ni en desacuerdo",
                                                            "De acuerdo",
                                                            "Totalmente de acuerdo"), ordered = T)

# Plot it (SAVE IT!!!!)
x11()
gg <- fqTable4 %>% ggplot(aes(x = Categoria, y = Porcentaje*100)) +
  geom_bar(stat = "identity") +
  xlab("") + ylab("Porcentaje (%)") +
  coord_flip() +
  facet_wrap(~ Variable, scales = "free") +
  theme_bw() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  theme(axis.title.x = element_text(size = 13, face = 'bold'),
        axis.title.y = element_text(size = 13, face = 'bold'),
        axis.text = element_text(size = 12))
ggsave("../_results/_descriptive_analysis/frecuencias_organizacion.png", plot = gg, width = 22, height = 10, units = "in"); rm(fqTable4, gg)


options(warn=-1)
p.chisq4 = matrix(0, nrow=ncol(organizacion), ncol=ncol(organizacion), byrow=T)
for(i in 1:ncol(organizacion)){
  for(j in 1:ncol(organizacion)){
    p.chisq4[i,j] = round(chisq.test(organizacion[,i],organizacion[,j])$p.value,3)
  }
}; rm(i); rm(j)

diag(p.chisq4) = NA
colnames(p.chisq4) = colnames(organizacion)
rownames(p.chisq4) = colnames(organizacion)

color_scale = colorRampPalette(c("tomato3","lightyellow","lightseagreen"), space="rgb")(50)
# png('./_results/chi_test.png', height = 7, width = 7, units = "in", res = 300)
heatmap.2(p.chisq4,
          main="Independence test Organization",
          key.title="Chi-square test",
          key.xlab="p-value",
          Rowv=F,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(11,11))

#----------------------------------------------------------#
# Analisis Canonico Tecnologia / Aprendizaje 
#----------------------------------------------------------#
suppressMessages(library(GGally))
suppressMessages(library(CCA))

km_data <- read.spss(file = "../_data/Base GConocimiento PymeS  Valle_2017.sav", to.data.frame = T, use.value.labels = F) # F
aprendizaje <- km_data %>% dplyr::select(P5_4, P5_5, P6_1, P6_2, P6_5, P7_2, P8_1, P13_13:P13_16)
tecnologia <- km_data %>% dplyr::select(P3_4, P5_6, P7_4, P7_5, P11_1:P11_5, P13_5, P13_6, P13_7, P13_8)

corrplot(cor(cbind(aprendizaje, tecnologia)), method = "square")

# Y: aprendizaje
# X: tecnologia
cc1 <- cc(tecnologia, aprendizaje) 
cc1$cor[1]
cc1$cor[1:3]

cc1[3:4]

cc2 <- comput(tecnologia, aprendizaje, cc1)
cc2[3:6]
cc2[3:6]$corr.X.xscores %>% View
cc2[3:6]$corr.Y.yscores %>% View

ggplot(data = data.frame(P7_2 = aprendizaje$P7_2, P7_5 = tecnologia$P7_5), aes(x = P7_2, y = P7_5, size = 28, alpha = .6)) + geom_point()

# Tests of canonical dimensions
ev <- (1 - cc1$cor^2)

n <- dim(aprendizaje)[1]
p <- length(aprendizaje)
q <- length(tecnologia)
k <- min(p, q)
m <- n - 3/2 - (p + q)/2

w <- rev(cumprod(rev(ev)))

# initialize
d1 <- d2 <- f <- vector("numeric", k)

for (i in 1:k) {
  s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
  si <- 1/s
  d1[i] <- p * q
  d2[i] <- m * s - p * q/2 + 1
  r <- (1 - w[i]^si)/w[i]^si
  f[i] <- r * d2[i]/d1[i]
  p <- p - 1
  q <- q - 1
}

pv <- pf(f, d1, d2, lower.tail = FALSE)
(dmat <- cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv))

# standardized psych canonical coefficients diagonal matrix of apre sd's
s1 <- diag(sqrt(diag(cov(aprendizaje))))
s1 %*% cc1$xcoef
# standardized psych canonical coefficients diagonal matrix of tec sd's
s2 <- diag(sqrt(diag(cov(tecnologia))))
s2 %*% cc1$ycoef

#----------------------------------------------------------#
# Cuantificacion optima de variables
#----------------------------------------------------------#

library(homals)

### Aprendizaje
faData   <- all_data[,mtch_fa]
faHomals <- homals(faData, active=rep(TRUE, ncol(faData)), level='ordinal', sets=list(1:ncol(faData)))
faQuan   <- faHomals$low.rank # Quantified variables
faDataqF <- faData

for(i in 1:ncol(faDataqF)){
  
  faDataqF[,i] <- as.character(faDataqF[,i])
  dfQuan <- as.data.frame(faQuan[[i]])
  
  for(j in 1:length(rownames(dfQuan))){
    mtch <- which(faDataqF[,i]==rownames(dfQuan)[j])
    if(length(mtch)>0){
      faDataqF[,i][which(faDataqF[,i]==rownames(dfQuan)[j])] <- dfQuan[j,1]
    }
  }
  
  faDataqF[,i] <- as.numeric(faDataqF[,i])
  
}; rm(i,j,mtch)
rm(faData, faHomals, dfQuan, faQuan)

