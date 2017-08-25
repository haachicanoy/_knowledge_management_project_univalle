library(tidyverse)
library(foreign)

km_data <- read.spss(file = "../_data/Base GConocimiento PymeS  Valle_2017.sav", to.data.frame = T, use.value.labels = T) # F
names(km_data)

# Aprendizaje
aprendizaje <- km_data %>% dplyr::select(P5_4, P5_5, P6_1, P6_2, P6_5, P7_2, P8_1, P13_13:P13_16)
aprendizaje %>% glimpse

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
          Rowv=NULL,
          Colv=NULL,
          col=color_scale,
          linecol=NULL,
          tracecol=NULL,
          density.info="density",
          denscol="blue",
          margins=c(11,11))
# dev.off(); rm(catVar, p.chisq, color_scale)

