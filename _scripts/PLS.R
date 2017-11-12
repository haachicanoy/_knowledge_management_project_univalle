#--------------------------------------------------------------------------#
# Structural Equation Modeling
#--------------------------------------------------------------------------#

# load package "plspm"
library(plspm)

#---------------------------------------------------------------------------------------------#
# pilares  
pilar1<-km_data%>%select(P5_4, P5_5, P6_1, P6_2, P6_5, P7_2, P8_1, P13_13:P13_16,
                         P3_4, P5_6, P7_4, P7_5, P11_1:P11_5, P13_5, P13_6, P13_7, P13_8,
                         P9_1:P9_6)

pilar2<-km_data%>%select(P3_1:P3_3, P3_5:P3_8, P8_3, P8_4, P8_5, P13_1, P13_2, P13_3, P13_4,
                         P5_1:P5_3, P6_3, P6_4, P7_1, P7_3, P8_2, P13_9, P13_11,
                         P9_1:P9_6)
#---------------------------------------------------------------------------------------------#

#------ Aprendizaje + Tecnologia -> Gestion del Conocnimento ------ #
# Definir la matrix
myinner1 = matrix(c(0,0,0,0,0,0,1,1,0),
                 nrow=3, ncol=3, byrow=TRUE)
rownames(myinner1) = c("Aprendizaje", "Tecnologia", "G.Conocimiento")
colnames(myinner1) = c("Aprendizaje", "Tecnologia", "G.Conocimiento")

# Definir listado de variables
myouter1 = list(1:11, 11:24, 24:30)
# Definir modos
mymodes1 = c("A", "A", "A")
# Analisis PLS-PM 
mypls1 = plspm(pilar1, myinner1, myouter1, mymodes1)
# Graficos de analisis PLS
plot(mypls1)
plot(mypls1, what="loadings")

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


