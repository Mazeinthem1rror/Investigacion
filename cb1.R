##############################################
# Codebook ---------------------------------
# Author: M. Constanza Ayala (mcayala@uc.cl)
# Fecha última modificación: 15 enero 2023
##############################################


# 1. DATA PREPARATION ---------------------------------------------
options(scipen=999)
rm(list=(ls()))

# 2. Carga de paquetes---------------------------------------------
pacman::p_load(haven, tidyverse, sjPlot, codebook)

# 3. Carga de BBDD------------------------------------------------
load(file = "input/data/data_list.RData")
dim(data) #751 obs, #113 variables
names(data)

# 4. CREATION VARIABLES -----------------------------------------
# 4.1. Character to factor ------------------------------------------------
data <- data %>%
  mutate_if(is.character,as.factor)

# 4.2. Treatment ----------------------------------------------------------
table(data$control2,exclude = F)
table(data$trat1.2,exclude = F)
table(data$trat2.2,exclude = F)

data <- data %>% 
  mutate(trat=case_when(
    !is.na(control2) ~ 0,
    !is.na(trat1.2) ~ 1,
    !is.na(trat2.2) ~ 2
  ))
table(data$trat,exclude = F)
data$trat <-  factor(data$trat,
                     levels = c(0:2),
                     labels = c("Control",
                                "Treatment1",
                                "Treatment2"))
# 4.4 Outcome: List ------------------------------------------------------
data <- data %>% 
  mutate(list=case_when(
    is.na(control2)~trat1.2,
    TRUE ~ control2)) %>% 
  mutate(list=case_when(
    is.na(list)~trat2.2,
    TRUE ~ list))
table(data$list,exclude = F)

#borrar las variables: control2, trat1.2, trat2.2
# 3.6. Geographic area ----------------------------------------------------

#Comuna to region
#Comuna to region
table(data$comuna,exclude = F)

data <- data %>% 
  mutate(region=case_when(
    comuna=="Algarrobo" ~ 5,
    comuna=="Alto Biobío" ~ 8,
    comuna=="Alto del Carmen" ~ 3,
    comuna=="Alto Hospicio" ~ 1,
    comuna=="Angol" ~ 9,
    comuna=="Antofagasta" ~ 2,
    comuna=="Arauco" ~ 8,
    comuna=="Arica" ~ 15,
    comuna=="Aysén" ~ 11,
    comuna=="Buin" ~ 13,
    comuna=="Calama" ~ 2,
    comuna=="Caldera" ~ 3,
    comuna=="Calle Larga" ~ 5,
    comuna=="Cañete" ~ 8,
    comuna=="Casablanca" ~ 5,
    comuna=="Castro" ~ 10,
    comuna=="Catemu" ~ 5,
    comuna=="Cauquenes" ~ 7,
    comuna=="Cerrillos" ~ 13,
    comuna=="Cerro Navia" ~ 13,
    comuna=="Chaitén" ~ 10,
    comuna=="Chiguayante" ~ 8,
    comuna=="Chillán" ~ 16,
    comuna=="Chillán Viejo" ~ 16,
    comuna=="Cochamó" ~ 10,
    comuna=="Colbún" ~ 7,
    comuna=="Concepción" ~ 8,
    comuna=="Conchalí" ~ 13,
    comuna=="Concón" ~ 5,
    comuna=="Constitución" ~ 7,
    comuna=="Copiapó" ~ 3,
    comuna=="Coquimbo" ~ 4,
    comuna=="Coronel" ~ 8,
    comuna=="Coyhaique" ~ 11,
    comuna=="Curacaví" ~ 13,
    comuna=="Curanilahue" ~ 8,
    comuna=="Curicó" ~ 7,
    comuna=="El Bosque" ~ 13,
    comuna=="El Tabo" ~ 5,
    comuna=="Estación Central" ~ 13,
    comuna=="Frutillar" ~ 10,
    comuna=="Futrono" ~ 14,
    comuna=="Hualpén" ~ 8,
    comuna=="Huechuraba" ~ 13,
    comuna=="Independencia" ~ 13,
    comuna=="Iquique" ~ 1,
    comuna=="La Calera" ~ 5,
    comuna=="La Cisterna" ~ 13,
    comuna=="La Florida" ~ 13,
    comuna=="La Granja" ~ 13,
    comuna=="La Reina" ~ 13,
    comuna=="La Serena" ~ 4,
    comuna=="Lampa" ~ 13,
    comuna=="Las Condes" ~ 13,
    comuna=="Limache" ~ 7,
    comuna=="Limache" ~ 5,
    comuna=="Linares" ~ 7,
    comuna=="Llaillay" ~ 5,
    comuna=="Llanquihue" ~ 10,
    comuna=="Lo Barnechea" ~ 13,
    comuna=="Los Lagos" ~ 14,
    comuna=="Machali" ~ 6,
    comuna=="Macul" ~ 6,
    comuna=="Macul" ~ 13,
    comuna=="Maipú" ~ 13,
    comuna=="Maule" ~ 7,
    comuna=="Melipeuco" ~ 9,
    comuna=="Natales" ~ 12,
    comuna=="Ñuñoa" ~ 13,
    comuna=="Osorno" ~ 10,
    comuna=="Padre Hurtado" ~ 13,
    comuna=="Panguipulli" ~ 14,
    comuna=="Pedro Aguirre Cerda" ~ 13,
    comuna=="Pelarco" ~ 7,
    comuna=="Peñaflor" ~ 13,
    comuna=="Peñalolén" ~ 13,
    comuna=="Pencahue" ~ 7,
    comuna=="Penco" ~ 8,
    comuna=="Pirque" ~ 13,
    comuna=="Providencia" ~ 13,
    comuna=="Puente Alto" ~ 13,
    comuna=="Puerto Montt" ~ 10,
    comuna=="Punta Arenas" ~ 12,
    comuna=="Purranque" ~ 10,
    comuna=="Quilicura" ~ 13,
    comuna=="Quillota" ~ 5,
    comuna=="Quilpué" ~ 5,
    comuna=="Quinta Normal" ~ 13,
    comuna=="Rancagua" ~ 6,
    comuna=="Recoleta" ~ 13,
    comuna=="Renca" ~ 13,
    comuna=="Rengo" ~ 6,
    comuna=="Rinconada" ~ 5,
    comuna=="Saavedra" ~ 9,
    comuna=="Salamanca" ~ 4,
    comuna=="San Antonio" ~ 5,
    comuna=="San Bernardo" ~ 13,
    comuna=="San Carlos" ~ 16,
    comuna=="San Clemente" ~ 7,
    comuna=="San Esteban" ~ 5,
    comuna=="San Felipe" ~ 5,
    comuna=="San Joaquín" ~ 13,
    comuna=="San Miguel" ~ 13,
    comuna=="San Pedro de la Paz" ~ 8,
    comuna=="San Ramón" ~ 13,
    comuna=="Santiago" ~ 13,
    comuna=="Talagante" ~ 13,
    comuna=="Talca" ~ 7,
    comuna=="Talcahuano" ~ 8,
    comuna=="Temuco" ~ 9,
    comuna=="Teno" ~ 7,
    comuna=="Tirúa" ~ 8,
    comuna=="Tomé" ~ 8,
    comuna=="Valdivia" ~ 14,
    comuna=="Valparaíso" ~ 5,
    comuna=="Vicuña" ~ 4,
    comuna=="Vilcún" ~ 9,
    comuna=="Villa Alegre" ~ 7,
    comuna=="Villa Alemana" ~ 5,
    comuna=="Viña del Mar" ~ 5,
    comuna=="Vitacura" ~ 13,
    comuna=="Yerbas Buenas" ~ 7
  ))
table(data$region,exclude = F)

data$region <- factor(data$region,
                      levels = c(1:16),
                      labels = c("Region de Tarapacá","Region de Antofagasta","Region de Atacama",
                                 "Region de Coquimbo","Region de Valparaíso","Region del Libertador General Bernardo O'higgins",
                                 "Region del Maule","Region del Biobio","Region de La Araucania", "Region de Los Lagos",
                                 "Region del General Carlos Ibañez del Campo","Region de Magallanes y la Antartica Chilena","Region Metropolitana de Santiago",
                                 "Region de Los Rios", "Region de Arica y Parinacota", "Region de Ñuble"))
table(data$region, exclude = F)

view_df(data)
codebk <- codebook_table(data)
library(xlsx)

