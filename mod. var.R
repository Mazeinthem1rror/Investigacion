##############################################
# Codebook ---------------------------------
# Autoras: M. Constanza Ayala (mcayala@uc.cl)
# Josefa Espinoza Arcos
# Fecha última modificación: 01 Abril 2023
##############################################

#Índice
# 1. Data preparación
# 2. Carga de paquetes
# 3. Carga de BBDD
# 4. Modificacion variables


# 1. DATA PREPARATION ---------------------------------------------
options(scipen=999)
rm(list=(ls()))

# 2. Carga de paquetes---------------------------------------------
pacman::p_load(readxl, sjmisc, dplyr, tidyverse, sjPlot)

# 3. Carga BBDD----------------------------------------------------
datos_proc <- readRDS(file="input/data/datos_proc.rds")

# 4. Modificacion de variables-------------------------------------
#Grupo de conflicto 1
table(datos_proc$group.conflict_1)

datos_proc <- datos_proc %>%
  mutate_at(vars(starts_with("group.conflict_")),
            ~case_when(.== "Muy fuerte" ~ 1, .== "Fuerte" ~ 2, 
                       .== "Débil" ~ 3, .== "Muy débil" ~ 4, TRUE~NA_real_,))

table(datos_proc$group.conflict_1)

datos_proc$group.conflict_1 <-factor(datos_proc$group.conflict_1,
                                     levels = c (1:4),
                                     labels = c("Muy fuerte","Fuerte","Débil","Muy débil"))
datos_proc$group.conflict_2 <-factor(datos_proc$group.conflict_2,
                                     levels = c (1:4),
                                     labels = c("Muy fuerte","Fuerte","Débil","Muy débil"))
datos_proc$group.conflict_3 <-factor(datos_proc$group.conflict_3,
                                     levels = c (1:4),
                                     labels = c("Muy fuerte","Fuerte","Débil","Muy débil"))
datos_proc$group.conflict_4 <-factor(datos_proc$group.conflict_4,
                                     levels = c (1:4),
                                     labels = c("Muy fuerte","Fuerte","Débil","Muy débil"))
datos_proc$group.conflict_5 <-factor(datos_proc$group.conflict_5,
                                     levels = c (1:4),
                                     labels = c("Muy fuerte","Fuerte","Débil","Muy débil"))  
datos_proc$group.conflict_6 <-factor(datos_proc$group.conflict_6,
                                     levels = c (1:4),
                                     labels = c("Muy fuerte","Fuerte","Débil","Muy débil")) 
#Comunalidad-------------------------------------------------------------------------------
datos_proc <- datos_proc %>%
  mutate_at(vars(starts_with("commonality_")),
            ~case_when(.== "Muy lejos" ~ 1, .== "Lejos" ~ 2, 
                       .== "Cerca" ~ 3, .== "Muy cerca" ~ 4, TRUE~NA_real_,))
table(datos_proc$commonality_1)
datos_proc$commonality_1 <-factor(datos_proc$commonality_1,
                                     levels = c (1:4),
                                     labels = c("Muy lejos","Lejos","Cerca","Muy cerca")) 
datos_proc$commonality_2 <-factor(datos_proc$commonality_2,
                                  levels = c (1:4),
                                  labels = c("Muy lejos","Lejos","Cerca","Muy cerca"))
datos_proc$commonality_3 <-factor(datos_proc$commonality_3,
                                  levels = c (1:4),
                                  labels = c("Muy lejos","Lejos","Cerca","Muy cerca")) 
datos_proc$commonality_4 <-factor(datos_proc$commonality_4,
                                  levels = c (1:4),
                                  labels = c("Muy lejos","Lejos","Cerca","Muy cerca"))
datos_proc$commonality_5 <-factor(datos_proc$commonality_5,
                                  levels = c (1:4),
                                  labels = c("Muy lejos","Lejos","Cerca","Muy cerca")) 
datos_proc$commonality_6 <-factor(datos_proc$commonality_6,
                                  levels = c (1:4),
                                  labels = c("Muy lejos","Lejos","Cerca","Muy cerca")) 
datos_proc$commonality_7 <-factor(datos_proc$commonality_7,
                                  levels = c (1:4),
                                  labels = c("Muy lejos","Lejos","Cerca","Muy cerca")) 
table(datos_proc$commonality_7)

#Percpecion de discriminación-----------------------------------------------------------
datos_proc <- datos_proc %>%
  mutate_at(vars(starts_with("perc.discrim_")),
            ~case_when(.== "Mucha" ~ 1, .== "Algo" ~ 2, 
                       .== "Bastante" ~ 3, .== "Nada" ~ 4, TRUE~NA_real_,))
table(datos_proc$perc.discrim_1)

datos_proc$perc.discrim_1 <-factor(datos_proc$perc.discrim_1,
                                  levels = c (1:4),
                                  labels = c("Mucha","Algo","Bastante","Nada")) 
table(datos_proc$perc.discrim_1)

datos_proc$perc.discrim_2 <-factor(datos_proc$perc.discrim_2,
                                   levels = c (1:4),
                                   labels = c("Mucha","Algo","Bastante","Nada")) 
table(datos_proc$perc.discrim_2)

datos_proc$perc.discrim_3 <-factor(datos_proc$perc.discrim_3,
                                   levels = c (1:4),
                                   labels = c("Mucha","Algo","Bastante","Nada")) 
table(datos_proc$perc.discrim_3)

datos_proc$perc.discrim_4 <-factor(datos_proc$perc.discrim_4,
                                   levels = c (1:4),
                                   labels = c("Mucha","Algo","Bastante","Nada")) 
table(datos_proc$perc.discrim_4)

datos_proc$perc.discrim_5 <-factor(datos_proc$perc.discrim_5,
                                   levels = c (1:4),
                                   labels = c("Mucha","Algo","Bastante","Nada")) 
table(datos_proc$perc.discrim_5)

datos_proc$perc.discrim_6 <-factor(datos_proc$perc.discrim_6,
                                   levels = c (1:4),
                                   labels = c("Mucha","Algo","Bastante","Nada")) 
table(datos_proc$perc.discrim_6)

datos_proc$perc.discrim_7 <-factor(datos_proc$perc.discrim_7,
                                   levels = c (1:4),
                                   labels = c("Mucha","Algo","Bastante","Nada")) 
table(datos_proc$perc.discrim_7)

#Vinculo con la discriminacion
datos_proc <- datos_proc %>%
  mutate_at(vars(starts_with("lfate1")),
            ~case_when(.== "Mucho" ~ 1, .== "Bastante" ~ 2, 
                       .== "Poco" ~ 3, .== "Nada" ~ 4, TRUE~NA_real_,))
table(datos_proc$lfate1)

datos_proc$lfate1 <-factor(datos_proc$lfate1,
                                   levels = c (1:4),
                                   labels = c("Mucho","Bastante","Poco","Nada")) 
table(datos_proc$lfate1)

datos_proc <- datos_proc %>%
  mutate_at(vars(starts_with("lfate2")),
            ~case_when(.== "Mucho" ~ 1, .== "Bastante" ~ 2, 
                       .== "Poco" ~ 3, .== "Nada" ~ 4, TRUE~NA_real_,))
table(datos_proc$lfate2)


