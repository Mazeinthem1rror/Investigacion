##############################################
# Codebook ---------------------------------
# Autoras: M. Constanza Ayala (mcayala@uc.cl)
# Josefa Espinoza Arcos
# Fecha última modificación: 18 Marzo 2023
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
pacman::p_load(readxl, sjmisc, dplyr, tidyverse, sjPlot, codebook)

# 3. Carga BBDD----------------------------------------------------
datos <- readxl::read_excel(path = "input/data/codebk.xlsx")

# 4. Modificacion de variables-------------------------------------
datos<- datos %>% 
  mutate_at(vars(starts_with("group.conflict_")), 
            ~case_when(.==4 ~1, .==3 ~4,
                       .==2~2,.==1~3,
                       TRUE~NA_real_,))


  
  
  
