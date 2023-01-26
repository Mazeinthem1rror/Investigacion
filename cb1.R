##############################################
# Codebook ---------------------------------
# Author: M. Constanza Ayala (mcayala@uc.cl)
##Josefa Espinoza
# Fecha última modificación: 26 enero 2023
##############################################

#Índice
# 1. DATA PREPARATION
# 2. Carga de paquetes
# 3. Carga de BBDD
# 4. CREATION VARIABLES
# 4.1. Character to factor
# 4.2. Treatment
# 4.3 Outcome: List
# 4.4. Geographic area



# 1. DATA PREPARATION ---------------------------------------------
options(scipen=999)
rm(list=(ls()))

# 2. Carga de paquetes---------------------------------------------
pacman::p_load(haven, tidyverse, sjPlot, codebook,sjlabelled)

#library(haven) #abrir data en otros formatos
#library(tidyverse)
#library(sjPlot)
#library(codebook)
#library(memisc)
#library(sjlabelled) #para poner nombre a la variable 

# 3. Carga de BBDD------------------------------------------------
load(file = "input/data/data_list.RData")
dim(data) #751 obs, #113 variables
names(data)
view_df(data)

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

#Con paquete sjlabelled
data$trat <- set_label(data$trat, label = "Grupo de tratamiento") 


# 4.3 Outcome: List ------------------------------------------------------
data <- data %>% 
  mutate(list=case_when(
    is.na(control2)~trat1.2,
    TRUE ~ control2)) %>% 
  mutate(list=case_when(
    is.na(list)~trat2.2,
    TRUE ~ list))
table(data$list,exclude = F)

#Con paquete sjlabelled
data$list <- set_label(data$list, label = "Outcome list") 

#Borrar variables que no vamos a usar
data <- data %>% 
  dplyr::select(-c(control2, trat1.2, trat2.2,
                   control1_1:trat2.1_5))
names(data)

# 4.4. Geographic area ----------------------------------------------------

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

#Con paquete sjlabelled
data$region <- set_label(data$region, label = "Región") 

#Etiquetar variables restantes--------------------------------------------------
data$atencion1 <- set_label(data$atencion1, label = "Pregunta de atencion") 

#Grupos de conflicto------------------------------------------------------------
data$group.conflict_1<- set_label(data$group.conflict_1, 
                                  label = "Grupo conflicto Chilenos e Inmigrantes")
data$group.conflict_2<- set_label(data$group.conflict_2, 
                                  label = "Grupo conflicto Chilenos y Haitianos")
data$group.conflict_3<- set_label(data$group.conflict_3, 
                                  label = "Personas de diferente raza")
data$group.conflict_4<- set_label(data$group.conflict_4, 
                                  label = "Chilenos y Venezolanos")
data$group.conflict_5<- set_label(data$group.conflict_5, 
                                  label = "Chilenos e Indigenas")
data$group.conflict_6<- set_label(data$group.conflict_6, 
                                  label = "Chilenos y Peruanos")
#Group Consciousness: Commonality-----------------------------------------------
data$commonality_1<- set_label(data$commonality_1, 
                                  label = "Cercanía respecto a interes con haitianos")
data$commonality_2<- set_label(data$commonality_2, 
                               label = "Cercanía respecto a interes con chilenos")
data$commonality_3<- set_label(data$commonality_3, 
                               label = "Cercanía respecto a interes con pueblos indigenas")
data$commonality_4<- set_label(data$commonality_4, 
                               label = "Cercanía respecto a interes con peruanos")
data$commonality_5<- set_label(data$commonality_5, 
                               label = "Cercanía respecto a interes con venezolanos")
data$commonality_6<- set_label(data$commonality_6, 
                               label = "Cercanía respecto a interes con inmigrantes")
data$commonality_7<- set_label(data$commonality_7, 
                               label = "Cercanía respecto a interes con mapuches")

#Group Consciousness: Collective action-----------------------------------------
data$collective.action<- set_label(data$collective.action, 
                               label = "Pregunta de atencion")

#Group Consciousness: Discriminacion percibida----------------------------------
data$perc.discrim_1<- set_label(data$perc.discrim_1, 
                               label = "Discriminacion percibida frente a peruanos")
data$perc.discrim_2<- set_label(data$perc.discrim_2, 
                                label = "Discriminacion percibida frente a chilenos")
data$perc.discrim_3<- set_label(data$perc.discrim_3, 
                                label = "Discriminacion percibida frente a venezolanos")
data$perc.discrim_4<- set_label(data$perc.discrim_4, 
                                label = "Discriminacion percibida frente a pueblos indigenas")
data$perc.discrim_5<- set_label(data$perc.discrim_5, 
                                label = "Discriminacion percibida frente a mapuches")
data$perc.discrim_6<- set_label(data$perc.discrim_6, 
                                label = "Discriminacion percibida frente a inmigrantes")
data$perc.discrim_7<- set_label(data$perc.discrim_7, 
                                label = "Discriminacion percibida frente a haitianos")

#Linked fate--------------------------------------------------------------------
data$lfate1<- set_label(data$lfate1, 
                                label = "vinculacion entre la discriminacion 
                        inmigrante y su vida")

data$lfate2<- set_label(data$lfate2, 
                        label = "vinculacion entre la discriminacion de minorias
                        etnicas y su vida")
#Group Affect-------------------------------------------------------------------
data$group.affect_1<- set_label(data$group.affect_1, 
                        label = "Sentimientos en escala muy bueno a muy malo 
                        respecto a venezolanos")
data$group.affect_2<- set_label(data$group.affect_2, 
                                label = "Sentimientos en escala muy bueno a muy malo 
                        respecto a mapuches")
data$group.affect_3<- set_label(data$group.affect_3, 
                                label = "Sentimientos en escala muy bueno a muy malo 
                        respecto a haitianos")
data$group.affect_4<- set_label(data$group.affect_4, 
                                label = "Sentimientos en escala muy bueno a muy malo 
                        respecto a peruanos")
data$group.affect_5<- set_label(data$group.affect_5, 
                                label = "Sentimientos en escala muy bueno a muy malo 
                        respecto a inmigrantes")
data$group.affect_6<- set_label(data$group.affect_6, 
                                label = "Sentimientos en escala muy bueno a muy malo 
                        respecto a chilenos")
data$group.affect_7<- set_label(data$group.affect_7, 
                                label = "Sentimientos en escala muy bueno a muy malo 
                        respecto a pueblos indigenas")
#Preguntas de atencion---------------------------------------------------------
data$atencion2_1<- set_label(data$atencion2_1, 
                                label = "Atencion color favorito")
data$atencion2_2<- set_label(data$atencion2_2, 
                             label = "Atencion color favorito")
data$atencion2_3<- set_label(data$atencion2_3, 
                             label = "Atencion Color favorito")
data$atencion2_4<- set_label(data$atencion2_4, 
                             label = "Atencion Color favorito")
data$atencion2_5<- set_label(data$atencion2_5, 
                             label = "Atencion Color favorito")
data$atencion2_6<- set_label(data$atencion2_6, 
                             label = "Atencion Color favorito")
#Caracteristicas en educación A-------------------------------------------------
data$niveduc.colegio_1<- set_label(data$niveduc.colegio_1, 
                             label = "Nivel educativo pre-básica")
data$niveduc.colegio_2<- set_label(data$niveduc.colegio_2, 
                                   label = "Nivel Educación básica")
data$niveduc.colegio_3<- set_label(data$niveduc.colegio_3, 
                                   label = "Nivel Educación media
                                   humanista-científica")
data$niveduc.colegio_4<- set_label(data$niveduc.colegio_4, 
                                   label = "Nivel Educación media 
                                   técnico-profesional")
data$niveduc.colegio_5<- set_label(data$niveduc.colegio_5, 
                                   label = "Otro nivel educativo")
data$otroniveduc<- set_label(data$otroniveduc, 
                                   label = "¿En cuál otro nivel educativo enseña?")
#Asignaturas
data$asignatura_1<- set_label(data$asignatura_1, 
                             label = "Lenguaje y Comunicación")
data$asignatura_2<- set_label(data$asignatura_2, 
                              label = "Matematicas")
data$asignatura_3<- set_label(data$asignatura_3, 
                              label = "Historia, geografia y ciencias sociales")
data$asignatura_4<- set_label(data$asignatura_4, 
                              label = "Biología")
data$asignatura_5<- set_label(data$asignatura_5, 
                              label = "Física")
data$asignatura_6<- set_label(data$asignatura_6, 
                              label = "Quimica")
data$otraasignatura<- set_label(data$otraasignatura, 
                              label = "Otra asignatura")

#Caracteristicas en educacion B------------------------------------------------
data$col.comuna<- set_label(data$col.comuna, 
                              label = "Comuna en la que se encuentra el colegio")

data$atencion2_4<- set_label(data$atencion2_4, 
                             label = "Atencion dependencia del colegio")

data$col.dep<- set_label(data$col.dep, 
                             label = "¿Qué tipo de dependencia tiene este colegio?")

data$anioscolegio<- set_label(data$anioscolegio, 
                         label = "¿Cuántos años lleva trabajando en este colegio?")

data$aniossistema<- set_label(data$aniossistema, 
                              label = "Trayectoria profesional")

#Caracteristicas en educacion C-------------------------------------------------

data$contrato<- set_label(data$contrato, 
                              label = "Tipo de contrato")

data$otrocontacto<- set_label(data$otrocontacto, 
                              label = "Otro tipo de contrato")

data$jornada<- set_label(data$jornada, 
                              label = "Jornada laboral")

#Características en educación D-------------------------------------------------
data$porc.ind<- set_label(data$porc.ind, 
                             label = "Porcentaje de estudiantes indígenas en el curso")
data$porc.mig<- set_label(data$porc.mig, 
                                   label = "Porcentaje de estudiante migrantes 
                                   en el curso")

#Caracteristica en educacion E--------------------------------------------------
data$nac.est_1<- set_label(data$nac.est_1, 
                                   label = "Tiene estudiantes venezolanos")
data$nac.est_2<- set_label(data$nac.est_2, 
                           label = "Tiene estudiantes peruanos")
data$nac.est_3<- set_label(data$nac.est_3, 
                           label = "Tiene estudiantes haitianos")
data$nac.est_4<- set_label(data$nac.est_4, 
                           label = "Tiene estudiantes de pueblos indigenas")
data$nac.est_5<- set_label(data$nac.est_5, 
                           label = "Tiene estudiantes mapuches")
#Caracteristicas en educacion F-------------------------------------------------
data$exp1_1<- set_label(data$exp1_1, label = "Mis estudiantes terminaran la 
                        educación media")
data$exp1_2<- set_label(data$exp1_2, label = "Mis estudiantes tendrán un buen 
                      desempeño en su futura trayectoria")
data$exp1_3<- set_label(data$exp1_3, label = "Mis estudiantes tienen competencias
                        para entender los objetivos del curso")
data$exp1_4<- set_label(data$exp1_4, label = "Mis estudiantes tienen lo necesario
                        personal o socialmente, para completar sus estudios")
data$exp1_5<- set_label(data$exp1_5, label = "Mis estudiantes encontrarán el
                        camino de su vida")
data$exp1_6<- set_label(data$exp1_6, label = "guerras mundiales")

#Caracteristicas en educacion G-------------------------------------------------
data$exp2<- set_label(data$exp2,label = "Cual cree que será el nivel educacional
                             mas alto que completaran sus estudiantes")

#Preguntas de indentificacion finales A-----------------------------------------
data$religion<- set_label(data$religion, label = "Religion a la que pertenece o acerca")
data$educ.profes<- set_label(data$educ.profes, label = "Mayor nivel educacional alcanzado")

#Identificacion final B---------------------------------------------------------
data$activ<- set_label(data$activ, label = "Actividad principal ultimo mes")
data$otroactiv<- set_label(data$otroactiv, label = "Otra actividad que realizo el ultimo mes")
data$ingresos<- set_label(data$ingresos, label = "Ingreso aprox hogar")

#Identificacion final C---------------------------------------------------------
data$indig<- set_label(data$indig, label = "Pertenencia a pueblos originarios")
data$nacion<- set_label(data$nacion, label = "Nacionalidad")
data$otranacion<- set_label(data$otranacion, label = "Otras nacionalidades")
data$politica_1<- set_label(data$politica_1, label = "Identificacion Politica")

#Preguntas directas-------------------------------------------------------------
data$direct1<- set_label(data$direct1, label = "Dificultad de aprendizaje en haitianos")
data$direct2<- set_label(data$direct2, label = "Dificultad de aprendizaje en Mapuches")

view_df(data)


#Esto pendiente
codebk <- codebook_table(data)
library(xlsx)
write.xlsx(codebk, file="Data/Codebook.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)



