# =============================================================================#
# Fecha: 2022-12-07 
# Código de la charla
# {paquetes demogRáficos}
# Ana Escoto 
# =============================================================================#


# Paquetes                        ----------------------------------------------
if (!require("pacman")) install.packages("pacman")#instala pacman si se requiere

pacman::p_load(tidyverse, readxl, magrittr, # paquetes de uso
               wppExplorer, wpp2019,
               WDI, geodata, 
               inegiR, apyramid,
               fmsb)

# Se secciona este código de acuerdo a las diapositivas

# {wppExplorer} ----  

## {wppExplorer} (I)  ----
  
# Voy poniendo library para que se sepa qué paquetes aunque los cargamos de la 
# línea 10 en adelante
library(tidyverse)
library(wppExplorer)

#Ejemplo
tfr <- wpp.indicator("fert") # pedir el indicador

tfr %>% 
  dplyr::glimpse()



## {wppExplorer} (II) ----

# Fitro de país

wpp.by.country(tfr,  'MX') %>% head()

## {wppExplorer} (III) ----

# Fitro de año

wpp.by.year(tfr,  '2010') %>% head()

# {WDI} ----

## {WDI} (I) ----

library(WDI)
WDIsearch('gender') %>% head()


## {WDI} (II)  ----

WDI(indicator='SG.VAW.REFU.ZS',
    country="all", 
    start=1960, 
    end=2020) %>%
  na.omit() %>% 
  head()

# {geodata} ----
## {geodata} (I) ----

# The {WorldClim} database

library(geodata)
tmax_data <- worldclim_global(var = "tmax", 
                              res = 10,
                              path= getwd())

tmax_data

## {geodata} (II) ----

# ¿Qué tanto calor hace en diciembre?
  

library(ggplot2)

# Converting the raster object into a dataframe
tmax_data_may_df <- as.data.frame(tmax_data$wc2.1_10m_tmax_12, xy = TRUE, na.rm = TRUE)
rownames(tmax_data_may_df) <- c()

ggplot(
  data = tmax_data_may_df,
  aes(x = x, y = y)
) +
  geom_raster(aes(fill = wc2.1_10m_tmax_12)) +
  labs(
    title = "Temperaturas máximas en diciembre",
    subtitle = "En 1970-2000"
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_gradientn(
    name = "Temperature (°C)",
    colours = c("#0094D1", "#68C1E6", "#FEED99", "#AF3301"),
    breaks = c(-20, 0, 20, 40)
  )


## {geodata} (III) ----

# También se puede descargar la "Gridded Population of the World"
# (Una base para densidad poblacional de CIESIN)


pop <- population(2020, res=10, path=getwd())
pop

# Nos bajo un tiff con la información, revisa tu directorio de trabajo

# {inegiR} ----
## {inegiR} + API INEGI (I) ----

# https://www.inegi.org.mx/app/desarrolladores/generatoken/Usuarios/token_Verify
token<-"aqui va tu token" 

# El paquete inegiR permite revisar los datos del BIE

library(inegiR)
inegi_series(serie=628194,# este número está en el sitio este el IPC
                     token = token) %>% 
  head()


## {inegiR} + API INEGI (II)

# Guardamos esto como un objeto


# Podemos guardar esta consulta en un objeto y luego graficar

consulta1<-inegi_series(serie=628194, # IPC mensual
                        token = token)

str(consulta1)



## {inegiR} + API INEGI (III)

# Una fácil gráfica

consulta1 %>% 
  ggplot()+
  aes(x=as.Date(date),
      y=values) +
  geom_line()


# {apyramid} ----
## {apyramid} (I)  ----

# Una pirámide muy sencilla, primero descargo mi población, pero usando directamente wpp2019

library(wpp2019)
data(popF) 
data(popM) 


# Me salto un par de pasos hasta tener una base para méxico

popF %<>% 
  pivot_longer(cols = `1950`:`2020`,
               names_to = "anio",
               values_to = "mujeres") %>% 
  filter(name=="Mexico") %>% 
  filter(anio=="2020")

popM %<>% 
  pivot_longer(cols = `1950`:`2020`,
               names_to = "anio",
               values_to = "hombres") %>% 
  filter(name=="Mexico") %>% 
  filter(anio=="2020")


popmex2020<-merge(popM,popF, by=names(popF[,-5]))

popmex2020 %<>% 
  pivot_longer(cols = c(hombres, mujeres),
               names_to = "sex",
               values_to = "poblacion") 

popmex2020 %<>% 
  mutate(edad=parse_number(age)) %>% 
  mutate(edad_factor=as.factor(edad))


popmex2020 # en formato long


## {apyramid} (II) ----

library(apyramid)

popmex2020 %>% 
  age_pyramid(edad_factor, # edad
              split_by = sex,
              count=poblacion) # sexo



## {apyramid} (III) ----

# Una ventaja es que es compatible con {ggplot2} 


popmex2020 %>% 
  age_pyramid(edad_factor, # edad
              split_by = sex,
              count=poblacion) + # sexo
  labs(title="Pirámide de Población",
       subtitle = "México, 2020",
       y ="Miles de personas", # ojo
       x = "Edad", # ojo
       caption = "Fuente: World Population Prospects, 2019" )


# LexisPlotR ----
## LexisPlotR (I) ----

# Una herramienta muy útil son los diagramas de Lexis

library(LexisPlotR)
# Dibuje una cuadrícula de Lexis desde el año 2010 hasta el año 2015, 
# que representa las edades de 0 a 5 años.

lexis_grid(year_start = 2010, 
           year_end = 2015, 
           age_start = 0, 
           age_end = 5)


## LexisPlotR(II) ----


lexis_grid(year_start = 2010, 
           year_end = 2015, 
           age_start = 0, 
           age_end = 5) %>% 
  lexis_age(age = 2)



## LexisPlotR (III) ----

lexis_grid(year_start = 2010, 
           year_end = 2015, 
           age_start = 0, 
           age_end = 5) %>% 
  lexis_age(age = 2, 
            fill = "red")


## LexisPlotR (IV) ----

lexis_grid(year_start = 2010, 
           year_end = 2015, 
           age_start = 0, 
           age_end = 5) %>% 
  lexis_age(age = 2) %>% 
  lexis_cohort(cohort = 2013) %>% 
  lexis_year(year = 2011)

# fmsb ----
## {fmsb} (II)

Este paquete tiene cosas muy interesantes. Es un paquete no sólo para demografía pero permite ajustar algunas funciones demográficas

**Limitantes:** como que está en japonés :P

Un ejemplo con el índice de Whipple, que mide la atracción digital. 
Necesitamos datos en edades singulares:
  
  


readxl::read_excel("sv1992.xlsx") %>% 
  head() %>% 
  janitor::clean_names()# datos de ipums




sv1992<-readxl::read_excel("sv1992.xlsx") %>% 
  janitor::clean_names() %>% # datos de ipums
  mutate(age=as.numeric(age))



## {fmsb} (III)

library(fmsb)

sv1992 %>%
  filter(!age>64) %>% 
  count(age, wt=male) %>% 
  with(
    WhipplesIndex(n)
  )


## {demography} ----

# {DemoTools}  -----

## {DemoTools} (I) ----

# Es quizás el paquete más completo. Todavía no está en CRAN.
# Así que su instalación se debe hacer desde su versión en desarrollo y además intalar Stan

# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# remotes::install_github("timriffe/DemoTools")
library(DemoTools)


## {DemoTools} (II) ----

### Índices de atraccción ----

#### Whipple  ----

check_heaping_whipple(Value=sv1992$male,
                      Age= sv1992$age, 
                      ageMin = 25, 
                      ageMax = 60, 
                      digit = c(0, 5))


#### Noumbissi ----

check_heaping_noumbissi(sv1992$male, 
                        Age=sv1992$age, 
                        ageMin = 30, 
                        ageMax = 60,
                        digit = 0)


## {DemoTools} (III) ----


### Tablas de vida ----
# 
# Este paquete nos da la oportunidad de construir las tablas de vida con diferentes insumos, 
# con **cualquiera** de las siguientes opciones:
#   
# - Vector de muertes y vector de Población media
# 
# - Vector de tasas de Mortalidad (nMx)
# 
# - Vector de cocientes de mortalidad (nqx)
# 
# - Vector de efectivos a edad exacta (lx)


## {DemoTools} (IV) -----

### Tablas de vida  ----.

## Input: nMx 

# Datos de México 2000
nMx <- c(0.025429618,
         0.000895531,
         0.000364678,
         0.000480071,
         0.000979976,
         0.001661119,
         0.002167313,
         0.002549786,
         0.00307099,
         0.003970018,
         0.005461053,
         0.007799417,
         0.011317907,
         0.016516166,
         0.024145341,
         0.035168272,
         0.051143602,
         0.074042144,
         0.136811785)

# Nuestros grupos de edad 

grupo_eda<-c(0,1,seq(5,85,by=5))
AgeInt <- inferAgeIntAbr(vec = nMx)

mx_lifetable2000 <- lt_abridged(nMx = nMx, 
                                Age = grupo_eda,
                                AgeInt = AgeInt,
                                axmethod = "un",
                                Sex = "m",
                                mod = FALSE)


## {DemoTools} (V) ----

### Tablas de vida -----


round(mx_lifetable2000, digits = 2)  %>%
  head()




## {DemoTools} (VI)

### Otras funciones ----
# 
# ¡No da tiempo para todas!
#   
# Este paquete puede producir tablas abreviadas, tablas singulares y extrapolar hasta 109 años
# 
# También hay funciones de suavizamiento de información.

# {mxmaps} ----

## {mxmaps} (I) ----

### Instalación  ----
# 
# Este paquete es un paquete muy sencillo para hacer mapas, desarrollado por Diego Valle Jones.
# 
# Permite hacer mapas a nivel municipal y estatal de México.
# 
# Se instala también desde su versión en desarrollo 


#remotes::install_github("diegovalle/mxmaps")
library(mxmaps)

# Este paquete ya viene con algunos datos de población de los censos mexicanos.

## {mxmaps} (II) ----

### Nivel estatal
# 
# El primer ejemplo utiliza con los datos a nivel estatal de población. 
# Si tienes una base con la codificación de los datos en los estados de acuerdo a INEGI,
# en una variable que se llama región, no habrá problema

data("df_mexstate_2020")
df_mxstate_2020 %>% head()
df_mxstate_2020$value <- df_mxstate_2020$pop

mxstate_choropleth(df_mxstate_2020,
                   title = "Población total, por estado 2020")


## {mxmaps} (III) -----

### Nivel municipal -----
# 
# Este segundo ejemplo utiliza con los datos a nivel estatal de población. 
# Si tienes una base con la codificación de los datos en los municipio de acuerdo 
# a INEGI, en una variable que se llama región, no habrá problema


data("df_mxmunicipio_2020")
df_mxmunicipio_2020 %>% head()

df_mxmunicipio_2020$value <-  df_mxmunicipio_2020$indigenous_language / 
  df_mxmunicipio_2020$pop * 100

mxmunicipio_choropleth(df_mxmunicipio_2020, 
                       num_colors = 1,
                       title = "Porcentaje de la población hablante de lengua indígena, 2020",
                       legend = "%")



