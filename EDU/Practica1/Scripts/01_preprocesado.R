# Carga de datos ----------------------------------------------------------

# Cargamos paquetes



# Librerías ---------------------------------------------------------------

library(dplyr)
library(data.table)
library(readxl)
library(esquisse)
library(plotly)
library(ggplot2)

options(scipen = 999)

# Cargar datos ------------------------------------------------------------

datos<-fread(input = "Data/CNA2014_ENCABEZADO_15.csv",
             sep = ";") %>% 
  select(COD_VEREDA, TIPO_UC, S05_TENENCIA, P_S5PAUTOS, P_S7P82, P_S7P84F, P_S7P85B) %>% 
  filter(TIPO_UC == 1) %>%
  mutate(S05_TENENCIA =as.character(S05_TENENCIA))

str(datos)
glimpse(datos)
# Limpieza ----------------------------------------------------------------

t_homologacion_7 <- readxl::read_excel(
  path ="Data/Tablasdehomologacion.xlsx",
  sheet = "Hoja2") %>% 
  mutate(S05_TENENCIA =as.character(S05_TENENCIA)) #esta linea es como una "confirmación" de que queremos que convierta las tablas a tipo string
str(t_homologacion_7)

datos_dep <- datos %>% 
  left_join(t_homologacion_7, by = c("S05_TENENCIA" = "S05_TENENCIA")) %>% 
  select(Predominancia, P_S7P85B) %>% 
  na.omit()

str(datos_dep)
#la c actua como un vector que conecta las dos tablas. En este caso tienen el mismo nombre
#Aquí se podría colocar otro left_join() y hacer otra homologación de tablas. 
#El otro metodo se llama innerjoin y sirve para hacer la INTERSECCIÓN de las dos tablas

# TDF Cualitativa ---------------------------------------------------------

#Es decir: Tabla de Distribución de frecuencias para la viariable cualitativa

tdf_S05_TENENCIA <- datos_dep %>% #recordar que datos_dep son los "datos limpios"
  group_by(Predominancia) %>% #Predominancia es la segunda parte de la tabla de homologación
  summarise(n_i= n()) %>%  #con esto se sacan las frecuencias absolutas
  arrange(desc(n_i)) %>% #arrange sirve para organizar según una variable. desc() es para hacerlo de forma descendente
  mutate(N_i = cumsum(n_i),
         f_i = n_i/sum(n_i),
         F_i = cumsum(f_i))


# Gráfico -----------------------------------------------------------------

barplot(table(datos_dep$Predominancia)) #con el signo $ se puede acceder a las columnas del conjunto de datos  

#install.packages("esquisse") #esto solo se debe ejecutar una vez (ya se hizo)
#library(esquisse)
#esquisse::esquisser(viewer = "browser") #Esto solo es cuando se quiere abrir la interfaz para obtener el código

ggplot(datos_dep) +
  aes(x = Predominancia) +
  geom_bar(fill = "#112446") +
  labs(title = "Distribución CNA") +
  theme_minimal()

#install.packages("plotly")


# TDF variable cuantitativa -----------------------------------------------

# Leer paquete DT, Data table. 

# Numero de clases

k = round(1 + 3.3 * log10(nrow(datos_dep)))
k

# Rango
rango = max(datos$P_S7P85B, na.rm = T) - 
  min(datos_dep$P_S7P85B, na.rm = T) # el metodo na.rm =T sirve para 
rango

# Longitud (es decir, que tan ancha es cada clase)
longitud = rango/k
longitud

# Cortes

cortes <- min(datos$P_S7P85B, na.rm = T) + c(seq(0, k, 1))  * longitud
# c(seq(0, k, 1)) indica que en el vector, se hará una secuencia que parta de 0 y llegue hasta k, avanzando de a 1.
cortes


# TDF - Leche -------------------------------------------------------------

tdf_P_S7P85B <- datos_dep %>%  
  mutate(P_S7P85B_c = as.factor(cut(P_S7P85B, #el _c solo es una forma de diferenciar esta variable categorica de la variable numerica original
                          breaks = cortes,
                          levels = cortes,
                          include.lowest = T,
                          dig.lab = 6)
                          )
         ) %>% 
  group_by(P_S7P85B_c, .drop = F, .add = F) %>% 
  summarise(n_i = n()) %>% 
  mutate(N_i = cumsum(n_i),
         f_i = n_i/sum(n_i),
         F_i = cumsum(f_i),
         x_i = cortes[1:k] + longitud/2, #x_i es la amplitud
         c_i = abs(cortes[1:k] - cortes[2:(k+1)]),
         d_i = n_i/c_i) 
# :) Muito bemm

# Histograma
hist(datos_dep$P_S7P85B)
mean(datos_dep$P_S7P85B)
median(datos_dep$P_S7P85B)
