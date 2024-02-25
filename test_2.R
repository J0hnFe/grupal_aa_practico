# Dataset grande de poblacion
library(foreign) # para leer archivos .sav
library(caTools) # para sample.split
library(factoextra) # para visualizar kmeans, fbnclust
library(cluster) # para kmeans
library(dplyr)
library(miscFuncs) # para recodeValue2Value
library(tidyr)
library(fpc) # para cluster DBSCAN

rm(list = ls()) # limpiar entorno de trabajo
# setwd("C:/Users/fing.labcom/Downloads/grupal_aa 1/grupal_aa/grupal_aa")

dataset <- read.csv2(file = "dataset_main.csv")
# Filtrar el conjunto de datos (solo guayas)
dataset_guayas <- dplyr::filter(dataset, DPA_1 == 9)

# Lista de nombres de columnas
columnas <- c(
    "ID", "SINIESTROS", "ENTE_DE_CONTROL",
    "LATITUD_Y", "LONGITUD_X", "DPA_1",
    "PROVINCIA", "CANTON", "PARROQUIA",
    "DIRECCION", "ZONA_PLANIFICACION", "ZONA",
    "ID_DE_LA_VIA", "NOMBRE_DE_LA_VIA",
    "JERARQUIA_DE_LA_VIA", "UBICACION_DE_LA_VIA",
    "FECHA", "HORA", "DIA_1", "MES_1", "FERIADO",
    "CODIGO_CAUSA", "CAUSA_PROBABLE", "TIPO_DE_SINIESTRO",
    "TIPO_DE_VEHICULO_1", "SERVICIO_1", "AUTOMOVIL",
    "BICICLETA", "BUS", "CAMION", "CAMIONETA", "EMERGENCIAS",
    "ESPECIAL", "FURGONETA", "MOTOCICLETA", "NO_IDENTIFICADO",
    "SCOOTER_ELECTRICO", "TRICIMOTO", "VEHICULO_DEPORTIVO_UTILITARIO",
    "SUMA_DE_VEHICULOS", "TIPO_ID_1", "SEXO_1", "CONDICION_1",
    "PARTICIPANTE_1", "CASCO_1", "CINTURON_1", "PERIODO_1"
)

# Bucle for para establecer cada columna a NULL
for (col in columnas) {
    dataset_guayas[[col]] <- NULL
}

# set.seed(0)
ratio <- 0.06 # dividimos la parte para test (30%)
split <- sample.split(dataset_guayas$ANIO, SplitRatio = ratio)
training_set <- subset(dataset_guayas, split == TRUE)

# guardo mi subdataset de prueba
write.csv2(training_set, file = "datos_test.csv", row.names = FALSE)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<
# Paso3
rm(list = ls()) # limpiar entorno de trabajo
accidentes <- read.csv2(file = "datos_test.csv")
df <- accidentes

# Funcion para reemplazar valores, eliminar datos erroneos o atípicos
recodeValue2Value <- function(df, value1, value2) {
    tmpDs <- ifelse(df == value1, value2, df)
    return(tmpDs)
}

df$EDAD_1 <- recodeValue2Value(df$EDAD_1, -1, 1) # Reemplazando valores negativos por 1

# Preprocesamiento
colSums(is.na(df))

row.has.na<-apply(df, 1, function(x){any(is.na(x))} )
df<-df[!row.has.na,]

df <- df %>% mutate_if(is.character, as.factor)
df <- scale(df) # escalado de los valores, saca media y desviacion (evito sesgo)

# Grafico del codo
fviz_nbclust(df, kmeans, method = "wss") #4

#Metodo de la brecha 
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 10)
fviz_gap_stat(gap_stat, linecolor = "blue")

# fviz_nbclust(df, kmeans, method = "wss") + geom_vline(xintercept = 8, linetype = 2) + labs(subtitle = "Elbow method")

# Dendograma
res.dist <- dist(df, method = "euclidean")
res.hc <- hclust(d = res.dist, method = "ward.D2")

fviz_dend(res.hc, 
          k = 4, 
          cex = 0.75,
          lwd = 1,
          k_colors = c("purple", "blue", "darkgreen", "orange"), 
          color_labels_by_k = TRUE,
          rect = TRUE)


# Cluster kmeans
km.res <- kmeans(df, 4, nstart = 25)  # ejecuto kmeans en el dataset con 5 clusters # nolint
print(km.res)


# Al sacar la información me sirve para verificar que (por ejemplo) la varianza se mantiene similar # nolint
# incluso luego de la normalizacion, lo que indica que no hay sesgos en los datos # nolint

#Grafico clusters - Kmeans
fviz_cluster(
  km.res, 
  data = df, 
  ellipse.type = "euclid",
  star.plot = TRUE,
  rapel = TRUE,
  ggtheme = theme_minimal()
)

# dsafdsa

rr<-cbind(df,cluster=km.res$cluster)#el simbolo de $ es para dar el nombre a la columna y elcbind es para añadir una columna
head(rr)
fviz_cluster(
  km.res,data = rr,
  palette=c("#2E9FDF","#00AFBB","#E7B800","#FC4E07", "darkgreen"),
  ellipse.type = "euclid",
  star.plot=TRUE,
  rapel.plot=TRUE,
  ggtheme = theme_minimal()
)


