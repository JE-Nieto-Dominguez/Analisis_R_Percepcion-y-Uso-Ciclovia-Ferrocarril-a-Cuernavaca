#CRÉDITOS -----------------------------------------------------------------------------------------

#Análisis realizado en junio del 2024 por el Lic. José Eduardo Nieto Domínguez
#https://orcid.org/0009-0003-9136-1839

#Parte de la participación del grupo "INTERSECTA" para el concurso "Mejores Calles" 2024 de ITDP,
#en colaboración con la Unidad Proletaria Pedregal San Nicolás A.C. (UPPESAN)
#https://nuevaconstituyente.org/uppesan/

#WORKINGDIRECTORY ---------------------------------------------------------------------------------

#Modificar utilizando la carpeta donde se encuentra el archivo "Percepción y uso de la ciclovía.csv"
setwd("C:/Users/PC/OneDrive/Documents/Investigación/Análisis de datos/Concurso_ITDP Mejores Calles")

#MÓDULOS A USAR -----------------------------------------------------------------------------------

#tidyverse para la mayoría de las funciones en este análisis.
#likert para el trabajo con las escalas.
#ggplots para graficar.
#showtext para ajustar la fuente de los gráficos que quiso el equipo de arquitectas.

library(tidyverse)
library(likert)
library(ggplot2)
library(showtext)


#MARCOS DE DATOS PRINCIPALES ----------------------------------------------------------------------

#NOTA: Todos los marcos de esta sección son necesarios para los análisis.

#Marco principal, tomado directamente del archivo csv con las respuestas al cuestionario.

#Con la función mutate_all, se convierten en NA todas las capturas que quedaron en blanco.
#Se usa la función mutate, junto con fct_lump_min, para reducir todas las respuestas en la 
#columna "ALCALDÍA",con una frecuencia mínima de 2, en la categoría "OTRA".
#Se hace lo mismo para la columna "COLONIA_GRAL", con una frecuencia mínima de 3.

DF_Principal <- read.csv("Percepción y Uso de la Ciclovía.csv") |>
                mutate_all(~replace(., . == '', NA))

#Marco sólo con las respuestas de las personas que SÍ usan la ciclovía.

SubDF_SÍUsaciclovía <- DF_Principal |> 
                    filter(USACICLOVÍA == "SÍ") |> 
  mutate(ALCALDÍA = fct_lump_min(ALCALDÍA, min = 2, other_level = "OTRA")) |>
  mutate(COLONIA_GRAL = fct_lump_min(COLONIA_GRAL, min = 5, other_level = "OTRA"))

#Marco sólo con las respuestas de las personas que NO usan la ciclovía.

SubDF_NOUsaciclovía <- DF_Principal |> 
                    filter(USACICLOVÍA == "NO") |>
  mutate(ALCALDÍA = fct_lump_min(ALCALDÍA, min = 2, other_level = "OTRA")) |>
  mutate(COLONIA_GRAL = fct_lump_min(COLONIA_GRAL, min = 2, other_level = "OTRA"))
  
#ANÁLISIS: DATOS DEMOGRÁFICOS GENERALES DEL CUESTIONARIO -----------------------------------------

#Se crean marcos de datos a partir de tablas de los datos demográficos de quienes sí usan la ciclovía,
#y de quienes no la utilizan.

#Género: Sí usan ciclovía

Tabla_SCGénero <- table(SubDF_SÍUsaciclovía$GÉNERO) |> 
  as.data.frame() |> 
  rename("PERSONAS" = Freq, 
         "GÉNERO" = Var1) |> 
  filter_if(is.numeric, all_vars((.) != 0))

#Género: NO usan ciclovía

Tabla_NCGénero <- table(SubDF_NOUsaciclovía$GÉNERO) |> 
  as.data.frame() |> 
  rename("PERSONAS" = Freq, 
         "GÉNERO" = Var1) |> 
  filter_if(is.numeric, all_vars((.) != 0))

#Ocupación: Sí usan ciclovía

Tabla_SCOcupación <- table(SubDF_SÍUsaciclovía$OCUPACIÓN_GRAL) |> 
  as.data.frame() |> 
  rename("PERSONAS" = Freq, 
         "OCUPACIÓN" = Var1) |> 
  filter_if(is.numeric, all_vars((.) != 0))

#Ocupación: NO usan ciclovía

Tabla_NCOcupación <- table(SubDF_NOUsaciclovía$OCUPACIÓN_GRAL) |> 
  as.data.frame() |> 
  rename("PERSONAS" = Freq, 
         "OCUPACIÓN" = Var1) |> 
  filter_if(is.numeric, all_vars((.) != 0))

#Alcaldía: Sí usan ciclovía

Tabla_SCAlcaldía <- table(SubDF_SÍUsaciclovía$ALCALDÍA) |> 
  as.data.frame() |> 
  rename("PERSONAS" = Freq, 
         "ALCALDÍA" = Var1) |> 
  filter_if(is.numeric, all_vars((.) != 0))

#Alcaldía: NO usan ciclovía

Tabla_NCAlcaldía <- table(SubDF_NOUsaciclovía$ALCALDÍA) |> 
  as.data.frame() |> 
  rename("PERSONAS" = Freq, 
         "ALCALDÍA" = Var1) |> 
  filter_if(is.numeric, all_vars((.) != 0))

#Colonia: SÍ usan ciclovía

Tabla_SCColonia <- table(SubDF_SÍUsaciclovía$COLONIA_GRAL) |> 
  as.data.frame() |> 
  rename("PERSONAS" = Freq, 
         "COLONIA" = Var1) |> 
  filter_if(is.numeric, all_vars((.) != 0))


#Colonia: NO usan ciclovía

Tabla_NCColonia <- table(SubDF_NOUsaciclovía$COLONIA_GRAL) |> 
  as.data.frame() |> 
  rename("PERSONAS" = Freq, 
         "COLONIA" = Var1) |> 
  filter_if(is.numeric, all_vars((.) != 0))

#ANÁLISIS: MODOS DE TRASLADO POR LA CICLOVÍA -----------------------------------------------------

#Creación de un marco de datos, que denomino "intermedio", a partir de la información de quienes
#respondieron SÍ usar la ciclovía. Lo señalo con el prefijo "PROCESO".

#Con la función select, se trabaja únicamente con las columnas PEATÓN_USO, CICLISTA_USO y
#MOTOCICLISTA_USO, del marco de datos de quienes SÍ usan la ciclovía.
#Después, con la función pivot_longer, se reagrupa la información de uso en una columna,
#llamada USO_TRANSPORTE (con valores "SÍ" y "NO"), contextualizada por la columna TIPO_TRANSPORTE.
#Posteriormente, con la función filter, se conservan sólo las observaciones que tienen un "SÍ"
#en la columna USO_TRANSPORTE.
#Por último, con la función select, se guardan las columnas ID y TIPO_TRANSPORTE.

PROCESO_MDT <- SubDF_SÍUsaciclovía |> 
               select(ID, PEATÓN_USO, CICLISTA_USO, MOTOCICLISTA_USO)|>
               pivot_longer(cols = c("PEATÓN_USO", "CICLISTA_USO", "MOTOCICLISTA_USO"),
                            names_to = "TIPO_TRANSPORTE",
                            values_to = "USO_TRANSPORTE")  |> 
               filter(USO_TRANSPORTE == "SÍ") |> 
               select(ID, TIPO_TRANSPORTE)

#Marco de datos final, creado a partir del marco de datos "intermedio".

#Se crea, y se guarda como marco de datos, con la función as.data.frame, una tabla con las
#frecuencias de los valores de la columna TIPO_TRANSPORTE.
#Después, con la función rename, se renombran las columnas del marco de datos recién creado.
#La columna Freq pasa a ser USO_TRASLADO, y la columna Var1 pasa a ser MODO_TRASLADO.
#Por último, con la función mutate y un conjunto de ifelse, se cambian los valores de la columna
#MODO_TRASLADO. CICLISTA_USO pasa a ser BICICLETA, MOTOCICLISTA_USO pasa a ser MOTOCICLETA,
#y PEATÓN_USO pasa a ser A PIE.

DF_ModosDeTraslado <- as.data.frame(table(PROCESO_MDT$TIPO_TRANSPORTE)) |> 
                      rename("USO_TRASLADO" = Freq, 
                             "MODO_TRASLADO" = Var1)|> 
                      mutate(MODO_TRASLADO = ifelse(MODO_TRASLADO == "CICLISTA_USO", "BICICLETA",
                                  ifelse(MODO_TRASLADO == "MOTOCICLISTA_USO", "MOTOCICLETA",
                                     ifelse(MODO_TRASLADO == "PEATÓN_USO", "A PIE", 
                                            MODO_TRASLADO))))

#ANÁLISIS: ESCALAS DE LIKERT ---------------------------------------------------------------------

#Preparar el marco de datos que contenga sólo la información relevante para las Escalas de Likert, 
#a partir de DF_Principal.

#Con la función select se conservan las columnas correspondientes a las escalas de Likert, los ID
#para identificar cada cuestionario y la columna USACICLOVÍA para agrupar las respuestas al graficar.
#Después, usando mutate_all, se reemplazan las respuestas textuales por sus equivalentes numéricos
#en una escala. ESTE PASO ES IMPORTANTE PARA EL CORRECTO FUNCIONAMIENTO DEL PAQUETE LIKERT.
#Como no se pueden dejar respuestas en blanco, se reemplazan todos los NA por 3 (el equivalente a
#una posición neutral en la escala).

DF_Likert <- DF_Principal |> 
             select("ID", "USACICLOVÍA",
                    "LIKERT_CRUZAR.SIN.RIESGO.DE.SER.ATROPELLADO", "LIKERT_ME.SIENTO.SEGURO", "LIKERT_BIEN.ILUMINADA",
                    "LIKERT_PARADAS.DE.TRANSPORTE.ACCESIBLES", "LIKERT_SEÑALIZACIONES.CLARAS",
                    "LIKERT_SUPERFICIE.UNIFORME", "LIKERT_ZONAS.DE.DESCANSO", "LIKERT_ANCHO.ADECUADO") |>
             mutate_all(~replace(., . == 'MUY EN DESACUERDO', 1)) |>
             mutate_all(~replace(., . == 'EN DESACUERDO', 2)) |>
             mutate_all(~replace(., . == 'NEUTRAL', 3)) |>
             mutate_all(~replace(., is.na(.), 3)) |>
             mutate_all(~replace(., . == 'DE ACUERDO', 4)) |>
             mutate_all(~replace(., . == 'MUY DE ACUERDO', 5)) |>
             mutate_all(~replace(., . == 'SÍ', 'SÍ LA USA')) |>
             mutate_all(~replace(., . == 'NO', 'NO LA USA'))

#Convertir las columnas correspondientes a las escalas de Likert en factores ordenados.
#SI ESTO NO SE HACE, NO FUNCIONARÁ EL PAQUETE LIKERT.
#Se aplica el método Split-Apply-Combine.

#Split: Crear un marco de datos a partir de DF_Likert sólo con las columnas de preguntas.
#Para que el paquete Likert funcione, Es necesario guardar esto en un objeto llamado items. 
items <- as.data.frame(DF_Likert[, 3:10])

#Apply: #Crear un vector con las opciones de respuesta ordenadas.
opciones = c("Muy en desacuerdo", "En desacuerdo", "Neutral", "De acuerdo", "Muy de acuerdo")

#Usar una función que reemplaza los números en las columnas de items por cada opción de respuesta
#y los guarda como factores.
for(i in 1:ncol(items)) {
  items[,i] = factor(items[,i], levels=1:5, labels=opciones, ordered=TRUE)
}

#Combine: Juntar todo en el marco de datos DF_Likert.
DF_Likert <- bind_cols(DF_Likert[, 1:2], items)

#Cambiar el nombre de las columnas por el de preguntas descriptivas
#Guardar, después, con la función as.data.frame. Si se guarda como tibble, no funcionará el paquete
#likert para graficar

DF_Likert <- DF_Likert |>
             rename ("Tiene el ancho adecuado para circular en ella" = LIKERT_ANCHO.ADECUADO,
                     "Está bien iluminada" = LIKERT_BIEN.ILUMINADA,
                     "Cuando se transita por ella, 
                     se puede cruzar la calle sin riesgo de ser atropellado(a)" = LIKERT_CRUZAR.SIN.RIESGO.DE.SER.ATROPELLADO,
                     "La ciclovía es segura / me siento seguro(a) usándola" = LIKERT_ME.SIENTO.SEGURO,
                     "Hay paradas de transporte público accesibles y cercanas" = LIKERT_PARADAS.DE.TRANSPORTE.ACCESIBLES,
                     "Tiene señalizaciones claras" = LIKERT_SEÑALIZACIONES.CLARAS,
                     "Su superficie es uniforme, sin baches u obstáculos" = LIKERT_SUPERFICIE.UNIFORME,
                     "Tiene zonas donde puedo detenerme a descansar" = LIKERT_ZONAS.DE.DESCANSO)  |>
             as.data.frame()

#Se hace lo mismo en items

items <- items |>
  rename ("Tiene el ancho adecuado para circular en ella" = LIKERT_ANCHO.ADECUADO,
          "Está bien iluminada" = LIKERT_BIEN.ILUMINADA,
          "Cuando se transita por ella, 
                     se puede cruzar la calle sin riesgo de ser atropellado(a)" = LIKERT_CRUZAR.SIN.RIESGO.DE.SER.ATROPELLADO,
          "La ciclovía es segura / me siento seguro(a) usándola" = LIKERT_ME.SIENTO.SEGURO,
          "Hay paradas de transporte público accesibles y cercanas" = LIKERT_PARADAS.DE.TRANSPORTE.ACCESIBLES,
          "Tiene señalizaciones claras" = LIKERT_SEÑALIZACIONES.CLARAS,
          "Su superficie es uniforme, sin baches u obstáculos" = LIKERT_SUPERFICIE.UNIFORME,
          "Tiene zonas donde puedo detenerme a descansar" = LIKERT_ZONAS.DE.DESCANSO)  |>
  as.data.frame()

#CREAR GRÁFICAS -----------------------------------------------------------------------------------------

#NOTA: Sobre el uso de una fuente personalizada para los gráficos

#A petición del equipo de arquitectas, se utilizó la fuente Montserrat en las versiones de las gráficas
#que se presentaron a la UPPESAN. #Para ello, en todos los casos de las gráficas de pastel, se añadió 
#una línea de código como la siguiente: 
# + theme(text=element_text(size= 12, family="Montserrat"))
#Posteriormente, se guardó manualmente la gráfica generada en la pestaña "Plots" de la consola de R Studio. 

#Para efectos de replicabilidad, aquí se omite esa línea y se utiliza la función de ggsave para, al correr
#la totalidad del código, se generen todos los análisis y gráficas.

#Los únicos cambios son la fuente utilizada para el texto (aquí se usa la fuente por default) y la resolución
#de la imagen. Se tomó esta decisión pues, al momento de usar la función ggsave,
#se altera considerablemente el tamaño y la distribución del texto.


#Fuente a utilizar en los gráficos

#Aquí se utiliza el paquete showtext, para descargar y usar la fuente Montserrat, de Google.

font_add_google("Montserrat", "Montserrat")
showtext_auto()

#Adaptación de la paleta de colores que decidió el equipo para cada gráfica

Colores_2Opciones <- c("#8EA3BF", "#F29F8D")
Colores_3Opciones <- c("#8EA3BF", "#F29F8D", "#F2BC8D")
Colores_4Opciones <- c("#69688C", "#8EA3BF", "#F29F8D", "#6482B9")
Colores_5Opciones <- c("#283959", "#69688C", "#8EA3BF", "#F2BC8D", "#F29F8D")
Colores_6Opciones <- c("#8FB339", "#69688C", "#8EA3BF", "#F2BC8D", "#F29F8D", "#6482B9")
Colores_9Opciones <- c("#8FB339", "#69688C", "#8EA3BF", "#E98935", "#F2BC8D", "#F29F8D", 
                                "#EB6547", "#6482B9", "#C6DD92")

Colores_ModosDeTraslado <- c("#283959","#8EA3BF", "#F29F8D")

#Gráficas de escalas de Likert

Gráfica_LikertCiclovía1 <- plot(likert(DF_Likert[, 3:5], grouping = DF_Likert$USACICLOVÍA), 
                                colors = Colores_5Opciones, 
                                centered = TRUE, 
                                legend.position = "right") + 
                           ggtitle("Percepciones de la Ciclovía 'Ferrocarril a Cuernavaca' (1/3)")
Gráfica_LikertCiclovía2 <- plot(likert(DF_Likert[, 6:8], grouping = DF_Likert$USACICLOVÍA), 
                                colors = Colores_5Opciones, 
                                centered = TRUE, 
                                legend.position = "right") + 
                           ggtitle("Percepciones de la Ciclovía 'Ferrocarril a Cuernavaca' (2/3)")
Gráfica_LikertCiclovía3 <- plot(likert(DF_Likert[, 9:10], grouping = DF_Likert$USACICLOVÍA), 
                                colors = Colores_5Opciones, 
                                centered = TRUE, 
                                legend.position = "right") + 
                           ggtitle("Percepciones de la Ciclovía 'Ferrocarril a Cuernavaca' (3/3)")

#Gráfica de barras de  Modos de Traslado

Gráfica_ModosDeTraslado <- ggplot(DF_ModosDeTraslado, 
                                  aes(x=MODO_TRASLADO, 
                                      y=USO_TRASLADO)) + 
                           geom_bar(stat = "identity", fill = Colores_ModosDeTraslado) +
                           theme(text=element_text(size= 25, family="Montserrat")) +
                           ggtitle("Tipos de traslado por la Ciclovía 'Ferrocarril a Cuernavaca'") + 
                           theme(plot.title = element_text(hjust = 0.5)) +
                           xlab(NULL) +
                           ylab("Personas")

#Gráfica de pastel de Género: Sí Usan la Ciclovía

Gráfica_SCGénero <- ggplot(Tabla_SCGénero,
                          aes(x = "",
                              y = PERSONAS,
                              fill = GÉNERO)) +
                    geom_col() +
                    coord_polar(theta = "y") +
                    ggtitle("Participantes que sí usan la Ciclovía 'Ferrocarril a Cuernavaca'") + 
                    theme(plot.title = element_text(hjust = 0.1),
                          axis.title = element_blank(),
                          axis.text = element_blank(),
                          panel.grid = element_blank()) +
                    scale_fill_manual (values = Colores_3Opciones) +
                    geom_text(aes(label = PERSONAS),
                              position = position_stack(vjust =0.5))

#Gráfica de pastel de Género: NO Usan la Ciclovía

Gráfica_NCGénero <- ggplot(Tabla_NCGénero,
                           aes(x = "",
                               y = PERSONAS,
                               fill = GÉNERO)) +
                    geom_col() +
                    coord_polar(theta = "y") +
                    ggtitle("Participantes que no usan la Ciclovía 'Ferrocarril a Cuernavaca'") + 
                    theme(plot.title = element_text(hjust = 0.2),
                    axis.title = element_blank(),
                    axis.text = element_blank(),
                    panel.grid = element_blank()) +
                    scale_fill_manual (values = Colores_2Opciones) +
                    geom_text(aes(label = PERSONAS),
                              position = position_stack(vjust =0.5))

#Gráfica de pastel de Ocupación: SÍ Usan la Ciclovía

Gráfica_SCOcupación <- ggplot(Tabla_SCOcupación,
                         aes(x = "",
                             y = PERSONAS,
                             fill = OCUPACIÓN)) +
                       geom_col() +
                       coord_polar(theta = "y") +
                       ggtitle("Participantes que sí usan la Ciclovía 'Ferrocarril a Cuernavaca'") + 
                       theme(plot.title = element_text(hjust = 0),
                             axis.title = element_blank(),
                             axis.text = element_blank(),
                             panel.grid = element_blank()) +
                             scale_fill_manual (values = Colores_6Opciones) +
                             geom_text(aes(label = PERSONAS), 
                             position = position_stack(vjust = 0.5))

#Gráfica de pastel de Ocupación: NO Usan la Ciclovía

Gráfica_NCOcupación <- ggplot(Tabla_NCOcupación,
                              aes(x = "",
                                  y = PERSONAS,
                                  fill = OCUPACIÓN)) +
                       geom_col() +
                       coord_polar(theta = "y") +
                       ggtitle("Participantes que no usan la Ciclovía 'Ferrocarril a Cuernavaca'") + 
                       theme(plot.title = element_text(hjust = 0),
                       axis.title = element_blank(),
                       axis.text = element_blank(),
                       panel.grid = element_blank()) +
                       scale_fill_manual (values = Colores_3Opciones) +
                       geom_text(aes(label = PERSONAS), 
                       position = position_stack(vjust = 0.5))

#Gráfica de pastel de Alcaldía: SÍ Usan la Ciclovía

Gráfica_SCAlcaldía <- ggplot(Tabla_SCAlcaldía,
                              aes(x = "",
                                  y = PERSONAS,
                                  fill = ALCALDÍA)) +
                      geom_col() +
                      coord_polar(theta = "y") +
                      ggtitle("Participantes que sí usan la Ciclovía 'Ferrocarril a Cuernavaca'") + 
                      theme(plot.title = element_text(hjust = 0),
                      axis.title = element_blank(),
                      axis.text = element_blank(),
                      panel.grid = element_blank()) +
                      scale_fill_manual (values = Colores_3Opciones) +
                      geom_text(aes(label = PERSONAS), 
                      position = position_stack(vjust = 0.5))

#Gráfica de pastel de Alcaldía: NO Usan la Ciclovía

Gráfica_NCAlcaldía <- ggplot(Tabla_NCAlcaldía,
                             aes(x = "",
                                 y = PERSONAS,
                                 fill = ALCALDÍA)) +
                      geom_col() +
                      coord_polar(theta = "y") +
                      ggtitle("Participantes que no usan la Ciclovía 'Ferrocarril a Cuernavaca'") + 
                      theme(plot.title = element_text(hjust = 0),
                      axis.title = element_blank(),
                      axis.text = element_blank(),
                      panel.grid = element_blank()) +
                      scale_fill_manual (values = Colores_3Opciones) +
                      geom_text(aes(label = PERSONAS), 
                      position = position_stack(vjust = 0.5))

#Gráfica de barras de colonia: SÍ Usan la Ciclovía

Gráfica_SCColonia <- ggplot(Tabla_SCColonia, 
                            aes(x=COLONIA, 
                                y=PERSONAS)) + 
                     geom_bar(stat = "identity", fill = Colores_6Opciones) +
                     theme(text=element_text(size= 25, family="Montserrat")) +
                     ggtitle("Participantes que sí usan la Ciclovía 'Ferrocarril a Cuernavaca'") + 
                     theme(plot.title = element_text(hjust = 0.5)) +
                     xlab("Colonia") +
                     ylab("Personas") +
                     scale_x_discrete(guide = guide_axis(n.dodge = 2))

#Gráfica de barras de colonia: NO Usan la Ciclovía

Gráfica_NCColonia <- ggplot(Tabla_NCColonia, 
                            aes(x=COLONIA, 
                                y=PERSONAS)) + 
                     geom_bar(stat = "identity", fill = Colores_4Opciones) +
                     theme(text=element_text(size= 25, family="Montserrat")) +
                     ggtitle("Participantes que no usan la Ciclovía 'Ferrocarril a Cuernavaca'") + 
                     theme(plot.title = element_text(hjust = 0.5)) +
                     xlab("Colonia") +
                     ylab("Personas") +
                     scale_x_discrete(guide = guide_axis(n.dodge = 2))

#GUARDAR GRÁFICAS ---------------------------------------------------------------------------------------

#Gráficas de Likert

ggsave("Gráfica_EscalasdeLikert1_Original.png", plot = Gráfica_LikertCiclovía1, width = 7, height = 4, dpi = 300)
ggsave("Gráfica_EscalasdeLikert2_Original.png", plot = Gráfica_LikertCiclovía2, width = 7, height = 4, dpi = 300)
ggsave("Gráfica_EscalasdeLikert3_Original.png", plot = Gráfica_LikertCiclovía3, width = 7, height = 4, dpi = 300)

#Gráfica de Modos de Traslado

ggsave("Gráfica_ModosDeTraslado.png", plot = Gráfica_ModosDeTraslado, width = 6, height = 4, dpi = 300)

#Gráficas de Género

ggsave("Gráfica_SCGénero.png", plot = Gráfica_SCGénero, width = 6, height = 4, dpi = 300)
ggsave("Gráfica_NCGénero.png", plot = Gráfica_NCGénero, width = 6, height = 4, dpi = 300)

#Gráficas de Ocupación

ggsave("Gráfica_SCOcupación.png", plot = Gráfica_SCOcupación, width = 6, height = 4, dpi = 300)
ggsave("Gráfica_NCOcupación.png", plot = Gráfica_NCOcupación, width = 6, height = 4, dpi = 300)

#Gráficas de Alcaldía

ggsave("Gráfica_SCAlcaldía.png", plot = Gráfica_SCAlcaldía, width = 6, height = 4, dpi = 300)
ggsave("Gráfica_NCAlcaldía.png", plot = Gráfica_NCAlcaldía, width = 6, height = 4, dpi = 300)

#Gráficas de Colonia

ggsave("Gráfica_SCColonia.png", plot = Gráfica_SCColonia, width = 6, height = 4, dpi = 300)
ggsave("Gráfica_NCColonia.png", plot = Gráfica_NCColonia, width = 6, height = 4, dpi = 300)
