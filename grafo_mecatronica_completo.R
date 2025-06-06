# install.packages("visNetwork") Instalarlo si no esta
library(visNetwork)


grafo_mecatronica <- function(){
  # Cargar los datos
  nodos <- read.csv("nodos_mecatronica_completo_final.csv")
  aristas <- read.csv("aristas_mecatronica_completo_final.csv")
  
  # Asignar colores por bloque
  colores_bloques <- c("#FF9999", "#FFCC99", "#FFFF99", "#CCFF99", "#99FF99", "#99FFFF",
                       "#9999FF", "#CC99FF", "#FF99FF", "#FF99CC", "#CCCCCC")
  nodos$color <- colores_bloques[nodos$bloque + 1]
  
  # Estilo general de nodos
  nodos$shape <- "dot"
  nodos$size <- nodos$creditos * 3
  # Cada nodo va a tener los creditos, sus horas y el bloque al que pertenece
  nodos$title <- paste0("<b>", nodos$label, "</b><br>",
                        "Créditos: ", nodos$creditos, "<br>",
                        "Horas: ", nodos$horas, "<br>",
                        "Bloque: ", nodos$bloque)
  
  # Configuración de aristas
  aristas$arrows <- ifelse(aristas$tipo == "requisito", "to", "")
  aristas$dashes <- ifelse(aristas$tipo == "correquisito", TRUE, FALSE)
  aristas$smooth <- TRUE
  
  legend_items <- lapply(0:10, function(b) {
    list(label = paste("Bloque", b), shape = "dot", color = colores_bloques[b + 1])
  })
  
  # Mostrar grafo
  visNetwork(nodos, aristas, height = "800px", width = "100%") %>%
    visOptions(highlightNearest = TRUE,
               nodesIdSelection = TRUE) %>%
    visInteraction(navigationButtons = TRUE) %>%
    visLegend(addNodes = legend_items,
              useGroups = FALSE, position = "right") %>%
    visLayout(randomSeed = 123)
}