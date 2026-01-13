# =============================================== #
# DASHBOARD EXPLORATORIO - PUBLICACIONES EPICHRON
# ANÁLISIS TEMÁTICO CON KEYWORDS OFICIALES
# =============================================== #

# 1. CARGAR PAQUETES
library(shiny)
library(tidyverse)
library(data.table)
library(ggplot2)
library(plotly)
library(DT)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(viridisLite)

# 2. CARGAR DATOS (con manejo de errores)
cargar_datos <- function() {
  archivo <- "EpiChron_thematic_clean.csv"
  
  if (!file.exists(archivo)) {
    stop("No se encontró el archivo: ", archivo, 
         "\nAsegúrate de que esté en la misma carpeta que app.R")
  }
  
  tryCatch({
    df <- fread(archivo, sep = ";", encoding = "UTF-8")
    cat("Datos cargados correctamente. Filas:", nrow(df), "\n")
    return(df)
  }, error = function(e) {
    stop("Error al cargar datos: ", e$message)
  })
}

# Cargar datos inmediatamente (fuera de server/UI)
df <- cargar_datos()
df <- df[, 1:which("Santos_Class" == colnames(df))]

# Ver nombres de columnas
print(names(df))

# 4. DEFINIR LISTA DE KEYWORDS OFICIALES POR CATEGORÍA (renombrado a lista_keywords)
lista_keywords <- list(
  # Poblaciones
  Poblaciones = c(
    "Pregnant population",
    "Women population",  
    "Older population",
    "Migrant population"
  ),
  
  # Enfermedades/Condiciones
  Enfermedades = c(
    "COVID Disease",
    "CV Disease",
    "Dermatology Disease",
    "Metabolic Disease",
    "Breast Cancer Disease",
    "Cancer Disease",
    "Chronic Liver Disease",
    "Mental Disease",
    "COP Disease",
    "Respiratory Disease",
    "Diabetes Disease",
    "Pancreas Disease",
    "Obesity Disease",
    "Epilepsy Disease",
    "Dementia Disease"
  ),
  
  # Métodos/Estudios
  Metodos_Estudio = c(
    "Meta-analysis",
    "Systematic Review",
    "Network analysis",
    "Disease Pattern",
    "Disease Trajectories",
    "Factor analysis",
    "Predictive analysis",
    "Survey data",
    "PROMS",
    "Clinical Pattern",
    "Drug Pattern",
    "Biomarkers"
  ),
  
  # Bases de Datos/Cohortes
  Bases_Datos = c(
    "International Cohorts",
    "BIGAN Cohort",
    "CARhES Cohort",
    "EpiChron Cohort",
    "PRECOVID Disease Cohort",
    "SURBCAN Cohort",
    "MRisk-COVID Disease Cohort",
    "Andalusian database",
    "Italy database",
    "RELE Balear Island database",
    "SNACK Cohort",
    "RWD Cohort",
    "INTAFRADE Cohort",
    "Norway Cohort",
    "England Cohort",
    "Catalonia Cohort",
    "Italy Cohort",
    "National Cohorts"
  ),
  
  # Intervenciones/Tratamientos
  Intervenciones = c(
    "Treatment adherence",
    "Vaccine",
    "Drug-drug Interaction"
  ),
  
  # Resultados/Métricas
  Resultados = c(
    "Health outcomes",
    "Quality of life",
    "Cost of care",
    "Risk factor",
    "Health Inequity",
    "Use of Health Service",
    "Functional status",
    "Aging"
  ),
  
  # Metodologías/Enfoques
  Metodologias = c(
    "Intersectional analysis",
    "MINERVA",
    "FAIRS",
    "Interoperability",
    "Pharmacovigilance",
    "AESI",
    "Oncology",
    "Primary Care",
    "RCT",
    "Implementation",
    "Formation",
    "Editor letter",
    "CHRODIS",
    "Care Model",
    "Book Chapter"
  ),
  
  # Términos Generales
  Generales = c(
    "Social determinant",
    "Multimorbidity",
    "Comorbidity",
    "Specific condition",
    "RWD",
    "AI/ML",
    "Polypharmacy",
    "Pharmacoepidemiology",
    "Methodology"
  )
)

# Crear vector plano de todas las keywords
todas_keywords <- unlist(lista_keywords)
names(todas_keywords) <- NULL

# 5. FUNCIONES AUXILIARES
extraer_autores<- function(cita) {
  if (is.na(cita) || cita == "" || cita == "NA") {
    return(list(autor1 = NA, autor2 = NA))
  }
  
  # Imprimir para depuración (opcional)
  # cat("Procesando cita:", substr(cita, 1, 100), "...\n")
  
  # Limpiar la cita
  cita_limpia <- gsub("\n", " ", cita)
  cita_limpia <- gsub("\\s+", " ", cita_limpia)
  cita_limpia <- trimws(cita_limpia)
  
  # Caso 1: Si hay punto y coma, tomar lo que está antes
  if (grepl(";", cita_limpia)) {
    partes <- unlist(strsplit(cita_limpia, ";"))
    parte_autores <- trimws(partes[1])
  } else {
    # Caso 2: Si no hay punto y coma, tomar hasta el primer punto que no sea de inicial
    # Esto es para capturar: "Autor1, Autor2. Título del artículo"
    match <- regexpr("^[^.]*\\.[^.]", cita_limpia)
    if (match > 0) {
      parte_autores <- substr(cita_limpia, 1, match[1])
    } else {
      # Caso 3: Tomar hasta el primer punto
      partes <- unlist(strsplit(cita_limpia, "\\."))
      parte_autores <- trimws(partes[1])
    }
  }
  
  # Eliminar "et al" si está presente
  parte_autores <- gsub("et al\\..*$", "", parte_autores, ignore.case = TRUE)
  
  # Dividir por comas
  autores <- unlist(strsplit(parte_autores, ","))
  autores <- trimws(autores)
  
  # Filtrar elementos vacíos y muy cortos (< 2 caracteres)
  autores <- autores[autores != "" & nchar(autores) > 2]
  
  # Tomar máximo 2 autores
  if (length(autores) >= 2) {
    autor1 <- autores[1]
    autor2 <- autores[2]
    
    # Limpiar puntuación final
    autor1 <- gsub("[;.]$", "", autor1)
    autor2 <- gsub("[;.]$", "", autor2)
    
  } else if (length(autores) == 1) {
    autor1 <- gsub("[;.]$", "", autores[1])
    autor2 <- NA
  } else {
    autor1 <- NA
    autor2 <- NA
  }
  
  return(list(autor1 = autor1, autor2 = autor2))
}


# Función para extraer keywords oficiales del texto
extraer_keywords_oficiales <- function(texto) {
  if (is.na(texto) || texto == "") return(list())
  
  texto_clean <- tolower(trimws(texto))
  keywords_encontradas <- c()
  categorias_encontradas <- c()
  
  # Buscar cada keyword oficial
  for (categoria in names(lista_keywords)) {
    for (keyword in lista_keywords[[categoria]]) {
      keyword_lower <- tolower(keyword)
      
      # Buscar la keyword (como palabra completa)
      pattern <- paste0("\\b", gsub(" ", "\\\\s+", keyword_lower), "\\b")
      
      if (grepl(pattern, texto_clean, perl = TRUE)) {
        keywords_encontradas <- c(keywords_encontradas, keyword)
        categorias_encontradas <- c(categorias_encontradas, categoria)
      }
    }
  }
  
  return(list(
    keywords = unique(keywords_encontradas),
    categorias = unique(categorias_encontradas)
  ))
}

# Función genérica para crear gráficos de líneas con todas las keywords
crear_grafico_lineas_plotly <- function(datos, categoria_nombre, categoria_keywords, 
                                        titulo_base) {
  
  # Filtrar datos que tienen keywords
  datos_categoria <- datos[!is.na(keywords_oficiales) & keywords_oficiales != ""]
  
  if (nrow(datos_categoria) == 0) {
    return(plotly_empty() %>% 
             layout(title = paste("No hay keywords de", tolower(categoria_nombre))))
  }
  
  # Separar keywords y filtrar por la categoría
  keywords_list <- datos_categoria[, .(
    anio,
    keyword = unlist(strsplit(keywords_oficiales, "; "))
  )]
  
  keywords_filtradas <- keywords_list[keyword %in% categoria_keywords]
  
  if (nrow(keywords_filtradas) == 0) {
    return(plotly_empty() %>% 
             layout(title = paste("No hay keywords de", tolower(categoria_nombre))))
  }
  
  # Contar frecuencias por año y keyword
  datos_plot <- keywords_filtradas[, .(frecuencia = .N), by = .(anio, keyword)]
  
  # Obtener rango completo de años y keywords únicas
  anos_unicos <- sort(unique(datos_plot$anio))
  keywords_unicas <- unique(datos_plot$keyword)
  n_keywords <- length(keywords_unicas)
  
  # Crear un plot_ly vacío
  p <- plot_ly()
  
  # Paleta de colores mejorada - más saturada
  if (n_keywords <= 8) {
    # Usar Set1 para colores más saturados (rojos, azules, verdes intensos)
    colores <- brewer.pal(max(3, n_keywords), "Set1")
  } else {
    # Para muchas keywords, usar viridis con opción plasma (más vibrante)
    colores <- viridisLite::viridis(n_keywords, option = "plasma")
  }
  
  # Añadir una traza por cada keyword
  for (i in 1:n_keywords) {
    kw <- keywords_unicas[i]
    datos_kw <- datos_plot[keyword == kw]
    
    # Crear serie completa con todos los años
    datos_completos <- data.frame(
      anio = anos_unicos,
      frecuencia = sapply(anos_unicos, function(a) {
        idx <- which(datos_kw$anio == a)
        if (length(idx) > 0) datos_kw$frecuencia[idx] else 0
      })
    )
    
    p <- p %>%
      add_trace(
        x = datos_completos$anio,
        y = datos_completos$frecuencia,
        type = "scatter",
        mode = "lines+markers",
        name = kw,
        line = list(color = colores[i], width = 2),
        marker = list(color = colores[i], size = 6),
        hovertemplate = paste(
          categoria_nombre, ":", kw,
          "<br>Año: %{x}",
          "<br>Frecuencia: %{y}",
          "<extra></extra>"
        )
      )
  }
  
  # Configurar layout
  p <- p %>%
    layout(
      title = list(
        text = paste(titulo_base, "(", n_keywords, "keywords)"),
        x = 0.5,
        font = list(size = 16, family = "Arial")
      ),
      xaxis = list(
        title = "Año",
        tickvals = anos_unicos,
        ticktext = as.character(anos_unicos),
        tickangle = 45,
        showgrid = TRUE,
        gridcolor = '#f0f0f0',
        zeroline = FALSE,
        showline = TRUE,
        ticks = "outside"
      ),
      yaxis = list(
        title = "Frecuencia",
        zeroline = TRUE,
        zerolinecolor = '#d0d0d0',
        showgrid = TRUE,
        gridcolor = '#f0f0f0',
        range = c(0, max(datos_plot$frecuencia, na.rm = TRUE) * 1.1)
      ),
      legend = list(
        orientation = "v",
        x = 1.02,
        y = 0.5,
        font = list(size = 10)
      ),
      margin = list(l = 80, r = 150, b = 80, t = 80, pad = 10),
      hovermode = "closest",
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    ) %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("autoScale2d", "resetScale2d", "hoverClosestCartesian", 
                                 "hoverCompareCartesian", "toggleSpikelines"),
      displaylogo = FALSE
    )
  
  return(p)
}

# 6. PREPROCESAR DATOS
setnames(df, 
         old = c("AÑO PUBLICACIÓN", "ESTADO", "TÍTULO", "PUBLICACIÓN", "Q(JIF)", "Cita", "Santos_Class"),
         new = c("anio", "estado", "titulo", "publicacion", "cuartil", "cita", "santos_class"))

df <- df[anio != "" & !is.na(anio)]
df[, anio := as.numeric(anio)]
df[, cuartil := as.character(cuartil)]

# Extraer autores (usando función mejorada)

df <- df %>%
  mutate(
    # Asegurar que cita es character
    cita = as.character(cita),
    
    # Extraer autores usando rowwise() para procesar cada fila individualmente
    autores = map(cita, ~ extraer_autores(.x)),
    
    # Separar en columnas individuales
    autor1 = map_chr(
      autores,
      ~ ifelse(
        is.null(.x$autor1) || is.na(.x$autor1),
        NA_character_,
        as.character(.x$autor1)
      )
    ),
    autor2 = map_chr(
      autores,
      ~ ifelse(
        is.null(.x$autor2) || is.na(.x$autor2),
        NA_character_,
        as.character(.x$autor2)
      )
    )
  ) %>%
  select(-autores)  # Eliminar columna temporal

# Convertir de vuelta a data.table si es necesario
setDT(df)

# Procesar cuartiles - CORREGIDO
df[, cuartil_clean := fcase(
  grepl("Q1", cuartil, ignore.case = TRUE), "Q1",
  grepl("Q2", cuartil, ignore.case = TRUE), "Q2",
  grepl("Q3", cuartil, ignore.case = TRUE), "Q3",
  grepl("Q4", cuartil, ignore.case = TRUE), "Q4",
  grepl("sin cuartil", cuartil, ignore.case = TRUE), "Sin cuartil",
  default = "No especificado"
)]

# Asegurar que no haya NAs
df[is.na(cuartil_clean), cuartil_clean := "No especificado"]

# Crear variable numérica para cuartiles (para cálculos de media)
df[, valor_cuartil := fcase(
  cuartil_clean == "Q1", 4,
  cuartil_clean == "Q2", 3,
  cuartil_clean == "Q3", 2,
  cuartil_clean == "Q4", 1,
  cuartil_clean == "Sin cuartil", 0,
  cuartil_clean == "No especificado", 0,
  default = 0
)]

# Extraer keywords oficiales y categorías
df[, c("keywords_oficiales", "categorias_tematicas") := {
  resultado <- extraer_keywords_oficiales(santos_class)
  list(
    paste(resultado$keywords, collapse = "; "),
    paste(resultado$categorias, collapse = "; ")
  )
}, by = .I]

# Crear columnas individuales para cada categoría
for (categoria in names(lista_keywords)) {
  col_name <- paste0("cat_", gsub(" ", "_", tolower(categoria)))
  
  df[, (col_name) := 
       sapply(df$keywords_oficiales, function(x) {
         if (is.na(x) || x == "") return(FALSE)
         keywords_list <- strsplit(x, "; ")[[1]]
         any(keywords_list %in% lista_keywords[[categoria]])
       }, USE.NAMES = FALSE)]
}

# 7. CREAR LISTAS PARA FILTROS
# Extraer todas las keywords oficiales únicas encontradas
keywords_unicas <- unique(unlist(strsplit(df$keywords_oficiales[!is.na(df$keywords_oficiales) & df$keywords_oficiales != ""], "; ")))
keywords_unicas <- sort(keywords_unicas)

# Extraer categorías únicas
categorias_unicas <- unique(unlist(strsplit(df$categorias_tematicas[!is.na(df$categorias_tematicas) & df$categorias_tematicas != ""], "; ")))
categorias_unicas <- sort(categorias_unicas)

# 8. INTERFAZ DE USUARIO (UI)
ui <- fluidPage(
  titlePanel("Dashboard Exploratorio - Publicaciones EpiChron"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filtros Basicos"),
      sliderInput("anio_range", "Rango de anos:",
                  min = min(df$anio, na.rm = TRUE),
                  max = max(df$anio, na.rm = TRUE),
                  value = c(min(df$anio, na.rm = TRUE), 
                            max(df$anio, na.rm = TRUE)),
                  step = 1),
      selectInput("cuartil_filter", "Filtrar por cuartil:",
                  choices = c("Todos", "Q1", "Q2", "Q3", "Q4", "Sin cuartil", "No especificado"),
                  selected = "Todos"),
      
      hr(),
      h4("Filtros Tematicos (Keywords Oficiales)"),
      
      # Filtro por categoría
      selectInput("categoria_filter", "Filtrar por categoria:",
                  choices = c("Todas", categorias_unicas),
                  selected = "Todas"),
      
      # Filtro por keyword específica
      selectizeInput("keyword_filter", "Filtrar por keyword especifica:",
                     choices = c("Todas", keywords_unicas),
                     selected = "Todas",
                     multiple = TRUE,
                     options = list(maxItems = 3, placeholder = 'Busque keywords')),
      
      # Filtro por enfermedad específica
      selectizeInput("enfermedad_filter", "Filtrar por enfermedad:",
                     choices = c("Todas", lista_keywords$Enfermedades),
                     selected = "Todas",
                     multiple = TRUE,
                     options = list(maxItems = 3, placeholder = 'Seleccione enfermedades')),
      
      # Filtro por cohorte/base de datos
      selectizeInput("cohorte_filter", "Filtrar por cohorte/base de datos:",
                     choices = c("Todas", lista_keywords$Bases_Datos),
                     selected = "Todas",
                     multiple = TRUE,
                     options = list(maxItems = 3, placeholder = 'Seleccione cohortes')),
      
      hr(),
      h5("Informacion"),
      p("Total de publicaciones en dataset:", nrow(df)),
      p("Periodo:", min(df$anio, na.rm = TRUE), "-", max(df$anio, na.rm = TRUE)),
      p("Keywords oficiales encontradas:", length(keywords_unicas)),
      
      actionButton("reset_filters", "Reiniciar Filtros", icon = icon("refresh"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen General",
                 fluidRow(
                   column(6, plotlyOutput("plot_publicaciones_anio")),
                   column(6, plotlyOutput("plot_cuartiles"))
                 ),
                 fluidRow(
                   column(12, 
                          h4("Estadisticas clave"),
                          verbatimTextOutput("estadisticas_clave"))
                 )
        ),
        
        tabPanel("Analisis Tematico por Keywords",
                 fluidRow(
                   column(12,
                          h4("Top 10 Keywords Mas Frecuentes"),
                          plotlyOutput("plot_top_keywords"))
                 ),
                 hr(), br(),
                 h4("Evolucion de Keywords por Categoria"),
                 fluidRow(
                   column(12, plotlyOutput("plot_keywords_enfermedades"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("plot_keywords_metodos"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("plot_keywords_cohortes"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("plot_keywords_metodologias"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("plot_keywords_poblaciones"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("plot_keywords_intervenciones"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("plot_keywords_resultados"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("plot_keywords_generales"))
                 )
        ),
        
        tabPanel("Analisis Estrategico por Ano",
                 h4("Evolucion de Lineas de Investigacion"),
                 fluidRow(
                   column(12, plotlyOutput("plot_heatmap_tematico"))
                 ),
                 fluidRow(
                   column(12,
                          h4("Resumen Ejecutivo por Ano"),
                          DTOutput("tabla_resumen_estrategico"))
                 ),
                 fluidRow(
                   column(12,
                          h4("Fortalezas y Oportunidades por Linea"),
                          DTOutput("tabla_analisis_lineas"))
                 )
        ),
        
        tabPanel("Keywords Detalladas",
                 h4("Top 20 Keywords Mas Frecuentes"),
                 plotlyOutput("plot_distribucion_keywords"),
                 h4("Tabla de Frecuencias de Keywords"),
                 DTOutput("tabla_keywords_frecuencia"),
                 h4("Keywords por Publicacion"),
                 DTOutput("tabla_keywords_publicacion")
        ),
        
        # En la sección de Autores y Revistas (tabPanel), modifica:
        tabPanel("Autores y Revistas",
                 fluidRow(
                   column(12, 
                          h4("Frecuencia de Autores"),
                          p("Todos los autores que han sido primer autor al menos 1 vez"),
                          DTOutput("tabla_autores_combinada"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("plot_comparacion_autores"))
                 ),
                 fluidRow(
                   column(12,
                          h4("Top 10 Revistas"),
                          DTOutput("tabla_revistas"))
                 ),
                 fluidRow(
                   column(12, plotlyOutput("plot_top_revistas"))
                 )
        ),
        
        tabPanel("Cuartiles y Calidad",
                 h4("Evolucion de Calidad Global (Media de Cuartil por Ano)"),  # CAMBIADO
                 plotlyOutput("plot_cuartil_tematico"),
                 h4("Estadisticas de Cuartiles"),
                 verbatimTextOutput("estadisticas_cuartiles"),
                 h4("Distribucion de Cuartiles por Keyword"),
                 DTOutput("tabla_cuartiles_keywords")
        ),
        
        tabPanel("Datos Completos",
                 h4("Base de Datos Completa con Keywords"),
                 DTOutput("tabla_completa")
        )
      )
    )
  )
)

# 9. SERVIDOR (SERVER)
server <- function(input, output, session) {
  
  # Datos filtrados reactivos
  datos_filtrados <- reactive({
    filtered <- copy(df)
    
    # Filtrar por rango de anos
    filtered <- filtered[anio >= input$anio_range[1] & anio <= input$anio_range[2]]
    
    # Filtrar por cuartil
    if (input$cuartil_filter != "Todos") {
      filtered <- filtered[cuartil_clean == input$cuartil_filter]
    }
    
    # Filtrar por categoría
    if (input$categoria_filter != "Todas") {
      filtered <- filtered[grepl(input$categoria_filter, categorias_tematicas, ignore.case = TRUE)]
    }
    
    # Filtrar por keyword específica
    if (!"Todas" %in% input$keyword_filter && length(input$keyword_filter) > 0) {
      filtered <- filtered[
        sapply(keywords_oficiales, function(x) {
          if (is.na(x) || x == "") return(FALSE)
          keywords_vec <- strsplit(x, "; ")[[1]]
          any(input$keyword_filter %in% keywords_vec)
        })
      ]
    }
    
    # Filtrar por enfermedad
    if (!"Todas" %in% input$enfermedad_filter && length(input$enfermedad_filter) > 0) {
      filtered <- filtered[
        sapply(keywords_oficiales, function(x) {
          if (is.na(x) || x == "") return(FALSE)
          keywords_vec <- strsplit(x, "; ")[[1]]
          any(input$enfermedad_filter %in% keywords_vec)
        })
      ]
    }
    
    # Filtrar por cohorte
    if (!"Todas" %in% input$cohorte_filter && length(input$cohorte_filter) > 0) {
      filtered <- filtered[
        sapply(keywords_oficiales, function(x) {
          if (is.na(x) || x == "") return(FALSE)
          keywords_vec <- strsplit(x, "; ")[[1]]
          any(input$cohorte_filter %in% keywords_vec)
        })
      ]
    }
    
    filtered
  })
  
  # Observador para reiniciar filtros
  observeEvent(input$reset_filters, {
    updateSliderInput(session, "anio_range",
                      value = c(min(df$anio, na.rm = TRUE), max(df$anio, na.rm = TRUE)))
    updateSelectInput(session, "cuartil_filter", selected = "Todos")
    updateSelectInput(session, "categoria_filter", selected = "Todas")
    updateSelectizeInput(session, "keyword_filter", selected = "Todas")
    updateSelectizeInput(session, "enfermedad_filter", selected = "Todas")
    updateSelectizeInput(session, "cohorte_filter", selected = "Todas")
  })
  
  # 1. Gráfico: Publicaciones por ano
  output$plot_publicaciones_anio <- renderPlotly({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% layout(title = "No hay datos disponibles"))
    }
    
    datos_plot <- datos[, .(n = .N), by = anio][order(anio)]
    
    p <- ggplot(datos_plot, aes(x = factor(anio), y = n)) +
      geom_bar(stat = "identity", fill = "#4e79a7", alpha = 0.8) +
      geom_line(aes(group = 1), color = "#e15759", size = 1) +
      geom_point(color = "#e15759", size = 2) +
      labs(title = "Publicaciones por Ano",
           x = "Ano", y = "Numero de Publicaciones") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # 2. Gráfico: Distribución de cuartiles - CORREGIDO
  output$plot_cuartiles <- renderPlotly({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% layout(title = "No hay datos disponibles"))
    }
    
    datos_plot <- datos[, .(n = .N), by = cuartil_clean]
    
    # Ordenar cuartiles lógicamente
    orden_cuartiles <- c("Q1", "Q2", "Q3", "Q4", "Sin cuartil", "No especificado")
    datos_plot[, cuartil_clean := factor(cuartil_clean, levels = orden_cuartiles)]
    datos_plot <- datos_plot[order(cuartil_clean)]
    
    p <- ggplot(datos_plot, aes(x = cuartil_clean, y = n, fill = cuartil_clean)) +
      geom_bar(stat = "identity", alpha = 0.8) +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Distribucion de Cuartiles",
           x = "Cuartil", y = "Numero de Publicaciones",
           fill = "Cuartil") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # 3. Gráfico: Keywords de Enfermedades por año
  output$plot_keywords_enfermedades <- renderPlotly({
    datos <- datos_filtrados()
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No hay datos disponibles",
                      xaxis = list(title = "Año", visible = TRUE),
                      yaxis = list(title = "Frecuencia", visible = TRUE)))
    }
    
    crear_grafico_lineas_plotly(
      datos = datos,
      categoria_nombre = "Enfermedades",
      categoria_keywords = lista_keywords$Enfermedades,
      titulo_base = "Evolución de Enfermedades"
    )
  })
  
  # 4. Gráfico: Keywords de Métodos por año
  output$plot_keywords_metodos <- renderPlotly({
    datos <- datos_filtrados()
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No hay datos disponibles",
                      xaxis = list(title = "Año", visible = TRUE),
                      yaxis = list(title = "Frecuencia", visible = TRUE)))
    }
    
    crear_grafico_lineas_plotly(
      datos = datos,
      categoria_nombre = "Métodos",
      categoria_keywords = lista_keywords$Metodos_Estudio,
      titulo_base = "Evolución de Métodos de Estudio"
    )
  })
  
  # 5. Gráfico: Keywords de Cohortes por año
  output$plot_keywords_cohortes <- renderPlotly({
    datos <- datos_filtrados()
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No hay datos disponibles",
                      xaxis = list(title = "Año", visible = TRUE),
                      yaxis = list(title = "Frecuencia", visible = TRUE)))
    }
    
    crear_grafico_lineas_plotly(
      datos = datos,
      categoria_nombre = "Cohortes",
      categoria_keywords = lista_keywords$Bases_Datos,
      titulo_base = "Evolución de Cohortes/Bases de Datos"
    )
  })
  
  # 6. Gráfico: Keywords de Metodologías por año
  output$plot_keywords_metodologias <- renderPlotly({
    datos <- datos_filtrados()
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No hay datos disponibles",
                      xaxis = list(title = "Año", visible = TRUE),
                      yaxis = list(title = "Frecuencia", visible = TRUE)))
    }
    
    crear_grafico_lineas_plotly(
      datos = datos,
      categoria_nombre = "Metodologías",
      categoria_keywords = lista_keywords$Metodologias,
      titulo_base = "Evolución de Metodologías/Enfoques"
    )
  })
  
  # 7. Gráfico: Keywords de Poblaciones por año
  output$plot_keywords_poblaciones <- renderPlotly({
    datos <- datos_filtrados()
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No hay datos disponibles",
                      xaxis = list(title = "Año", visible = TRUE),
                      yaxis = list(title = "Frecuencia", visible = TRUE)))
    }
    
    crear_grafico_lineas_plotly(
      datos = datos,
      categoria_nombre = "Poblaciones",
      categoria_keywords = lista_keywords$Poblaciones,
      titulo_base = "Evolución de Poblaciones de Estudio"
    )
  })
  
  # 8. Gráfico: Keywords de Intervenciones por año
  output$plot_keywords_intervenciones <- renderPlotly({
    datos <- datos_filtrados()
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No hay datos disponibles",
                      xaxis = list(title = "Año", visible = TRUE),
                      yaxis = list(title = "Frecuencia", visible = TRUE)))
    }
    
    crear_grafico_lineas_plotly(
      datos = datos,
      categoria_nombre = "Intervenciones",
      categoria_keywords = lista_keywords$Intervenciones,
      titulo_base = "Evolución de Intervenciones/Tratamientos"
    )
  })
  
  # 9. Gráfico: Keywords de Resultados por año
  output$plot_keywords_resultados <- renderPlotly({
    datos <- datos_filtrados()
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No hay datos disponibles",
                      xaxis = list(title = "Año", visible = TRUE),
                      yaxis = list(title = "Frecuencia", visible = TRUE)))
    }
    
    crear_grafico_lineas_plotly(
      datos = datos,
      categoria_nombre = "Resultados",
      categoria_keywords = lista_keywords$Resultados,
      titulo_base = "Evolución de Resultados/Métricas"
    )
  })
  
  # 10. Gráfico: Keywords de Términos Generales por año
  output$plot_keywords_generales <- renderPlotly({
    datos <- datos_filtrados()
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No hay datos disponibles",
                      xaxis = list(title = "Año", visible = TRUE),
                      yaxis = list(title = "Frecuencia", visible = TRUE)))
    }
    
    crear_grafico_lineas_plotly(
      datos = datos,
      categoria_nombre = "Términos Generales",
      categoria_keywords = lista_keywords$Generales,
      titulo_base = "Evolución de Términos Generales"
    )
  })
  
  # 11. Gráfico: Top 10 keywords más frecuentes
  output$plot_top_keywords <- renderPlotly({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% layout(title = "No hay datos disponibles"))
    }
    
    keywords_data <- datos[!is.na(keywords_oficiales) & keywords_oficiales != ""]
    
    if (nrow(keywords_data) == 0) {
      return(plotly_empty() %>% layout(title = "No hay keywords disponibles"))
    }
    
    # Extraer todas las keywords
    todas_keywords <- unlist(strsplit(keywords_data$keywords_oficiales, "; "))
    
    # Contar frecuencia
    frecuencia_keywords <- data.table(keyword = todas_keywords)[
      , .(frecuencia = .N), by = keyword][order(-frecuencia)]
    
    # Tomar top 10
    top_10 <- frecuencia_keywords[1:min(10, .N)]
    
    p <- ggplot(top_10, aes(x = reorder(keyword, frecuencia), y = frecuencia, fill = frecuencia)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "#76b7b2", high = "#4e79a7") +
      coord_flip() +
      labs(title = "Top 10 Keywords Mas Frecuentes",
           x = "Keyword", y = "Frecuencia") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")
    
    ggplotly(p)
  })
  
  # 12. Heatmap: Evolución temática
  output$plot_heatmap_tematico <- renderPlotly({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% layout(title = "No hay datos disponibles"))
    }
    
    # Preparar datos para heatmap por año y categoría
    datos_plot <- datos[!is.na(categorias_tematicas) & categorias_tematicas != ""]
    
    if (nrow(datos_plot) == 0) {
      return(plotly_empty() %>% layout(title = "No hay categorias disponibles"))
    }
    
    # Separar categorías múltiples
    categorias_list <- datos_plot[, .(
      anio,
      categoria = unlist(strsplit(categorias_tematicas, "; "))
    )]
    
    # Contar por año y categoría
    heatmap_data <- categorias_list[, .(frecuencia = .N), by = .(anio, categoria)]
    
    # Crear heatmap
    p <- ggplot(heatmap_data, aes(x = factor(anio), y = categoria, fill = frecuencia)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "#e15759") +
      labs(title = "Evolucion de Lineas de Investigacion por Categoria",
           x = "Ano", y = "Categoria") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # 13. Tabla: Resumen estratégico por año
  output$tabla_resumen_estrategico <- renderDT({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(datatable(data.frame()))
    }
    
    resumen <- datos[, .(
      n_publicaciones = .N,
      n_q1 = sum(cuartil_clean == "Q1", na.rm = TRUE),
      porc_q1 = round(100 * sum(cuartil_clean == "Q1", na.rm = TRUE) / .N, 1),
      top_keywords = {
        keywords <- unlist(strsplit(keywords_oficiales[!is.na(keywords_oficiales) & keywords_oficiales != ""], "; "))
        if (length(keywords) > 0) {
          top <- names(sort(table(keywords), decreasing = TRUE))[1:min(3, length(unique(keywords)))]
          paste(top, collapse = ", ")
        } else {
          "No keywords"
        }
      },
      principales_cohortes = {
        cohortes <- unlist(strsplit(keywords_oficiales[!is.na(keywords_oficiales) & keywords_oficiales != ""], "; "))
        cohortes <- cohortes[cohortes %in% lista_keywords$Bases_Datos]
        if (length(cohortes) > 0) {
          top <- names(sort(table(cohortes), decreasing = TRUE))[1:min(2, length(unique(cohortes)))]
          paste(top, collapse = ", ")
        } else {
          "No cohortes"
        }
      },
      tendencia = {
        if (.N >= 3) {
          anos <- sort(unique(anio))
          if (length(anos) >= 3) {
            "Estable"
          } else if (max(anio) == max(df$anio)) {
            "En crecimiento"
          } else {
            "En descenso"
          }
        } else {
          "Nueva linea"
        }
      }
    ), by = anio][order(-anio)]
    
    datatable(resumen,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE,
              colnames = c("Ano", "N Public", "N Q1", "% Q1", 
                           "Keywords Principales", "Cohortes Principales",
                           "Tendencia")) %>%
      formatStyle("tendencia",
                  backgroundColor = styleEqual(
                    c("En crecimiento", "Estable", "En descenso", "Nueva linea"),
                    c("#d4edda", "#fff3cd", "#f8d7da", "#d1ecf1")
                  ))
  })
  
  # 14. Tabla: Análisis de líneas de investigación
  output$tabla_analisis_lineas <- renderDT({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(datatable(data.frame()))
    }
    
    # Analizar por categoría de keyword
    analisis_lineas <- rbindlist(lapply(names(lista_keywords), function(cat) {
      # Contar publicaciones en esta categoría
      pubs_cat <- sum(sapply(datos$keywords_oficiales, function(x) {
        if (is.na(x) || x == "") return(FALSE)
        any(strsplit(x, "; ")[[1]] %in% lista_keywords[[cat]])
      }))
      
      if (pubs_cat > 0) {
        # Calcular % en Q1
        pubs_q1 <- sum(sapply(seq_len(nrow(datos)), function(i) {
          if (datos$cuartil_clean[i] == "Q1") {
            keywords_i <- ifelse(is.na(datos$keywords_oficiales[i]) || datos$keywords_oficiales[i] == "", 
                                 "", datos$keywords_oficiales[i])
            any(strsplit(keywords_i, "; ")[[1]] %in% lista_keywords[[cat]])
          } else {
            FALSE
          }
        }))
        
        porc_q1 <- round(100 * pubs_q1 / pubs_cat, 1)
        
        # Identificar años de actividad
        anos_activos <- unique(datos[
          sapply(keywords_oficiales, function(x) {
            if (is.na(x) || x == "") return(FALSE)
            any(strsplit(x, "; ")[[1]] %in% lista_keywords[[cat]])
          })]$anio)
        
        # Determinar estado
        estado <- ifelse(max(anos_activos) == max(datos$anio), 
                         "Activa", 
                         ifelse(max(anos_activos) >= max(datos$anio) - 1, 
                                "Estable", "En declive"))
        
        data.table(
          Linea = cat,
          Publicaciones = pubs_cat,
          `% Q1` = porc_q1,
          `Años Activos` = length(anos_activos),
          `Ultimo Año` = max(anos_activos),
          Estado = estado,
          Fortalezas = ifelse(porc_q1 > 50, "Alta calidad", 
                              ifelse(pubs_cat > 5, "Volumen alto", "Especializada")
          )
        )
      }
    }), fill = TRUE)
    
    analisis_lineas <- analisis_lineas[order(-Publicaciones, -`% Q1`)]
    
    datatable(analisis_lineas,
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle("Estado",
                  backgroundColor = styleEqual(
                    c("Activa", "Estable", "En declive"),
                    c("#d4edda", "#fff3cd", "#f8d7da")
                  )) %>%
      formatStyle("Fortalezas",
                  backgroundColor = styleEqual(
                    c("Alta calidad", "Volumen alto", "Especializada"),
                    c("#c3e6cb", "#ffeeba", "#b8daff")
                  ))
  })
  
  # 15. Gráfico: Distribución de keywords (TOP 20)
  output$plot_distribucion_keywords <- renderPlotly({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% layout(title = "No hay datos disponibles"))
    }
    
    # Extraer todas las keywords y contar frecuencias
    keywords_data <- datos[!is.na(keywords_oficiales) & keywords_oficiales != ""]
    
    if (nrow(keywords_data) == 0) {
      return(plotly_empty() %>% layout(title = "No hay keywords disponibles"))
    }
    
    # Separar keywords
    todas_keywords <- unlist(strsplit(keywords_data$keywords_oficiales, "; "))
    
    # Contar frecuencias
    frecuencia_keywords <- data.table(keyword = todas_keywords)[
      , .(frecuencia = .N), by = keyword][order(-frecuencia)]
    
    # Tomar top 20
    top_20 <- frecuencia_keywords[1:min(20, .N)]
    
    p <- ggplot(top_20, aes(x = reorder(keyword, frecuencia), y = frecuencia, fill = frecuencia)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "#76b7b2", high = "#4e79a7") +
      coord_flip() +
      labs(title = "Top 20 Keywords Mas Frecuentes",
           x = "Keyword", y = "Frecuencia") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none")
    
    ggplotly(p)
  })
  
  # 16. Tabla: Frecuencia de keywords
  output$tabla_keywords_frecuencia <- renderDT({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(datatable(data.frame()))
    }
    
    keywords_data <- datos[!is.na(keywords_oficiales) & keywords_oficiales != ""]
    
    if (nrow(keywords_data) == 0) {
      return(datatable(data.frame()))
    }
    
    # Extraer y contar todas las keywords
    todas_keywords <- unlist(strsplit(keywords_data$keywords_oficiales, "; "))
    
    frecuencia <- data.table(keyword = todas_keywords)[
      , .(Frecuencia = .N,
          `% del total` = round(100 * .N / nrow(keywords_data), 1)),
      by = keyword][order(-Frecuencia)]
    
    # Añadir categoría
    frecuencia[, Categoria := sapply(keyword, function(kw) {
      cat_encontrada <- NA
      for (cat in names(lista_keywords)) {
        if (kw %in% lista_keywords[[cat]]) {
          cat_encontrada <- cat
          break
        }
      }
      cat_encontrada
    })]
    
    datatable(frecuencia,
              options = list(pageLength = 20, scrollX = TRUE),
              rownames = FALSE,
              filter = "top")
  })
  
  # 17. Tabla: Keywords por publicación
  output$tabla_keywords_publicacion <- renderDT({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(datatable(data.frame()))
    }
    
    tabla <- datos[, .(Ano = anio,
                       Titulo = titulo,
                       Revista = publicacion,
                       Cuartil = cuartil_clean,
                       Keywords = keywords_oficiales,
                       Categorias = categorias_tematicas)]
    
    datatable(tabla,
              options = list(pageLength = 20, scrollX = TRUE,
                             columnDefs = list(
                               list(width = '300px', targets = 1)  # Ancho fijo para título
                             )),
              rownames = FALSE,
              filter = "top") %>%
      formatStyle("Cuartil",
                  backgroundColor = styleEqual(
                    c("Q1", "Q2", "Q3", "Q4", "Sin cuartil", "No especificado"),
                    c("#d4edda", "#fff3cd", "#f8d7da", "#f5c6cb", "#e8e8e8", "#f8f9fa")
                  ))
  })
  
  # 18. Tabla: Autores
  output$tabla_autores <- renderDT({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(datatable(data.frame()))
    }
    
    # Depuración: ver qué autores tenemos
    cat("Total de registros:", nrow(datos), "\n")
    cat("Autor1 no nulos:", sum(!is.na(datos$autor1) & datos$autor1 != ""), "\n")
    cat("Autor2 no nulos:", sum(!is.na(datos$autor2) & datos$autor2 != ""), "\n")
    
    if (sum(!is.na(datos$autor1) & datos$autor1 != "") == 0) {
      # Si no hay autores extraídos, intentar extraer directamente
      cat("Intentando extraer autores directamente...\n")
      
      # Crear una lista para almacenar todos los autores
      todos_autores <- c()
      
      for (i in 1:nrow(datos)) {
        cita <- datos$cita[i]
        if (!is.na(cita) && cita != "") {
          # Intentar extraer el primer autor
          partes <- unlist(strsplit(cita, ","))
          if (length(partes) > 0) {
            primer_autor <- trimws(partes[1])
            # Limpiar
            primer_autor <- gsub("et al\\..*", "", primer_autor, ignore.case = TRUE)
            primer_autor <- gsub(";.*", "", primer_autor)
            primer_autor <- gsub("\\.$", "", primer_autor)
            
            if (primer_autor != "") {
              todos_autores <- c(todos_autores, primer_autor)
            }
          }
        }
      }
      
      if (length(todos_autores) > 0) {
        frecuencia_autores <- data.table(autor = todos_autores)[
          , .(Frecuencia = .N), by = autor][order(-Frecuencia)]
        
        # Crear tabla con los top 10
        top_autores <- frecuencia_autores[1:min(10, .N)]
        
        datatable(top_autores,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  colnames = c("Autor", "Frecuencia"))
      } else {
        datatable(data.frame(Mensaje = "No se pudieron extraer autores de las citas"))
      }
    } else {
      # Usar los autores ya extraídos
      autores_data <- rbind(
        datos[!is.na(autor1) & autor1 != "", .(autor = autor1)],
        datos[!is.na(autor2) & autor2 != "", .(autor = autor2)]
      )
      
      if (nrow(autores_data) > 0) {
        autores_summary <- autores_data[, .(
          Frecuencia = .N
        ), by = autor][order(-Frecuencia)][1:10]
        
        datatable(autores_summary,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  colnames = c("Autor", "Frecuencia"))
      } else {
        datatable(data.frame(Mensaje = "No hay datos de autores disponibles"))
      }
    }
  })
  
  # 19. Tabla: Autores combinada - Frecuencia como primer autor y frecuencia total
  output$tabla_autores_combinada <- renderDT({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos disponibles")))
    }
    
    # Obtener todos los autores únicos que han sido primer autor
    autores_primer_lista <- datos$autor1[!is.na(datos$autor1) & datos$autor1 != ""]
    
    if (length(autores_primer_lista) == 0) {
      return(datatable(data.frame(Mensaje = "No hay autores que hayan sido primer autor")))
    }
    
    autores_primer_unicos <- unique(autores_primer_lista)
    
    # Calcular frecuencia como primer autor
    frecuencia_primer <- data.table(
      autor = autores_primer_lista
    )[, .(Frecuencia_Primer_Autor = .N), by = autor]
    
    # Calcular frecuencia total (autor1 + autor2)
    autores_total <- data.table(
      autor = c(datos$autor1[!is.na(datos$autor1) & datos$autor1 != ""],
                datos$autor2[!is.na(datos$autor2) & datos$autor2 != ""])
    )
    
    # Filtrar solo autores que han sido primer autor al menos 1 vez
    autores_total <- autores_total[autor %in% autores_primer_unicos]
    
    frecuencia_total <- autores_total[, .(Frecuencia_Total = .N), by = autor]
    
    # Combinar ambas frecuencias
    tabla_combinada <- merge(frecuencia_primer, frecuencia_total, by = "autor", all.x = TRUE)
    
    # Calcular diferencia
    tabla_combinada[, Diferencia := Frecuencia_Total - Frecuencia_Primer_Autor]
    
    # Ordenar por frecuencia como primer autor (descendente)
    tabla_combinada <- tabla_combinada[order(-Frecuencia_Primer_Autor)]
    
    datatable(tabla_combinada,
              options = list(
                pageLength = 20, 
                scrollX = TRUE,
                scrollY = "400px",
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
              ),
              extensions = 'Buttons',
              rownames = FALSE,
              colnames = c("Autor", "Frecuencia como Primer Autor", 
                           "Frecuencia Total (A1+A2)", "Diferencia"),
              caption = "Frecuencia de autores (solo aquellos que han sido primer autor al menos 1 vez)",
              class = 'display nowrap') %>%
      formatStyle("Frecuencia_Primer_Autor",
                  background = styleColorBar(range(tabla_combinada$Frecuencia_Primer_Autor), '#f28e2b'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle("Frecuencia_Total",
                  background = styleColorBar(range(tabla_combinada$Frecuencia_Total), '#4e79a7'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle("Diferencia",
                  background = styleColorBar(range(tabla_combinada$Diferencia), '#59a14f'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
  })
  
  # 20a. Gráfico: Top autores por frecuencia total
  output$plot_top_autores_total <- renderPlotly({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% layout(title = "No hay datos disponibles"))
    }
    
    # Combinar autor1 y autor2 para frecuencia total
    autores_total <- data.table(
      autor = c(datos$autor1[!is.na(datos$autor1) & datos$autor1 != ""],
                datos$autor2[!is.na(datos$autor2) & datos$autor2 != ""])
    )
    
    if (nrow(autores_total) == 0) {
      return(plotly_empty() %>% layout(title = "No se pudieron extraer autores"))
    }
    
    # Calcular frecuencia total y desglose
    frecuencia_autores <- autores_total[, .(
      n = .N,
      como_primer_autor = sum(autor %in% datos$autor1[!is.na(datos$autor1) & datos$autor1 != ""]),
      como_segundo_autor = sum(autor %in% datos$autor2[!is.na(datos$autor2) & datos$autor2 != ""])
    ), by = autor]
    
    # Ordenar por frecuencia total y tomar top 15
    top_autores <- frecuencia_autores[order(-n)]
    
    # Crear datos en formato largo para gráfico apilado
    top_autores_long <- melt(top_autores, 
                             id.vars = "autor", 
                             measure.vars = c("como_primer_autor", "como_segundo_autor"),
                             variable.name = "tipo_autor",
                             value.name = "frecuencia")
    
    top_autores_long[, tipo_autor := ifelse(tipo_autor == "como_primer_autor", 
                                            "Primer Autor", "Segundo Autor")]
    
    # Ordenar autores por frecuencia total
    orden_autores <- top_autores[order(-n)]$autor
    top_autores_long[, autor := factor(autor, levels = orden_autores)]
    
    p <- ggplot(top_autores_long, aes(x = autor, y = frecuencia, fill = tipo_autor,
                                      text = paste("Autor:", autor,
                                                   "<br>Tipo:", tipo_autor,
                                                   "<br>Frecuencia:", frecuencia))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Primer Autor" = "#4e79a7", 
                                   "Segundo Autor" = "#f28e2b")) +
      coord_flip() +
      labs(title = "Top 15 Autores (Frecuencia Total - Autor1 + Autor2)",
           x = "Autor", y = "Frecuencia Total",
           fill = "Rol") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            axis.text.y = element_text(size = 9))
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.5, y = -0.2),
             hoverlabel = list(bgcolor = "white"))
  })
  
  
  
  # 20b. Gráfico de comparación: Frecuencia total vs Frecuencia como primer autor
  output$plot_comparacion_autores <- renderPlotly({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% layout(title = "No hay datos disponibles"))
    }
    
    # Reutilizar la misma lógica que la tabla combinada
    autores_primer_lista <- datos$autor1[!is.na(datos$autor1) & datos$autor1 != ""]
    
    if (length(autores_primer_lista) == 0) {
      return(plotly_empty() %>% layout(title = "No hay autores que hayan sido primer autor"))
    }
    
    autores_primer_unicos <- unique(autores_primer_lista)
    
    # Calcular frecuencia como primer autor
    frecuencia_primer <- data.table(
      autor = autores_primer_lista
    )[, .(Frecuencia_Primer = .N), by = autor]
    
    # Calcular frecuencia total (autor1 + autor2)
    autores_total <- data.table(
      autor = c(datos$autor1[!is.na(datos$autor1) & datos$autor1 != ""],
                datos$autor2[!is.na(datos$autor2) & datos$autor2 != ""])
    )
    
    autores_total <- autores_total[autor %in% autores_primer_unicos]
    
    frecuencia_total <- autores_total[, .(Frecuencia_Total = .N), by = autor]
    
    # Combinar
    comparacion <- merge(frecuencia_primer, frecuencia_total, by = "autor", all.x = TRUE)
    
    # Ordenar por frecuencia como primer autor y tomar top 20
    top_comparacion <- comparacion[order(-Frecuencia_Primer)][1:min(20, nrow(comparacion))]
    
    # Crear datos en formato largo
    comparacion_long <- melt(top_comparacion,
                             id.vars = "autor",
                             measure.vars = c("Frecuencia_Total", "Frecuencia_Primer"),
                             variable.name = "Metrica",
                             value.name = "Frecuencia")
    
    comparacion_long[, Metrica := ifelse(Metrica == "Frecuencia_Total", 
                                         "Total (A1+A2)", "Primer Autor (A1)")]
    
    # Ordenar autores por frecuencia como primer autor
    orden_autores <- top_comparacion[order(-Frecuencia_Primer)]$autor
    comparacion_long[, autor := factor(autor, levels = orden_autores)]
    
    p <- ggplot(comparacion_long, aes(x = autor, y = Frecuencia, fill = Metrica,
                                      text = paste("Autor:", autor,
                                                   "<br>Métrica:", Metrica,
                                                   "<br>Frecuencia:", Frecuencia))) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      scale_fill_manual(values = c("Total (A1+A2)" = "#4e79a7", 
                                   "Primer Autor (A1)" = "#f28e2b")) +
      coord_flip() +
      labs(title = "Comparación: Frecuencia Total vs Primer Autor (Top 20 por A1)",
           x = "Autor", y = "Frecuencia",
           fill = "Métrica") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            axis.text.y = element_text(size = 9))
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.5, y = -0.15),
             hoverlabel = list(bgcolor = "white"),
             margin = list(b = 100))
  })
  
  # 21. Tabla: Revistas
  output$tabla_revistas <- renderDT({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(datatable(data.frame()))
    }
    
    revistas_summary <- datos[, .(
      Publicaciones = .N,
      `% Q1` = round(100 * sum(cuartil_clean == "Q1", na.rm = TRUE) / .N, 1),
      Ultimo_Ano = max(anio, na.rm = TRUE),
      Keywords_Principales = {
        keywords <- unlist(strsplit(keywords_oficiales[!is.na(keywords_oficiales) & keywords_oficiales != ""], "; "))
        if (length(keywords) > 0) {
          top <- names(sort(table(keywords), decreasing = TRUE))[1:min(3, length(unique(keywords)))]
          paste(top, collapse = ", ")
        } else {
          "-"
        }
      }
    ), by = publicacion][order(-Publicaciones)][1:10]
    
    datatable(revistas_summary,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # 22. Gráfico: Top revistas
  output$plot_top_revistas <- renderPlotly({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% layout(title = "No hay datos disponibles"))
    }
    
    top_revistas <- datos[, .(n = .N), by = publicacion][order(-n)][1:10]
    
    p <- ggplot(top_revistas, aes(x = reorder(publicacion, n), y = n)) +
      geom_bar(stat = "identity", fill = "#f28e2b") +
      coord_flip() +
      labs(title = "Top 10 Revistas",
           x = "Revista", y = "Numero de Publicaciones") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  # 23. Gráfico: Evolución de calidad global (Media de Cuartil por Año) - MODIFICADO
  output$plot_cuartil_tematico <- renderPlotly({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(plotly_empty() %>% layout(title = "No hay datos disponibles"))
    }
    
    # Calcular la media del valor de cuartil por año para todos los datos filtrados
    calidad_global <- datos[, .(
      media_cuartil = mean(valor_cuartil, na.rm = TRUE),
      n_publicaciones = .N,
      n_q1 = sum(cuartil_clean == "Q1", na.rm = TRUE),
      porcentaje_q1 = round(100 * sum(cuartil_clean == "Q1", na.rm = TRUE) / .N, 1)
    ), by = anio][order(anio)]
    
    # Verificar que tenemos al menos 2 puntos para la línea de tendencia
    if (nrow(calidad_global) < 2) {
      return(plotly_empty() %>% layout(title = "No hay suficientes años para mostrar tendencia"))
    }
    
    # Crear el gráfico
    p <- ggplot(calidad_global, aes(x = anio, y = media_cuartil)) +
      geom_line(color = "#4e79a7", size = 1.5, alpha = 0.8) +
      geom_point(aes(size = n_publicaciones, color = porcentaje_q1), alpha = 0.8) +
      geom_smooth(method = "lm", se = TRUE, color = "#e15759", linetype = "dashed", 
                  size = 0.8, alpha = 0.2, fill = "#e15759") +
      scale_color_gradient(low = "#f28e2b", high = "#59a14f", 
                           name = "% Q1") +
      scale_size_continuous(range = c(3, 10), name = "N Publicaciones") +
      labs(title = "Evolucion de Calidad Global (Media de Cuartil por Ano)",
           subtitle = "Los filtros aplicados afectan los datos mostrados",
           x = "Ano", y = "Media de Valor de Cuartil") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14),
            plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray50"),
            legend.position = "right",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(0, 4), breaks = 0:4, 
                         labels = c("0: Sin/N.E.", "1: Q4", "2: Q3", "3: Q2", "4: Q1")) +
      scale_x_continuous(breaks = seq(min(calidad_global$anio), max(calidad_global$anio), by = 1))
    
    # Añadir etiquetas con información detallada
    p <- p + geom_text(data = calidad_global, 
                       aes(label = paste0("n=", n_publicaciones, "\n", porcentaje_q1, "% Q1")),
                       vjust = -1.5, size = 2.5, color = "gray30")
    
    # Convertir a plotly
    ggplotly(p, tooltip = c("x", "y", "n_publicaciones", "porcentaje_q1")) %>%
      layout(hoverlabel = list(bgcolor = "white"),
             legend = list(orientation = "v", x = 1.05, y = 0.5),
             annotations = list(
               x = 0.5, y = -0.2,
               text = "Nota: Valor de cuartil - 4:Q1 (Excelente), 3:Q2, 2:Q3, 1:Q4, 0:Sin cuartil/No especificado",
               showarrow = FALSE,
               xref = "paper", yref = "paper",
               xanchor = "center", yanchor = "top",
               font = list(size = 10, color = "gray50")
             ))
  })
  
  # 24. Estadísticas de cuartiles
  output$estadisticas_cuartiles <- renderText({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return("No hay datos disponibles")
    }
    
    total_pubs <- nrow(datos)
    pubs_q1 <- nrow(datos[cuartil_clean == "Q1"])
    porc_q1 <- round(100 * pubs_q1 / total_pubs, 1)
    
    # Calcular media del valor de cuartil
    media_cuartil <- mean(datos$valor_cuartil, na.rm = TRUE)
    
    # Calcular tendencia en últimos 3 años
    ultimos_3_anos <- datos[anio >= max(anio, na.rm = TRUE) - 2]
    if (nrow(ultimos_3_anos) > 0) {
      porc_q1_3anos <- round(100 * sum(ultimos_3_anos$cuartil_clean == "Q1", na.rm = TRUE) / nrow(ultimos_3_anos), 1)
      media_cuartil_3anos <- mean(ultimos_3_anos$valor_cuartil, na.rm = TRUE)
      
      tendencia_q1 <- ifelse(porc_q1_3anos > porc_q1, "MEJORANDO", 
                             ifelse(porc_q1_3anos < porc_q1, "BAJANDO", "ESTABLE"))
      tendencia_media <- ifelse(media_cuartil_3anos > media_cuartil, "MEJORANDO",
                                ifelse(media_cuartil_3anos < media_cuartil, "BAJANDO", "ESTABLE"))
    } else {
      tendencia_q1 <- "NO HAY DATOS RECIENTES"
      tendencia_media <- "NO HAY DATOS RECIENTES"
    }
    
    # Calcular rango de años con datos
    anos_con_datos <- unique(datos$anio)
    
    paste(
      "ESTADISTICAS DE CALIDAD:\n",
      "=======================\n",
      "Total publicaciones: ", total_pubs, "\n",
      "Rango de años con datos: ", min(anos_con_datos), "-", max(anos_con_datos), "\n",
      "Publicaciones en Q1: ", pubs_q1, " (", porc_q1, "%)\n",
      "Media de valor de cuartil: ", round(media_cuartil, 2), "\n\n",
      "TENDENCIAS (ultimos 3 años):\n",
      "===========================\n",
      "Tendencia % Q1: ", tendencia_q1, "\n",
      "Tendencia media cuartil: ", tendencia_media, "\n\n",
      "ESCALA DE VALORES DE CUARTIL:\n",
      "=============================\n",
      "4 = Q1 (Excelente)\n",
      "3 = Q2 (Muy bueno)\n",
      "2 = Q3 (Bueno)\n",
      "1 = Q4 (Aceptable)\n",
      "0 = Sin cuartil / No especificado\n\n",
      "INTERPRETACION:\n",
      "===============\n",
      ifelse(media_cuartil >= 3, 
             "Calidad global: EXCELENTE (>3.0)",
             ifelse(media_cuartil >= 2,
                    "Calidad global: BUENA (2.0-3.0)",
                    "Calidad global: MEJORABLE (<2.0)"))
    )
  })
  
  # 25. Tabla: Cuartiles por keyword
  output$tabla_cuartiles_keywords <- renderDT({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(datatable(data.frame()))
    }
    
    keywords_data <- datos[!is.na(keywords_oficiales) & keywords_oficiales != ""]
    
    if (nrow(keywords_data) == 0) {
      return(datatable(data.frame()))
    }
    
    # Analizar calidad por keyword
    calidad_keywords <- rbindlist(lapply(unique(unlist(strsplit(keywords_data$keywords_oficiales, "; "))), function(kw) {
      pubs_kw <- keywords_data[
        sapply(keywords_oficiales, function(x) {
          if (is.na(x) || x == "") return(FALSE)
          kw %in% strsplit(x, "; ")[[1]]
        })
      ]
      
      if (nrow(pubs_kw) >= 2) {  # Bajar el umbral a 2 para más keywords
        porc_q1 <- round(100 * sum(pubs_kw$cuartil_clean == "Q1", na.rm = TRUE) / nrow(pubs_kw), 1)
        media_valor <- round(mean(pubs_kw$valor_cuartil, na.rm = TRUE), 2)
        data.table(Keyword = kw,
                   Publicaciones = nrow(pubs_kw),
                   `%_Q1` = porc_q1,
                   `Media_Cuartil` = media_valor,
                   Ultimo_Ano = max(pubs_kw$anio, na.rm = TRUE))
      }
    }), fill = TRUE)
    
    calidad_keywords <- calidad_keywords[!is.na(Keyword)][order(-`Media_Cuartil`, -Publicaciones)]
    
    datatable(calidad_keywords,
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle("Media_Cuartil",
                  backgroundColor = styleInterval(
                    c(0, 1, 2, 3),
                    c("#f8d7da", "#ffeeba", "#fff3cd", "#d4edda", "#c3e6cb")
                  )) %>%
      formatStyle("%_Q1",
                  backgroundColor = styleInterval(
                    c(0, 25, 50, 75),
                    c("#f8d7da", "#ffeeba", "#fff3cd", "#d4edda", "#c3e6cb")
                  ))
  })
  
  # 26. Tabla completa
  output$tabla_completa <- renderDT({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(datatable(data.frame()))
    }
    
    datatable(datos[, .(Ano = anio,
                        Titulo = titulo,
                        Revista = publicacion,
                        Cuartil = cuartil_clean,
                        Keywords_Oficiales = keywords_oficiales,
                        Categorias = categorias_tematicas,
                        `1er_Autor` = autor1,
                        `2do_Autor` = autor2)],
              options = list(pageLength = 20, 
                             scrollX = TRUE,
                             scrollY = "500px",
                             columnDefs = list(
                               list(width = '300px', targets = 1),  # Título
                               list(width = '150px', targets = 2),  # Revista
                               list(width = '200px', targets = 4)   # Keywords
                             )),
              rownames = FALSE,
              filter = "top") %>%
      formatStyle("Cuartil",
                  backgroundColor = styleEqual(
                    c("Q1", "Q2", "Q3", "Q4", "Sin cuartil", "No especificado"),
                    c("#d4edda", "#fff3cd", "#f8d7da", "#f5c6cb", "#e8e8e8", "#f8f9fa")
                  ))
  })
  
  # 27. Estadísticas clave
  output$estadisticas_clave <- renderText({
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return("No hay datos disponibles para los filtros seleccionados.")
    }
    
    total_pubs <- nrow(datos)
    anos_cubiertos <- paste(min(datos$anio, na.rm = TRUE), 
                            "-", max(datos$anio, na.rm = TRUE))
    pubs_q1 <- nrow(datos[cuartil_clean == "Q1"])
    porc_q1 <- round(100 * pubs_q1 / total_pubs, 1)
    media_cuartil <- round(mean(datos$valor_cuartil, na.rm = TRUE), 2)
    
    # Contar keywords únicas
    keywords_unicas <- unique(unlist(strsplit(datos$keywords_oficiales[!is.na(datos$keywords_oficiales) & datos$keywords_oficiales != ""], "; ")))
    
    # Enfermedades más estudiadas
    enfermedades <- keywords_unicas[keywords_unicas %in% lista_keywords$Enfermedades]
    top_enfermedades <- if (length(enfermedades) > 0) {
      freq <- table(enfermedades)
      paste(names(sort(freq, decreasing = TRUE))[1:min(3, length(freq))], collapse = ", ")
    } else {
      "Ninguna"
    }
    
    # Cohortes más utilizadas
    cohortes <- keywords_unicas[keywords_unicas %in% lista_keywords$Bases_Datos]
    top_cohortes <- if (length(cohortes) > 0) {
      freq <- table(cohortes)
      paste(names(sort(freq, decreasing = TRUE))[1:min(3, length(freq))], collapse = ", ")
    } else {
      "Ninguna"
    }
    
    paste(
      "ESTADISTICAS DEL PERIODO SELECCIONADO:\n",
      "=====================================\n",
      "Total de publicaciones: ", total_pubs, "\n",
      "Período: ", anos_cubiertos, "\n",
      "Publicaciones en Q1: ", pubs_q1, " (", porc_q1, "%)\n",
      "Media de valor de cuartil: ", media_cuartil, "\n",
      "Keywords oficiales distintas: ", length(keywords_unicas), "\n\n",
      "LINEAS DE INVESTIGACION PRINCIPALES:\n",
      "===================================\n",
      "Enfermedades más estudiadas: ", top_enfermedades, "\n",
      "Cohortes más utilizadas: ", top_cohortes, "\n",
      "Último año con publicaciones: ", max(datos$anio, na.rm = TRUE), "\n",
      "Publicaciones en el último año: ", nrow(datos[anio == max(anio, na.rm = TRUE)]), "\n\n",
      "RECOMENDACIONES:\n",
      "===============\n",
      ifelse(media_cuartil >= 3, 
             "✅ Excelente calidad - Mantener estrategia actual",
             ifelse(media_cuartil >= 2,
                    "⚠️  Calidad aceptable - Considerar mejorar targeting de revistas",
                    "❌ Calidad baja - Revisar estrategia de publicación"))
    )
  })
}

# 10. EJECUTAR LA APLICACION
shinyApp(ui = ui, server = server)