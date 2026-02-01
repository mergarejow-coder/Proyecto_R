
#               Diploma de Desarrollo Profesional y Laboral 
#                  en Ciencia de Datos para las Finanzas
#                     CENTRO DESARROLLO GERENCIAL
#
#                                MODULO 
#                             Finanzas en R

#                  Wilfredo Antonio Mergarejo Aponte
#
#===============================================================
# SISTEMA DE GESTIÓN DE LIQUIDEZ A CORTO PLAZO (MVP + MODO MANUAL)
#===============================================================

# INSTALACIÓN DE PAQUETES

# install.packages(c("tidyverse","lubridate","readxl","writexl","janitor","shiny","DT"))

library(shiny)
library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)
library(janitor)
library(DT)

#==============================================================
# FUNCIONES DEL MODELO
#==============================================================

#En este bloque se definen las funciones centrales del modelo de gestión de 
#liquidez, para validar la estructura de los datos, limpiarlos 
#y transformarlos, así como proyectar el flujo de caja a corto plazo y generar 
#recomendaciones financieras automáticas.

   #Validación de estructura 

validate_required_cols <- function(df, required, df_name = "dataframe") {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(sprintf(
      "En '%s' faltan columnas obligatorias: %s",
      df_name, paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  df
}
  #Limpieza de datos

clean_transform <- function(saldo, cobros, pagos) {
  
  saldo <- saldo %>%
    clean_names() %>%
    validate_required_cols(c("fecha","saldo"), "saldo") %>%
    mutate(
      fecha = as_date(fecha),
      saldo = suppressWarnings(as.numeric(saldo))
    ) %>%
    filter(!is.na(fecha), !is.na(saldo)) %>%
    arrange(fecha)
  
  cobros <- cobros %>%
    clean_names() %>%
    validate_required_cols(c("fecha","monto","cliente","estado"), "cobros") %>%
    mutate(
      fecha = as_date(fecha),
      monto = suppressWarnings(as.numeric(monto)),
      cliente = as.character(cliente),
      estado = str_to_upper(as.character(estado))
    ) %>%
    filter(!is.na(fecha), !is.na(monto)) %>%
    mutate(monto = if_else(monto < 0, 0, monto))
  
  pagos <- pagos %>%
    clean_names() %>%
    validate_required_cols(c("fecha","monto","categoria","criticidad","estado"), "pagos") %>%
    mutate(
      fecha = as_date(fecha),
      monto = suppressWarnings(as.numeric(monto)),
      categoria = as.character(categoria),
      criticidad = str_to_lower(as.character(criticidad)),
      estado = str_to_upper(as.character(estado))
    ) %>%
    filter(!is.na(fecha), !is.na(monto)) %>%
    mutate(monto = if_else(monto < 0, 0, monto))
  
  list(saldo = saldo, cobros = cobros, pagos = pagos)
}

     #Proyección de flujo de caja diaria

project_cash <- function(saldo_inicial, cobros, pagos, horizonte = 30, umbral = 800000) {
  hoy <- Sys.Date()
  fechas <- tibble(fecha = seq(hoy, hoy + horizonte, by = "day"))
  
  flujo <- fechas %>%
    left_join(
      cobros %>%
        filter(estado != "PAGADO") %>%
        group_by(fecha) %>%
        summarise(cobros = sum(monto), .groups = "drop"),
      by = "fecha"
    ) %>%
    left_join(
      pagos %>%
        filter(estado != "PAGADO") %>%
        group_by(fecha) %>%
        summarise(pagos = sum(monto), .groups = "drop"),
      by = "fecha"
    ) %>%
    mutate(
      cobros = replace_na(cobros, 0),
      pagos  = replace_na(pagos, 0),
      flujo_neto = cobros - pagos,
      saldo = saldo_inicial + cumsum(flujo_neto),
      semaforo = case_when(
        saldo < 0 ~ "ROJO",
        saldo < umbral ~ "AMARILLO",
        TRUE ~ "VERDE"
      )
    )
  
  flujo
}
    #Decisiones financieras

recommend_actions <- function(flujo, cobros, pagos, umbral = 800000, ventana = 15) {
  hoy <- Sys.Date()
  peor <- flujo %>% slice_min(saldo, n = 1)
  deficit <- max(0, umbral - peor$saldo)
  
  cobranza_prioritaria <- cobros %>%
    filter(estado != "PAGADO", fecha <= hoy + ventana) %>%
    arrange(desc(monto), fecha)
  
  pagos_reprogramables <- pagos %>%
    filter(estado != "PAGADO", criticidad == "baja", fecha <= hoy + ventana) %>%
    arrange(desc(monto), fecha)
  
  sugerencia_linea <- tibble(
    fecha_critica = peor$fecha,
    saldo_minimo_proyectado = peor$saldo,
    umbral = umbral,
    deficit_estimado = deficit,
    recomendacion = if_else(
      deficit > 0,
      "Se recomienda acelerar cobranza y/o evaluar uso parcial de línea de crédito.",
      "No se requiere línea de crédito según la proyección actual."
    )
  )
  
  list(
    peor_dia = peor,
    cobranza_prioritaria = cobranza_prioritaria,
    pagos_reprogramables = pagos_reprogramables,
    sugerencia_linea = sugerencia_linea
  )
}

     #Entrega ejecutiva del análisis

export_report <- function(flujo, recs, file_out) {
  write_xlsx(
    list(
      "Proyeccion_Caja" = flujo,
      "Peor_Dia" = recs$peor_dia,
      "Cobranza_Prioritaria" = recs$cobranza_prioritaria,
      "Pagos_Reprogramables" = recs$pagos_reprogramables,
      "Sugerencia_Linea" = recs$sugerencia_linea
    ),
    path = file_out
  )
}

#============================================================
# INTERFAZ DEL USUARIO (UI)
#============================================================

# Este bloque me permite ingresar datos mediante carga de archivos Excel o ingreso manual, 
# configurar parámetros de análisis y ejecutar el modelo de liquidez. 
# Además, presenta de forma visual los resultados, alertas y tablas clave.


ui <- fluidPage(
  titlePanel("Sistema de Gestión de Liquidez de Corto Plazo (MVP)"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "modo",
        "Fuente de datos",
        choices = c("Excel" = "excel", "Manual" = "manual"),
        selected = "excel",
        inline = TRUE
      ),
      
      
  # MODO EXCEL
    
      conditionalPanel(
        condition = "input.modo == 'excel'",
        fileInput("file_saldo", "Subir saldo.xlsx", accept = c(".xlsx",".xls")),
        fileInput("file_cobros", "Subir cobros.xlsx", accept = c(".xlsx",".xls")),
        fileInput("file_pagos",  "Subir pagos.xlsx",  accept = c(".xlsx",".xls")),
        tags$hr()
      ),
      
  # MODO MANUAL
      
      conditionalPanel(
        condition = "input.modo == 'manual'",
        numericInput("saldo_manual", "Saldo inicial (hoy)", value = 10000000, min = 0, step = 50000),
        tags$hr(),
        h4("Cobros (editable)"),
        DTOutput("dt_cobros"),
        fluidRow(
          column(6, actionButton("add_cobro", "Agregar fila cobro")),
          column(6, actionButton("del_cobro", "Borrar fila seleccionada", class = "btn-danger"))
        ),
        tags$hr(),
        h4("Pagos (editable)"),
        DTOutput("dt_pagos"),
        fluidRow(
          column(6, actionButton("add_pago", "Agregar fila pago")),
          column(6, actionButton("del_pago", "Borrar fila seleccionada", class = "btn-danger"))
        ),
        tags$hr()
      ),
      
      numericInput("umbral", "Umbral mínimo de liquidez", value = 8000000, min = 0, step = 50000),
      selectInput("horizonte", "Horizonte de proyección (días)", choices = c(7,14,30), selected = 30),
      numericInput("ventana", "Ventana para recomendaciones (días)", value = 15, min = 1, step = 1),
      tags$hr(),
      actionButton("run", "Ejecutar análisis", class = "btn-primary"),
      br(), br(),
      downloadButton("download_report", "Descargar reporte Excel")
    ),
    
    mainPanel(
      uiOutput("status_box"),
      plotOutput("plot_saldo", height = "300px"),
      tags$hr(),
      h4("Peor día (mínimo saldo proyectado)"),
      tableOutput("tbl_peor"),
      tags$hr(),
      h4("Cobranza prioritaria"),
      tableOutput("tbl_cobranza"),
      tags$hr(),
      h4("Pagos reprogramables (criticidad baja)"),
      tableOutput("tbl_pagos_rep"),
      tags$hr(),
      h4("Sugerencia de línea de crédito"),
      tableOutput("tbl_linea")
    )
  )
)

#============================================================
# SERVIDOR
#============================================================

# Aquí se conecta la interfaz de usuario con la lógica del modelo, 
# permitiendo la ejecución del análisis, la validación de datos y el 
# control de errores. Administra las reacciones que almacenan 
# resultados y actualiza las tablas y reportes.

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    ready = FALSE,
    flujo = NULL,
    recs = NULL,
    msg = NULL,
    cobros_manual = NULL,
    pagos_manual = NULL
  )
  
  # Datos por defecto (Manual)
  
  hoy <- Sys.Date()
  default_cobros <- tibble(
    fecha = hoy,
    monto = 0,
    cliente = "Cliente X",
    estado = "PENDIENTE"
  )
  default_pagos <- tibble(
    fecha = hoy,
    monto = 0,
    categoria = "Proveedor",
    criticidad = "baja",
    estado = "PENDIENTE"
  )
  
  rv$cobros_manual <- default_cobros
  rv$pagos_manual  <- default_pagos
  
  # Tablas editables (Manual) + selección de filas
  
  output$dt_cobros <- renderDT({
    datatable(
      rv$cobros_manual,
      editable = TRUE,
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = 5, dom = "tip")
    )
  })
  
  output$dt_pagos <- renderDT({
    datatable(
      rv$pagos_manual,
      editable = TRUE,
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = 5, dom = "tip")
    )
  })
  
  # Capturar ediciones COBROS (FIX: col 0-based -> +1)
  
  observeEvent(input$dt_cobros_cell_edit, {
    req(input$modo == "manual")
    info <- input$dt_cobros_cell_edit
    req(info)
    
    i <- info$row
    j <- info$col + 1
    v <- info$value
    
    df <- rv$cobros_manual
    colname <- names(df)[j]
    
    if (colname == "fecha")  v <- as.Date(v)
    if (colname == "monto")  v <- suppressWarnings(as.numeric(v))
    if (colname == "estado") v <- toupper(as.character(v))
    
    df[i, j] <- v
    rv$cobros_manual <- df
  }, ignoreNULL = TRUE)
  
  # Capturar ediciones PAGOS (FIX: col 0-based -> +1)
  
  observeEvent(input$dt_pagos_cell_edit, {
    req(input$modo == "manual")
    info <- input$dt_pagos_cell_edit
    req(info)
    
    i <- info$row
    j <- info$col + 1
    v <- info$value
    
    df <- rv$pagos_manual
    colname <- names(df)[j]
    
    if (colname == "fecha")       v <- as.Date(v)
    if (colname == "monto")       v <- suppressWarnings(as.numeric(v))
    if (colname == "criticidad")  v <- tolower(as.character(v))
    if (colname == "estado")      v <- toupper(as.character(v))
    
    df[i, j] <- v
    rv$pagos_manual <- df
  }, ignoreNULL = TRUE)
  
  # Agregar filas
  
  observeEvent(input$add_cobro, {
    rv$cobros_manual <- bind_rows(rv$cobros_manual, default_cobros)
  })
  
  observeEvent(input$add_pago, {
    rv$pagos_manual <- bind_rows(rv$pagos_manual, default_pagos)
  })
  
  # Borrar fila seleccionada (Cobros)
  
  observeEvent(input$del_cobro, {
    req(input$modo == "manual")
    sel <- input$dt_cobros_rows_selected
    if (length(sel) == 0) {
      showNotification("Selecciona una fila en Cobros para borrarla.", type = "warning")
      return()
    }
    df <- rv$cobros_manual
    df <- df[-sel, , drop = FALSE]
    if (nrow(df) == 0) df <- default_cobros
    rv$cobros_manual <- df
  })
  
  # Borrar fila seleccionada (Pagos)
  
  observeEvent(input$del_pago, {
    req(input$modo == "manual")
    sel <- input$dt_pagos_rows_selected
    if (length(sel) == 0) {
      showNotification("Selecciona una fila en Pagos para borrarla.", type = "warning")
      return()
    }
    df <- rv$pagos_manual
    df <- df[-sel, , drop = FALSE]
    if (nrow(df) == 0) df <- default_pagos
    rv$pagos_manual <- df
  })
  
  # Ejecutar análisis
  
  observeEvent(input$run, {
    tryCatch({
      
      # MODO EXCEL
      
      if (input$modo == "excel") {
        req(input$file_saldo, input$file_cobros, input$file_pagos)
        
        saldo <- read_excel(input$file_saldo$datapath)
        cobros <- read_excel(input$file_cobros$datapath)
        pagos  <- read_excel(input$file_pagos$datapath)
        
        df <- clean_transform(saldo, cobros, pagos)
        
        saldo_inicial <- df$saldo %>%
          arrange(desc(fecha)) %>%
          slice(1) %>%
          pull(saldo)
        
        cobros_use <- df$cobros
        pagos_use  <- df$pagos
      }
      
      # MODO MANUAL
      
      if (input$modo == "manual") {
        req(input$saldo_manual)
        
        saldo_inicial <- as.numeric(input$saldo_manual)
        
        saldo <- tibble(fecha = Sys.Date(), saldo = saldo_inicial)
        cobros <- rv$cobros_manual
        pagos  <- rv$pagos_manual
        
        df <- clean_transform(saldo, cobros, pagos)
        cobros_use <- df$cobros
        pagos_use  <- df$pagos
      }
      
      flujo <- project_cash(
        saldo_inicial = saldo_inicial,
        cobros = cobros_use,
        pagos  = pagos_use,
        horizonte = as.integer(input$horizonte),
        umbral = as.numeric(input$umbral)
      )
      
      recs <- recommend_actions(
        flujo = flujo,
        cobros = cobros_use,
        pagos  = pagos_use,
        umbral = as.numeric(input$umbral),
        ventana = as.integer(input$ventana)
      )
      
      rv$flujo <- flujo
      rv$recs <- recs
      rv$ready <- TRUE
      rv$msg <- "OK"
      
    }, error = function(e) {
      rv$ready <- FALSE
      rv$flujo <- NULL
      rv$recs <- NULL
      rv$msg <- e$message
    })
  })
  
  output$status_box <- renderUI({
    if (isTRUE(rv$ready)) {
      worst <- rv$flujo$semaforo
      sem_global <- if ("ROJO" %in% worst) "ROJO" else if ("AMARILLO" %in% worst) "AMARILLO" else "VERDE"
      
      tagList(
        h3(paste("Estado de liquidez:", sem_global)),
        p(paste("Fecha de ejecución:", Sys.Date())),
        p(paste("Modo:", ifelse(input$modo == "excel", "Excel", "Manual")))
      )
    } else {
      tagList(
        h3("Estado de liquidez: (sin ejecutar)"),
        if (!is.null(rv$msg)) tags$p(style="color: #b00020;", paste("Error:", rv$msg)) else NULL
      )
    }
  })
  
  output$plot_saldo <- renderPlot({
    req(rv$ready, rv$flujo)
    plot(
      rv$flujo$fecha, rv$flujo$saldo,
      type = "l",
      xlab = "Fecha",
      ylab = "Saldo proyectado"
    )
    abline(h = as.numeric(input$umbral), lty = 2)
  })
  
  output$tbl_peor <- renderTable({
    req(rv$ready, rv$recs)
    rv$recs$peor_dia %>% select(fecha, saldo, cobros, pagos, semaforo)
  })
  
  output$tbl_cobranza <- renderTable({
    req(rv$ready, rv$recs)
    rv$recs$cobranza_prioritaria %>% head(15)
  })
  
  output$tbl_pagos_rep <- renderTable({
    req(rv$ready, rv$recs)
    rv$recs$pagos_reprogramables %>% head(15)
  })
  
  output$tbl_linea <- renderTable({
    req(rv$ready, rv$recs)
    rv$recs$sugerencia_linea
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("reporte_liquidez_mvp_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(rv$ready, rv$flujo, rv$recs)
      export_report(rv$flujo, rv$recs, file)
    }
  )
}

#============================================================
# EJECUCIÓN APP
#============================================================

# Es en este bloque final es donde se inicia y ejecuta la aplicación Shiny, 
# integrando la interfaz de usuario y el servidor en un entorno 
# interactivo. Permite el despliegue del sistema para su uso en 
# tiempo real.

shinyApp(ui, server)
