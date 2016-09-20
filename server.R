library(rhandsontable)
library(shiny)
library(shinyjs)

source('init.R')

shinyServer(function(input,output,session){
    
    values <- reactiveValues()
    
    observe({
        ## Filtro por semestre y grupo
        semestre <- which(semestres == input$semestre)
        grupo <- input$grupo
        dt <- leeHorario(grupo, semestre)
        dt[,
           c("Grupo", "Semestre", "Titulacion") := NULL]
        values$data <- dt
        file.copy(file.path(tipoFolder,
                            paste0(grupo, '_', semestre, '.pdf')),
                  tempdir(),
                  overwrite = TRUE)
    })
    
    output$table <- renderRHandsontable({
        ## Obtengo la información del reactive values, que cambia
        ## cuando elegimos semestre/grupo o cuando modificamos la
        ## tabla
        df <- values$data
        hot <- rhandsontable(df,
                             rowHeaders = NULL,
                             stretchH = TRUE)
        ## Las columnas no se editan ni ordenar
        hot <- hot_cols(hot,
                        allowColEdit = FALSE,
                        columnSorting = FALSE)
        hot <- hot_col(hot, col = "Tipo",
                       source = tipos,
                       type = 'autocomplete',
                       strict = TRUE)
        hot <- hot_col(hot, col = "Dia",
                       source = dias,
                       type = 'autocomplete',
                       strict = TRUE)
        hot <- hot_col(hot, col = "Asignatura",
                       source = levels(df$Asignatura),
                       type = 'autocomplete',
                       strict = TRUE)
        hot <- hot_col(hot, col = "HoraInicio",
                       source = horas,
                       type = 'autocomplete',
                       strict = TRUE)
        hot <- hot_col(hot, col = "HoraFinal",
                       source = horas,
                       type = 'autocomplete',
                       strict = TRUE)
        hot <- hot_col(hot, col = "Aula",
                       source = c("", aulas, grupos),
                       type = 'autocomplete',
                       default = "",
                       strict = TRUE)
        if ("Itinerario" %in% names(df))
            hot <- hot_col(hot, col = "Itinerario",
                           source = c("", "A", "B"),
                           default = "",
                           strict = TRUE)
        hot
    })
    
    ## Si hay cambios actualizo el data.frame en el reactive values
    observeEvent(input$table,
                 values$data <- hot_to_r(input$table))
    
    output$pdfViewer <- renderUI(
    {
        ## Añado enlace reactivo para que actualice contenido del
        ## iframe si aprieto botón "refresh"
        refresh <- input$refresh
        semestre <- which(semestres == input$semestre)
        tags$iframe(style="height:600px; width:100%",
                    src=paste0("pdfs/",
                               input$grupo,
                               "_", semestre,
                               ".pdf#zoom=page-width")
                    )
    })
    ## Refresco PDF
    observeEvent(input$refresh,
    {
        ## Leo tabla, y grupo y semestre (no incluidos en tabla)
        df <- values$data
        semestre <- which(semestres == input$semestre)
        grupo <- input$grupo
        df$Grupo <- grupo
        ## Genero timetable PDF en directorio temporal
        csv2tt(df, grupo, semestre,
               colorByTipo = TRUE,
               dest = tempdir())
    })
    ## Grabo datos en csv
    observeEvent(input$update,
    {
        ## Leo tabla
        df <- values$data
        ## Recupero semestre, grupo y titulacion (no incluidos en tabla)
        semestre <- which(semestres == input$semestre)
        grupo <- input$grupo
        titulacion <- whichDegree(grupo)
        ## Los añado en la tabla como columnas adicionales
        df$Grupo <- grupo
        df$Semestre <- semestre
        df$Titulacion <- titulacion
        ## Graba la tabla csv
        escribeHorario(df, grupo, semestre)
        info('Tabla modificada correctamente.')
        ## Genera PDF con color por asignatura
        csv2tt(df, grupo, semestre,
               colorByTipo = TRUE,
               dest = tipoFolder)
        ## Genera PDF con color por asignatura
        csv2tt(df, grupo, semestre,
               colorByTipo = FALSE,
               dest = asigFolder)
        info('PDFs generados correctamente.')
        ## Vuelca en webdav
        copyWeb(grupo, semestre, tipoFolder, webTipo)
        copyWeb(grupo, semestre, asigFolder, webAsignatura)
        ## Actualizo el fichero completo del semestre
        for (folder in file.path(webdav, c('tipo', 'asignatura')))
        {
            pdfs <- file.path(folder,
                              paste0('S', semestre),
                              paste0(grupos, "_", semestre, ".pdf"))
            result <- file.path(folder,
                                paste0("ETSIDI_2016_2017_Grado_S",
                                       semestre, ".pdf"))
            system2('pdftk',
                    c(pdfs, "cat output", result)
                    )
        }
        ## Mensaje para usuario si nada falla
        info('Horarios publicados.')
    })
    
}) 

