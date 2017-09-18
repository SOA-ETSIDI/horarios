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
                            paste0('S', semestre), 
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
                       source = asignaturas,
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
        if (any(df$Itinerario != ""))
            ttItinerario(df, grupo, semestre,
                         colorByTipo = TRUE,
                         dest = tempdir())
        else 
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
        ## Rutas de ficheros PDFs
        tipoSemFolder <- file.path(tipoFolder,
                                   paste0('S', semestre))
        asigSemFolder <- file.path(asigFolder,
                                   paste0('S', semestre))
        ## Genera PDF con color por tipo
        if (any(df$Itinerario != ""))
            ttItinerario(df, grupo, semestre,
                         colorByTipo = TRUE,
                         dest = tipoSemFolder)
        else 
            csv2tt(df, grupo, semestre,
                   colorByTipo = TRUE,
                   dest = tipoSemFolder)
        ## Genera PDF con color por asignatura
        if (any(df$Itinerario %in% c('A', 'B')))
            ttItinerario(df, grupo, semestre,
                         colorByTipo = FALSE,
                         dest = asigSemFolder)
        else
            csv2tt(df, grupo, semestre,
                   colorByTipo = FALSE,
                   dest = asigSemFolder)
        ## Actualizo el fichero completo del semestre
        for (folder in file.path(pdfFolder, c('tipo', 'asignatura')))
        {
            actualizaPDF(folder, semestre)
        }

        info('PDFs generados correctamente.')
    })
    ## Publico PDFs en web
    observeEvent(input$publish,
    {
        semestre <- which(semestres == input$semestre)
        grupo <- input$grupo
        ## Rutas de ficheros PDFs
        tipoSemFolder <- file.path(tipoFolder,
                                   paste0('S', semestre))
        asigSemFolder <- file.path(asigFolder,
                                   paste0('S', semestre))
        ## Vuelca en webdav
        okWebTipo <- copyWeb(grupo, semestre, tipoSemFolder, webTipo)
        okWebAsig <- copyWeb(grupo, semestre, asigSemFolder, webAsignatura)
        if (okWebTipo & okWebAsig)
        {
            ## Actualizo el fichero completo del semestre
            for (folder in file.path(webdav, c('tipo', 'asignatura')))
            {
                actualizaPDF(folder, semestre)
            }
            ## Mensaje para usuario si nada falla
            info('Horarios publicados.')
            ## Actualizo aulas
            source('horariosAula.R')
        } else info('Error al publicar.')
    })
    
}) 

