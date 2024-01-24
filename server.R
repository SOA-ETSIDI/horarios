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
        titulacion <- dt$Titulacion[1]
        dt[,
           c("Grupo", "Semestre", "Titulacion") := NULL]
        values$data <- dt

        values$asignaturas <- levels(factor(asignaturas[Titulacion == titulacion,
                                                        titlecase(Asignatura)]))
        destination <- ifelse(grupo %in% masters,
                              masterFolder,
                              tipoFolder)
        file.copy(file.path(destination,
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
                       type = 'dropdown',
                       strict = TRUE)
        hot <- hot_col(hot, col = "Dia",
                       source = dias,
                       type = 'dropdown',
                       strict = TRUE)
        hot <- hot_col(hot, col = "Asignatura",
                       source = values$asignaturas,
                       type = 'autocomplete',
                       strict = TRUE)
        hot <- hot_col(hot, col = "HoraInicio",
                       source = horas,
                       type = 'dropdown',
                       strict = TRUE)
        hot <- hot_col(hot, col = "HoraFinal",
                       source = horas,
                       type = 'dropdown',
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
    {
        hot = isolate(input$table)
        if (!is.null(hot))
        {
            values$data <- hot_to_r(hot)
        }
    })
        
    
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
        hMin <- minHour(df$HoraInicio)
        hMax <- maxHour(df$HoraFinal)
        height <- min(14/(hMax - hMin), 2)
        ## Color por tipo o por asignatura
        colorTipo <- !input$color
        ## Genero timetable PDF en directorio temporal
        if (any(df$Itinerario %in% c('A', 'B')))
            ttItinerario(df, grupo, semestre,
                         colorByTipo = colorTipo,
                         hInicio = hMin,
                         hFin = hMax,
                         hourHeight = height,
                         dest = tempdir())
        else 
            csv2tt(df, grupo, semestre,
                   colorByTipo = colorTipo,
                   hInicio = hMin,
                   hFin = hMax,
                   hourHeight = height,
                   dest = tempdir())
    })
    ## Grabo datos en csv
    observeEvent(input$update,
    {
        ## Leo tabla
        df <- values$data
        hMin <- minHour(df$HoraInicio) 
        hMax <- maxHour(df$HoraFinal)
        height <- min(14/(hMax - hMin), 2)
        ## Recupero semestre, grupo y titulacion (no incluidos en tabla)
        semestre <- which(semestres == input$semestre)
        grupo <- input$grupo
        if (grupo %in% masters)
            titulacion <- grupo
        else
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
        masterSemFolder <- file.path(masterFolder,
                                   paste0('S', semestre))

        if (any(df$Itinerario %in% c('A', 'B')))
        {## Hay alguna franja con itinerario
            
            ## Genera PDF con color por tipo 
            ttItinerario(df, grupo, semestre,
                         colorByTipo = TRUE,
                         hInicio = hMin,
                         hFin = hMax,
                         hourHeight = height,
                         dest = tipoSemFolder)
            ## Genera PDF con color por asignatura
            ttItinerario(df, grupo, semestre,
                         colorByTipo = FALSE,
                         hInicio = hMin,
                         hFin = hMax,                         
                         hourHeight = height,
                         dest = asigSemFolder)
        }
        else
        {## Horario sin itinerario
            if (grupo %in% masters)
            {
                ## En master usamos nombre de titulación para
                ## encabezar el horario
                idx <- match(grupo, masters)
                titulo <- names(masters)[idx]
                ## Genera PDF para master coloreando por asignatura
                csv2tt(df, titulo, semestre,
                       colorByTipo = FALSE,
                       hInicio = hMin,
                       hFin = hMax,                                                
                       hourHeight = height,
                       dest = masterSemFolder)
            }
            else
                {##Horario de grado
                    ## Genera PDF con color por tipo 
                    csv2tt(df, grupo, semestre,
                           colorByTipo = TRUE,
                           hInicio = hMin,
                           hFin = hMax,                                                    
                           hourHeight = height,
                           dest = tipoSemFolder)
                    ## Genera PDF con color por asignatura
                    csv2tt(df, grupo, semestre,
                           colorByTipo = FALSE,
                           hInicio = hMin,
                           hFin = hMax,                         
                           hourHeight = height,
                           dest = asigSemFolder)
                }
        }
        
        ## Actualizo el fichero completo del semestre si se trata de
        ## un grado
        if (input$grupo %in% grupos) 
            for (folder in file.path(pdfFolder, 'grado',
                                     c('tipo', 'asignatura')))
                actualizaPDF(folder, semestre)
        ## Si hemos llegado hasta aquí todo ha ido bien
        info('PDFs generados correctamente.')
        
        ## Actualiza repositorio GitHub
        tryCatch({
            git("Actualiza horarios y aulas")
            info("Repositorio actualizado.")
            },
            error = function(e)
                info("Error al actualizar el repositorio."))

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
        masterSemFolder <- file.path(masterFolder,
                                   paste0('S', semestre))
        ## Vuelca en webdav
        if (grupo %in% grupos) ## Grados
        {
            withProgress(message = "Publicando horarios...",
            {
                nSteps <- 5
                okWebTipo <- copyWeb(grupo, semestre,
                                     tipoSemFolder, webTipo)
                incProgress(1/nSteps)
                okWebAsig <- copyWeb(grupo, semestre,
                                     asigSemFolder, webAsignatura)
                incProgress(1/nSteps)
                if (okWebTipo & okWebAsig)
                {
                    ## Actualizo el fichero completo del semestre
                    for (folder in file.path(webdav, 'grado',
                                             c('tipo', 'asignatura')))
                    {
                        actualizaPDF(folder, semestre)
                        incProgress(1/nSteps)
                    }
                    ## Actualizo aulas
                    aulasPDF(semestre)
                    incProgress(1/nSteps)
                    ## Mensaje para usuario si nada falla
                    info('Horarios y asignación de aulas publicados.')
                } else info('Error al publicar.')
            })
        }
        else ## Master
        {
            withProgress(message = "Publicando horarios...",
            {
                nSteps <- 2
                okWebMaster <- copyWeb(grupo, semestre,
                                       masterSemFolder, webMaster)
                incProgress(1/nSteps)
                if (okWebMaster)
                {
                    ## Actualizo aulas
                    aulasPDF(semestre)
                    incProgress(1/nSteps)
                    ## Mensaje para usuario si nada falla
                    info('Horarios y asignación de aulas publicados.')
                }
                else
                    info('Error al publicar.')
            })
            
        }
    })
})

