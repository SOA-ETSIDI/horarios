library(rhandsontable)
library(shiny)
library(shinyjs)

source('init.R')

## Habilito el directorio temporal como fuente de recursos para mostrar en la web
addResourcePath(prefix = "tmpPDF", directoryPath = tempdir())

shinyServer(function(input,output,session){
    
    values <- reactiveValues()
    
    observe({
        ## Filtro por semestre y grupo
        semestre <- which(semestres == input$semestre)
        grupo <- input$grupo
        dt <- leeHorario(grupo, semestre)
        titulacion <- dt$Titulacion[1]
        ## Elimino estos campos para que no se puedan modificar en la tabla
        dt[,
           c("Grupo", "Semestre", "Titulacion") := NULL]
        values$data <- dt
        ## Grupo y semestre puedo recuperarlos de input$grupo e
        ## input$semestre, pero titulacion no, así que lo guardo
        values$titulacion <- titulacion
        values$asignaturas <- levels(factor(asignaturas[Titulacion == titulacion,
                                                        titlecase(Asignatura)]))
        if (grupo %in% c(masters, otrosMaster))
            destination <- masterFolder
        else if (grupo == optativasIS)
            destination <-  ISFolder
        else
            destination <- tipoFolder

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
                    src=paste0("tmpPDF/",
                               input$grupo,
                               "_", semestre,
                               ".pdf#zoom=page-width")
                    )
    })
    ## Refresco PDF
    observeEvent(input$refresh,
    {
        ## Leo tabla...
        df <- values$data
        ## y añado grupo, semestre y titulación (previamente eliminados de la tabla)
        semestre <- which(semestres == input$semestre)
        df$Semestre <- semestre
        grupo <- input$grupo
        df$Grupo <- grupo
        df$Titulacion <- values$titulacion
        hMin <- minHour(df$HoraInicio)
        hMax <- maxHour(df$HoraFinal)
        height <- min(14/(hMax - hMin), 1.5)
        ## Color por tipo o por asignatura
        colorTipo <- !input$color
        ## Genero timetable PDF en directorio temporal
        if (any(df$Itinerario %in% c('A', 'B')))
            ttItinerario(df, grupo, 
                         colorByTipo = colorTipo,
                         hInicio = hMin,
                         hFin = hMax,
                         hourHeight = height,
                         dest = tempdir())
        else 
            csv2tt(df, grupo, 
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
        height <- min(14/(hMax - hMin), 1.5)
        ## Recupero semestre, grupo y titulacion (no incluidos en tabla)
        semestre <- which(semestres == input$semestre)
        grupo <- input$grupo
        titulacion <- values$titulacion
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
        ISSemFolder <- file.path(ISFolder,
                                   paste0('S', semestre))

        if (any(df$Itinerario %in% c('A', 'B')))
        {## Hay alguna franja con itinerario
            
            ## Genera PDF con color por tipo 
            ttItinerario(df, grupo, 
                         colorByTipo = TRUE,
                         hInicio = hMin,
                         hFin = hMax,
                         hourHeight = height,
                         dest = tipoSemFolder)
            ## Genera PDF con color por asignatura
            ttItinerario(df, grupo, 
                         colorByTipo = FALSE,
                         hInicio = hMin,
                         hFin = hMax,                         
                         hourHeight = height,
                         dest = asigSemFolder)
        }
        else
        {## Horario sin itinerario
            if (grupo %in% c(masters, otrosMaster, optativasIS))
            {
                ## En master e IS usamos nombre de titulación para
                ## encabezar el horario
                vals <- c(masters, otrosMaster, optativasIS)
                idx <- match(grupo, vals)
                titulo <- names(vals)[idx]
                if (grupo == "IS")
                    SemFolder <- ISSemFolder
                else
                    SemFolder <- masterSemFolder
                
                ## Genera PDF para master o IS coloreando por
                ## asignatura
                csv2tt(df, titulo,
                       colorByTipo = FALSE,
                       hInicio = hMin,
                       hFin = hMax,                                                
                       hourHeight = height,
                       dest = SemFolder)
            }
            else
                {##Horario de grado
                    ## Genera PDF con color por tipo 
                    csv2tt(df, grupo, 
                           colorByTipo = TRUE,
                           hInicio = hMin,
                           hFin = hMax,                                                    
                           hourHeight = height,
                           dest = tipoSemFolder)
                    ## Genera PDF con color por asignatura
                    csv2tt(df, grupo, 
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
        ISSemFolder <- file.path(ISFolder,
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
        else ## Master o IS
        {
            withProgress(message = "Publicando horarios...",
            {
                nSteps <- 2
                if (grupo == "IS")
                    
                    okWeb <- copyWeb(grupo, semestre,
                                       ISSemFolder, webIS)
                else
                    okWeb <- copyWeb(grupo, semestre,
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

