source('../misc/defs.R')
source('../misc/funciones.R')
semestres <- c("Septiembre-Enero", "Febrero-Junio")

source('csv2tt.R')

## Incluye nuevos grupos (por definir) de doble grado
grupos <- c("M101", "D102", "Q103", "A104", "E105", "EE105",
            "M106", "D107", "DM107", "Q108", "A109", "E100", 
            "M201", "DM201", "D202", "Q203", "A204", "E205",
            "M206", "A207", "E208", "EE208",
            "M301", "A302", "E303", "EE309",
            "M306", "D307", "DM306", "Q308", "A309",
            "M401", "D402", "DM406", "Q403", "A404",
            "M406", "E407", "A408")

asignaturas <- read.csv2('../misc/asignaturas.csv')$Asignatura
asignaturas <- titlecase(levels(asignaturas))

pdfFolder <- "pdfs"
tipoFolder <- file.path(pdfFolder, 'tipo')
asigFolder <- file.path(pdfFolder, 'asignatura')

## webdav <- '/var/www/webdav/horarios/grado'
## webTipo <- file.path(webdav, 'tipo')
## webAsignatura <- file.path(webdav, 'asignatura')

cursoActual <- '2017_2018'

actualizaPDF <- function(ruta, semestre)
{
    pdfs <- file.path(ruta,
                      paste0('S', semestre),
                      paste0(grupos, "_", semestre, ".pdf"))
    result <- file.path(ruta,
                        paste0("ETSIDI_", cursoActual,
                               "_Grado_S", semestre,
                               ".pdf"))
    system2('pdftk',
            c(pdfs, "cat output", result)
            )
}

## copyWeb <- function(grupo, semestre, from, to)
## {
##     fichero <- paste0(grupo, '_', semestre, '.pdf')
##     toFolder <- file.path(to, paste0('S', semestre))
##     if (!dir.exists(toFolder)) dir.create(toFolder)
##     file.copy(file.path(from, fichero),
##               file.path(toFolder, fichero),
##               overwrite = TRUE)
## }
