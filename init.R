source('../misc/defs.R')
source('../misc/funciones.R')

semestres <- c("Septiembre-Enero", "Febrero-Junio")

source('csv2tt.R')
## Horas posibles en selector
horas <- hhSeq(h1 = "08:00", h2 = "21:30", by = "30 min")

asignaturas <- fread('../misc/asignaturas.csv', stringsAsFactors = TRUE)

pdfFolder <- "pdfs"
tipoFolder <- file.path(pdfFolder, 'grado', 'tipo')
asigFolder <- file.path(pdfFolder, 'grado', 'asignatura')
masterFolder <- file.path(pdfFolder, 'master')

webdav <- '/var/www/webdav/horarios'
webTipo <- file.path(webdav, 'grado', 'tipo')
webAsignatura <- file.path(webdav, 'grado', 'asignatura')
webMaster <- file.path(webdav, 'master')


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

copyWeb <- function(grupo, semestre, from, to)
{
    fichero <- paste0(grupo, '_', semestre, '.pdf')
    toFolder <- file.path(to, paste0('S', semestre))
    if (!dir.exists(toFolder)) dir.create(toFolder)
    file.copy(file.path(from, fichero),
              file.path(toFolder, fichero),
              overwrite = TRUE)
}
