source('../misc/defs.R')
source('../misc/funciones.R')
semestres <- c("Septiembre-Enero", "Febrero-Junio")

source('csv2tt.R')

pdfFolder <- "pdfs"
tipoFolder <- file.path(pdfFolder, 'tipo')
asigFolder <- file.path(pdfFolder, 'asignatura')

webdav <- '/var/www/webdav/horarios/grado'
webTipo <- file.path(webdav, 'tipo')
webAsignatura <- file.path(webdav, 'asignatura')

copyWeb <- function(grupo, semestre, from, to)
{
    fichero <- paste0(grupo, '_', semestre, '.pdf')
    toFolder <- file.path(to, paste0('S', semestre))
    if (!dir.exists(toFolder)) dir.create(toFolder)
    file.copy(file.path(from, fichero),
              file.path(toFolder, fichero),
              overwrite = TRUE)
}
