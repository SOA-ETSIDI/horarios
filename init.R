source('../misc/defs.R')
source('../misc/funciones.R')

semestres <- c("Septiembre-Enero", "Febrero-Junio")

source('csv2tt.R')

## Horas posibles en selector
horas <- hhSeq(h1 = "08:00", h2 = "21:30", by = "30 min")

## Asignaturas y aulas
asignaturas <- fread('../misc/asignaturas.csv', stringsAsFactors = TRUE)


## Rutas
horariosPath <- 'csv/'
pdfFolder <- "pdfs"
tipoFolder <- file.path(pdfFolder, 'grado', 'tipo')
asigFolder <- file.path(pdfFolder, 'grado', 'asignatura')
masterFolder <- file.path(pdfFolder, 'master')
ISFolder <- file.path(pdfFolder, 'IS')

webdav <- '/var/www/webdav/horarios'
webTipo <- file.path(webdav, 'grado', 'tipo')
webAsignatura <- file.path(webdav, 'grado', 'asignatura')
webMaster <- file.path(webdav, 'master')
webIS <- file.path(webdav, 'IS')

webdavAula <- '/var/www/webdav/aulas/'

## Horarios con aulas
leeHorarios <- function()
{
    files <- dir(horariosPath, pattern = '.csv$')
    dt <- rbindlist(lapply(paste0(horariosPath, files),
                                 fread,
                                 na.string = "", 
                                 encoding = 'UTF-8'),
                          fill = TRUE)
    ## Los másteres no tienen grupo: le asigno el código de la titulacion
    dt[is.na(Grupo), Grupo := Titulacion]
    ## Devuelve objeto
    dt
}

horarios <- leeHorarios()

## Funciones auxiliares
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


aulasPDF <- function(semestre)
{
    horarios <- leeHorarios()
    ## No incluyo la hora de inicio: una actividad queda determinada por
    ## su nombre, tipo, grupo y dia (y aporta la información de aula)
    dth <- horarios[Semestre == semestre,
                    .(
                        Asignatura,
                        Tipo,
                        Grupo,
                        Dia = factor(Dia, dias),
                        HoraInicio, HoraFinal,
                        Aula
                    )]
    setkey(dth, Asignatura, Tipo, Grupo, Dia)
    ## Me quedo con los registros únicos (elimino las duplicidades por diferentes horas de inicio)
    ##dth <- unique(dth)

    for (aula in aulas)
    {
        xx <- dth[grepl(aula, Aula)]
        if (nrow(xx) > 0)
        {
            xx[, `:=`(Aula = Grupo,
                      Grupo = aula)
               ]
            try(csv2tt(xx,
                       nombre = aula,
                       semestre = semestre,
                       colorByTipo = FALSE))   
        }
    }

    old <- setwd(tempdir())

    file.remove(c("LogoETSIDI.pdf", "LogoUPM.pdf"))

    pdfs <- paste0(aulas, '_', semestre, '.pdf')

    files <- dir(pattern = 'pdf')

    pdfs <- pdfs[pdfs %in% files]

    completo <- paste0("Ocupacion_Aulas_", semestre, "S.pdf")

    system2("pdftk", c(paste(pdfs, collapse = " "),
                       "cat output",
                       completo)
            )

    file.copy(c(pdfs, completo), webdavAula, overwrite = TRUE)

    setwd(old)
}

git <- function(x)
{
    withProgress(message = "Actualizando repositorio ...",{
    
        system2("git", "add .")
        incProgress(1/5)
        system2("git",
                paste("commit -m '", x, "'"))
        incProgress(2/5)
        system2("git", "push")
        incProgress(1)
    })
}
