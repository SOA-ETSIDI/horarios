library(data.table)

source('../misc/funciones.R')
source('../misc/defs.R')
semestres <- c("Septiembre-Enero", "Febrero-Junio")

semestreActual <- 1
webdavAula <- '/var/www/webdav/aulas/'

## Horarios con aulas
horariosPath <- 'csv/'
files <- dir(horariosPath, pattern = '.csv$')
horarios <- rbindlist(lapply(paste0(horariosPath, files),
                             fread,
                             na.string = "", 
                             encoding = 'UTF-8'),
                      fill = TRUE)
## Los másteres no tienen grupo: le asigno el código de la titulacion
horarios[is.na(Grupo), Grupo := Titulacion]
## No incluyo la hora de inicio: una actividad queda determinada por
## su nombre, tipo, grupo y dia (y aporta la información de aula)
dth <- horarios[Semestre == semestreActual,
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

## Genera tablas de ocupación
source('csv2tt.R')

## Ocupación Aulas de Grado
aulasGrado <- c("A10", "A11", "A12", "A13",
                "A21", "A22", "A23", "A24", "A25", "A26", "A27",
                "A34", "A35",
                "B11", "B12",
                "B21", "B22",
                "B31", "B32",
                "B41", "B42")

for (aula in aulasGrado)
{
    xx <- dth[grepl(aula, Aula)]
    if (nrow(xx) > 0)
    {
     xx[, Aula := Grupo]
    try(csv2tt(xx,
               nombre = aula,
               semestre = semestreActual,
               colorByTipo = FALSE))   
    }
}

aulasMaster <- paste0("Mstr", 1:4)

## Aulas de Máster
for (aula in aulasMaster)
{
    ##    xx <- dth[Aula == aula]
    xx <- dth[grepl(aula, Aula)]
    if (nrow(xx) > 0)
    {
     xx[, Aula := Grupo]
    try(csv2tt(xx,
               nombre = aula,
               semestre = semestreActual,
               colorByTipo = FALSE))   
    }
}

old <- setwd(tempdir())
file.remove(c("LogoETSIDI.pdf", "LogoUPM.pdf"))
pdfs <- dir(pattern = 'pdf')

completo <- paste0("Ocupacion_Aulas_", semestreActual, "S.pdf")

system2("pdftk", c(paste(pdfs, collapse = " "),
                   "cat output",
                   completo)
                   )

file.copy(c(pdfs, completo), webdavAula, overwrite = TRUE)

setwd(old)

