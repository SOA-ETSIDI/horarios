library(data.table)

source('../misc/funciones.R')
source('../misc/defs.R')
semestres <- c("Septiembre-Enero", "Febrero-Junio")

semestreActual <- 2
cursoActual <- '2016-17'

## Horarios con aulas
horariosPath <- '../horarios/csv/'
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

source('csv2tt.R')

aulas <- levels(factor(dth$Aula))

for (aula in aulas)
{
    xx <- dth[Aula == aula]
    xx[, Aula := Grupo]
    try(csv2tt(xx,
               nombre = aula,
               semestre = semestreActual,
               colorByTipo = FALSE))
}


