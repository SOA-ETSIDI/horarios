library(data.table)

source('init.R')

pdfFolder <- 'pdfs'


makeTimetable <- function(g, s, color = 'tipo', dest)
{
    if (color == 'tipo')
    {
        colorTipo <- TRUE
        myFolder <- file.path(dest, 'tipo', paste0('S', s))
        if (!dir.exists(myFolder)) dir.create(myFolder, recursive = TRUE)
    } else
    {
        colorTipo <- FALSE
        myFolder <- file.path(dest, 'asignatura', paste0('S', s))
        if (!dir.exists(myFolder)) dir.create(myFolder, recursive = TRUE)
    }
    
    hh <- leeHorario(g, s)
    ## Algunos grupos (mecánicos) tienen itinerarios
    if (any(hh$Itinerario != ""))
    {
        ## Genero un PDF para cada itinerario
        csv2tt(hh[Itinerario %in% c("", "A")],
               g, s, itinerario = "A",
               colorByTipo = colorTipo,
               dest = myFolder)
        csv2tt(hh[Itinerario %in% c("", "B")],
           g, s, itinerario = "B",
           colorByTipo = colorTipo,
           dest = myFolder)
        ## Y los junto en un PDF común
        old <- setwd(myFolder)
        pdfs <- paste0(g, c("A", "B"), "_", s, ".pdf")
        system2('pdftk', c(pdfs, 
                           "cat",
                           "output",
                           paste0(g, "_", s, ".pdf")))
        ## borrando los individuales
        file.remove(pdfs)
        setwd(old)
    } 
    else csv2tt(hh, g, s,
                colorByTipo = colorTipo,
                dest = myFolder)
}

myGrupos <- grupos
mySems <- 1:2

for (g in myGrupos)
{
    for (s in mySems)
    {
        makeTimetable(g, s = s, color = 'tipo', dest = pdfFolder)
        makeTimetable(g, s = s, color = 'asignatura', dest = pdfFolder)
    }
}

## Uno todos los PDFs en uno por semestre
actualizaPDF(file.path(pdfFolder, "tipo"), 1)
actualizaPDF(file.path(pdfFolder, "tipo"), 2)

actualizaPDF(file.path(pdfFolder, "asignatura"), 1)
actualizaPDF(file.path(pdfFolder, "asignatura"), 2)




## MASTERES
## MUIP

for (s in 1:2)
{
        MUIP <- leeHorario("56AA", s)
        csv2tt(MUIP,
               "MUIP", s,
               hInicio = 15, hFin = 21,
               hourHeight = 2)
}
old <- setwd(tempdir())
system2('pdftk',
        c("MUIP_1.pdf", "MUIP_2.pdf",
          "cat",
          "output 56AA.pdf"))
setwd(old)

## MUIE
for (s in 1:2)
{
        MUIE <- leeHorario("56AB", s)
        csv2tt(MUIE,
               "MUIE", s,
               hInicio = 16, hFin = 21,
               hourHeight = 2)
}
old <- setwd(tempdir())
system2('pdftk',
        c("MUIE_1.pdf", "MUIE_2.pdf",
          "cat",
          "output 56AB.pdf"))
setwd(old)


## MUIDI
for (s in 1:2)
{
        MUIDI <- leeHorario("56AC", s)
        csv2tt(MUIDI,
               "MUIDI", s,
               hInicio = 8, hFin = 15,
               hourHeight = 1.8)
}
old <- setwd(tempdir())
system2('pdftk',
        c("MUIDI_1.pdf", "MUIDI_2.pdf",
          "cat",
          "output 56AC.pdf"))
setwd(old)


