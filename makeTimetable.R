library(openxlsx)
library(data.table)

source('csv2tt.R')

pdfFolder <- '../data/horarios/pdfs/'
pdfFolder <- tempdir()

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

for (g in grupos)
{
    for (s in 1:2)
    {
        makeTimetable(g, s = 1, color = 'tipo', dest = pdfFolder)
        makeTimetable(g, s = 1, color = 'asignatura', dest = pdfFolder)
    }
}

## Uno todos los PDFs en uno por semestre
pdfS1 <- paste0(grupos, "_1.pdf")
pdfS2 <- paste0(grupos, "_2.pdf")

for (folder in file.path(pdfFolder, c('tipo', 'asignatura')))
{
    old <- setwd(folder)
    system2('pdftk', c(pdfS1,
                       "cat",
                       "output ETSIDI_2016_2017_Grado_S1.pdf"))
    system2('pdftk', c(pdfS2,
                       "cat",
                       "output ETSIDI_2016_2017_Grado_S2.pdf"))
    setwd(old)
}

## Genero libro excel

## S1
S1 <- lapply(grupos, leeHorario, semestre = 1)
names(S1) <- grupos

old <- setwd(tempdir())
xls1 <- write.xlsx(S1,
                  file = "ETSIDI_2016_2017_Grado_S1.xlsx",
                  creator = 'SOA-ETSIDI')
lapply(seq_along(grupos), function(i)
{
    setColWidths(xls1, sheet = i,
                 cols = 1:9,
                 widths = "auto")
})
saveWorkbook(xls1, file = "ETSIDI_2016_2017_Grado_S1.xlsx", overwrite = TRUE)
setwd(old)

S2 <- lapply(grupos, leeHorario, semestre = 2)
names(S2) <- grupos

old <- setwd(tempdir())
xls2 <- write.xlsx(S2,
                  file = "ETSIDI_2016_2017_Grado_S2.xlsx",
                  creator = 'SOA-ETSIDI')
lapply(seq_along(grupos), function(i)
{
    setColWidths(xls2, sheet = i,
                 cols = 1:9,
                 widths = "auto")
})
saveWorkbook(xls2, file = "ETSIDI_2016_2017_Grado_S2.xlsx", overwrite = TRUE)
setwd(old)

## MASTERES
## MUIP
for (s in 1:2)
{
        MUIP <- leeHorario("56AA", s)
        ## Genero un PDF para cada itinerario
        csv2tt(MUIP[Itinerario %in% c("", "Investigador")],
               "MUIP", s, "Investigador")
        csv2tt(MUIP[Itinerario %in% c("", "Profesional")],
               "MUIP", s, "Profesional")
        ## Y los junto en un PDF común
        old <- setwd(tempdir())
        pdfs <- paste0("56AA", c("Profesional", "Investigador"), "_", s, ".pdf")
        system2('pdftk', c(pdfs, 
                           "cat",
                           "output",
                           paste0("56AA", "_", s, ".pdf")))
        ## borrando los individuales
        file.remove(pdfs)
        system2('pdftk',
                c("56AA_1.pdf", "56AA_2.pdf",
                  "cat",
                  "output 56AA.pdf"))
        setwd(old)
} 
old <- setwd(tempdir())
system2('pdftk',
        c("56AA_1.pdf", "56AA_2.pdf",
          "cat",
          "output 56AA.pdf"))
setwd(old)

## MUIE
for (s in 1:2)
{
        MUIE <- leeHorario("56AB", s)
        ## Genero un PDF para cada itinerario
        csv2tt(MUIE[Itinerario %in% c("", "Mecatrónica")],
               "MUIE", s, "Mecatrónica")
        csv2tt(MUIE[Itinerario %in% c("", "Distribución")],
               "MUIE", s, "Distribución")
        ## Y los junto en un PDF común
        old <- setwd(tempdir())
        pdfs <- paste0("56AB", c("Mecatrónica", "Distribución"), "_", s, ".pdf")
        system2('pdftk', c(pdfs, 
                           "cat",
                           "output",
                           paste0("56AB", "_", s, ".pdf")))
        ## borrando los individuales
        file.remove(pdfs)
        setwd(old)
} 
old <- setwd(tempdir())
system2('pdftk',
        c("56AB_1.pdf", "56AB_2.pdf",
          "cat",
          "output 56AB.pdf"))
setwd(old)


## MUIDI
for (s in 1:2)
{
        MUIDI <- leeHorario("56AC", s)
        csv2tt(MUIDI,
               "MUIDI", s)
}
old <- setwd(tempdir())
system2('pdftk',
        c("56AC_1.pdf", "56AC_2.pdf",
          "cat",
          "output 56AC.pdf"))
setwd(old)


