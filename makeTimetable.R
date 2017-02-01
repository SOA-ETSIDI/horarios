library(openxlsx)
library(data.table)

source('csv2tt.R')
source('../misc/defs.R')

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
pdfS1 <- paste0(grupos, "_1.pdf")
for (folder in file.path(pdfFolder, c('tipo', 'asignatura'), 'S1'))
{
    old <- setwd(folder)
    system2('pdftk', c(pdfS1,
                       "cat",
                       "output ETSIDI_2016_2017_Grado_S1.pdf"))
    system2('mv', 'ETSIDI_2016_2017_Grado_S1.pdf ../')
    system2('rm', 'Logo*.pdf')
    setwd(old)
}

pdfS2 <- paste0(grupos, "_2.pdf")
for (folder in file.path(pdfFolder, c('tipo', 'asignatura'), 'S2'))
{
    old <- setwd(folder)
    system2('pdftk', c(pdfS2,
                       "cat",
                       "output ETSIDI_2016_2017_Grado_S2.pdf"))
    system2('mv', 'ETSIDI_2016_2017_Grado_S2.pdf ../')
    system2('rm', 'Logo*.pdf')
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
               "MUIP", s, "Investigador (Aula Máster 3)")
        csv2tt(MUIP[Itinerario %in% c("", "Profesional")],
               "MUIP", s, "Profesional (Aula Máster 4)")
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
               "MUIE", s, "Mecatrónica (Aula Máster 2 )")
        csv2tt(MUIE[Itinerario %in% c("", "Distribución")],
               "MUIE", s, "Distribución (Aula Máster 1)")
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


