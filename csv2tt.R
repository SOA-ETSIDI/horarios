library(data.table)

source('../misc/defs.R')
source('../misc/funciones.R')
semestres <- c("Septiembre-Enero", "Febrero-Junio")


## Lee y escribe horarios en CSV
leeHorario <- function(grupo, semestre)
{
    hh <- fread(paste0('csv/',
                       grupo, '_', semestre,
                       '.csv'),
                stringsAsFactors = TRUE)
    ## Algunos ficheros pueden tener filas mal formadas: las elimino
    hh <- hh[!is.na(Dia)]
    if ("Itinerario" %in% names(hh))
        hh[, Itinerario := factor(Itinerario,
                                  levels = c("A", "B", ""))]
    hh[, Dia := factor(Dia, levels = dias, ordered = TRUE)]
    hh[, Aula := as.character(Aula)]
    if("Comentarios" %in% names(hh))
        hh$Comentarios <- as.character(hh$Comentarios)
    setkey(hh, Dia, HoraInicio)
    hh
}

escribeHorario <- function(df, grupo, semestre)
{
    path <- 'csv/'
    file <- paste0(grupo, '_', semestre,
                   '.csv')
    write.csv2(df, file = paste0(path, file), row.names = FALSE)
    ## system(paste('git add', paste0(path, file)))
    ## system(paste('git commit -m "Update', file, 'via shiny"'))
    ## system('git push origin master')
}


## Devuelve character H.M (con punto  en lugar de :,  porque así está definida la plantilla de LaTeX)
formatHora2 <- function(x)
{
    format(as.POSIXct(x, format='%H:%M'),
           format = '%H.%M')
}

## Utilidad para juntar en una cadena asignaturas o tipos de docencia que coinciden en horario
join <- function(x, collapse = ' \\\\ ')
{
    x <- unique(x)
    ## Descarto elementos vacíos
    x <- x[x != ""]
    N <- length(x)
##    if (N > 1) x <- shorten(x, 20)
    paste(x, collapse = collapse)
}

shorten <- function(x, width)
{
    x <- as.character(x)
    ifelse(nchar(x) > width, 
    {
        long <- c("Ingeniería", "Ingenieriles",
                  "Técnicas", "Operaciones",
                  "Aplicaciones", "Aplicados", "Aplicadas", "Aplicado", "Aplicada",
                  "Propiedades", "Conocimiento",
                  "Transformación",
                  "Sistemas",
                  "Eléctricos", "Electrónicos",
                  "Informáticos",
                  "Industriales", "Industrial")
        longTo <- c("Ing.", "Ing.",
                    "Téc.", "Op.",
                    "Apl.", "Apl.", "Apl.", "Apl.", "Apl.",
                    "Prop.", "Con.",
                    "Trans.",
                    "Sis.",
                    "Elec.", "Elec.",
                    "Inf.",
                    "Ind.", "Ind.")
        short <- c(" de ", " y ", " en ", " a ", " o ",
                   " los ", " las ", " el ", " la ")
        from <- c(long, short)
        to <- c(longTo, rep(" ", length(short)))
        for (i in seq_along(from))
        {
            x <- gsub(from[i], to[i], x)
        }
        trimws(strtrim(x, width))
    }, trimws(x))
}

## Genera un timetable en PDF a partir de un CSV
template <- readLines("timetable.tex")
## hh es un data.table con el contenido de un horario de grupo,
## normalmente a través de leeHorario
csv2tt <- function(hh, nombre, semestre, itinerario = "",
                   colorByTipo = TRUE,
                   dest = tempdir(),
                   preamble = template,
                   hInicio = 8, hFin = 21, hourHeight = 1.1,
                   semString = semestres)
{
    hh <- as.data.table(hh)
    grupo <- as.character(hh$Grupo[1])
    if (missing(nombre)) nombre <- grupo

    ## Recorto el nombre de la asignatura según el espacio disponible
    hh[,
       N := .N, ## Número de asignaturas que coinciden en una misma franja
       by = .(Dia, HoraInicio)]
    
    hh[,
       Asignatura :=
           {
               dh <- diffHour(HoraInicio, HoraFinal)
               width <- 1 / N * 21 * hourHeight *
                   findInterval(dh,
                                seq(1, 8, .5),
                                right = TRUE) 
               shorten(Asignatura, width)
           }
       ]
    ## Algunos horarios tienen una columna de comentarios
    hasComments <- "Comentarios" %in% names(hh)
    ## La columna de comentarios (si existe) irá como footnote
    if (hasComments)
    {
        hh[is.na(Comentarios),
           Comentarios := ""]
        hh[Comentarios != "",
           Asignatura := paste0(Asignatura,
                                "\\footnote{",
                                Comentarios, "}")
           ]
    }
    ## Si dos asignaturas coinciden en horario,
    ## las concatena en un único string
    ## Idem para el tipo, y además abreviamos su descripción
    hh <- hh[,
             list(
                 Asignatura = join(Asignatura, collapse = " / "),
                 Tipo = join(abTipo(Tipo), collapse = "|"),
                 Aula = join(Aula, collapse = "|"),
                 HoraFinal = HoraFinal
             ),
             by = .(Dia, HoraInicio)]
    ## Según el tipo uso un formato u otro (definidos en timetable.tex)
    if (isTRUE(colorByTipo))
    {
        hh[, formato := Tipo]
        hh[!(formato %in% sTipos),
           formato := "misc"]
    } else
    {## color definido por asignatura
        hh[,
           formato := LETTERS[.GRP],
           by = Asignatura]
    }

    ## Ordena por dia y hora de inicio
    setorder(hh, Dia, HoraInicio)
    ## Cabecera del documento, después del preambulo
    header <- c("\\begin{document}",
                "\\begin{center}",
                paste0("\\section*{",
                       paste(nombre, itinerario),
                       " (", semString[semestre],
                       ")", "}"),
                "\\begin{timetable}{",
                hInicio, "}{", hFin, "}{",
                hourHeight, "}")
    ## Hora tuthora y comidas: en los másteres no hay.
    if (grupo %in% masters)
    {
        tuthTex <- ""
        comidaTex <- ""
    } else {
        ## Hora tuthora, dependiendo de grupo de mañana o tarde
        tuthora <- ifelse(MoT(grupo) == "M",
                          braces(tuthoraM),
                          braces(tuthoraT))
        tuthTex <- paste0("\\shortcalentry[tut]{", 1:5,
                          "}", tuthora,
                          "{Hora Tuthora}{}")

        ## Horario de comida
        comida <- ifelse(MoT(grupo) == "M",
                         braces(comedorM),
                         braces(comedorT))

        comidaTex <- paste0("\\shortcalentry[free]{", 1:5, "}",
                            comida,
                            "{Comida}{}")
    }
    ## Franjas horarias de docencia
    htex <- hh[, paste0("\\calentry",
                        "[", formato, "]",
                        "{", match(Dia, dias), "}",
                        "{", formatHora2(HoraInicio), "}",
                        "{", formatHora2(HoraFinal), "}",
                        "{", Asignatura, "}",
                        "{", Tipo, "}",
                        "{", Aula, "}")
               ]
    
    ending <- c("\\end{timetable}",
                "\\end{center}",
                "\\end{document}")
    ## Logos
    file.copy(paste0('../misc/',
                     c('LogoETSIDI.pdf', 'LogoUPM.pdf')),
              dest)
    old <- setwd(dest)
    on.exit(setwd(old))
    texFile <- paste0(grupo, itinerario, "_", semestre, ".tex")
    ##texFile <- make.names(texFile)
    writeLines(c(preamble, header, tuthTex, comidaTex, htex, ending),
               con = texFile)
    system2('pdflatex', texFile)
    files2clean <- list.files('.', "(tex|log|aux)")
    file.remove(files2clean)
    ##    file.remove(c('LogoETSIDI.pdf', 'LogoUPM.pdf'))
}

ttItinerario <- function(hh, nombre, semestre,
                         colorByTipo = TRUE,
                         hInicio = 8, hFin = 21, hourHeight = 1.1,
                         dest = tempdir())
{
    ## Genero un PDF para cada itinerario
    csv2tt(hh[Itinerario %in% c("", "A")],
           nombre, semestre, itinerario = "A",
           colorByTipo = colorByTipo,
           hInicio = hInicio, hFin = hFin,
           hourHeight = hourHeight,
           dest = dest)
    csv2tt(hh[Itinerario %in% c("", "B")],
           nombre, semestre, itinerario = "B",
           colorByTipo = colorByTipo,
           hInicio = hInicio, hFin = hFin,
           hourHeight = hourHeight,
           dest = dest)
    ## Y los junto en un PDF común
    old <- setwd(dest)
    pdfs <- paste0(nombre, c("A", "B"), "_", semestre, ".pdf")
    system2('pdftk', c(pdfs, 
                       "cat",
                       "output",
                       paste0(nombre, "_", semestre, ".pdf")))
    ## borrando los individuales
    file.remove(pdfs)
    setwd(old)
}
