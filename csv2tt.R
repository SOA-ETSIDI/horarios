library(data.table)

source('../aux/defs.R')
source('../aux/funciones.R')

semestres <- c("Septiembre-Enero", "Febrero-Junio")

## Lee y escribe horarios en CSV
leeHorario <- function(grupo, semestre)
{
    hh <- fread(paste0('../data/horarios/',
                       grupo, '_', semestre,
                       '.csv'),
                stringsAsFactors = TRUE)
    hh[, Dia := factor(Dia, levels = dias, ordered = TRUE)]
    hh[, Aula := as.character(Aula)]
    setkey(hh, Dia, HoraInicio)
    hh
}

escribeHorario <- function(df, grupo, semestre)
{
    path <- '../data/horarios/'
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
## Horas posibles en selector
horas <- hhSeq(h1 = "08:15", h2 = "21:15", by = "30 min")

## Utilidad para juntar en una cadena asignaturas o tipos de docencia que coinciden en horario
join <- function(x, collapse = ' \\\\ ')
{
    x <- unique(x)
    N <- length(x)
    if (N > 1) x <- shorten(x, 20)
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
                  "Industriales", "Industrial")
        longTo <- c("Ing.", "Ing.",
                    "Téc.", "Op.",
                    "Apl.", "Apl.", "Apl.", "Apl.", "Apl.",
                    "Prop.", "Con.",
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
template <- readLines("../aux/timetable.tex")
## hh es un data.table con el contenido de un horario de grupo,
## normalmente a través de leeHorario
csv2tt <- function(hh, grupo, semestre, itinerario = "",
                   dest = tempdir(),
                   preamble = template,
                   semString = semestres)
{
    hh <- as.data.table(hh)
    
    ## Si dos asignaturas coinciden en horario,
    ## las concatena en un único string
    ## Idem para el tipo, y además abreviamos su descripción
    hh <- hh[,
             .(
                 Asignatura = join(Asignatura, collapse = " / "),
                 Tipo = join(abTipo(Tipo), collapse = "|"),
                 HoraFinal,
                 dh = diffHour(HoraInicio, HoraFinal)),
             by = .(Dia, HoraInicio)]
    ## Recorto el nombre de la asignatura según el espacio disponible
    hh[, width := findInterval(dh, seq(1, 4, .5), right = TRUE) * 20]
    ## Según el tipo uso un formato u otro (definidos en timetable.tex)
    hh[, formato := Tipo]
    hh[!(formato %in% sTipos),
       formato := "misc"]
    
    ## Cabecera del documento, después del preambulo
    header <- c("\\begin{document}",
                "\\begin{center}",
                paste0("\\section*{",
                       paste(grupo, itinerario),
                       " (", semString[semestre],
                       ")", "}"),
                "\\begin{timetable}{8}{21}")
    ## Hora tuthora, dependiendo de grupo de mañana o tarde
    tuthora <- ifelse(MoT(grupo) == "M",
                      "{11.15}{11.45}",
                      "{17.15}{17.45}")
    tuthTex <- paste0("\\shortcalentry[tut]{", 1:5,
                      "}", tuthora,
                      "{Hora Tuthora}{}")

    ## Horario de comida
    comida <- paste0("\\shortcalentry[free]{", 1:5,
                      "}", "{13.45}{15.15}",
                     "{Comida}{}")
    ## Franjas horarias de docencia
    htex <- hh[, paste0("\\calentry",
                        "[", formato, "]",
                        "{", match(Dia, dias), "}",
                        "{", formatHora2(HoraInicio), "}",
                        "{", formatHora2(HoraFinal), "}",
                        "{", shorten(Asignatura, width), "}",
                        "{", Tipo, "}")
               ]
    
    ending <- c("\\end{timetable}",
                "\\end{center}",
                "\\end{document}")
    ## Logos
    file.copy(paste0('../aux/',
                    c('LogoETSIDI.pdf', 'LogoUPM.pdf')),
              dest)
    old <- setwd(dest)
    texFile <- paste0(grupo, itinerario, "_", semestre, ".tex")
    writeLines(c(preamble, header, tuthTex, comida, htex, ending),
               con = texFile)
    system2('pdflatex', texFile)
    files2clean <- list.files('.', "(tex|log|aux)")
    file.remove(files2clean)
    setwd(old)
}

