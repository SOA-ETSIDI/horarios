library(rhandsontable)
library(shiny)
library(shinyjs)

source('csv2tt.R')
addResourcePath("pdf", "pdf/")

logoUPM <- "http://www.upm.es/sfs/Rectorado/Gabinete%20del%20Rector/Logos/UPM/EscPolitecnica/EscUpmPolit_p.gif"
logoETSIDI <- "http://www.upm.es/sfs/Rectorado/Gabinete%20del%20Rector/Logos/EUIT_Industrial/ETSI%20DISEN%C2%A6%C3%A2O%20INDUSTRIAL%20pqn%C2%A6%C3%A2.png"


## Cabecera con logos
header <- fluidRow(
    column(4, align = 'center', img(src = logoUPM)),
    column(4, align = 'center',
           h2("Horarios y aulas"),
           h5("Subdirección de Ordenación Académica")),
    column(4, align = 'center', img(src = logoETSIDI))
)

selector <- wellPanel(
    fluidRow(
        column(12,
               selectInput('semestre', "Semestre: ",
                           choices = semestres,
                           selected = semestres[1]))
    ),
    fluidRow(
        column(12, selectInput('grupo', "Grupo: ",
                              choices = grupos,
                              selected = "M101"))
        )
    )

editor <- div(id = 'editor',
              fluidRow(
                  column(12, 
                         rHandsontableOutput('table'))
              ),
              br(),
              fluidRow(
                  column(12,
                         actionButton("update",
                                      "Confirmar",
                                      icon = icon("check"))
                         )
              ))

pdfUI <- div(id = 'pdfUI',
             fluidRow(column(12,
                             actionButton("refresh",
                                          "Actualizar",
                                          icon = icon("refresh"))
                             )),
             fluidRow(column(12,
                             htmlOutput("pdfViewer")
                             ))
             )

## UI completa
shinyUI(
    fluidPage(
        useShinyjs(),  # Set up shinyjs
        includeCSS("styles.css"),
        header,
        fluidRow(
            column(2, selector),
            column(10,
                   tabsetPanel(
                       tabPanel('Tabla', editor),
                       tabPanel('Vista', pdfUI)
                   ))
            )
    ))

