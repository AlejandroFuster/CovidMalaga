#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

my_packages = c("readr", "dplyr", "ggplot2","reshape2","polyreg","pracma","scales","DT")
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)
install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))

port <- Sys.getenv('PORT')

library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(polyreg)
library(pracma)
library(scales)
library(DT)

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Malaga"),

    plotOutput('distPlot'),
    
    hr(),
    
    fluidRow(
        column(3,
               h4("Estimacion de contagios reales"),
               numericInput("tasa", "Tasa de mortalidad:",  
                           min = 0, max = 1, value = 0.37),
               br(),
               textOutput("contagios")
        ),
        column(4,
               h4("Datos"),
               DT::dataTableOutput("mytable"))
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    url <<- "https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/stpivot/stpivot/Print?cube=a2a26120-d774-4fe0-b3f1-202eb541551f&type=3&foto=si&ejecutaDesde=&codConsulta=38228&consTipoVisua=JP"
    showModal(modalDialog("Cargando datos...", footer=NULL))
    data <<- read_delim(url, 
                        ";", escape_double = FALSE, col_types = cols(Fecha = col_date(format = "%d/%m/%Y"), 
                                                                     Territorio = col_character(), X5 = col_skip()), 
                        locale = locale(date_names = "es"), na = "NA", 
                        trim_ws = TRUE)
    
    
    df = filter(data,grepl("laga",Territorio),Medida=="Confirmados")
    df = with(df, df[order(Fecha),])
    df$dia <- seq.int(nrow(df))
    df$casosNuevos <<- c(0,diff(df$Valor))
    l <<- lm(Valor ~ dia + I(Valor^2) + I(Valor^3) + I(Valor^4), data=df)
    estima <<- function(f,dia){
        return(f[1]*dia^4+f[2]*dia^3+f[3]*dia^2+f[4]*dia+f[5])
    }
    
    ll <<- data.frame(Fecha=seq(df$Fecha[1],by="day",length.out=(nrow(df)+10)),
                      dia = seq(nrow(df)+10))
    
    ll$Valor = estima(f,ll$dia)
    
    output$distPlot <- renderPlot({
        removeModal()
        ggplot(data=df,aes(Fecha,Valor)) + 
            geom_point(data=df,colour="hotpink") + 
            geom_line(data=df,colour="hotpink") +
            geom_line(data=ll,linetype=6,colour="red") + 
            geom_point(data=ll[ll$Fecha>tail(df,1)$Fecha,],colour="red") + 
            scale_y_continuous(limits = c(0, 5000), breaks = seq(0, 7400, by = 200)) +
            scale_x_date(date_breaks = "3 days",date_labels="%d/%m",limits=c(df$Fecha[1],tail(ll,1)$Fecha))
        
    })
    
    observeEvent(input$tasa,{
        fall = filter(data,grepl("laga",Territorio),Medida=="Fallecimientos")
        fall <- with(fall, fall[order(Fecha),])
        ult = tail(fall,1)$Valor
        contagiosReales <- function(TasaMortalidad){
            return(ult*100/TasaMortalidad)
        }
        str <- c("Contagios reales: ")
        str <- c(str,round(contagiosReales(input$tasa),digits=0))
        output$contagios <- renderText({paste(str)})
    })
    
    output$mytable = DT::renderDataTable({
        df
    })
    
    
}

# Run the application 
shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port),
  ui = ui,
  server = server
)

