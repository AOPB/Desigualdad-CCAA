library(shiny)
library(tidyverse)
library(ggtext)
library(glue)

load("www/B.Rda")
highlight = function(x, pat, color="black", family="") {
  ifelse(grepl(pat, x), glue("<b style='font-family:{family}; color:{color}'>{x}</b>"), x)
}



Temporal<-function(Comunidad){
  B%>%filter(CCAA==Comunidad)%>%
  ggplot(aes(x=as.numeric(Anys),y=Valors), alpha=5)+
  geom_point(aes(color=Noms))+
  geom_line(aes(color=Noms))+geom_text(aes(label=round(Valors,2)), size=2.5, vjust=-1)+
  theme_minimal()+labs(title=paste("Evolución de la desigualdad en",Comunidad,"(2007-2018)"),y="Gini",x="")+
  theme(legend.title = element_blank(), plot.title=element_text(h=0.5),panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank())+
  scale_color_brewer(palette = "YlOrRd")
}



Comparativo<-function(fecha, renta){
  B%>%filter(Anys==fecha)%>%filter(Renta==renta)%>%
  ggplot(aes(x=reorder(CCAA, Valors), y=Valors, fill=Color))+
  geom_col(alpha=0.85)+
  geom_text(aes(label=round(Valors,2)),color="white", size=2.7, position=position_stack(vjust=0.5))+
  theme_minimal()+
  labs(title=paste("Coeficiente de Gini de la",renta,"en las CCAA para el año",fecha),y=NULL,x=NULL)+
  theme(legend.title = element_blank(), plot.title=element_text(h=0.5, size = 15, colour = "grey20"),
        panel.grid.minor.y = element_blank(),panel.grid.major.y = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.major.x = element_line(color = "gray", linetype = 8),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8),
        text = element_text(family = "OfficinaSansITC", size = 11),
        legend.position = "none")+
  scale_fill_manual(values=c("#014d64","#e3120b"))+
  scale_x_discrete(labels= function(x) highlight(x, "España", "#e3120b")) +
  theme(axis.text.y=element_markdown())+
  coord_flip()}

levels(B$Renta)
library(shinythemes)
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(title = "Índice",
                tabPanel(title="Comparativa",
                         tags$head(
                           tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Arial');
                                           h1 {
                                           font-family: 'Arial';
                                           font-style: normal; /* normal, italic, oblique.. */
                                           font-weight: normal /* normal, bold, num */
                                           line-height: 0.4; /* separación línies */
                                           color: #B40404
                                           }"))
                                   ),
                         headerPanel('La desigualdad en las CCAAs'),
                         wellPanel(
                           selectInput("Renta", "Tipo de renta**", c("Renta de Mercado","Renta Bruta","Renta Disponible")),
                           selectInput("Ano", "Año", as.character(2007:2018), selected="2018")),
                         tags$em("** La",tags$strong("Renta de Mercado"), "es la renta salida directamente del sector privado.
                                 En la ",tags$strong("Renta Bruta"), ", además, se incluyen las transferencias.
                                 Finalmente, la ",tags$strong("Renta Disponible"), " incluye, no solo transferencias, sino también impuestos directos"),
                         br(),
                         br(),
                         textOutput("Explicacion"),
                         br(),
                         plotOutput("GraficoA"),
                         br(),
                         strong("¿Qué es el Coeficiente de Gini?"),
                         p("Es una medida de desigualdad que va desde 0 a 1, en donde 0
                           se corresponde con la perfecta igualdad (todos tienen los mismos
                           ingresos) y donde el valor 1 se corresponde con la perfecta
                           desigualdad (una persona tiene todos los ingresos y los demás ninguno).")),
                tabPanel(title="Evolución",
                                    tags$head(
                                      tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Arial');
                                                      h1 {
                                                      font-family: 'Arial';
                                                      font-style: normal; /* normal, italic, oblique.. */
                                                      font-weight: normal /* normal, bold, num */
                                                      line-height: 0.4; /* separación línies */
                                                      color: #B40404
                                                      }"))
                                   ),
                         headerPanel('Evolución de la desigualdad por CCAAs'),
                         wellPanel(
                  selectInput("Comunidad", "Comunidad autónoma:", levels(B$CCAA))),
                  br(),
                  br(),
                          plotOutput("GraficoB"),
                br(),
                          strong("¿Qué es el Coeficiente de Gini?"),
                          p("Es una medida de desigualdad que va desde 0 a 1, en donde 0
                            se corresponde con la perfecta igualdad (todos tienen los mismos
                            ingresos) y donde el valor 1 se corresponde con la perfecta
                            desigualdad (una persona tiene todos los ingresos y los demás ninguno)."))))


server <- function(input, output) {
  output$GraficoA<-renderPlot({
    Comparativo(fecha=input$Ano, renta=input$Renta)})
output$GraficoB<-renderPlot({
  Temporal(Comunidad=input$Comunidad)})}


shinyApp(ui = ui, server = server)




