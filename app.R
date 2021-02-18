library("shiny")
library("data.table")
library("ggplot2")

skills <- as.data.table(read.csv("techstack.csv"))
skills[,Category:=as.factor(Category)]

numerics <- which(sapply(skills,is.numeric))
numericcolumns <- names(skills[ , numerics, with=FALSE])


ui <- fluidPage(
    tags$head(

      tags$style(HTML("
      body {
        background-color: #141010;
        color: #fff6fb;
        font-family: -apple-system,BlinkMacSystemFont,'Roboto','Segoe UI','Helvetica Neue','Lucida Grande',Arial,sans-serif;
      }
      .shiny-input-container {
        color: ##fff6fb;
      }"))
    ),

    fluidRow(
        column(12,
               
           titlePanel("Tims Interactive Tech Stack"),
           
           
           tags$div("This interactive tech stack displays my technical and methodlogical skills according to some dimensions (e. g. years of experience, rate of daily usage) - Variables see below."),
           
           tags$br(),
           
           tags$div("Feel free to experiment with the inputs on the left. The interactive graph was created with ggplot2 and shiny in R. Raw data and code are available via GitHub:"),
           
           tags$a("https://github.com/tim-sadler/techstack", href="https://github.com/tim-sadler/techstack"),
           
           tags$br(),  

           tags$br(),   
           
        )
    ),
    
    fluidRow(
        column(4,
            selectInput("dimY",
                        "Y-Axis:",
                        numericcolumns,
                        numericcolumns[1]),
            selectInput("dimX",
                        "X-Axis:",
                        numericcolumns,
                        numericcolumns[2]),
            sliderInput("jitterlevel","Jitter:",0,1,0.5,step=0.1)
        ),
        column(4,
               
               checkboxGroupInput("cats","Categories:",levels(unique(skills[,Category])), selected = levels(unique(skills[,Category]))[c(1,3,6)])
               
        ),
        column(4,
               
               
               tags$div(tags$strong("Variables:")),
               tags$div("Years of experience,"),
               tags$div("daily usage,"),
               tags$div("enthusiam (while using, scaled from 1 to 5),"),
               tags$div("(subjective) ease of use (1, hard to 5, simple)")
               
        )
    ),
    plotOutput("distPlot", width = "100%"),
    tags$br()

)


server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        xl <- skills[Category %in% input$cats,unique(max(.SD)),.SDcols = c(input$dimX)]
        yl <- skills[Category %in% input$cats,unique(max(.SD)),.SDcols = c(input$dimY)]

        ggplot() + 
            geom_text(data = skills[Category %in% input$cats], aes_string(x=input$dimX,y=input$dimY,label='Name',color='Category'),position=position_jitter(width = 0.5, height = input$jitterlevel, seed = 42),size=5, fontface = "bold", show.legend = FALSE) + 
            geom_point(data = skills[Category %in% input$cats], aes_string(x=input$dimX,y=input$dimY,color='Category'),size=NA) + 
            scale_x_continuous(limits=c(0, xl+1)) +
            scale_y_continuous(limits=c(0, yl+1)) +
            coord_equal(ratio=(xl/(yl))) +
            theme(legend.position="bottom", legend.background = element_rect(fill = "transparent",colour = "#141010"), legend.text = element_text(size = 12, color = "#fff6fb")) +
            theme(plot.background = element_rect(fill = "#141010")) +
            theme(
                panel.grid.major = element_line(colour = "#fff6fb"),
                panel.grid.minor = element_line(colour = "#fff6fb"),
                panel.background = element_rect(fill = "transparent",colour = "#141010"),
            ) +
            theme(legend.key=element_blank()) +
            guides(colour=guide_legend(override.aes=list(size=6,fill=NA))) +
            theme(axis.text=element_text(size=12,color="#fff6fb"),plot.subtitle=element_text(size=10,color="#fff6fb"),axis.title=element_text(size=14,face="bold",color="#fff6fb"),title=element_text(size=16,color="#fff6fb"),axis.text.x=element_text(color="#fff6fb"),text=element_text(color="#fff6fb"))

    }, height = 700, bg="transparent")
}

shinyApp(ui = ui, server = server)
