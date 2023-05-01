library(shiny)

ui <- fluidPage(
    titlePanel("Diagnostic test performance demo"),
    sidebarLayout(
        sidebarPanel(
          sliderInput("sens", "Sensitivity (%):", min = 0, max = 100, value = 70),
          sliderInput("sensDec", "Sensitivity (add decimals):", min = 0, max = 100, value = 0),
          sliderInput("spec", "Specificity (%):", min = 0, max = 100, value = 95),
          sliderInput("specDec", "Specificity (add decimals):", min = 0, max = 100, value = 0),
          sliderInput("prev", "Pre-test probability (%):", min = 0, max = 100, value = 5)
        ),
        mainPanel(
           plotOutput("pvplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  PPV <- function(x, sens, spec) (sens*x) / (sens*x + (1-spec)*(1-x))
  NPV <- function(x, sens, spec) (spec*(1-x)) / (spec*(1-x) + (1-sens)*x)
  
  output$pvplot <- renderPlot({
    prev <- input$prev/100
    sens <- min(input$sens/100 + input$sensDec/10^4, 1)
    spec <- min(input$spec/100 + input$specDec/10^4, 1)
    ppv <- PPV(prev, sens, spec)
    npv <- NPV(prev, sens, spec)
    
    curve(PPV(x, sens, spec), from=0, to=1, lwd=2, col="blue", ylim=c(0,1.01),
          main=sprintf("PPV/NPV for sensitivity=%s%% and specificity=%s%%", sens*100, spec*100),
          xlab="Pre-test probability", ylab="PPV/NPV (%)", 
          xaxt="n", yaxt="n", xaxs="i", yaxs="i", bty="l")
    curve(NPV(x, sens, spec), from=0, to=1, lwd=2, col="red", ylim=c(0,1), add=TRUE)
    
    abline(v=prev, lty="dotted")
    axis(1, at=axTicks(1), labels=axTicks(1)*100)
    axis(2, at=axTicks(2), labels=axTicks(2)*100)
    lines(c(0,prev), rep(ppv,2), lty="dotted")
    lines(c(0,prev), rep(ppv,2), lty="dotted")
    points(rep(prev,2), c(ppv, npv))
    legend("bottom", c("PPV","NPV"), col=c("blue","red"), lwd=2, horiz=TRUE, bty="n")
    mtext(sprintf("For pre-test probability %s%%: PPV=%s%%, NPV=%s%%", prev*100, round(ppv*100,1), round(npv*100,1)), side=3, line=0)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
