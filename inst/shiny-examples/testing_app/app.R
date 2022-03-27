require(shiny)
require(DT)
require(RShinyHelpers)
require(bslib)

ui <- fluidPage(theme=bslib::bs_theme(),
   
    bootstrap_slider_colour(slider_index = 0,colour = 'mediumseagreen'),
    DT_selected_row_colour(colour = 'red'),
    DT_hover_row_colour(colour = 'pink'),
    DT_peripheral_colour(colour = 'grey'),

    titlePanel("RShinyHelpers Shiny tutorial"),

    sidebarLayout(
        sidebarPanel(style='background-color:transparent;',
            sliderInput("bins",
                        "Dud slider",
                        min = 1,
                        max = 50,
                        value = 30),
            
                        vbox_reactive(label = 'reactive value box',textid = 'textid','mediumseagreen'),
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
          h2('Finance Box',style='text-align:center;'),
          div(style='margin:auto;width:40%;',
          finance_vbox(toplot = -(1:10+runif(n=10,min=-1,max=1)),label = 'market index',textid = '£1M',col = 'black')),
          br(),br(),

          h2('Info box'),
          
          div(style='float:right;',
          vbox_graphic(col = 'darkred',
                       label = 'Example',
                       textid = '€4K',
                       graph_title = 'Market',value = 1:50+rep(1:10,each=5)*runif(50,min = -10,max=+10))
                       
          ),
          vbox(label = 'static value box','hello',col = 'darkred'),
          
          h2('Multi valued info box'),
          four_value(c('BCT','ETH','KLM','DOGE'),c('$4.00','£5.34','€7.47','£9.68'))
           
        )
    ),
           h2('DT styling'),
           br(),
           DT::DTOutput('mtcars'),
           br(),
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    output$mtcars <- renderDT(DT::datatable(mtcars))
    
    output$textid <- renderText(input$bins)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
