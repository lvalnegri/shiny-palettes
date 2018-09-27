pkgs <- c(
    'classInt', 'data.table', 'shiny', 
    'RColorBrewer', 'rcartocolor', 'cartography', 'ghibli', 'Redmonder', 'scico', 'inlmisc', 'viridis', 'wesanderson'
)
lapply(pkgs, require, char = TRUE)
palettes <- fread('palettes.csv')
pkg_names <- unique(palettes$package)
source('palettes.R')

ui <- bootstrapPage(
    
    selectInput('cbo_pkg_nms', 'PACKAGE:', choices = pkg_names),
    
    uiOutput('ui_pkg_nms'),
    
    sliderInput('sld_brp_cls', 'NUMBER OF CLASSES:', min = 3, max = 20, value = 7, ticks = FALSE),
    
    checkboxInput('chk_rev', 'REVERSE COLOURS'),
    
    plotOutput('plot')
    
)


server <- function(input, output){

    output$ui_pkg_nms <- renderUI({
        
        y <- palettes[package == input$cbo_pkg_nms]
        
        attr.lst <- structure(
                lapply( 
                    unique(y$type), 
                    function(x) 
                        structure(
                            y[type == x, name], 
                            names = paste0(y[type == x, title], ' [', y[type == x, max_cols], ']')
                        )
                ), 
                names = toupper(unique(y$type))
        )
        
        selectInput('cbo_pal_nms', 'PALETTE:', choices = attr.lst)
        
    })
    
    output$plot <- renderPlot({ 
        
        n_breaks <- input$sld_brp_cls
        
        barplot(
            1:n_breaks, 
            col = get_colours_codes(
                        n_breaks, 
                        pkg_name = input$cbo_pkg_nms, 
                        pal_name = input$cbo_pal_nms, 
                        is_pal_rev = input$chk_rev
            )
        )
        
    })

}

shinyApp(ui = ui, server = server)

