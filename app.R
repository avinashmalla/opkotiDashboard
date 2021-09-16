library(shiny)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(shinydashboard)
library(DT)
library(plotly)
library(ggrepel)
# install.packages("DT")
df <- read_csv('forDash.csv')

r1 = min(unique(df$totalArea))
r2 = max(unique(df$totalArea))
big_cities <- list('Helsinki', 'Espoo', 'Tampere', 'Vantaa', 'Oulu', 'Turku', 'Jyväskylä', 'Kuopio', 'Lahti', 'Pori')

df$link
# Define UI for application
ui <- dashboardPage(
    dashboardHeader(title = "Analyzing housing data"),
    dashboardSidebar(
        sidebarMenu(useShinyjs(), id = "sidebarid",
            menuItem("Graphs",
                     tabName = "graphs-tab",
                     icon = icon("chart-line")),

            menuItem("Tables",
                     tabName = "tables-tab",
                     icon = icon("table")),
            conditionalPanel(
                'input.sidebarid == "tables-tab"',
                radioButtons(
                    inputId = "pcORcity",
                    label = "Choose One:",
                    choices = NULL,
                    selected = "big10Radio",
                    inline = F,
                    width = NULL,
                    choiceNames = list("Postal Code", "City", "Big 10"),
                    choiceValues = list("pcRadio", "cityRadio", "big10Radio")
                ),
                uiOutput("radOutput"),
                uiOutput("distOut"),
                uiOutput("areaInputSlider"),
                uiOutput('houseTypeOut'),

                checkboxGroupInput(
                    "numRooms", "Number of Rooms:",
                    choiceNames =list("1","2","3","4","5+"),
                    choiceValues =
                        list("1","2","3","4","5+"),
                    inline = T,
                    selected = list("1","2","3","4","5+")
                )
            )
        ) #sidebarMenu close
    ), #sidebar close
    dashboardBody(
        tabItems(
            tabItem(tabName = "graphs-tab",
                    tabBox(width = 12,
                           tabPanel("Data Visualization",
                                    fluidRow(column(12,align = "center",
                                                    fluidRow(
                                                        switchInput(inputId = "big10a",
                                                                    label = "Select big 10 cities",
                                                                    inline = T,
                                                                    labelWidth = '64px',
                                                                    size = 'normal',
                                                                    width = '100px',
                                                                    value = F)
                                        ))),
                                    fluidRow(
                                        column(7,fluidRow(plotlyOutput('pl1'))),
                                        column(5,fluidRow(plotOutput('pl2')))
                                        ),
                                    fluidRow(column(12),br(),br()),
                                    fluidRow(column(6, fluidRow(plotlyOutput('pl3'))),
                                             column(6, fluidRow(plotlyOutput('pl4'))),
                                             ),
                                    fluidRow(column(12),br(),br()),
                                    fluidRow(column(6, fluidRow(plotOutput('pl5'))),
                                             column(6, fluidRow(plotOutput('pl6')))
                                             )
                                    ))),
            tabItem(tabName = "tables-tab",
                    tabItem(tabName = "data-table-section",
                            tabBox(width = 12,
                                tabPanel("Data Table",
                                         div(style = 'overflow-x: scroll', dataTableOutput('results'))
                                         )
                                )
                    )
            )
        )
    )
)

server <- function(input, output, session) {

    output$radOutput <- renderUI({
        if(input$pcORcity == 'pcRadio'){
            selectInput("pcodeInput", "Postal Code:",
                        sort(unique(df$postalCode)),
                        selectize = F,
                        multiple = T,
                        selected = unique(df$postalCode)
                        )
        }
        else if(input$pcORcity == 'cityRadio'){
            selectInput("cityInput", "City:", choices = sort(unique(df$city)))
        }
        else{
            selectInput("cityInput", "Big 10 cities", choices = big_cities)
        }
    })

    df0 <- eventReactive(input$cityInput,{
            df %>% filter(city %in% input$cityInput)
    })


    output$distOut <- renderUI({
        if(input$pcORcity == 'cityRadio'){
            selectInput(
                "distInput", "District:",
                choices = sort(unique(df0()$district)),
                selectize = F,
                multiple = T,
                selected = unique(df0()$district))
        }
        else if (input$pcORcity == 'big10Radio'){
            selectInput("big10DistInput", "District:",
                        choices = sort(unique(df0()$district)),
                        selectize = F,
                        multiple = T,
                        selected = unique(df0()$district)
            )
        }
    })

    df3 <- reactive({
        if (input$pcORcity == 'cityRadio'){
            df0() %>% filter(district %in% input$distInput)
        }
        else if (input$pcORcity == "pcRadio"){
            df0() %>% filter(postalCode %in% input$pcodeInput)
        }
        else if (input$pcORcity == 'big10Radio'){
            df0() %>% filter(district %in% input$big10DistInput)
        }
    })

    output$areaInputMin <- renderUI({
        r1 = min(unique(df3()$totalArea))
        r2 = max(unique(df3()$totalArea))
        numericInput("minArea", "Total Area:",
                     width = '100%',value = r1, min = r1, max = r2
        )
    })

    output$areaInputMax <- renderUI({
        r1 = min(unique(df3()$totalArea))
        r2 = max(unique(df3()$totalArea))
        numericInput("maxArea", "",
                     width = '100%', value = r2, min = r1, max = r2
        )
    })

    output$areaInputSlider <- renderUI({
        r1 = min(unique(df3()$totalArea))
        r2 = max(unique(df3()$totalArea))
        sliderInput("slider1", "Total Area", value = c(r1, r2), min = r1, max = r2)
    })

    output$houseTypeOut <- renderUI({
        ch = c(unique(as.character(df3()$listingType)))
        checkboxGroupInput("houseType", "Choose house type:",
                           choices =ch, selected = ch
        )
    })

    df4 <- reactive({
        df3() %>%
            filter(totalArea >= input$slider1[1], totalArea <= input$slider1[2])
    })

    df5 <- reactive({
        df4() %>%
            filter(listingType %in% input$houseType)
    })


    df6 <- reactive({
        df5() %>%
            mutate(n_r = ifelse(numberOfRooms<=4, numberOfRooms, '5+')) %>%
            filter(n_r %in% input$numRooms) %>%
            select(-n_r)
    })


    output$results <- renderDataTable(
        datatable(df6(),escape = F, options = list(pageLength = 10,lengthChange=F),
                  colnames = c(
                      'livingArea(m2)' = 'livingArea',
                      'totalArea(m2)' = 'totalArea',
                      'price/m2' = 'pricePMsq',
                      '#Rooms' = 'numberOfRooms')) %>%
            formatCurrency(columns = c(4,7), currency = "\U20AC") %>%
            formatStyle(columns = c(4,7), 'text-align' = 'right') %>%
            formatStyle(columns = c(3), 'text-align' = 'center')
                  )

    df_graph1 <- eventReactive(input$big10a,{
        if (input$big10a == T){
            df %>%
                filter(city %in% big_cities)
        }
        else{
            df
            }
    })

    output$pl1 <- renderPlotly({

        p <- df_graph1() %>%
            group_by(city) %>%
            summarize(AvgPricePMsq = mean(pricePMsq, na.rm = T)) %>%
            ggplot(aes(x = reorder(city, -AvgPricePMsq), y = AvgPricePMsq, fill = city)) +
            geom_bar(stat = 'identity') +
            xlab('City') + ylab('Average price per meter sq') +
            theme(axis.text.x = element_text(angle = 90), legend.position = 'none') +
            ggtitle("Average Price per sq meters v Cities")

        p <- ggplotly(p)
        p
    })

    output$pl2 <-renderPlot({
        p <- df_graph1() %>%
            group_by(listingType) %>%
            summarise(freq = n()) %>% # freq table for types of houses
            mutate(fraction = freq/sum(freq), # percentages
                   ymax = cumsum(fraction), #cumulative percentages (top of each rectangle)
                   ymin = c(0, head(ymax, n = -1)), #bottom of each rectangle

                   labelPosition = (ymax + ymin) / 2, #position of the label
                   label = paste0(listingType, ":", round(fraction*100,1),"%") #label
            ) %>%
            ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=listingType)) +
            geom_rect() +
            geom_text(x=3.8, aes(y=labelPosition, label=label, color=listingType), size=4) + # x here controls label position (inner / outer)
            scale_fill_brewer(palette = 'PuOr') +
            coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
            xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
            theme_void() +
            ggtitle('Types of houses')+
            theme(legend.position = "none")

        # p <-ggplotly(p)
        p
    })


    output$pl3 <- renderPlotly({
        p <- df_graph1() %>%
        # p <- df %>% filter (city %in% big_cities) %>%
            ggplot(aes(listingType, debtFreePrice, fill = listingType)) +
            geom_boxplot(color = '#3366FF', outlier.colour = "red", outlier.shape = 1, outlier.alpha = 0.2) +
            ylim(0, 1000000)+
            xlab('Type of Building') + ylab('Price')+
            ggtitle('Boxplot of prices for each type of building') +
            theme(legend.position='none')

        p <- ggplotly(p)
        # overrides black outline of outliers
        p$x$data[[1]]$marker$line$color = "red"
        # overrides black extreme outlier color
        p$x$data[[1]]$marker$outliercolor = "red"
        # overrides black not as extreme outlier color
        p$x$data[[1]]$marker$color = "red"

        p
    })


    output$pl4 <- renderPlotly({
            p <- df_graph1() %>%
                mutate(NumRooms = factor(ifelse(numberOfRooms<=4, numberOfRooms, '5+'), levels = c("1","2","3","4","5+"))) %>%
                ggplot(aes(price, colour = NumRooms)) +
                geom_bar(aes(fill = NumRooms))+
                # geom_freqpoly(binwidth = 500) +
                scale_x_binned(limits = c(0,500000)) +
                theme(legend.title = element_text(size = 8))+
                ggtitle("Prices and Number of rooms")

            p <- ggplotly(p)
            p
        })

    output$pl5 <- renderPlot({
        p <- df_graph1() %>%
            filter(city %in% big_cities) %>%
            group_by(city) %>%
            summarise(
                mp = mean(debtFreePrice),
                mbage = mean(buildingAge))%>%
            ggplot(aes(x = mbage, y = mp, label = city)) +
            geom_point(col="red", alpha = 0.6, size = 1) +
            xlab('Avg Building Age') + ylab('Average Price') +
            geom_label_repel(aes(label = city),
                             box.padding   = 0.35,
                             point.padding = 0.5,
                             max.overlaps = 30,
                             segment.color = 'grey50') +
            scale_y_continuous(labels = scales::comma)+
            ggtitle("Average Building Age and Price in Big 10 Cities")

        p
    })

    output$pl6 <- renderPlot({
        p <- df_graph1() %>%
            filter(city %in% big_cities) %>%
            group_by(city) %>%
            summarise(
                mppmsq = mean(pricePMsq),
                mbage = mean(buildingAge))%>%
            ggplot(aes(x = mbage, y = mppmsq, label = city)) +
            geom_point(col="blue", alpha = 0.6, size = 1)  +
            xlab('Avg Building Age') + ylab('Average Price per meter sq') +
            geom_label_repel(aes(label = city),
                             box.padding   = 0.35,
                             point.padding = 0.5,
                             max.overlaps = 40,
                             segment.color = 'grey50')+
            ggtitle("Average Building Age and Price per sq. meters in Big 10 Cities")

        p
    })
}

shinyApp(ui = ui, server = server)
