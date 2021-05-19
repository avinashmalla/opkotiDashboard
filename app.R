library(shiny)
library(shinyjs)
library(tidyverse)
library(shinydashboard)
library(DT)
library(plotly)
# install.packages("DT")
df <- read_csv('forDash.csv')

# df<- df %>%
#     mutate(price = format(round(price, 2), nsmall = 2), pricePMsq = format(round(pricePMsq, 2), nsmall = 2))

r1 = min(unique(df$totalArea))
r2 = max(unique(df$totalArea))

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Analyzing housing data"),
    dashboardSidebar(
        sidebarMenu(useShinyjs(),
            menuItem("Graphs",
                     tabName = "graphs-tab",
                     icon = icon("chart-line")),
            menuItem("Tables",
                     tabName = "tables-tab",
                     icon = icon("table"))
        ), #sidebarMenu close

        radioButtons(
            inputId = "pcORcity",
            label = "Choose One:",
            choices = NULL,
            selected = NULL,
            inline = T,
            width = NULL,
            choiceNames = list("Postal Code", "City"),
            choiceValues = list("pcRadio", "cityRadio")
        ),
        uiOutput("radOutput"),
        uiOutput("distOut"),

        div(
            style="display:inline-block",
            uiOutput('areaInputMin')
            ),
        div(
            style="display:inline-block",
            uiOutput('areaInputMax')
            ),

        uiOutput('houseTypeOut'),

        checkboxGroupInput(
            "numRooms", "Number of Rooms:",
            choiceNames =list("1","2","3","4","5+"),
            choiceValues =
                list("1","2","3","4","5+"),
            inline = T,
            selected = list("1","2","3","4","5+")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "graphs-tab",
                    tabItem(tabName = "graph-section",
                            # actionButton("hideshow", "Hide/show Amenities"),
                            # uiOutput('amen'),
                            # box(width=12,height = 800,
                            div(style = 'overflow-x: scroll', plotOutput('pl1')),
                            div(style = 'overflow-x: scroll', plotlyOutput('pl2')),
                            div(style = 'overflow-x: scroll', plotlyOutput('pl3')),

                            # )
                    )
            ),
            tabItem(tabName = "tables-tab",
                    tabItem(tabName = "data-table-section",
                            # actionButton("hideshow", "Hide/show Amenities"),
                            # uiOutput('amen'),
                            # box(width=12,height = 800,
                            div(style = 'overflow-x: scroll', dataTableOutput('results'))
                            # )
                    )
            )
            ),
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
        else{
            selectInput("cityInput", "City:", choices = sort(unique(df$city)))
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
    })

    df2 <- reactive({
        df0() %>% filter(district %in% input$distInput)
    })

    df3 <- reactive({
        if (input$pcORcity == "pcRadio"){
            df %>% filter(postalCode %in% input$pcodeInput)
        }
        else{df2()}
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

    output$houseTypeOut <- renderUI({
        ch = c(unique(as.character(df3()$listingType)))
        checkboxGroupInput("houseType", "Choose house type:",
                           choices =ch, selected = ch
        )
    })

    df4 <- reactive({
        df3() %>%
            filter(totalArea >= input$minArea, totalArea <= input$maxArea)
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

    output$amen <- renderUI({
        checkboxGroupInput("amenities",
                           "Must have:",
                           choiceNames =list("Centrum",
                                             "Sauna",
                                             "Balcony",
                                             "Parking",
                                             "Walk-in Closet",
                                             "StorageRoom"),
                           choiceValues = list("centrum",
                                               "hasSauna",
                                               "hasBalcony",
                                               "hasParking",
                                               "hasWalkInCloset",
                                               "hasStorageRoom"),
                           selected = list("centrum", "hasSauna"),
                           inline = T
        )
    })

    observeEvent(input$hideshow, {
        toggle("amen", anim = T)
    })

    df7 <- reactive({
        ch = input$amenities
        df6() %>%
            filter(df6()[ch] == '1')
    })

    output$results <- renderDataTable(
        datatable(df6(),escape = F, options = list(pageLength = 10,lengthChange=T),
                  colnames = c(
                      'livingArea(m2)' = 'livingArea',
                      'totalArea(m2)' = 'totalArea',
                      'price/m2' = 'pricePMsq',
                      '#Rooms' = 'numberOfRooms')) %>%
            formatCurrency(columns = c(4,7), currency = "\U20AC") %>%
            formatStyle(columns = c(4,7), 'text-align' = 'right') %>%
            formatStyle(columns = c(3), 'text-align' = 'center')
                  )

    output$pl1 <- renderPlot({
        df %>%
            # mutate(lprice = log(pricePMsq)) %>%
            # ggplot(aes(x = lprice, fill = ..count..)) +
            # geom_histogram(binwidth = 0.05)
            ggplot(aes(x = pricePMsq, fill = ..count..)) +
            geom_histogram(binwidth = 50)
            # xlim(5,15)
    })

    output$pl2 <- renderPlotly({
        p <- df %>%
            ggplot(aes(x = factor(listingType), y = price, fill = factor(listingType))) +
            geom_boxplot(alpha=0.2) +
            stat_summary(fun.y=mean, geom="point", shape=20, size=4, color="red", fill="red")+
            theme(legend.position="none")+
            ggtitle("Figure 4 Boxplot of Price by Listing Type")+
            theme(plot.title = element_text(hjust = 0.5)) +
            ylim(0,1e+06)

        p <- ggplotly(p)
        p
    })

    output$pl3 <- renderPlotly({
        p <- df %>%
            mutate(n_r = ifelse(numberOfRooms<=4, numberOfRooms, '5+'), lprice = log(price)) %>%
            ggplot(aes(price)) +
            geom_histogram(aes(fill = n_r), position = position_stack(reverse = TRUE), binwidth = 25000)+
            coord_flip()

        p <- ggplotly(p)
        p
    })

}

shinyApp(ui = ui, server = server)
