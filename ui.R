sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Altmetric.com", tabName = "altmetric", icon = icon("dashboard")),
    menuSubItem("Sunburst", tabName = "sun", icon = icon("angle-double-right"), selected = NULL),
    menuSubItem("Data", tabName = "data", icon = icon("angle-double-right"), selected = NULL),
    menuSubItem("Organization", tabName = "org", icon = icon("angle-double-right"), selected = NULL),
    menuSubItem("Pivot", href = "https://ttso.shinyapps.io/acrisaltmetricspivot", icon = icon("angle-double-right"), selected = NULL),
    menuItem("Impactstory", tabName = "is", icon = icon("dashboard")),
    selectInput(inputId = "school", 
                label = "School", 
                choices = c("All", schools),
                multiple = FALSE,
                selected = "All"),
    selectInput("xc", "Horizontal axis", as.list(metrics), selected = "Twitter"),
    selectInput("yc", "Vertical axis", as.list(metrics), selected = "Mendeley")
  )
)


body <- dashboardBody(
  
  
  # https://github.com/timelyportfolio/sunburstR/issues/3
  tags$head(
    tags$style(type = "text/css", "#sunburst { width: 500px; height: 500px; position: relative; }"),
    tags$style(type = "text/css", ".small-box p { font-size: 15px; }")
    ),

  tabItems(
    
    tabItem("altmetric",
            fluidRow(
              column(
                width = 8,
                box(title = "Scatterplot by School",
                    status = "success",
                    solidHeader = TRUE,
                    width = "100%",
                    height = "600px",
                    ggvisOutput("gv"))
              ),
              column(
                width = 4,
                valueBoxOutput("nrofitemswithmetrics", width = "100%"),
                valueBoxOutput("maxaltmetrics", width = "100%"),
                valueBoxOutput("maxgplus", width = "100%"),
                valueBoxOutput("maxfb", width = "100%"),
                valueBoxOutput("maxwikipedia", width = "100%"))
            ),
            fluidRow(
                box(title = "Select max 2 items",
                  width = 4,
                  height = "300px",
                  selectizeInput(inputId = 'items', label = 'Items', choices = NULL, options = list(maxItems = 2))
                  ),
                box(title = "Barchart",
                  status = "success",
                  solidHeader = TRUE,
                  width =  8,
                  height = "600px",
                  showOutput("chart", "nvd3"),
                  HTML('<style>.rChart {width: 100%; height: 400px}</style>'))
                )
    ),
    
    tabItem("data",
            fluidRow(
              box(title = "Table",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  height = "600px",
                  DT::dataTableOutput("datatable", 
                                      width = "100%",
                                      height = "600px"))
              )
    ),
    
    
    tabItem("is",
            fluidRow(
              box(title = "Collection",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  height = "200px",
                  selectizeInput(
                    inputId = "istitle", 
                    label = "Select item",
                    width = "100%",
                    choices = c("", sort(tolower(unique(paste0(issstats$Type, " ", issstats$Title))))),
                    options = list(maxItems = 1)
                    ),
                    uiOutput("item")
                  )
              ),
            fluidRow(
              box(title = "Weekly statistics",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  heigth = "400px",
                  dygraphOutput("dygraph", width = "100%", height = "400px"))
            )
    ),
    
    tabItem("org",
            fluidRow(
              column(
                width = 12,
                box(title = "Units by School",
                    status = "info",
                    solidHeader = TRUE,
                    width = "100%",
                    height = "600px",
                    forceNetworkOutput("force", width = "100%", height = "500px"))
              )
            )
    ),
    tabItem("sun",
            fluidRow(
              column(
                width = 6,
                box(title = "Altmetric.com data by unit",
                    status = "info",
                    solidHeader = TRUE,
                    width = "100%",
                    height = "600px",
                    sunburstOutput("sunburst", width = "100%", height = "500px")),
                    downloadButton('downloadSun', 'Download')
              )
            )
    )
  ))


dashboardPage(
  dashboardHeader(title = "Altmetrics",
                  titleWidth = "500"),
  sidebar,
  body,
  skin = "black"
)

