library(shinydashboard)

header = dashboardHeader(title="Restaurants you may Like!",
                        titleWidth =300)
body = dashboardBody(
  fluidRow(
    column(width=4,
           box(width=NULL, 
                 selectInput("pick_user", label="User ID:",
                             selected = "u1", multiple =F,
                             choices = paste("u",1:2059))
               
           ),
           box(width=NULL,
               htmlOutput("userinfo",height =400)
           )
    ),
    column(width = 8,
           box(width = NULL, solidHeader = TRUE,
               htmlOutput("restinfo", height=400)
               
           )
           
    )
  )
)
dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body,
  skin="red"
)