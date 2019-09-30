dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sidebarMenu(id="sidebarMenu"
                               ,menuItem("Отчет № 3", tabName = "report_3", icon = icon("book"))
                               ,menuItem("Отчет № 6", tabName = "report_6", icon = icon("book"))
                               ,menuItem("Отчет № 7", tabName = "report_7", icon = icon("book")))
                               ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "report_3"
              ,fluidRow(
                column(12, uiOutput("Report_3_UI_com"))
              )
      ),
      tabItem(tabName = "report_6"
              ,fluidRow(
                column(12, uiOutput("Report_6_UI_com"))
              )
      ),
      tabItem(tabName = "report_7"
              ,fluidRow(
                column(12, uiOutput("Report_7_UI_com"))
              )
      )
    )
  )
)