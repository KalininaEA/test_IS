dashboardPage(
  dashboardHeader(title = "“Все на дно!”",
                  tags$li(column(12,textOutput("cdate")),class = "dropdown"),
                  tags$li(
                    a(icon("info-circle"), title = "Разработчик"),
                    id="developer",
                    class = "dropdown")),
  dashboardSidebar(sidebarMenu(id="sidebarMenu",
                               menuItem("Отчет № 3", tabName = "report_3", icon = icon("book")),
                               menuItem("Отчет № 6", tabName = "report_6", icon = icon("book")),
                               menuItem("Отчет № 7", tabName = "report_7", icon = icon("book")))
                               ),
  dashboardBody(
    HTML(text="
         <div id='sharebarZakl' onclick='return up()' style='backgroundcolor:white; width:5%; height:auto'>
         <a href='#' onclick='return up()'> <img src='UP.png' alt ='UP' style=' width:50%; height:auto'></img></a></div>
         <script language='javascript' type='text/javascript'>
         $(document).ready(function() {
         $('#sharebarZakl').css({'position':'fixed', 'right':'10px', 'bottom':'10%', 'overflow':'auto', 'padding':'10px 5px', 'text-align':'left', 'background-color':'rgba(202,200,203,0.25)', '-webkit-border-radius':'10px', '-moz-border-radius':'10px', 'border-radius':'10px', 'z-index':'9999999','display':'none'});
         $(window).scroll(function(){
         var top = Math.max(document.body.scroolTop, document.documentElement.scrollTop);

         if($(window).scrollTop()>120){
         $('#sharebarZakl').css({'display':'block'});
         } else {
         $('#sharebarZakl').css({'display':'none'});
         }
         });
         function up(obj_label){
         var t =0;
         var top = Math.max(document.body.scroolTop, document.documentElement.scrollTop);
         if(top>0){
         window.scrollBy(0,-100);
         t = setTimeout('up()',20);
         } else clearTimeout(t);
         return false;

         }
         up('sharebarZakl');

         });
         </script>
         "),
    bsModal(id = "dev",title = h3('Аналитика потребностей пользователей интернет-магазина “Все на дно!”') 
            ,trigger = "developer"
            ,h4("Разработчик: Аналитическое агентство “Последний шанс”")
            ,h4("Версия: 1.0 ")
    ),
    tabItems(
      tabItem(tabName = "report_3",
              fluidRow(
                column(12, uiOutput("Report_3_UI_com"))
              )
      ),
      tabItem(tabName = "report_6",
              fluidRow(
                column(12, uiOutput("Report_6_UI_com"))
              )
      ),
      tabItem(tabName = "report_7",
              fluidRow(
                column(12, uiOutput("Report_7_UI_com"))
              )
      )
    )
  )
)