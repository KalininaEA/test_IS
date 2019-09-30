library(shiny)
server <- function(session,input, output) {

  #----Отчет №3----
  
  output$Report_3_UI_com=renderUI({ 
    fluidRow(
      box(status="primary",solidHeader=T,title="Составление отчета № 3", width=12
          ,fluidPage(
            column(12,h2("  "))
            ,column(12
                    ,column(6,selectInput("category_selectInput", "Категория товара", table_translite_category["ru"])))
            ,column(12
                    ,column(3,dateInput("repot_3_day1", label="Дата начала периода", value="2018-08-03", min="2018-08-03", max="2018-08-15", format = "dd.mm.yyyy", weekstart = 1,  language = "ru"))
                    ,column(2,timeInput("repot_3_time1", label="Время", seconds=F))
                    ,column(1,h2(" "))
                    ,column(3,dateInput("repot_3_day2", label="Дата окончания периода", value="2018-08-14", min="2018-08-03", max="2018-08-14", format = "dd.mm.yyyy", weekstart = 1,  language = "ru"))
                    ,column(2,timeInput("repot_3_time2", label="Время", seconds=F))
                    ,column(2,"")
            )
            ,column(12, h2("  "))
            ,column(12,
                    column(3,actionButton(inputId="DisplayReport_3",label="Отобразить"))
            )
            ,column(12, h2("  "))
            ,column(12, uiOutput("Report_3_UI"))
          )
      )
    )
  })
  
  observeEvent(input$DisplayReport_3,{
    
    output$Report_3_UI=renderUI({
      data1<<-input$repot_3_day1
      time1<<-input$repot_3_time1
      data2<<-input$repot_3_day2
      time2<<-input$repot_3_time2
      date0=as.character(paste0(data1, " ", strftime(time1,"%T")))
      date1=as.character(paste0(data2, " ", strftime(time2,"%T")))
      if (as.POSIXct(date1)>as.POSIXct(date0))
      {
        table_goods_page=table_goods_page_func(date0,date1)
        if (dim(table_goods_page)[1]>0)
        {
          table=table_category_time_func(table_goods_page, category=input$category_selectInput)
          table_max_sum=max(table$sum)
          period=table[which(table$sum==table_max_sum),"period"]
          period_name=ifelse(period=="Утро (04:00 - 13:00)", "утром",
                             ifelse(period=="День (13:00 - 17:00)", "днем",
                                    ifelse(period=="Вечер (17:00 - 22:00)", "вечером",
                                           ifelse(period=="Ночь (22:00 - 04:00)", "ночью"
                                           ))))
          fluidRow(
            box(status="primary",solidHeader=T,title=paste0("Отчет по категории ", isolate({input$category_selectInput})), width=12,
                renderText({ isolate({paste0("В период с ", format(as.POSIXct(paste0(data1, " ", strftime(time1,"%T"))), "%H:%M %d.%m.%Y"), 
                                             " по ", format(as.POSIXct(paste0(data2, " ", strftime(time2,"%T"))), "%H:%M %d.%m.%Y"), " категорию ", input$category_selectInput,
                                             " чаще всего просматривали ",period_name, " (", table_max_sum, " раз).")}) }),
                renderDataTable({
                  names(table)=c("Период времени суток", "Количество посещений")
                  datatable(table,selection = "single",
                            options =
                              list(pageLength=dim(table)[1],
                                   dom = '',
                                   rownames = F,
                                   columnDefs = list(list(className='dt-center', targets=2))
                              )
                  )
                })
            ))
          } else
          {
            fluidRow(
              box(status="primary",solidHeader=T,title=paste0("Отчет по категории ", isolate({input$category_selectInput})), width=12,
                  renderText({ isolate({paste0("В период с ", format(as.POSIXct(paste0(data1, " ", strftime(time1,"%T"))), "%H:%M %d.%m.%Y"), 
                                               " по ", format(as.POSIXct(paste0(data2, " ", strftime(time2,"%T"))), "%H:%M %d.%m.%Y"), " категорию ", input$category_selectInput,
                                               " не просматривали")}) })
              )
            )
          }
      } else
      {
        fluidRow(
          column(12,renderText({ paste0("Выберите корректный период времени")}) )
)
      }
     
    })
  })
  
  #----Отчет №6----
  
  output$Report_6_UI_com=renderUI({ 
    fluidRow(
      box(status="primary",solidHeader=T,title="Составление отчета № 6 'Количество брошенных (не оплаченных) корзин отчетный период времени'", width=12
          ,fluidPage(
            column(12,h2("  "))
            ,column(12
                    ,column(3,dateInput("repot_6_day1", label="Дата начала периода", value="2018-08-03", min="2018-08-03", max="2018-08-15", format = "dd.mm.yyyy", weekstart = 1,  language = "ru"))
                    ,column(2,timeInput("repot_6_time1", label="Время", seconds=F))
                    ,column(1,h2(" "))
                    ,column(3,dateInput("repot_6_day2", label="Дата окончания периода", value="2018-08-14", min="2018-08-03", max="2018-08-14", format = "dd.mm.yyyy", weekstart = 1,  language = "ru"))
                    ,column(2,timeInput("repot_6_time2", label="Время", seconds=F))
                    ,column(2,"")
            )
            ,column(12, h2("  "))
            ,column(12,
                    column(3,actionButton(inputId="DisplayReport_6",label="Отобразить"))
            )
            ,column(12, h2("  "))
            ,column(12, uiOutput("Report_6_UI"))
          )
      )
    )
  })
  
  observeEvent(input$DisplayReport_6,{
    
    output$Report_6_UI=renderUI({
      data1<<-input$repot_6_day1
      time1<<-input$repot_6_time1
      data2<<-input$repot_6_day2
      time2<<-input$repot_6_time2
      date0=as.character(paste0(data1, " ", strftime(time1,"%T")))
      date1=as.character(paste0(data2, " ", strftime(time2,"%T")))
      if (as.POSIXct(date1)>as.POSIXct(date0))
      {
        n_pay_cart_table=n_pay_cart_func(date0,date1)
        filling_n_pay_cart_table=filling_n_pay_cart_func(n_pay_cart_table)
        
        if (dim(n_pay_cart_table)[1]>0)
        {
          fluidRow(
            box(status="primary",solidHeader=T,title=paste0("Отчет № 6 'Количество брошенных (не оплаченных) корзин отчетный период времени'"), width=12,
                renderText({ isolate({paste0("В период с ", format(as.POSIXct(paste0(data1, " ", strftime(time1,"%T"))), "%H:%M %d.%m.%Y"), 
                                             " по ", format(as.POSIXct(paste0(data2, " ", strftime(time2,"%T"))), "%H:%M %d.%m.%Y"), 
                                             " количество брошенных (не оплаченных) корзин составило ",dim(n_pay_cart_table)[1])}) }),
                renderDataTable({
                  names(filling_n_pay_cart_table)=c("Корзина", "Содержимое корзины")
                  datatable(filling_n_pay_cart_table,#selection = "single",
                            options=
                              list(pageLength=ifelse(dim(filling_n_pay_cart_table)[1]<10,dim(filling_n_pay_cart_table)[1],10),
                                   dom='lrtip',
                                   rownames=F,
                                   columnDefs=list(list(className='dt-center', targets=2)),
                                   language=list(
                                     search='Поиск:'
                                     ,lengthMenu="Отображать по  _MENU_  записей"
                                     ,paginate=list('next'= "Следующая",previous="Предыдущая")
                                     ,info="Отображено с _START_ по _END_ из _TOTAL_ записей"
                                   )
                                   )
                  )
                })
            ))
        } else
        {
          fluidRow(
            box(status="primary",solidHeader=T,title=paste0("Отчет № 6 'Количество брошенных (не оплаченных) корзин отчетный период времени'"), width=12,
                renderText({ isolate({paste0("В период с ", format(as.POSIXct(paste0(data1, " ", strftime(time1,"%T"))), "%H:%M %d.%m.%Y"), 
                                             " по ", format(as.POSIXct(paste0(data2, " ", strftime(time2,"%T"))), "%H:%M %d.%m.%Y"), 
                                             " брошенных (не оплаченных) корзин не было")}) })
            )
          )
        }
      } else
      {
        fluidRow(
          column(12,renderText({ paste0("Выберите корректный период времени")}) )
        )
      }
      
    })
  })
  
  #----Отчет №7----
  
  output$Report_7_UI_com=renderUI({ 
    fluidRow(
      box(status="primary",solidHeader=T,title="Составление отчета № 7 'Повторные покупки, совершенные пользователями за отчетный пероид времени'", width=12
          ,fluidPage(
            column(12,h2("  "))
            ,column(12
                    ,column(3,dateInput("repot_7_day1", label="Дата начала периода", value="2018-08-03", min="2018-08-03", max="2018-08-15", format = "dd.mm.yyyy", weekstart = 1,  language = "ru"))
                    ,column(2,timeInput("repot_7_time1", label="Время", seconds=F))
                    ,column(1,h2(" "))
                    ,column(3,dateInput("repot_7_day2", label="Дата окончания периода", value="2018-08-14", min="2018-08-03", max="2018-08-14", format = "dd.mm.yyyy", weekstart = 1,  language = "ru"))
                    ,column(2,timeInput("repot_7_time2", label="Время", seconds=F))
                    ,column(2,"")
            )
            ,column(12, h2("  "))
            ,column(12,
                    column(3,actionButton(inputId="DisplayReport_7",label="Отобразить"))
            )
            ,column(12, h2("  "))
            ,column(12, uiOutput("Report_7_UI"))
          )
      )
    )
  })
  
  observeEvent(input$DisplayReport_7,{
    
    output$Report_7_UI=renderUI({
      data1<<-input$repot_7_day1
      time1<<-input$repot_7_time1
      data2<<-input$repot_7_day2
      time2<<-input$repot_7_time2
      date0=as.character(paste0(data1, " ", strftime(time1,"%T")))
      date1=as.character(paste0(data2, " ", strftime(time2,"%T")))
      if (as.POSIXct(date1)>as.POSIXct(date0))
      {
        table_repurchase=repurchase_func(date0,date1)
        if (!is.null(table_repurchase)) filling_purchase_table=filling_purchase_func(table_repurchase) else filling_purchase_table=data.frame(c())
        
        if (dim(filling_purchase_table)[1]>0)
        {
          fluidRow(
            box(status="primary",solidHeader=T,title=paste0("Отчет № 7 'Повторные покупки, совершенные пользователями за отчетный пероид времени'"), width=12,
                renderText({ isolate({paste0("В период с ", format(as.POSIXct(paste0(data1, " ", strftime(time1,"%T"))), "%H:%M %d.%m.%Y"), 
                                             " по ", format(as.POSIXct(paste0(data2, " ", strftime(time2,"%T"))), "%H:%M %d.%m.%Y"), 
                                             " количество повторных покупок составило ",max(filling_purchase_table$No_device))}) }),
                renderDataTable({
                  filling_purchase_table=filling_purchase_table[c("No_device", "Date", "filling")]
                  names(filling_purchase_table)=c("Покупатель", "Дата совершения покупки", "Содержимое корзины покупателя")
                  datatable(filling_purchase_table,#selection = "single",
                            options=
                              list(pageLength=ifelse(dim(filling_purchase_table)[1]<10,dim(filling_purchase_table)[1],10),
                                   dom='lrtip',
                                   rownames=F,
                                   columnDefs=list(list(className='dt-center', targets=2:3)),
                                   language=list(
                                     search='Поиск:'
                                     ,lengthMenu="Отображать по  _MENU_  записей"
                                     ,paginate=list('next'= "Следующая",previous="Предыдущая")
                                     ,info="Отображено с _START_ по _END_ из _TOTAL_ записей"
                                   )
                              )
                  )
                })
            ))
        } else
        {
          fluidRow(
            box(status="primary",solidHeader=T,title=paste0("Отчет № 7 'Повторные покупки, совершенные пользователями за отчетный пероид времени'"), width=12,
                renderText({ isolate({paste0("В период с ", format(as.POSIXct(paste0(data1, " ", strftime(time1,"%T"))), "%H:%M %d.%m.%Y"), 
                                             " по ", format(as.POSIXct(paste0(data2, " ", strftime(time2,"%T"))), "%H:%M %d.%m.%Y"), 
                                             " повторных покупок не было")}) })
            )
          )
        }
      } else
      {
        fluidRow(
          column(12,renderText({ paste0("Выберите корректный период времени")}) )
        )
      }
      
    })
  })
  
}