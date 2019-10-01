library(shiny)
server <- function(session,input, output) {
  
  #----Отчет №3----
  
  output$Report_3_UI_com=renderUI({ 
    fluidRow(
      box(status="primary",solidHeader=T,title="Составление отчета № 3 'Анализ количества просмотров категорий товара за отчетный период времени'", width=12,
          fluidPage(
            column(12, h2("  ")),
            column(12, column(6,selectInput("category_selectInput", "Категория товара", table_translite_category$ru))),
            column(12, column(3,dateInput("repot_3_day1", label="Дата начала периода", value="2018-08-03", min="2018-08-03", max="2018-08-15", format = "dd.mm.yyyy", weekstart = 1,  language = "ru")),
                   column(2, timeInput("repot_3_time1", label="Время", seconds=F)),
                   column(1, h2(" ")),
                   column(3, dateInput("repot_3_day2", label="Дата окончания периода", value="2018-08-14", min="2018-08-03", max="2018-08-14", format = "dd.mm.yyyy", weekstart = 1,  language = "ru")),
                   column(2, timeInput("repot_3_time2", label="Время", seconds=F)),
                   column(2, "")),
            column(12, h2("")),
            column(12, column(3,actionButton(inputId="DisplayReport_3",label="Отобразить"))),
            column(12, h2("")),
            column(12, uiOutput("Report_3_UI"))
          )
      )
    )
  })
  
  observeEvent(input$DisplayReport_3,{
    
    output$Report_3_UI=renderUI({
      isolate({
      data1=input$repot_3_day1
      time1=input$repot_3_time1
      data2=input$repot_3_day2
      time2=input$repot_3_time2
      date0=as.character(paste0(data1, " ", strftime(time1,"%T")))
      date1=as.character(paste0(data2, " ", strftime(time2,"%T")))
      if (as.POSIXct(date1)>as.POSIXct(date0))
      {
        table_goods_page=table_goods_page_func(date0,date1)
        if (dim(table_goods_page)[1]>0)
        {
          table0=table_category_time_func(table_goods_page, category=input$category_selectInput)
          table1=table0[[1]]
          table_max_sum=max(table1$sum)
          period=table1[which(table1$sum==table_max_sum),"period"]
          period_name=ifelse(period=="Утро (04:00 - 13:00)", "утром",
                             ifelse(period=="День (13:00 - 17:00)", "днем",
                                    ifelse(period=="Вечер (17:00 - 22:00)", "вечером",
                                           ifelse(period=="Ночь (22:00 - 04:00)", "ночью"))))
          fluidRow(
            box(status="primary", solidHeader=T, title=paste0("Отчет по категории товара '", isolate({input$category_selectInput}), "'"), width=12,
                column(12, style="font-weight: bold; font-size: 15px",
                       renderText({
                         isolate({paste0("В период с ", format(as.POSIXct(paste0(data1, " ", strftime(time1,"%T"))), "%H:%M %d.%m.%Y"), 
                                         " по ", format(as.POSIXct(paste0(data2, " ", strftime(time2,"%T"))), "%H:%M %d.%m.%Y"), " категорию '", input$category_selectInput,
                                         "' чаще всего просматривали ",period_name, " (", table_max_sum, " раз).")}) }) ),
                column(12, h2("")),
                column(12, style="font-weight: bold; font-size: 17px", renderText({isolate({paste0("Количество посещений категории товара '", input$category_selectInput, "' ")})})),
                column(12, h2("")),
                column(12, renderDataTable({
                  names(table1)=c("Период времени суток", "Количество посещений")
                  datatable(table1, selection = "single", options=list(pageLength=dim(table)[1], dom='', rownames=F, columnDefs=list(list(className='dt-center', targets=1:2))))})),
                column(12, h2("")),
                column(12, renderPlot({
                  table2=table0[[2]]
                  isolate({
                    plot(x=table2$period, y=table2$sum,
                     type="o", pch=16, col=c("#001559"),
                     xlab="Время", ylab="Количество просмотров",
                     main=paste0("Статистика просмотров категории товара '", input$category_selectInput, "' "),
                     axes=F,cex.main=2, lwd=3,
                     col.lab="#1A1A1A", col.main="#004F9F",
                )
                grid()
                axis(side=2, col.axis="#1A1A1A")
                axis(side=1, 0:23, table2$h, col.axis="#1A1A1A")
                u=par("usr")
                arrows(u[1], u[3], u[2], u[3], code=2, angle=10, xpd=TRUE, lwd=2, col="#969696")
                arrows(u[1], u[3], u[1], u[4], code=2, angle=10, xpd=TRUE, lwd=2, col="#969696")
                  })
                }))
            ))
        } else
        {
          fluidRow(
            box(status="primary", solidHeader=T, title=paste0("Отчет по категории товара '", isolate({input$category_selectInput}), "'"), width=12,
                renderText({ isolate({paste0("В период с ", format(as.POSIXct(paste0(data1, " ", strftime(time1,"%T"))), "%H:%M %d.%m.%Y"), 
                                             " по ", format(as.POSIXct(paste0(data2, " ", strftime(time2,"%T"))), "%H:%M %d.%m.%Y"), 
                                             " категорию товара '", input$category_selectInput, "' не просматривали.")}) })  ))
        }
      } else
      {
        fluidRow(column(12, style="font-weight: bold; font-size: 15px", renderText({ paste0("Выберите корректный период времени")})))
      }
    })
    })
  })
  
  
  #----Отчет №6----
  
  output$Report_6_UI_com=renderUI({ 
    fluidRow(
      box(status="primary", solidHeader=T, title="Составление отчета № 6 'Количество брошенных (не оплаченных) корзин отчетный период времени'", width=12,
          fluidPage(
            column(12, h2("")),
            column(12, column(3, dateInput("repot_6_day1", label="Дата начала периода", value="2018-08-03", min="2018-08-03", max="2018-08-15", format = "dd.mm.yyyy", weekstart = 1,  language = "ru")),
                   column(2, timeInput("repot_6_time1", label="Время", seconds=F)),
                   column(1, h2(" ")),
                   column(3, dateInput("repot_6_day2", label="Дата окончания периода", value="2018-08-14", min="2018-08-03", max="2018-08-14", format = "dd.mm.yyyy", weekstart = 1,  language = "ru")),
                   column(2, timeInput("repot_6_time2", label="Время", seconds=F)),
                   column(2, "")),
            column(12, h2("")),
            column(12, column(3, actionButton(inputId="DisplayReport_6", label="Отобразить"))),
            column(12, h2("")),
            column(12, uiOutput("Report_6_UI"))
          )
      )
    )
  })
  
  observeEvent(input$DisplayReport_6,{
    
    output$Report_6_UI=renderUI({
      isolate({
      data1=input$repot_6_day1
      time1=input$repot_6_time1
      data2=input$repot_6_day2
      time2=input$repot_6_time2
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
                column(12, style="font-weight: bold; font-size: 15px",
                       renderText({ isolate({paste0("В период с ", format(as.POSIXct(paste0(data1, " ", strftime(time1,"%T"))), "%H:%M %d.%m.%Y"), 
                                                    " по ", format(as.POSIXct(paste0(data2, " ", strftime(time2,"%T"))), "%H:%M %d.%m.%Y"), 
                                                    " количество брошенных (не оплаченных) корзин составило ",dim(n_pay_cart_table)[1], ".")}) })),
                column(12, h2("")),
                column(12, style="font-weight: bold; font-size: 17px", renderText({paste0("Содержимое брошенных корзин")})),
                column(12, h2("")),
                renderDataTable({
                  names(filling_n_pay_cart_table)=c("Корзина", "Содержимое корзины")
                  datatable(filling_n_pay_cart_table,
                            options=
                              list(pageLength=ifelse(dim(filling_n_pay_cart_table)[1]<10,dim(filling_n_pay_cart_table)[1],10),
                                   dom='lrtip', rownames=F,
                                   columnDefs=list(list(className='dt-center', targets=2)),
                                   language=list(
                                     search='Поиск:'
                                     ,lengthMenu="Отображать по  _MENU_  записей"
                                     ,paginate=list('next'= "Следующая",previous="Предыдущая")
                                     ,info="Отображено с _START_ по _END_ из _TOTAL_ записей")))
                })
            ))
        } else
        {
          fluidRow(
            box(status="primary", solidHeader=T, title=paste0("Отчет № 6 'Количество брошенных (не оплаченных) корзин отчетный период времени'"), width=12,
                column(12, style="font-weight: bold; font-size: 15px", renderText({ isolate({paste0("В период с ", format(as.POSIXct(paste0(data1, " ", strftime(time1,"%T"))), "%H:%M %d.%m.%Y"), 
                                                                                                    " по ", format(as.POSIXct(paste0(data2, " ", strftime(time2,"%T"))), "%H:%M %d.%m.%Y"), 
                                                                                                    " брошенных (не оплаченных) корзин не было.")}) }))
            )
          )
        }
      } else
      {
        fluidRow(column(12, style="font-weight: bold; font-size: 15px", renderText({ paste0("Выберите корректный период времени")})))
      }
      })
    })
  })
  
  
  #----Отчет №7----
  
  output$Report_7_UI_com=renderUI({ 
    fluidRow(
      box(status="primary", solidHeader=T, title="Составление отчета № 7 'Повторные покупки, совершенные пользователями за отчетный пероид времени'", width=12,
          fluidPage(column(12, h2("")),
                    column(12, column(3, dateInput("repot_7_day1", label="Дата начала периода", value="2018-08-03", min="2018-08-03", max="2018-08-15", format = "dd.mm.yyyy", weekstart = 1,  language = "ru")),
                           column(2, timeInput("repot_7_time1", label="Время", seconds=F)),
                           column(1, h2("")),
                           column(3, dateInput("repot_7_day2", label="Дата окончания периода", value="2018-08-14", min="2018-08-03", max="2018-08-14", format = "dd.mm.yyyy", weekstart = 1,  language = "ru")),
                           column(2, timeInput("repot_7_time2", label="Время", seconds=F)))
                    ,column(12, h2("  "))
                    ,column(12, column(3, actionButton(inputId="DisplayReport_7",label="Отобразить")))
                    ,column(12, h2("  "))
                    ,column(12, uiOutput("Report_7_UI")))
      )
    )
  })
  
  observeEvent(input$DisplayReport_7,{
    
    output$Report_7_UI=renderUI({
      isolate({
      data1=input$repot_7_day1
      time1=input$repot_7_time1
      data2=input$repot_7_day2
      time2=input$repot_7_time2
      date0=as.character(paste0(data1, " ", strftime(time1,"%T")))
      date1=as.character(paste0(data2, " ", strftime(time2,"%T")))
      if (as.POSIXct(date1)>as.POSIXct(date0))
      {
        table_repurchase=repurchase_func(date0,date1)
        if (!is.null(table_repurchase)) filling_purchase_table<<-filling_purchase_func(table_repurchase) else filling_purchase_table<<-data.frame(c())
        
        if (dim(filling_purchase_table)[1]>0)
        {
          fluidRow(
            box(status="primary", solidHeader=T, title=paste0("Отчет № 7 'Повторные покупки, совершенные пользователями за отчетный пероид времени'"), width=12,
                column(12, style="font-weight: bold; font-size: 15px",renderText({ isolate({paste0("В период с ", format(as.POSIXct(paste0(data1, " ", strftime(time1,"%T"))), "%H:%M %d.%m.%Y"), 
                                             " по ", format(as.POSIXct(paste0(data2, " ", strftime(time2,"%T"))), "%H:%M %d.%m.%Y"), 
                                             " количество повторных покупок составило ", max(filling_purchase_table$No_device), ".")}) })),
                column(12, h2("")),
                column(12, style="font-weight: bold; font-size: 17px", renderText({paste0("Повторные покупки, совершенные пользователями")})),
                column(12, h2("")),
                renderDataTable({
                  filling_purchase_table=filling_purchase_table[c("No_device", "Date", "filling")]
                  names(filling_purchase_table)=c("Покупатель", "Дата совершения покупки", "Содержимое корзины покупателя")
                  datatable(filling_purchase_table,
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
                }),
                column(12, downloadButton("excel7","Сохранить отчет"))
            ))
        } else
        {
          fluidRow(
            box(status="primary",solidHeader=T,title=paste0("Отчет № 7 'Повторные покупки, совершенные пользователями за отчетный период времени'"), width=12,
                column(12, style="font-weight: bold; font-size: 15px", renderText({ isolate({paste0("В период с ", format(as.POSIXct(paste0(data1, " ", strftime(time1,"%T"))), "%H:%M %d.%m.%Y"), 
                                                                                                    " по ", format(as.POSIXct(paste0(data2, " ", strftime(time2,"%T"))), "%H:%M %d.%m.%Y"), 
                                                                                                    " повторных покупок не было.")}) }))
            )
          )
        }
      } else
      {
        fluidRow(column(12, style="font-weight: bold; font-size: 15px", renderText({ paste0("Выберите корректный период времени")})))
      }
      })
    })
  })
  
  output$excel7 <- downloadHandler(
    
    filename = function() { "Отчет 7.xlsx" },
    content = function(file){
      filling_purchase_table=filling_purchase_table[c("No_device", "Date", "filling")]
      names(filling_purchase_table)=c("Покупатель", "Дата совершения покупки", "Содержимое корзины покупателя")
      filling_purchase_table$`Дата совершения покупки`=format(as.POSIXct(filling_purchase_table$`Дата совершения покупки`),"%d.%m.%Y %H:%M:%S")
      data1=input$repot_7_day1
      data2=input$repot_7_day2
      time1=strftime(input$repot_7_time1, "%T")
      time2=strftime(input$repot_7_time2, "%T")
      Name<-paste0("Отчет № 7 'Повторные покупки, совершенные пользователями за отчетный период времени'")
      Period<-paste0("за период"," ",as.character(format(as.POSIXct(data1),"%d.%m.%Y %H:%M:%S"))," ",as.character(time1)," - ",as.character(format(as.POSIXct(data2),"%d.%m.%Y %H:%M:%S"))," ",as.character(time2))
      empty_Table_otchet=data.frame(matrix(vector(), 3000, 5),stringsAsFactors=F)
      names(empty_Table_otchet)=names(filling_purchase_table)
      
      InformationRegion1<-paste0("В отчетный период времени количество повторных покупок составило ", max(filling_purchase_table$Покупатель), ".")
      TableRegion<-filling_purchase_table
      
      time1<<-time1
      time2<<-time2
      data1<<-data1
      data2<<-data2
      Name<<-Name
      Period<<-Period
      InformationRegion1<<-InformationRegion1
      TableRegion<<-TableRegion
      empty_Table_otchet<<-empty_Table_otchet
      wbFilename <- paste(getwd(), "WWW", "Report 7.xlsx",sep="/")
      wb <- loadWorkbook(wbFilename, create = TRUE)
      setStyleAction(wb, XLC$"STYLE_ACTION.DATA_FORMAT_ONLY") 
      sheet <- "Отчет № 7"
      createSheet(wb, name = sheet)
      writeWorksheet(wb, data=Name, sheet=sheet, startRow=1, startCol=2, header=FALSE)
      writeWorksheet(wb, data=Period, sheet=sheet, startRow=2, startCol=3, header=FALSE)
      writeWorksheet(wb, data=InformationRegion1, sheet=sheet, startRow=4, startCol=1, header=FALSE)
      writeWorksheet(wb, data=empty_Table_otchet, sheet=sheet, startRow=6, startCol=1, header=TRUE)
      writeWorksheet(wb, data=TableRegion, sheet=sheet, startRow=6, startCol=1, header=TRUE)
      saveWorkbook(wb)
      file.copy(paste(getwd(), "WWW", "Report 7.xlsx",sep="/"), file)
    })
  
  
  
}