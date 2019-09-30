#----Исходные таблицы----
table_start_page_func=function(){
  table_start_page=sqlQuery(ch1,"select * from table_start_page")
  table_start_page
}

table_goods_page_func=function(date0,date1){
  table_goods_page=sqlQuery(ch1,paste0("select * from table_goods_page where Date >= '",date0,"' and Date < '",date1,"'"))
  table_goods_page
}

table_cart_page_func=function(date0,date1){
  table_cart_page=sqlQuery(ch1,paste0("select * from table_cart_page where Date >= '",date0,"' and Date < '",date1,"'"))
  table_cart_page
}

table_pay_page_func=function(date0,date1){
  table_pay_page=sqlQuery(ch1,paste0("select * from table_pay_page where Date >= '",date0,"' and Date < '",date1,"'"))
  table_pay_page
}

table_success_pay_func=function(date0,date1){
  table_success_pay=sqlQuery(ch1,paste0("select * from table_success_pay where Date >= '",date0,"' and Date < '",date1,"'"))
  table_success_pay
}

table_translite_category=data.frame("en"=c("caviar", "canned_food", "fresh_fish", "frozen_fish", "semi_manufactures"),
                                    "ru"=c("Икра", "Консервированная продукция",  "Свежая рыба", "Замороженная рыба", "Полуфабрикаты"))

table_translite_goods=data.frame("en"=c("sprats", "pate_of_tuna", "black_caviar", "pike", "shark",
                                        "salmon", "midii", "crab_meat", "peljad", "codfish", 
                                        "crucian", "stuffing_eel", "soup_set", "tuna", "marlene",
                                        "herring", "squid_rings", "squash_caviar", "red_caviar", "smelt",
                                        "salmon_cutlet", "eel", "carp"),
                                 "ru"=c("Шпроты ", "Паштет из тунца",  "Черная икра", "Щука", "Акула",
                                        "Лосось", "Мидии", "Крабовое мясо", "Пелядь", "Треска", 
                                        "Карась", "Фаршированный угорь", "Суповый набор", "Тунец", "Марлин",
                                        "Сельдь", "Кольца кальмаров", "Кабачковая икра", "Красная икра", "Корюшка",
                                        "Котлеты из лосося", "Угорь", "Карп"))


#----Отчет №3----
table_category_time_func=function(table_goods_page, category){
  сategory_en=table_translite_category[which(table_translite_category$ru==category),"en"]
  table=table_goods_page[which(table_goods_page$сategory==сategory_en),]
  table$time=as.numeric(format(as.POSIXct(table$Date),"%H"))
  table[which(table$time>=04 & table$time<12),"flag"]=1 #Утро
  table[which(table$time>=12 & table$time<17),"flag"]=2 #День
  table[which(table$time>=17 & table$time<22),"flag"]=3 #Вечер
  table[which(is.na(table$flag)),"flag"]=4 #Ночь
  table1=data.frame("period"=c("Утро (04:00 - 13:00)","День (13:00 - 17:00)","Вечер (17:00 - 22:00)","Ночь (22:00 - 04:00)"),
                    "sum"=c(dim(table[which(table$flag==1),])[1], 
                            dim(table[which(table$flag==2),])[1],
                            dim(table[which(table$flag==3),])[1],
                            dim(table[which(table$flag==4),])[1]
                    ))
  table1
}


#----Отчет №6----
n_pay_cart_func=function(date0,date1){
  table_cart_page=table_cart_page_func(date0,date1)
  table_success_pay=table_success_pay_func(date0,date1)
  table_goods_page=table_goods_page_func(format(as.POSIXct(date0)-3600*24, "%Y-%m-%d %H:%M:%S"),date1)
  table=merge(table_cart_page, table_success_pay, by=c("ID_device", "cart_id"), all.x=T, all.y=T)
  n_pay_cart_table=table[which(is.na(table$Date.y)),]
  for (i in 1:dim(n_pay_cart_table)[1])
  {
    tab1=table_goods_page[which(as.character(table_goods_page$ID_device)==n_pay_cart_table[i,"ID_device"]),]
    tab1$Date=as.POSIXct(tab1$Date)
    max_time=max(tab1[which(tab1$Date<=as.POSIXct(n_pay_cart_table[i,3]) & !is.na(tab1$goods)),"Date"])
    n_pay_cart_table[i,"goods"]=tab1[which(tab1$Date==max_time),"goods"]
    n_pay_cart_table[i,"goods_ru"]=table_translite_goods[which(table_translite_goods$en==n_pay_cart_table[i,"goods"]),"ru"]
  }
  n_pay_cart_table
}

filling_n_pay_cart_func=function(n_pay_cart_table){
  unique_cart_id=unique(n_pay_cart_table$cart_id)
  filling_n_pay_cart_table=data.frame("cart"=c(),"filling"=c())
  for (i in 1:length(unique_cart_id)){
    filling_n_pay_cart_table[i,"cart"]=i
    for (j in 1:length(n_pay_cart_table[which(n_pay_cart_table$cart_id==unique_cart_id[i]),"goods_ru"])){ 
      table=n_pay_cart_table[which(n_pay_cart_table$cart_id==unique_cart_id[i]),]
      goods=n_pay_cart_table[which(n_pay_cart_table$cart_id==unique_cart_id[i]),"goods_ru"]
      filling_j=paste0(table[j,"goods_ru"], " - ", table[j,"amount"],  " шт.")
      filling_n_pay_cart_table[i,"filling"]=paste0(ifelse(is.na(filling_n_pay_cart_table[i,"filling"]),"",paste0(filling_n_pay_cart_table[i,"filling"], ", ")), filling_j)
    }
  }
  filling_n_pay_cart_table
}

#----Отчет №7----
repurchase_func=function(date0,date1){
  table_success_pay=table_success_pay_func(date0,date1)
  table_cart_page=sqlQuery(ch1,paste0("select * from table_cart_page"))
  table_goods_page=sqlQuery(ch1,paste0("select * from table_goods_page"))
  
  unique_ID_device=unique(table_success_pay$ID_device)
  table_repurchase=c()
  for (i in 1:length(unique_ID_device)){
    tab1=table_success_pay[which(table_success_pay$ID_device==unique_ID_device[i]),]
    if (dim(tab1)[1]>1)
    {
      tab1=merge(tab1, table_cart_page, by=c("cart_id","ID_device"))
      for (j in 1:dim(tab1)[1]){ 
        tab2=table_goods_page[which(as.character(table_goods_page$ID_device)==tab1[j,"ID_device"]),]
        tab2$Date=as.POSIXct(tab2$Date)
        max_time=max(tab2[which(tab2$Date<=as.POSIXct(tab1[j,4]) & !is.na(tab2$goods)),"Date"])
        tab1[j,"goods"]=tab2[which(tab2$Date==max_time),"goods"]
        tab1[j,"goods_ru"]=table_translite_goods[which(table_translite_goods$en==tab1[j,"goods"]),"ru"]
      }
      if (is.null(table_repurchase)) table_repurchase=tab1 else table_repurchase=rbind(table_repurchase,tab1)
    }
  }
  table_repurchase
}

filling_purchase_func=function(table_repurchase){
  unique_cart_id=unique(table_repurchase$cart_id)
  filling_purchase_table=data.frame("No_device"=c(), "ID_device"=c(), "cart_id"=c(),  "Date"=c(), "filling"=c())
  for (i in 1:length(unique_cart_id)){
    table=table_repurchase[which(table_repurchase$cart_id==unique_cart_id[i]),]
    filling_purchase_table[i,"ID_device"]=table[1,"ID_device"]
    filling_purchase_table[i,"cart_id"]=table[1,"cart_id"]
    filling_purchase_table[i,"Date"]=table[1,"Date.x"]
    for (j in 1:dim(table)[1]){ 
      filling_j=paste0(table[j,"goods_ru"], " - ", table[j,"amount"],  " шт.")
      filling_purchase_table[i,"filling"]=paste0(ifelse(is.na(filling_purchase_table[i,"filling"]),"",paste0(filling_purchase_table[i,"filling"], ", ")), filling_j)
    }
  }
  for (k in 1:length(unique(filling_purchase_table$ID_device))){filling_purchase_table[which(filling_purchase_table$ID_device==unique(filling_purchase_table$ID_device)[k]),"No_device"]=k}
  filling_purchase_table 
}


