getwd()
#----- Загрузка пакетов------
if (require(RODBC)==F) install.packages('RODBC', dependencies=T)

#-----Запись данных в БД------
ch1=odbcConnect("BD")
sqlQuery(ch1,"SET DATEFORMAT ymd")
# Формирование массива с новыми данными
new_mass=read.table("logs.txt", header = FALSE, sep = "", dec = ".")
# Обработка новых данных и запись в БД
table_start_page=data.frame("Date"=c(),"ID_device"=c())
table_goods_page=data.frame("Date"=c(),"ID_device"=c(),"сategory"=c(),"goods"=c())
table_cart_page=data.frame("Date"=c(),"ID_device"=c(),"goods_id"=c(),"amount"=c(),"cart_id"=c())
table_pay_page=data.frame("Date"=c(),"ID_device"=c(),"user_id"=c(),"cart_id"=c())
table_success_pay=data.frame("Date"=c(),"ID_device"=c(),"cart_id"=c())
for (i in 1:dim(new_mass)[1])
{
  val=strsplit(as.character(new_mass[i,8]),"/")[[1]][4]
  if (is.na(val)) table_start_page=rbind(table_start_page, 
                                         data.frame("Date"=paste(new_mass[i,3], new_mass[i,4]),
                                                    "ID_device"=new_mass[i,7])) else
                                                    {
                                                      if (substr(val, 1, 5)!="cart?" & substr(val, 1, 4)!="pay?" & substr(val, 1, 11)!="success_pay") 
                                                        table_goods_page=rbind(table_goods_page,
                                                                               data.frame("Date"=paste(new_mass[i,3], new_mass[i,4]),
                                                                                          "ID_device"=new_mass[i,7],
                                                                                          "сategory"=strsplit(val,"/")[[1]][1],
                                                                                          "goods"=strsplit(as.character(new_mass[i,8]),"/")[[1]][5]))
                                                      
                                                      if (substr(val, 1, 5)=="cart?") table_cart_page=rbind(table_cart_page,
                                                                                                            data.frame("Date"=paste(new_mass[i,3], new_mass[i,4]),
                                                                                                                       "ID_device"=new_mass[i,7],
                                                                                                                       "goods_id"=strsplit(strsplit(val,"&")[[1]][1],"=")[[1]][2],
                                                                                                                       "amount"=strsplit(strsplit(val,"&")[[1]][2],"=")[[1]][2],
                                                                                                                       "cart_id"=strsplit(strsplit(val,"&")[[1]][3],"=")[[1]][2]
                                                                                                            ))
                                                      
                                                      if (substr(val, 1, 4)=="pay?") table_pay_page=rbind(table_pay_page,
                                                                                                          data.frame("Date"=paste(new_mass[i,3], new_mass[i,4]),
                                                                                                                     "ID_device"=new_mass[i,7],
                                                                                                                     "user_id"=strsplit(strsplit(val,"&")[[1]][1],"=")[[1]][2],
                                                                                                                     "cart_id"=strsplit(strsplit(val,"&")[[1]][2],"=")[[1]][2]
                                                                                                          ))
                                                      
                                                      if (substr(val, 1, 11)=="success_pay") table_success_pay=rbind(table_success_pay,
                                                                                                                     data.frame("Date"=paste(new_mass[i,3], new_mass[i,4]),
                                                                                                                                "ID_device"=new_mass[i,7],         
                                                                                                                                "cart_id"=substr(val, 13, nchar(val))
                                                                                                                     )) 
                                                    }
  
}
table_start_page$Date=as.character(table_start_page$Date)
table_goods_page$Date=as.character(table_goods_page$Date)
table_cart_page$Date=as.character(table_cart_page$Date)
table_pay_page$Date=as.character(table_pay_page$Date)
table_success_pay$Date=as.character(table_success_pay$Date)

# Запись новых данных в БД
sqlSave(ch1, dat=table_start_page,  append=T, rownames=F)
sqlSave(ch1, dat=table_goods_page,  append=T, rownames=F)
sqlSave(ch1, dat=table_cart_page,   append=T, rownames=F)
sqlSave(ch1, dat=table_pay_page,    append=T, rownames=F)
sqlSave(ch1, dat=table_success_pay, append=T, rownames=F)
# sqlQuery(ch1,"drop table table_start_page")
# sqlQuery(ch1,"drop table table_goods_page")
# sqlQuery(ch1,"drop table table_cart_page")
# sqlQuery(ch1,"drop table table_pay_page")
# sqlQuery(ch1,"drop table table_success_pay")


