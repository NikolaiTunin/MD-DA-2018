#По адресу https://raw.githubusercontent.com/SergeyMirvoda/da2016/master/data/calif_penn_2011.csv 
#можно получить набор данных, содержащий информацию о домовладениях в Калифорнии и Пенсильвании за 2011г. 
#Информация сгруппированна по зонам переписи (Census tracts).
#В построении диаграмм может помочь книга The R Cookbook. Рецепты 10.1 и 10.2.

#Загрузите данные в датафрейм, который назовите data.
data<-data.frame(read.csv("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2018/master/data/calif_penn_2011.csv"))

#Сколько строк и столбцов в data?
nrow(data)
ncol(data)

#Выполните следующую команду и объясните, что она делает. colSums(apply(data,c(1,2), is.na))
colSums(apply(data,c(1,2), is.na)) #считает, сколько потерянных значений в каждом столбце датафрейма

#Функция na.omit() принимает датафрейм и возвращает новый датафрейм, игнорируя строки, содержащие значение NA. 
#Используйте эту функцию для удаления строк с неполными данными.
data.fixed<-na.omit(data)

#Сколько строк было удалено?
nrow(data)-nrow(data.fixed)

#Соответствует ли результат выполнения, значениям из пункта 3?
colSums(apply(data.fixed,c(1,2), is.na)) #видимо, да



#Переменная(колонка) Built_2005_or_later содержит данные о проценте домов, построенных с 2005 года. Постройте диаграмму 
#рассеяния (scatterplot) медианы стоимости домов (переменная Median_house_value) относительно процента новых домов.
plot(data.fixed$Median_house_value, data.fixed$Built_2005_or_later, 
     main = "Диаграмма рассеяния медианы стоимости домов относительно процента домов, построенных с 2005 года",
     xlab = "Медианы стоимости домов", ylab = "Процента домов, построенных с 2005 года")

#Постройте ещё два графика для каждого из штатов отдельно. 
#Номер штата содержится в переменной (STATEFP), где Калифорния 6-й штат, а Пенсильвания 42.
pointer<-match(42, data.fixed$STATEFP)
plot(data.fixed$Median_house_value[1:pointer-1], data.fixed$Built_2005_or_later[1:pointer-1], 
     main = "Диаграмма рассеяния медианы стоимости домов относительно процента домов, построенных с 2005 года 
     в Калифорнии",xlab = "Медианы стоимости домов", ylab = "Процента домов, построенных с 2005 года")
plot(data.fixed$Median_house_value[pointer:10605], data.fixed$Built_2005_or_later[pointer:10605], 
     main = "Диаграмма рассеяния медианы стоимости домов относительно процента домов, построенных с 2005 года 
     в Пенсильвании",xlab = "Медианы стоимости домов", ylab = "Процента домов, построенных с 2005 года")



#Уровень найма (vacancy rate) — доля домов, которые не были заняты. 
#В данных содержатся колонки, содержащие общее количество домовладений и количество не занятых домовладений.
#В датафрейм data добавьте новую колонку vacancy_rate, которая должна содержать вышеописанный показатель.
data.fixed$vacancy.rate<-data.fixed$Vacant_units/data.fixed$Total_units

#Найдите минимум, максимум, среднее и медиану полученных значений показателя.
min(data.fixed$vacancy.rate)
max(data.fixed$vacancy.rate)
mean(data.fixed$vacancy.rate)
median(data.fixed$vacancy.rate)

#Постройте диаграмму уровня найма относительно медианы стоимости домов. Что можно заметить?
plot(data.fixed$vacancy.rate,data.fixed$Median_house_value, main = "Диаграмма уровня найма относительно 
     медианы стоимости домов",xlab = "Уровень найма", ylab = "Медиана стоимости домов")
#Чем выше медиана стоимости домов в зоне переписи, тем ниже уровень найма

#Колонка COUNTYFP содержит числовой код округа внутри штата. Нас интересуют Butte County (округ 7 в Калифорнии), 
#Santa Clara (округ 85 в Калифорнии) и York County (округ 133 в Пенсильвании).
#Объясните, что делает приведённый в конце задания код и как именно он это делает.
acc <- c()
for (tract in 1:nrow(ca_pa)) {
  if (ca_pa$STATEFP[tract] == 6) {
    if (ca_pa$COUNTYFP[tract] == 1) {
      acc <- c(acc, tract)
    }
  }
}
accmv <- c()
for (tract in acc) {
  accmv <- c(accmv, ca_pa[tract,10])
}
median(accmv)
#1.Cохраняет в вектор acc номера строк структуры данных ca_pa 1 округа Калифорнии.
#2.Сохраняет в вектор accmv медианы стоимости домов для всех зон переписи 1 округа Калифорнии.
#3.Считает медиану медиан стоимости домов для всех зон переписи 1 округа Калифорнии.

#Напишите другим способом в одну строку, то же самое, что делает нижеуказанный код. 
#Способов получить тот же ответ множество, достаточно одного.
median(data.fixed$Median_house_value[data.fixed$STATEFP == 6][data.fixed$COUNTYFP == 1], na.rm = TRUE)

#Найдите средний процент построенных домовладений в округах (Butte County, Santa Clara, York County)
california.indicies<- data.fixed$STATEFP == 6
pennsylvania.indicies<- data.fixed$STATEFP == 42

bc.indicies<-data.fixed$COUNTYFP == 7
sc.indicies<-data.fixed$COUNTYFP == 85
yc.indicies<-data.fixed$COUNTYFP == 133

california.total.units<-sum(data.fixed$Total_units[california.indicies])
pennsylvania.total.units<-sum(data.fixed$Total_units[pennsylvania.indicies])


mean(data.fixed$Total_units[california.indicies & bc.indicies]/california.total.units, na.rm = TRUE)
mean(data.fixed$Total_units[california.indicies & sc.indicies]/california.total.units, na.rm = TRUE)
mean(data$Total_units[pennsylvania.indicies & yc.indicies]/pennsylvania.total.units, na.rm = TRUE)


#Функция cor рассчитывает коэффициент корреляции между двумя переменными. Рассчитайте корреляцию между 
#медианы стоимости домовладений (Median_house_value) и процентом построенных домов (Built_2005_or_later):
#для всего набора данных
cor(data.fixed$Median_house_value,data.fixed$Built_2005_or_later)

#для Калифорнии
cor(data.fixed$Median_house_value[california.indicies],data.fixed$Built_2005_or_later[california.indicies])

#для Пенсильвании
cor(data.fixed$Median_house_value[pennsylvania.indicies],data.fixed$Built_2005_or_later[pennsylvania.indicies])

#для округа Butte County
cor(data.fixed$Median_house_value[california.indicies & bc.indicies],data.fixed$Built_2005_or_later[california.indicies & bc.indicies])

#для округа Santa Clara
cor(data.fixed$Median_house_value[california.indicies & sc.indicies],data.fixed$Built_2005_or_later[california.indicies & sc.indicies])

#для округа York County
cor(data.fixed$Median_house_value[pennsylvania.indicies & yc.indicies],data.fixed$Built_2005_or_later[pennsylvania.indicies & yc.indicies])

#Постройте три диаграммы медианы стоимости домовладений (Median_house_value) относительно медианы дохода 
#(Median_household_income) для трёх округов. Допустимо указать все три на одном графике.
plot(data.fixed$Median_house_value[california.indicies & bc.indicies],
     data.fixed$Median_household_income[california.indicies & bc.indicies], 
     main = "Диаграмма медианы стоимости домовладений относительно медианы дохода владельцев",
     xlab = "Медиана стоимости домовладений", 
     ylab = "Медиана дохода владельцев", col = "red")
points(data.fixed$Median_house_value[california.indicies & sc.indicies],
      data.fixed$Median_household_income[california.indicies & sc.indicies], col = "green")
points(data.fixed$Median_house_value[pennsylvania.indicies & yc.indicies],
       data.fixed$Median_household_income[pennsylvania.indicies & yc.indicies], col = "blue")