#Используйте команду readLines для загрузки файла в текстовый вектор html.
#Сколько строк в файле?
#Сколько символов в файле?
html<-readLines("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2018/master/data/forbes.htm")
NROW(html)
sum(nchar(html))

#Откройте файл в текстовом редакторе. Найдите строки, 
#содержащие данные о Билле Гейтсе и Ларри Эллисоне и запомните размер их дохода.
#Билл Гейтс - $72 B
#Билл Гейтс - $41 B

#Напишите шаблон регулярного выражения и используйте функцию grep, чтобы извлечь размер дохода из данных в векторе html. 
#Удостоверьтесь, что полученный вектор номеров строк содержит содержит ровно 100 записей 
#и ссылается на номера строк в котрых действительно есть информация о доходе, а не просто первый попавшийся текст.
pattern<-"[$].*\\sB"
income<-grep(pattern, html)
length(income)
income[1:100]

#Напишите код, использовав регулярное выражение из п. 3, и функции regexp и regmatches, чтобы извлечь все данные о доходе. Проверьте следующее:
#Должно быть ровно сто значений.
#Самый большой доход должен быть доход Билла Гейтса
#Такой доход должен быть в списке один раз.
#В всписке должна быть цифра, которую мы запомнили для Ларри Эллисона.
#Должно быть как минимум два значения, встречающихся несколько раз.
income<-regexpr(pattern, html)
income<-regmatches(html, income)
income

#В данных доход представлен в формате "$42 B", что означает 42 * 10^9. 
#Преобразуйте этот формат в числовой и сохраните в вектор worths. Удостоверьтесь в следующем:
pattern2<-"\\d+,*\\d*"
income2<-regexpr(pattern2, income)
income2<-regmatches(income, income2)
income2<-sub(",",".", income2)
options(digits = 3)
worths<-as.double(income2)
worths<-worths*10^9
worths

#worths является вектором и в нём сто занчений типа double.
is.vector(worths)
length(worths)
is.double(worths)

#Все элементы вектора worths больше 1 миллиарда.
count<-0
for (i in 1:length(worths)){
  ifelse(worths[i]>10^9,count<-count+1,break())
}
ifelse (count == length(worths), "TRUE", "FALSE")

#Самое большое число это доход Билла Гейтса.
max(worths)

#Используйте вектор worths , чтобы выяснить следующее:
#Какова медиана ста этих записей?
median(worths)

#Средний доход?
mean(worths)

#Как много людей из этого списка имеют доход больше 5млрд., 10, 25?
worths
how_many<-function(income.border, worth=worths){
  count<-0
  for (i in 1:length(worth)){
    if (worth[i]>income.border){
      count<- count+1
    }
  }
  return(count)
}
how_many(5e+09)
how_many(10e+09)
how_many(25e+09)

#Какой их общий доход?
sum(worths)

#Какую долю от общего дохода, составляет пятёрка самых богатых.
mean(worths>=3.60e+10)

#Какую долю от общего дохода, составляют 20 самых богатых.
sum(worths[1:20])/sum(worths)

#В данных федерального резерва США найдите показатель дохода всех домохозяйств (Household net worth) 
#в соответвующем году, какую долю общего дохода составляют 100 богатейших людей?
net.worth<-76824.5e+09
sum(worths)/net.worth