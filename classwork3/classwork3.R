#Загрузите данные о землятресениях
anss<-readLines("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2017/master/data/earthquakes_2011.html", warn=FALSE)

#Выберите строки, которые содержат данные с помощью регулярных выражений и функции grep
pattern<-"[0-9]{5}|AK"
earthquakes_data<-grep(pattern, anss, value = TRUE)

#Проверьте что все строки (all.equal) в результирующем векторе подходят под шаблон.
all.equal(earthquakes_data,pattern)
