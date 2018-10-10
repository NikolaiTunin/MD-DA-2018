library(MASS)
data(Cars93)
View(Cars93)

#Выполните команду summary() на полном наборе данных. Можно ли по результату выполнения сказать 
#сколько строк в датафрейме? Если да, напишите сколько. Если нет, то приведите другой способ.
summary(Cars93)
nrow(Cars93) #93 строки

#Найдите среднюю цену машин с задним приводом.
sum<-0
count<-0
for (i in 1:nrow(Cars93))
{
  if (Cars93[i, 10] == "Rear")
  {
    sum<-sum+Cars93[i, 5]
    count<-count+1
  }
}
sum/count

#Найдите минимальное число лошадиных сил автомобиля для 7 пассажиров. Для 6 пассажиров.
flag<-0
for (i in 1:nrow(Cars93)){
  if (Cars93[i, 18] == 7)
  {
    if (flag == 0)
    {
      min<-Cars93[i,13]
      flag<-1
    }
    if (Cars93[i, 13] < min)
    {
      min<-Cars93[i, 13]
    }
  }
}
min

flag<-0
for (i in 1:nrow(Cars93)){
  if (Cars93[i, 18] == 6)
  {
    if (flag == 0)
    {
      min<-Cars93[i,13]
      flag<-1
    }
    if (Cars93[i, 13] < min)
    {
      min<-Cars93[i, 13]
    }
  }
}
min

#Найдите машины с максимальным, минимальным и средним(медианой) расстоянием, 
#которая машина может проехать по трассе. Вам понадобятся 2 колонки, чтобы рассчитать расстояние. Какие?
highway.max.distance<-Cars93[,8]*Cars93[,17] #используются колонки "MPG.highway" и "Fuel.tank.capacity"
min(highway.max.distance)
max(highway.max.distance)
median(highway.max.distance)


#Выполните код и запустите эту функцию factory.run().
factory.run <- function (o.cars=1, o.trucks=1) {
  factory <- matrix(c(40,1,60,3),nrow=2, dimnames=list(c("трудодни","сталь"),c("автомобили","грузовики")))
  warehouse <- c(1600,70) #Доступно материалов на складе
  names(warehouse) <- rownames(factory)
  reserve <- c(8,1)
  names(reserve) <- rownames(factory)
  output <- c(o.cars, o.trucks)
  names(output) <- colnames(factory)
  
  steps <- 0 # Счётчик числа шагов цикла
  repeat 
  {
    steps <- steps + 1
    needed <- factory %*% output # Подсчитаем ресурсы, которые нам нужны для производства требуемого кол-ва машин
    message(steps)
    print(needed)
    # Если ресурсов достаточно и остаток меньше или равен резерву, то мы произвели максимум возможного.
    # Нужно прекращать
    if (all(needed <= warehouse) && all((warehouse - needed) <= reserve)) {
      break()
    }
    # Если заявка слишком большая и ресурсов недостаточно, уменьшим её на 10%
    if (all(needed > warehouse)) {
      output <- output * 0.9
      next()
    }
    # Если всё наоброт, то увеличим на 10%
    if (all(needed < warehouse)) {
      output <- output * 1.1
      next()
    }
    # Если мы потребили одного ресурса слишком много, а другого недостаточно,
    # то увеличим план на случайную величину
    output <- output * (1+runif(length(output),min=-0.1,max=0.1))
  }
  return(output)
}

factory.run()
#С каким входными значениями функция вызвана? Какой получился результат?
#Функция вызвана со значениями по умолчанию (1,1). 
#Результат: за доступные на складе материалы можно собрать 10 автомобилей и 19 грузовиков

#Повторите вызов 4 раза. Полученные ответы отличаются от полученных ранее? Если да, почему? Если нет, почему?
factory.run() # 10,19
factory.run() # 10,19
factory.run() # 9,20
factory.run() # 9,19
#Ответы отличаются из-за увеличения плана на случайную величину в теле функции

#В приведённом коде, переменные steps и output находятся внутри алгоритма. 
#Измените функцию так, чтобы она возвращала число шагов и произведённое количество машин.
factory.run <- function (o.cars=1, o.trucks=1) {
  factory <- matrix(c(40,1,60,3),nrow=2, dimnames=list(c("трудодни","сталь"),c("автомобили","грузовики")))
  warehouse <- c(1600,70) #Доступно материалов на складе
  names(warehouse) <- rownames(factory)
  reserve <- c(8,1)
  names(reserve) <- rownames(factory)
  output <- c(o.cars, o.trucks)
  names(output) <- colnames(factory)
  
  steps <- 0 # Счётчик числа шагов цикла
  repeat 
  {
    steps <- steps + 1
    needed <- factory %*% output # Подсчитаем ресурсы, которые нам нужны для производства требуемого кол-ва машин
    message(steps)
    print(needed)
    # Если ресурсов достаточно и остаток меньше или равен резерву, то мы произвели максимум возможного.
    # Нужно прекращать
    if (all(needed <= warehouse) && all((warehouse - needed) <= reserve)) {
      break()
    }
    # Если заявка слишком большая и ресурсов недостаточно, уменьшим её на 10%
    if (all(needed > warehouse)) {
      output <- output * 0.9
      next()
    }
    # Если всё наоброт, то увеличим на 10%
    if (all(needed < warehouse)) {
      output <- output * 1.1
      next()
    }
    # Если мы потребили одного ресурса слишком много, а другого недостаточно,
    # то увеличим план на случайную величину
    output <- output * (1+runif(length(output),min=-0.1,max=0.1))
  }
  names(steps)<- "шаги"
  ret<-c(steps,output)
  return(ret)
}
factory.run()

#Установите план равный тридцати автомобилям и 20 грузовикам и выполните функцию.
#Какой получили результат?
factory.run(30,20) #не получилось сделать указанное количество техники

#Каким получился итоговый запрос ресурсов (переменная needed)
#трудодни 1597.41484
#сталь      69.64656

#Как много итераций пришлось пройти, чтобы получить ответ (переменная steps)?
#999