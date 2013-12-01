### Чтение базы данных

data <- read.csv('11_2013_zakharov.csv',sep='\t',header=T,colClasses=c(rep('character',3),rep('factor',3),'character',rep('factor',37)))

### Подключение необходимых библиотек

library(gmodels)
library(ggplot2)

### Подготовка базы данных

# Удаление из базы методических пособий (ввиду малого количества и неполноты перечня)

data = subset(data,data$type!='Методичний посібник')
data = subset(data,data$type!='Підручник')
data[,5]<- factor(data[,5])

# Одни из тезисов расценены как статья
data[data$type=='Тези',]$type = 'Стаття'
data[,6]<- factor(data[,6])

# Удалена одна статья из Беларуси

data = subset(data,data$country!='Білорусь')

# Объединение дискурс-анализа и контент-анализа
data[data$discourse==1,]$content.analysis = 1

# Объединение анкетирования, опросов и интервью

data[data$form==1,]$interview = 1
data[data$questionnaire==1,]$interview = 1

# Объединение проективных методик

data[data$projpics==1,]$projmeth = 1
data[data$associations==1,]$projmeth = 1

# Объединение тренингов и экспериментов

data[data$training==1,]$experiment = 1

# Удаление лишних столбцов данных

names(data)
names(data[,c(1:8,11,13,16:19,21:ncol(data))])

data = data[,c(1:8,11,13,16:19,21:ncol(data))]

# Выделение теоретических публикаций и эмпирических исследований

theory = subset(data,data$theory==1)
empirical = subset(data,data$theory!=1)

# Обзор общего количества

nrow(data)
nrow(theory)
nrow(empirical)

# Обзор количества публикаций по типам

CrossTable(data[,5])

# Обзор количества публикаций разного типа по годам

CrossTable(data[,4])
CrossTable(theory[,4])
CrossTable(empirical[,4])

# Обзор применяемых методов

result = sapply(empirical[9:ncol(empirical)],table)

# Применение эмпирических методов

t(result[2,1:10])

# Применение статистических методов

t(result[2,11:ncol(result)])

# Распределение эмпирических методов по годам

CrossTable(empirical[,4],empirical[,9])[[1]][,2]    # Проективні методи
CrossTable(empirical[,4],empirical[,10])[[1]][,2]   # Тестування
CrossTable(empirical[,4],empirical[,11])[[1]][,2]   # Інтервью
CrossTable(empirical[,4],empirical[,12])[[1]][,2]   # Фокус-групи
CrossTable(empirical[,4],empirical[,13])[[1]][,2]   # Написання творів
CrossTable(empirical[,4],empirical[,14])[[1]][,2]   # Контент-аналіз
CrossTable(empirical[,4],empirical[,15])[[1]][,2]   # Психосемантика
CrossTable(empirical[,4],empirical[,16])[[1]][,2]   # Спостереження
CrossTable(empirical[,4],empirical[,17])[[1]][,2]   # Оцінки експертів
CrossTable(empirical[,4],empirical[,18])[[1]][,2]   # Експеримент

# Распределение статистических методов по годам

CrossTable(empirical[,4],empirical[,19])[[1]][,2]   # Описова статистика
CrossTable(empirical[,4],empirical[,20])[[1]][,2]   # Кореляційний аналіз
CrossTable(empirical[,4],empirical[,21])[[1]][,2]   # Регресійний аналіз
CrossTable(empirical[,4],empirical[,22])[[1]][,2]   # Факторний аналіз
CrossTable(empirical[,4],empirical[,23])[[1]][,2]   # Тест Стьюдента
CrossTable(empirical[,4],empirical[,24])[[1]][,2]   # Біноміальний критерій
CrossTable(empirical[,4],empirical[,25])[[1]][,2]   # Кластерний аналіз
CrossTable(empirical[,4],empirical[,26])[[1]][,2]   # H-Крускала-Уоліса
CrossTable(empirical[,4],empirical[,27])[[1]][,2]   # Критерій кутового перетворення Фішера
CrossTable(empirical[,4],empirical[,28])[[1]][,2]   # Дисперсійний аналіз
CrossTable(empirical[,4],empirical[,29])[[1]][,2]   # Хі-квадрат Пірсона
CrossTable(empirical[,4],empirical[,30])[[1]][,2]   # Коефіцієнт Чупрова
CrossTable(empirical[,4],empirical[,31])[[1]][,2]   # Коефіцієнт Крамера
CrossTable(empirical[,4],empirical[,32])[[1]][,2]   # Коефіцієнт Сомерса
CrossTable(empirical[,4],empirical[,33])[[1]][,2]   # U-Манна-Уітні
CrossTable(empirical[,4],empirical[,34])[[1]][,2]   # Хі-квадрат Фрідмана
CrossTable(empirical[,4],empirical[,35])[[1]][,2]   # Критерій Фішера
CrossTable(empirical[,4],empirical[,36])[[1]][,2]   # T-Уілкоксона
CrossTable(empirical[,4],empirical[,37])[[1]][,2]   # Багатовимірне шкалювання
CrossTable(empirical[,4],empirical[,38])[[1]][,2]   # Перевірка нормальності розподілу

CrossTable(empirical[,4],empirical[,19])[[1]][,2]   # Опмсова статистика

CrossTable(empirical[,4],empirical[,23])[[1]][,2] +
CrossTable(empirical[,4],empirical[,26])[[1]][,2] +
CrossTable(empirical[,4],empirical[,28])[[1]][,2] +
CrossTable(empirical[,4],empirical[,33])[[1]][,2] +
CrossTable(empirical[,4],empirical[,34])[[1]][,2] +
CrossTable(empirical[,4],empirical[,35])[[1]][,2] +
CrossTable(empirical[,4],empirical[,36])[[1]][,2]  # Методи статистичного висновку (відмінності)

CrossTable(empirical[,4],empirical[,20])[[1]][,2] +
CrossTable(empirical[,4],empirical[,21])[[1]][,2]
CrossTable(empirical[,4],empirical[,19])[[1]][,2]  # Методи статистичного висновку (зв'язки)

CrossTable(empirical[,4],empirical[,24])[[1]][,2] +
CrossTable(empirical[,4],empirical[,27])[[1]][,2] +
CrossTable(empirical[,4],empirical[,29])[[1]][,2] +
CrossTable(empirical[,4],empirical[,38])[[1]][,2]  # Методи статистичного висновку (розподіли)

CrossTable(empirical[,4],empirical[,22])[[1]][,2] +
CrossTable(empirical[,4],empirical[,37])[[1]][,2]   # Багатомірні методи (структури)

CrossTable(empirical[,4],empirical[,25])[[1]][,2]   # Багатомірні методи (зв'язки)

# Описание выборок

a = vector()
for (i in 1:nrow(data)) {
a = c(a,as.numeric(strsplit(data$samples, ",")[[i]]))
}

summary(a)

qplot(a, geom="histogram",binwidth = 50) + ylab("Кількість") + xlab("Розмір вибірки")
qplot(a[a<300], geom="histogram",binwidth = 10) + ylab("Кількість") + xlab("Розмір вибірки")  + scale_x_continuous(breaks=seq(0, 300, 20))