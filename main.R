# task 2
df <- data.frame(
  var1 = c(11, 21, 31),
  var2 = c(12, 22, 32),
  var3 = c(13, 23, 33),
  var4 = c(14, 24, 34),
  row.names = c("case1", "case2", "case3")
)

# Выбрать значения var1, var2, var3 для case1.
# Выбрать значения всех переменных для case2, которые больше 22.

case1Values <- df[1, 1:3]
case2Values <- df[2, df[2,] > 22]

# Выбрать имена переменных для колонок 1 и 3.

varNames <- colnames(df)[c(1, 3)]

# Добавить колонку с именем Y и значениями -1, 0, 1.

df$Y <- c(-1, 0, 1)
# Удалить строку case2.
df <- df[-2,]
# Значения второй колонки возвести в третью степень.
df[, 2] <- df[, 2]^3

df

# task 16
# 16.Создать матрицу matr [m,n]. Значения m и n ввести с клавиатуры. Присвоить имена срокам и
# столбцам. Сформировать субматирцу из четных строк. Найти вектор максимальных
# элементов столбцов матрицы.
print(" ")
m <- 3
n <- 3
matr <- matrix(1:m * n * 5, nrow = m, ncol = n)
print(matr)
print(" ")
rownames(matr) <- paste("row", 1:m, sep = "")
colnames(matr) <- paste("col", 1:n, sep = "")
matr <- as.data.frame(matr)
print(matr[seq(2, m, 2),])
print(apply(matr, 2, max))



# task "unknown"
# set seed
# set.seed(123)
x <- sample(c(0, 1), 20, replace = TRUE, prob = c(0.3, 0.7))
# count number of zeros in x in 2 cases
print(x)
print(sum(x == 0))
cat(sum(!x)/length(x)*100,"%","\n")

