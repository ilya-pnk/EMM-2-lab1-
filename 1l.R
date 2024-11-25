
# Задание 1: Функция для моделирования процесса AR(1)
set.seed(123)

ar_process = function(n, theta) {
  x <- numeric(n)
  x[1] <- rnorm(1) # Начальное значение
  for (k in 2:n) {
    x[k] <- theta * x[k - 1] + rnorm(1)
  }
  return(x)
}

# Построение графиков для разных значений theta
n = 100
theta_values = c(0.5, 1, 1.5)

for (theta in theta_values) {
  x <- ar_process(n, theta)
  plot(x, type = "l", main = paste("AR(1), theta =", theta))
}
#замечаем изменение колебаний процесса с увеличением пар-ра tetha


# Задание 2: методом наименьших квадратов (МНК)

x <- ar_process(100, 0.5) 

# Зададим функцию МНК
mnk_function <- function(theta) {
  n <- length(x)
  mnk <- (x[2:n] - theta * x[1:(n-1)])^2
  return(sum(mnk))
}


# Функция для вычисления оптимального theta, приравняв производную к нулю
  optim_theta_mnk <- function(x) {
  n <- length(x)
  # Вычисляем производную и решаем уравнение для theta
  theta_start <- 0.5 # начальное значение для поиска
  result <- optim(theta_start, fn = function(theta) eval(mnk_function(theta)))
  return(result$par)# '$par' говорит о том что мы берем только оптимальное значение
}

# Пример использования
theta_mnk <- optim_theta_mnk(x)
theta_mnk


set.seed(123)
# Задание 3: Оценка максимального правдоподобия
ar_process <- function(n, theta) {
  x <- numeric(n)
  x[1] <- rnorm(1)
  for (k in 2:n) {
    x[k] <- theta * x[k-1] + rnorm(1)
  }
  return(x)
}




#  функциz для логарифма правдоподобия
log_MaxTrue <- function(theta) {
  n <- length(x)
  errors <- (x[2:n] - theta * x[1:(n-1)])^2
  return(-sum(errors)) 
}

#  нахождение оценки параметра theta методом МП
result <- optim(par = 0.5, fn = log_MaxTrue, control = list(fnscale = -1)) # fnscale = -1 для максимизации
theta_МП <- result$par

 theta_МП

# Задание 4: Оценка для выборок разного объема
 # для этого перепишем мнк через сумму
 mnk_function2 <- function(x) {
   n <- length(x)
   sum_xx <- sum(x[-1] * x[-n])
   sum_x2 <- sum(x[-n]^2)
   theta_mnk <- sum_xx / sum_x2
   return(theta_mnk)
 }
 
 
 
 
theta_4 =numeric(n - 9)
for (k in 10:n) {
  theta_4[k - 9] <-mnk_function2(x[1:k])
}
plot(10:n, theta_4, type = "l", main = " theta с изменением объема выборки", xlab = "Объем выборки", ylab = "Оценка theta")
set.seed(123)
# Задание 5: Построение устойчивого процесса AR(2)
ar2_process <- function(n, theta1, theta2) {
  x <- numeric(n)
  x[1:2] <- rnorm(2) # Начальные значения
  for (k in 3:n) {
    x[k] <- theta1 * x[k - 1] + theta2 * x[k - 2] + rnorm(1)
  }
  return(x)
}


# Пример для стационарного AR(2)
theta1 = 0.5
theta2 = 0.3
x = ar2_process(n, theta1, theta2)
plot(x, type = "l", main = paste("theta=", theta1,  theta2))

# Заданные значения параметров AR(2)
theta1;
theta2;

# Проверка корней
D = theta1^2 + 4 * theta2
lam1 = (theta1 + sqrt(D)) / 2
lam2 = (theta1 - sqrt(D)) / 2

lam1; abs(lam1)
lam2;  abs(lam2)

if (abs(lam1) < 1 & abs(lam2) <1) {
  ("Процесс AR(2) является стационарным.")
} else {
  ("Процесс AR(2) не является стационарным.")
}

# Задание 6: Оценка параметров AR(2)  функцией arima
library(stats)
ar2_model <- arima(x, order = c(2, 0, 0)) # вектор параметров из образца
print(ar2_model)

