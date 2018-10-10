#Загрузите данные в датафрейм. Адрес: github    https://raw???путь_к_файлу_найдите_сами???/data/gmp.dat 
gmp <- gmp <- read.table(file = "https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2018/master/data/gmp.dat")
gmp$pop <- gmp$gmp/gmp$pcgmp

estimate.scaling.exponent <- function(a, y0=6611, response=gmp$pcgmp,
                                      predictor = gmp$pop, maximum.iterations=100, 
                                      deriv.step = 1/100, step.scale = 1e-12, stopping.deriv = 1/100) {
  mse <- function(a) { mean((response - y0*predictor^a)^2) }
  for (iteration in 1:maximum.iterations) {
    deriv <- (mse(a+deriv.step) - mse(a))/deriv.step
    a <- a - step.scale*deriv
    if (abs(deriv) <= stopping.deriv) { break() }
  }
  fit <- list(a=a,iterations=iteration,
              converged=(iteration < maximum.iterations))
  return(fit)
}


#Пример вызова с начальным занчением a
options(digits=14)
a.before.del<-estimate.scaling.exponent(0.15);
a.before.del

#С помошью полученного коэффициента постройте кривую (функция curve) зависимости
curve((y0=6611)*x^estimate.scaling.exponent(0.15)$a,gmp$pop,xlab="Население",ylab="Доход на душу населения ($/человеко-год)", main="Метрополии США, 2006");


#Удалите точку из набора исходных данных случайным образом, как изменилось статистическая оценка коэффициента a?
point<-round(runif(1,max=367));
gmp<-gmp[-point,];
a.after.del<-estimate.scaling.exponent(0.15);
a.after.del;
a.after.del$a-a.before.del$a;


#Запустите оценку несколько раз с разных стартовых точек. Как изменилось значение a?
a.after.del<-estimate.scaling.exponent(0)
a.after.del$a-a.before.del$a


a.after.del<-estimate.scaling.exponent(0.1)
a.after.del$a-a.before.del$a


a.after.del<-estimate.scaling.exponent(0.15)
a.after.del$a-a.before.del$a


a.after.del<-estimate.scaling.exponent(0.17)
a.after.del$a-a.before.del$a


a.after.del<-estimate.scaling.exponent(0.18)
a.after.del$a-a.before.del$a


a.after.del<-estimate.scaling.exponent(0.2)
a.after.del$a-a.before.del$a


a.after.del<-estimate.scaling.exponent(0.25)
a.after.del$a-a.before.del$a