library(MASS)

step(lm(crim ~ zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + 
          lstat + medv, data = Boston), direction = "both")
