library(vars)
data("Canada") # 4 x 336

var.Ca = VAR(Canada, p = 1)
summary(var.Ca)

c = coef(var.Ca)
serial.test(var.Ca)
pv = 0.638
m = matrix(0,4,5)
i = 1

for (variable in c){
  elems = which(variable[, "Pr(>|t|)"] < pv)
  m[i,elems] = variable[elems,"Estimate"]
  i = i+1
}

A = m[,1:4]
B = matrix(m[,5],4,1)

library(lars)

x = Canada[1:(nrow(Canada) - 1), ]

fit.all = lapply(colnames(Canada),
  function(col) {
    y = Canada[-(1:1), col]
    lars(y = y, x = x, type = "lasso")
  })

pred.all = lapply(colnames(Canada),
  function(col) {
    y = Canada[-(1:1), col]
    lasso.cv = cv.lars(y = y, x = x, mode = "fraction")
    frac = lasso.cv$index[which.min(lasso.cv$cv)]
    i = which(colnames(Canada) == col)
    predict(fit.all[[i]], s = frac, type = "coef", mode = "lambda")
  })

m2 = matrix(0,4,4)
for(i in 1:4){
  m2[i,] = pred.all[[i]]$coefficients
}