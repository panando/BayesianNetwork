library(bnlearn)
data(marks)

dag.gs = cextend(gs(marks))

marksd2 = discretize(marks, method = "quantile", breaks = 2) 
marksd3 = discretize(marks, method = "quantile", breaks = 3) 
marksd4 = discretize(marks, method = "quantile", breaks = 4) 
marksd10 = discretize(marks, method = "quantile", breaks = 10) 

dagNplot <- function(){
  dag.gsd2 = cextend(gs(marksd2))
  dag.gsd3 = cextend(gs(marksd3))
  dag.gsd4 = cextend(gs(marksd4))
  dag.gsd10 = cextend(gs(marksd10))
  
  graphviz.plot(dag.gs)
  graphviz.plot(dag.gsd2)
  graphviz.plot(dag.gsd3)
  graphviz.plot(dag.gsd4)
  graphviz.plot(dag.gsd10)
  
}

dagNplot()

score(dag.gs, data = marks, type = "bic-g")
score(dag.gsd2, data = marks, type = "bic-g")
score(dag.gsd3, data = marks, type = "bic-g")
score(dag.gsd4, data = marks, type = "bic-g")
score(dag.gsd10, data = marks, type = "bic-g")


marksd2 = discretize(marks, method = "interval", breaks = 2) 
marksd3 = discretize(marks, method = "interval", breaks = 3) 
marksd4 = discretize(marks, method = "interval", breaks = 4) 
marksd10 = discretize(marks, method = "interval", breaks = 10)

dagNplot()

