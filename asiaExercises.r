library(bnlearn)
data(asia)

#?asia
#summary(asia)
#dys = [asia$A == "yes" & asia$D == "yes",]
#summary(dys)

#2.1

dag = model2network("[A][S][T|A][L|S][B|S][E|T:L][D|E:B][X|E]")

par(mfrow = c(1,3))
graphviz.plot((skeleton(dag)))
graphviz.plot((moral(dag)))
graphviz.plot((cpdag(dag)))

mblanket <- function(dag, node){
  # This function gets the markov blanket of a node in a bayesian network
  # It can be easily done with the moral graph, this is just an exercise to see how
  # defining my own functions works in R and to try to get used to navigating bayesian networks
  if(class(dag) != "bn")
    stop(paste("The argument '",deparse(substitute(dag)),"' is not a dag"),sep="")
  if(!any(nodes(dag)==node))
    stop(paste("The node ",deparse(substitute(node))," does not exist in ",deparse(substitute(dag))),sep="")
  
  library(sets)
  blanket = as.set(as.vector(rbind(parents(dag,node=node),children(dag,node=node))))
  
  for (i in children(dag,node=node))
    for(j in parents(dag,node=i))
      blanket = append(blanket,j)
  
  if (node %in% blanket)
    blanket[node] = NULL
  
  return(as.vector(blanket))
}


