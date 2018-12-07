# Voy a usar el dataset de Canada para ir probando el dmmhc

# Voy a coger el frame y lo voy a ensanchar con t-1,t y t+1 en cada fila, para el segundo paso del dmmhc
timeFormat = function(frame){
  nodes = names(frame)
  cols = dim(frame)[2]
  nodes2 <- character(cols*3)
  mat = matrix(nrow=dim(frame)[1]-2,ncol=3*cols)
  
  # Names for the new frame
  for(i in 1:cols)
  {
    nodes2[i] = paste(nodes[i],".t-1",sep="")
    nodes2[i+length(nodes)] = nodes[i]
    nodes2[i+2*length(nodes)] = paste(nodes[i],".t+1",sep="")
  }
  
  # Initialize the first two rows, then go diagonally all the way
  mat[1,(1:cols)] <- as.matrix(frame[1,(1:cols)])
  mat[1,((cols+1):(2*cols))] <- as.matrix(frame[2,(1:cols)])
  mat[2,(1:cols)] <- as.matrix(frame[2,(1:cols)])
  
  for(i in 3:dim(frame)[1]-2) #me falla el limite inferior
  {
    mat[i-2,((2*cols+1):(3*cols))] <- as.matrix(frame[i,(1:cols)])
    mat[i-1,((cols+1):(2*cols))] <- as.matrix(frame[i,(1:cols)])
    mat[i,(1:cols)] <- as.matrix(frame[i,(1:cols)])
  }
  mat[81,(1:(3*cols))]
}

library(bnlearn)
library(vars)

info <- data.frame(Canada) # El frame tiene uno tras otro las observaciones

net0.hc = hc(info) # La red estatica generada por el hill climbing, a comparar con la red estatica generada por el dmmhc

timeFormat(info)
#net0.dmmhc = dmmhc(info)