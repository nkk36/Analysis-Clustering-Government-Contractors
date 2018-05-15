int_hist = function(x, ylab="Frequency",...){
  b.plot = barplot(table(factor(x,levels=min(x):max(x))),space=0,xaxt="n",ylab=ylab,...);
  axis(1)
  
  output = list(plot = b.plot, counts = table(factor(x,levels=min(x):max(x))))
  return(output)
}