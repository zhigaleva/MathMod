x=10/(5-5)
x
a<-seq(-8,10,.5)
a
x=a[length(a):1]
x
y=c(a[1:round(length(a)/2)],99,a[(round(length(a)/2)+1):length(a)])
y
x=0:3
y=acos(cos(x))
x==y
dna=factor(rep(c("T","G","C","A"),c(16,38,6,10)))
ratio=summary(dna)/sum(summary(dna))
ratio
dna2=sample(c("A","T","G","G","G","C"), size=3826513,replace=TRUE)
ratio=summary(factor(dna2))/length(dna2)
ratio
