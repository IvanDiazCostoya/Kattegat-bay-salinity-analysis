
# Cargar los datos

rm(list=ls())

library(geoR)
data(package="geoR")
data(kattegat) # Datos de salinidad en la cuenca de Kattegat

# Mapa estudio

x11()
plot(c(320,870),c(5970,6520),type="n",xlab="X UTM (E - W)",ylab="Y UTM (N - S)",main="Salinity in the Kattegat bay")
rect(250,5000,1000,6800,col="cyan")
lapply(kattegat$dk, polygon, col="bisque2")
rect(298,5948,892,6542,lwd=1)
points(kattegat, add=TRUE,col="blue")
text(400,6420,"North Sea",col="blue")
text(495,6180,"Denmark",col="brown4")
text(780,6480,"Sweden",col="brown4")
text(800,6090,"Baltic Sea",col="blue")
text(750,5980,"Germany",col="brown4")
lines(c(530,770,770,530,530), c(6430,6430,6190,6190,6430),lwd=2,lty="dashed",col="blue")

# Mapa estudio 2

x11()
plot(c(530,770),c(6190,6430),type="n",xlab="X UTM (E - W)",ylab="Y UTM (N - S)",main="Salinidad en la cuenca de Kattegat")
rect(250,5000,1000,6800,col="cyan")
lapply(kattegat$dk, polygon, lwd=1,col="bisque2")
rect(520.5,6180,779.5,6439.5,lwd=1)
points(kattegat, add=TRUE,col="blue")
text(550,6250,"Dinamarca",col="brown4")
text(725,6400,"Suecia",col="brown4")

# Resumen de los datos

names(kattegat); summary(kattegat)

# Media de la salinidad e intervalo de confianza para la media al 95%

z=qnorm(0.05/2)
s=sd(kattegat$data)/sqrt(length(kattegat$data))
int1_m=mean(kattegat$data) + z*s
int2_m=mean(kattegat$data) - z*s

mean(kattegat$data); c(int1_m, int2_m)

# Cuasidesviación típica de la salinidad e intervalo de confianza al 95%

l=length(kattegat$data)
c1=qchisq(1-0.05/2,l-1)
c2=qchisq(0.05/2,l-1)
int1_s=(l-1)*var(kattegat$data)/c1
int2_s=(l-1)*var(kattegat$data)/c2

sd(kattegat$data); sqrt(c(int1_s, int2_s))

# Diagrama de cajas

x11()
boxplot(kattegat$data,main="Salinity box diagram",ylab="Salinity",col="cyan")

# Scatterplot3d de los datos

library(scatterplot3d)
library(RColorBrewer)

numcol=8 # Número de colores
paleta=brewer.pal(numcol,"BuPu") # Crea la paleta de 8 colores

# A cada dato asigna un color de la paleta en función de su valor rank()

colornum=cut(rank(kattegat$data), numcol, labels=FALSE) # Asigna número de color
codcol=paleta[colornum] # Asigna colores a cada dato

x11()
s3d=scatterplot3d(kattegat$coords[,1],kattegat$coords[,2], kattegat$data, type="h", angle=76, color=codcol, pch=20, cex.symbols=2, col.axis="gray", col.grid="gray", xlab="X UTM (E - W)", ylab="Y UTM (N - S)",zlab="Salinity",main="Salinity in the Kattegat bay")

# Histograma y curva normal de los datos

x11()
hist(kattegat$data,freq=F,col="cyan",xlab="Salinity",ylab="Density",main="Salinity histogram")
a=seq(-1,41,by=0.01)
b=dnorm(a,mean(kattegat$data),sd(kattegat$data))
lines(a,b,col="blue")

# Contraste de normalidad

shapiro.test(kattegat$data) # Se acepta la normalidad de los datos de salinidad

# Puntos de muestreo con datos diferenciados en función de los cuartiles
# Datos de salinidad en función de las coordenadas X e Y
# Histograma de frecuencias con función de densidad

x11()
plot(kattegat)

# Además de la dependencia espacial, también es interesante estudiar
# la salinidad en función de la distancia a la costa.
# Podemos generar un vector de distancias que asocie a cada punto de 
# muestreo su distancia mínima a la costa.
# Tarda un par de segundos en ejecutarse.

n1=length(kattegat$coords[,1])
n2=length(kattegat$dk$dk1[,1])
n3=length(kattegat$dk$dk2[,1])
n4=length(kattegat$dk$dk3[,1])
n5=length(kattegat$dk$dk4[,1])
n6=length(kattegat$dk$dk5[,1])
n7=length(kattegat$dk$dk6[,1])
n8=length(kattegat$dk$dk7[,1])
n9=length(kattegat$dk$dk8[,1])
n10=length(kattegat$dk$dk9[,1])
n11=length(kattegat$dk$dk10[,1])
n12=length(kattegat$dk$dk11[,1])
n13=length(kattegat$dk$dk12[,1])

d0=rep(1000,n1)
p0=matrix(0,70,2)

for (i in 1:n1)
	{
	for (j in 1:n2)
		{
		x1=kattegat$coords[i,1]
		y1=kattegat$coords[i,2]
		x2=kattegat$dk$dk1[j,1]
		y2=kattegat$dk$dk1[j,2]
		d1=sqrt((x2-x1)^2+(y2-y1)^2)
		if (d1<d0[i]) 
			{
			d0[i]=d1
			p0[i,1]=x2
			p0[i,2]=y2
			}
		}
	for (j in 1:n3)
		{
		x1=kattegat$coords[i,1]
		y1=kattegat$coords[i,2]
		x2=kattegat$dk$dk2[j,1]
		y2=kattegat$dk$dk2[j,2]
		d1=sqrt((x2-x1)^2+(y2-y1)^2)
		if (d1<d0[i]) 
			{
			d0[i]=d1
			p0[i,1]=x2
			p0[i,2]=y2
			}
		}
	for (j in 1:n4)
		{
		x1=kattegat$coords[i,1]
		y1=kattegat$coords[i,2]
		x2=kattegat$dk$dk3[j,1]
		y2=kattegat$dk$dk3[j,2]
		d1=sqrt((x2-x1)^2+(y2-y1)^2)
		if (d1<d0[i]) 
			{
			d0[i]=d1
			p0[i,1]=x2
			p0[i,2]=y2
			}
		}
	for (j in 1:n5)
		{
		x1=kattegat$coords[i,1]
		y1=kattegat$coords[i,2]
		x2=kattegat$dk$dk4[j,1]
		y2=kattegat$dk$dk4[j,2]
		d1=sqrt((x2-x1)^2+(y2-y1)^2)
		if (d1<d0[i]) 
			{
			d0[i]=d1
			p0[i,1]=x2
			p0[i,2]=y2
			}
		}
	for (j in 1:n6)
		{
		x1=kattegat$coords[i,1]
		y1=kattegat$coords[i,2]
		x2=kattegat$dk$dk5[j,1]
		y2=kattegat$dk$dk5[j,2]
		d1=sqrt((x2-x1)^2+(y2-y1)^2)
		if (d1<d0[i]) 
			{
			d0[i]=d1
			p0[i,1]=x2
			p0[i,2]=y2
			}
		}
	for (j in 1:n7)
		{
		x1=kattegat$coords[i,1]
		y1=kattegat$coords[i,2]
		x2=kattegat$dk$dk6[j,1]
		y2=kattegat$dk$dk6[j,2]
		d1=sqrt((x2-x1)^2+(y2-y1)^2)
		if (d1<d0[i]) 
			{
			d0[i]=d1
			p0[i,1]=x2
			p0[i,2]=y2
			}
		}
	for (j in 1:n8)
		{
		x1=kattegat$coords[i,1]
		y1=kattegat$coords[i,2]
		x2=kattegat$dk$dk7[j,1]
		y2=kattegat$dk$dk7[j,2]
		d1=sqrt((x2-x1)^2+(y2-y1)^2)
		if (d1<d0[i]) 
			{
			d0[i]=d1
			p0[i,1]=x2
			p0[i,2]=y2
			}
		}
	for (j in 1:n9)
		{
		x1=kattegat$coords[i,1]
		y1=kattegat$coords[i,2]
		x2=kattegat$dk$dk8[j,1]
		y2=kattegat$dk$dk8[j,2]
		d1=sqrt((x2-x1)^2+(y2-y1)^2)
		if (d1<d0[i]) 
			{
			d0[i]=d1
			p0[i,1]=x2
			p0[i,2]=y2
			}
		}
	for (j in 1:n10)
		{
		x1=kattegat$coords[i,1]
		y1=kattegat$coords[i,2]
		x2=kattegat$dk$dk9[j,1]
		y2=kattegat$dk$dk9[j,2]
		d1=sqrt((x2-x1)^2+(y2-y1)^2)
		if (d1<d0[i]) 
			{
			d0[i]=d1
			p0[i,1]=x2
			p0[i,2]=y2
			}
		}
	for (j in 1:n11)
		{
		x1=kattegat$coords[i,1]
		y1=kattegat$coords[i,2]
		x2=kattegat$dk$dk10[j,1]
		y2=kattegat$dk$dk10[j,2]
		d1=sqrt((x2-x1)^2+(y2-y1)^2)
		if (d1<d0[i]) 
			{
			d0[i]=d1
			p0[i,1]=x2
			p0[i,2]=y2
			}
		}
	for (j in 1:n12)
		{
		x1=kattegat$coords[i,1]
		y1=kattegat$coords[i,2]
		x2=kattegat$dk$dk11[j,1]
		y2=kattegat$dk$dk11[j,2]
		d1=sqrt((x2-x1)^2+(y2-y1)^2)
		if (d1<d0[i]) 
			{
			d0[i]=d1
			p0[i,1]=x2
			p0[i,2]=y2
			}
		}
	for (j in 1:n13)
		{
		x1=kattegat$coords[i,1]
		y1=kattegat$coords[i,2]
		x2=kattegat$dk$dk12[j,1]
		y2=kattegat$dk$dk12[j,2]
		d1=sqrt((x2-x1)^2+(y2-y1)^2)
		if (d1<d0[i]) 
			{
			d0[i]=d1
			p0[i,1]=x2
			p0[i,2]=y2
			}
		}
	}

# Resumen distancias a la costa

summary(d0)

# Representación de las distancias a la costa más cercana

x11()
plot(c(530,770),c(6190,6430),type="n",xlab="X UTM (E - W)",ylab="Y UTM (N - S)",main="Distance to the coast")
rect(250,5000,1000,6800,col="cyan")
lapply(kattegat$dk, polygon, lwd=1,col="bisque2")
rect(520.5,6180,779.5,6439.5,lwd=1)
points(kattegat, add=TRUE,col="blue")
for (i in 1:70) {lines(c(kattegat$coords[i,1],p0[i,1]),c(kattegat$coords[i,2],p0[i,2]),lty="dotted")}
text(550,6250,"Denmark",col="brown4")
text(725,6400,"Sweden",col="brown4")

# A partir de aquí, 4 ejemplos cortos de regresión
# de la salinidad en función de la distancia a la costa.
# No me queda claro si pides o no regresión para este ejercicio,
# por si acaso, ahí los dejo.
# Modelos lineal simple, GAM y polinómicos de grados 2 y 3.

library(mgcv)

mod0=lm(kattegat$data~d0)

mod1=gam(data~s(d0), data = kattegat, family = gaussian)

mod2=lm(data~poly(d0,2), data = kattegat)

mod3=lm(data~poly(d0,3), data = kattegat)

x11()
m=matrix(c(1,3,2,4),2,2) 
layout(m)
a=seq(-1,40,by=41.5/70) # ESTA LINEA?
b=data.frame(d0=seq(0,34.5,by=0.5)) # d0 USADO 2 VECES?
plot(d0,kattegat$data,xlab="Distance to the coast",ylab="Salinity",main="Lineal model")
lines(b$d0,predict(mod0,b),lty=1,col="cyan3",lwd=3)
plot(d0,kattegat$data,xlab="Distance to the coast",ylab="Salinity",main="GAM model")
lines(b$d0,predict(mod1,b),lty=1,col="blue",lwd=3)
plot(d0,kattegat$data,xlab="Distance to the coast",ylab="Salinity",main="2nd order polynomial model")
lines(b$d0,predict(mod2,b),lty=1,col="darkblue",lwd=3)
plot(d0,kattegat$data,xlab="Distance to the coast",ylab="Salinity",main="3rd order polynomial model")
lines(b$d0,predict(mod3,b),lty=1,col="darkorchid1",lwd=3)

AIC(mod0,mod1,mod2,mod3) # El modelo GAM parece el mejor ajustado en función del AIC

# La salinidad aumenta con la distancia a la costa hasta cierta distancia y luego tiende a estabilizarse

# Ya que el modelo GAM es el mejor ajustado en función del AIC, podemos analizarlo

summary(mod1) # Resumen del modelo
coef(mod1) # Coeficientes
anova(mod1) # p-valor de 0.0149, parece que hay correlación, CONTRASTE SIGNIFICATIVO O NO?

# Análisis de residuos de mod1
 
x11()
gam.check(mod1)

# Contraste normalidad residuos de mod1

shapiro.test(resid(mod1)) # Los residuos son normales, ERROR? NO SON NORMALES

x11()
hist(resid(mod1),freq=F,col="cyan",xlab="Residuos",ylab="Densidad",main="Histograma de los residuos del modelo GAM")
a=seq(-11,11,by=0.01)
b=dnorm(a,mean(resid(mod1)),sd(resid(mod1)))
lines(a,b,col="blue")


