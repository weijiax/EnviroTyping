
#install.packages("pixmap",repos="http://R-Forge.R-project.org")
library(pixmap)

# The first thing we need to do is convert the image into either a [R,G,B] (extension .ppm) 
# or a grayscale (extension .pgm). Let's start with the [R,G,B] image and see what the data 
# looks like in R:

system("convert rappa.jpg rappa.ppm")
rappa = read.pnm("rappa.ppm")
#Show the type of the information contained in our data:
str(rappa)

# Rather than a traditional data frame, when working with an image, 
# we have to refer to the elements in this data set with `@' rather 
# than with `$'

rappa@size

# We can then display a heat map showing the intensity of each individual color in each pixel:

rappa.red=rappa@red
rappa.green=rappa@green
rappa.blue=rappa@blue
image(rappa.green)


  rappa.green=t(rappa.green)[,nrow(rappa.green):1]
  image(rappa.green)

system("convert rappa.jpg rappa.pgm")
greyrappa = read.pnm("rappa.pgm")
str(greyrappa)
rappa.grey=greyrappa@grey
#again, rotate 90 degrees
rappa.grey=t(rappa.grey)[,nrow(rappa.grey):1]

# Show the image
image(rappa.grey, col=grey((0:1000)/1000))

# Computing the SVD of Dr. Rappa
  rappasvd=svd(rappa.grey)
  U=rappasvd$u
  d=rappasvd$d
  Vt=t(rappasvd$v)

 #   Now let's compute some approximations of rank 3, 10 and 50 by recreating the Matrix:
# Rank 3
  rappaR3=U[ ,1:3]%*%diag(d[1:3])%*%Vt[1:3, ]
  image(rappaR3, col=grey((0:1000)/1000))
# Rank 10
  rappaR10=U[ ,1:10]%*%diag(d[1:10])%*%Vt[1:10, ]
  image(rappaR10, col=grey((0:1000)/1000))
# Rank 25
  rappaR25=U[ ,1:25]%*%diag(d[1:25])%*%Vt[1:25, ]
  image(rappaR25, col=grey((0:1000)/1000))

#Let's examine the last 20 singular values:

d[140:160]

# Using the last 25 components:

rappa_bad25=U[ ,135:160]%*%diag(d[135:160])%*%Vt[135:160, ]
image(rappa_bad25, col=grey((0:1000)/1000))

  # Using the last 50 components:
  
  rappa_bad50=U[ ,110:160]%*%diag(d[110:160])%*%Vt[110:160, ]
image(rappa_bad50, col=grey((0:1000)/1000))

  # Using the last 100 components: (4 times as many components as it took us to recognize the face on the front end)
  
  rappa_bad100=U[ ,61:160]%*%diag(d[61:160])%*%Vt[61:160, ]
image(rappa_bad100, col=grey((0:1000)/1000))