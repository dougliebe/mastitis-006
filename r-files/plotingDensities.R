library(sm)

var = 'TotalRestTime'
# create value labels 
cyl.f <- factor(pairs.sick$Farm) 
# plot densities 
sm.density.compare(pairs.sick[,c(var)], pairs.sick$Farm, h = 0.2, xlab = var, main = NULL)
# title(main=var)

# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f)))) 
legend(locator(1), levels(cyl.f), fill=colfill)
