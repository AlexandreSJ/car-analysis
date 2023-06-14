cars <- c( 1, 3, 6, 4, 9)
trucks <- c(2,  5, 4, 5, 12)

png(filename="./temp.png")

plot(cars, type="o", col="blue", ylim=c(0,12))
lines(trucks, type="o", pch=22, lty=2, col="red")
title(main="Cars", col.main="red", font.main=4)

dev.off()
