pyra <- function(){
   library(sweden14)
   par(lwd = 2)
   fem <- getPop(sex = "females")
   mal <- getPop(sex = "males")
   m1 <- mal[, 1] / 1000
   f1 <- -fem[, 1] / 1000
   age <- 0:100
   ##plot(m1, age, type = "s", xlim = c(-70, 70), xlab = "")
   ##lines(f1, age, type = "s")
   ##abline(v = 0, h = 0)
   for (year in 2014){
##      Sys.sleep(1)
      i <- year - 1968
      m <- mal[, i] / 1000
      f <- -fem[, i] / 1000
      plot(m1, age, type = "s", xlim = c(-70, 70), xlab = "Size (thousands)", 
           axes = FALSE)
      axis(2, at = c(0, 21, 65, 100))
      axis(1, at = c(-60, -40, -20, 0, 20, 40, 60),
           labels = c("60", "40", "20", "0", "20", "40", "60"))
      box()
      lines(f1, age, type = "s")
      abline(v = 0, h = 0)
      lines(m, age, type = "s", col = "blue")
      lines(f, age, type = "s", col = "red")
      text(-50, 90, "Females 2014", col = "red")
      text(50, 90, "Males 2014", col = "blue")
      par(lwd = 1)
      abline(h = c(21, 65), lty = 2)
   }
}