pyra2 <- function(){
   library(sweden14)
   par(lwd = 2, cex = 1)
   fem <- getPop(sex = "females")
   mal <- getPop(sex = "males")
   m1 <- mal[, 1] / sum(mal[, 1]) * 100
   f1 <- -fem[, 1] / sum(fem[, 1]) * 100
   age <- 0:100
   ##plot(m1, age, type = "s", xlim = c(-70, 70), xlab = "")
   ##lines(f1, age, type = "s")
   ##abline(v = 0, h = 0)
   for (year in 2014){
##      Sys.sleep(1)
      i <- year - 1968
      m <- mal[, i] / sum(mal[, i]) * 100
      f <- -fem[, i] / sum(fem[, i]) * 100
      plot(m1, age, type = "s", xlim = c(-1.8, 1.8), xlab = "Size (per cent)",
           axes = FALSE)
      axis(1, at = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5), 
           labels = c(1.5, 1, 0.5, 0, 0.5, 1, 1.5))
      axis(2, at = c(0, 21, 65, 100))
      box()
      lines(f1, age, type = "s")
      abline(v = 0, h = 0)
      lines(m, age, type = "s", col = "blue")
      lines(f, age, type = "s", col = "red")
      text(-1.2, 90, "Females 2014", col = "red")
      text(1.2, 90, "Males 2014", col = "blue")
      par(lwd = 1)
      abline(h = c(21, 65), lty = 2)
   }
}