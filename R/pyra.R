pyra <- function(){
   library(Sweden)
   fem <- get.pop(sex = "females")
   mal <- get.pop(sex = "males")
   m1 <- mal[, 1] / 1000
   f1 <- -fem[, 1] / 1000
   age <- 0:105
   plot(m1, age, type = "s", xlim = c(-70, 70), xlab = "")
   lines(f1, age, type = "s")
   abline(v = 0, h = 0)
   for (year in 1970:2007){
      Sys.sleep(1)
      i <- year - 1968
      m <- mal[, i] / 1000
      f <- -fem[, i] / 1000
      plot(m1, age, type = "s", xlim = c(-70, 70), xlab = "Size (thousands)")
      lines(f1, age, type = "s")
      abline(v = 0, h = 0)
      lines(m, age, type = "s", col = "blue")
      lines(f, age, type = "s", col = "red")
   }
}