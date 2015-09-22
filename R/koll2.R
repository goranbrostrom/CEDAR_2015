koll2 <- function(antrep = 100){
   library(eha)
   dat = data.frame(T = 1:10, x = c(1:4, 5.02, 4.98, 7:10), event = rep(1, 10))
   dat = dat[order(dat$x), ]
   fit0 <- coxreg(Surv(T, event) ~ x, data = dat)
   llmax <- fit0$loglik[2]
   beta <- fit0$coef[1]
   ll0 <- coxreg(Surv(T, event) ~ 1, data = dat)$loglik[1]
   b <- seq(beta - 10, beta + 10, length = antrep)
   ll = numeric(antrep)
   for (i in 1:antrep){
      dat$offs <- b[i] * dat$x
      ll[i] = coxreg(Surv(T, event) ~ offset(offs), data = dat)$loglik[1]
   }
   plot(b, ll, type = "l", axes = FALSE, xlab = "beta", ylab = "loglihood", 
        ylim = c(-17, 0), lwd = 1.5)
   axis(1, at = c(round(beta, 2), 0))
   axis(2, at = c(round(ll0, 2), round(llmax, 2)))
   box()
   abline(v = c(0, beta), lty = 3)
   abline(h = c(ll0, llmax), lty = 2, col = "blue")
   lw <- function(b){
      -(b - beta)^2 / (2 * fit0$var[1, 1]) + llmax
   }
   lines(b, lw(b), col = "red")
   ##legend("bottom", legend = c("True", "Wald"), col = c("black", "red"), lty = 1)
   text(2, -3, 'Wald', col = "red")
   text(-3, -4, 'True', col = "black")
      list(b, ll, llmax, lw(0))
}