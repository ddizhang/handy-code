rangefrm = data.frame(
  mspe = c(mspe.n[[2]], mspe.sh[[2]], mspe.d[[2]], mspe.area[[2]],mspe.227[[2]],
           mspe.n[[4]], mspe.sales[[4]], mspe.d[[4]], mspe.area[[4]],mspe.227[[4]],
           mspe.n[[8]], mspe.sales[[8]], mspe.d[[8]], mspe.area[[8]],mspe.227[[8]]),
  range = rep(c(rep('1', 52), rep('2', 52),
                rep('3', 52), rep('4', 52),
                rep('5', 52)),3),
  ext = c(rep(2, 52*5), rep(4, 52*5), rep(8, 52*5)))

boxplot(mspe~range, data = rangefrm, subset = ext == 2,
        varwidth = TRUE, boxwex = 0.18, at = 1:5 - 0.4, 
        outline = FALSE, ylim = c(0.02, 0.14),
        xaxt="n", 
        col = "brown1", border = FALSE,
        main = "Store Range", ylab = "% Prediction Error, 50% Interval"
)
boxplot(mspe~range, data = rangefrm, subset = ext == 4,
        boxwex = 0.18, at = 1:5 - 0.2, 
        outline = FALSE, xaxt="n",
        add = TRUE, col = "brown3", border = FALSE)

boxplot(mspe~range, data = rangefrm, subset = ext == 8,
        boxwex = 0.18, at = 1:5 , range = 0.5,
        outline = FALSE, 
        add = TRUE, xaxt="n", col = "grey42", border = FALSE)
axis(side = 1, at = 1:5, 
     labels = c("Nation","Market" , "District", "Area", "Store"))
legend("topleft", c("Ext. 2w", "Ext. 4w", "Ext. 8w"),
       fill = c("brown1", "brown3", "grey42"))



summary = NULL
for (k in 1:52)
{
  f = tc00$tc[(trainLen+k):length(tc00$tc)]
  pp = pred[,k][1:(length(pred[,k])-k+1)]
  ll = lower[,k][1:(length(lower[,k])-k+1)]
  uu = upper[,k][1:(length(upper[,k])-k+1)]
  
  f = f[max(1,(length(f) - 51)):length(f)]
  pp = pp[max(1,(length(pp) - 51)):length(pp)]
  
  summary = rbind(summary, quantile(abs(pp-f)/f, probs = c(0.1, 0.35, 0.5, 0.65, 0.9)))
}


plot(summary[,3], type = "l", 
     ylim = c(min(summary[,1]), max(summary[,5])),
     xlab = "weeks ahead", ylab = "% Prediction Error",
     main = "Extrapolation")
polygon(c(1:52, rev(1:52)),
        c(summary[,1], rev(summary[,5])), col = "bisque1",
        border = NA) 
polygon(c(1:52, rev(1:52)),
        c(summary[,2], rev(summary[,4])), col = "bisque3",
        border = NA) 
lines(summary[,3], type = "l", col = "grey32", lwd = 2)
legend("topleft", c("Median","30% Interval", "90% Interval"),
       pch = c(NA,16,16), 
       col = c("grey32","bisque3", "bisque1"),
       lwd = c(2, 2,2))