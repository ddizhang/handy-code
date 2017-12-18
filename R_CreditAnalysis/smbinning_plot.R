smbinning.plot = function (ivout, option = "dist", sub = "") 
{
  r = ifelse(ivout$ivtable[nrow(ivout$ivtable) - 1, 2] == 0, 
             2, 1)
  if (option == "dist") {
    x_upper = nrow(ivout$ivtable) - r
    y_upper = max(ivout$ivtable[1:x_upper, 8]) * 1.25
    ch_dist = barplot(ivout$ivtable[1:x_upper, 8], names.arg = ivout$ivtable[1:x_upper, 
                                                                             1], axes = F, main = "Percentage of Cases", ylim = c(0, 
                                                                                                                                  y_upper), col = gray.colors(length(unique(ivout$ivtable[1:x_upper, 
                                                                                                                                                                                          1]))))
    text(x = ch_dist, y = ivout$ivtable[1:x_upper, 8], label = round(ivout$ivtable[1:x_upper, 
                                                                                   8] * 100, 1), pos = 3, cex = 1)
    abline(h = 0)
    mtext(sub, 3)
  }
  else if (option == "goodrate") {
    x_upper = nrow(ivout$ivtable) - r
    y_upper = max(ivout$ivtable[1:x_upper, 9], na.rm = T) * 
      1.25
    ch_goodrate = barplot(ivout$ivtable[1:x_upper, 9], names.arg = ivout$ivtable[1:x_upper, 
                                                                                 1], axes = F, main = "Good Rate (%)", ylim = c(0, 
                                                                                                                                y_upper), col = gray.colors(length(unique(ivout$ivtable[1:x_upper, 
                                                                                                                                                                                        1]))))
    text(x = ch_goodrate, y = ivout$ivtable[1:x_upper, 9], 
         label = round(ivout$ivtable[1:x_upper, 9] * 100, 
                       1), pos = 3, cex = 1)
    abline(h = 0)
    mtext(sub, 3)
  }
  else if (option == "badrate") {
    x_upper = nrow(ivout$ivtable) - r
    y_upper = max(ivout$ivtable[1:x_upper, 10], na.rm = T) * 
      1.25
    ch_badrate = barplot(ivout$ivtable[1:x_upper, 10], names.arg = ivout$ivtable[1:x_upper, 
                                                                                 1], axes = F, main = "Bad Rate (%)", ylim = c(0, 
                                                                                                                               y_upper), col = gray.colors(length(unique(ivout$ivtable[1:x_upper, 
                                                                                                                                                                                       1]))))
    text(x = ch_badrate, y = ivout$ivtable[1:x_upper, 10], 
         label = round(ivout$ivtable[1:x_upper, 10] * 100, 
                       1), pos = 3, cex = 1)
    abline(h = 0)
    mtext(sub, 3)
  }
  else if (option == "WoE") {
    x_upper = nrow(ivout$ivtable) - r
    woe = ivout$ivtable[1:x_upper, 13]
    woe[woe == Inf] = 0
    y_upper = max(woe, na.rm = T) * 
      1.25
    y_lower = min(woe, na.rm = T) * 
      1.25
    ch_woe = barplot(woe, names.arg = ivout$ivtable[1:x_upper, 
                                                                             1], axes = F, main = "Weight of Evidence", ylim = c(y_lower, 
                                                                                                                                 y_upper), col = gray.colors(length(unique(ivout$ivtable[1:x_upper, 
                                                                                                                                                                                         1]))))
    text(x = ch_woe, y = ivout$ivtable[1:x_upper, 13], label = round(ivout$ivtable[1:x_upper, 
                                                                                   13], 2), pos = 3, cex = 1)
    abline(h = 0)
    mtext(sub, 3)
  }
  else {
    return("Options are dist, goodrate, badrate, or WoE")
  }
}