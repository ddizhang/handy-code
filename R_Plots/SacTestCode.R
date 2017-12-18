
gg.dat = rbind(           
  data.frame(
    Group = rep("Test", length(min(dat$week):max(dat$week))),
    week = min(dat$week):max(dat$week),
    m = tapply(dat[dat$sample1 == 1,]$multiplier, dat[dat$sample1 == 1,]$week, mean, trim = 0.01), 
    c = tapply(dat[dat$sample1 == 1,]$sold, dat[dat$sample1 == 1,]$week, mean, trim = 0.01), 
    count = unname(table(dat[dat$sample1 == 1,]$week)) ),
  data.frame(
    Group = rep("Control", length(min(dat$week):max(dat$week))),
    week = min(dat$week):max(dat$week),
    m = tapply(dat[dat$sample1 == 0,]$multiplier, dat[dat$sample1 == 0,]$week, mean, trim = 0.01), 
    c = tapply(dat[dat$sample1 == 0,]$sold, dat[dat$sample1 == 0,]$week, mean, trim = 0.01),
    count = unname(table(dat[dat$sample1 == 1,]$week)))
  
)


dat$Group[dat$sample1 == 1] = "Test"
dat$Group[dat$sample1 == 0] = "Control"
dat$Group = as.factor(dat$Group)

dat.b4 = dat %>% 
  filter(sample1 == 1) %>%
  filter(afterTest == 0) %>%
  #drop the top percentage (outliers)
  filter(multiplier < quantile(multiplier, 0.995), multiplier > quantile(multiplier, 0.005)) %>%
  mutate(priceCut = binBreak(multiplier, type = "nBins", nBins = 50), 
         priceCut = as.numeric(as.character(priceCut)))
obscount = nrow(dat)

#price level and corresponding conversion rate
conv = tapply(dat.b4$sold, dat.b4$priceCut, mean)
pp = as.numeric(names(conv))
m1 = lm(conv ~ pp, weight = table(dat.b4$priceCut))

dat.af = dat %>% 
  filter(sample1 == 1) %>%
  filter(afterTest == 1) %>%
  #drop the top percentage (outliers)
  filter(multiplier < quantile(multiplier, 0.995), multiplier > quantile(multiplier, 0.005)) %>%
  mutate(priceCut = binBreak(multiplier, type = "nBins", nBins = 35), 
         priceCut = as.numeric(as.character(priceCut)))
obscount = nrow(dat)

#price level and corresponding conversion rate
conv2 = tapply(dat.af$sold, dat.af$priceCut, mean)
pp2 = as.numeric(names(conv2))
m2 = lm(conv2 ~ pp2, weight = table(dat.af$priceCut))


gg.dat2 = rbind(
  data.frame(
    Group = rep("Before Test", 50),
    c = conv,
    m = pp
  ),
  data.frame(
    Group = rep("After Test", 35),
    c = conv2,
    m = pp2
  ))


gg.dat3 = rbind(
  data.frame(
    Group = rep("Before Test", 50),
    m = seq(0.55, 1.5, length.out = 50),
    pred = predict(m1, data.frame(pp = seq(0.55, 1.5, length.out = 50))),
    pred.low = predict(m1, data.frame(pp = seq(0.55, 1.5, length.out = 50)), interval = "conf", level = 0.99995)[,2],
    pred.up = predict(m1, data.frame(pp = seq(0.55, 1.5, length.out = 50)), interval = "conf", level = 0.99995)[,3]
  ),
  data.frame(
    Group = rep("After Test", 35),
    m = seq(0.55, 1.5, length.out = 35),
    pred = predict(m2, data.frame(pp2 = seq(0.55, 1.5, length.out = 35))),
    pred.low = predict(m1, data.frame(pp = seq(0.55, 1.5, length.out = 35)), interval = "conf", level = 0.99995)[,2],
    pred.up = predict(m1, data.frame(pp = seq(0.55, 1.5, length.out = 35)), interval = "conf", level = 0.99995)[,3]
  ))




#-------------------------------------------------------------------------

col.t = rgb(238, 46, 36, max = 255)   #red
col.c =  rgb(0, 76, 153, max = 255)
col.c = "black"
col.v = "darkgrey"




#-------------------------------------------------------------------------



p1 <- ggplot(gg.dat, aes(week, m, colour = Group)) + 
  geom_vline(aes(xintercept = as.numeric(strftime('2016-9-5', format = "%W"))-1), 
             color = col.v, linetype = "longdash", size = 1.5) + 
  geom_line(aes(group = Group), size=1.2) + 
  xlab("Fiscal Week") + ylab("Average Price Level") +
  coord_cartesian(ylim = c(0.8, 1.1)) +
  ggtitle("Change in Effective Average Price Level") +
  scale_color_manual(values = c(col.c, col.t)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))


p2 <- ggplot(gg.dat, aes(week, c, colour = Group)) + 
  geom_vline(aes(xintercept = as.numeric(strftime('2016-9-5', format = "%W"))-1), 
             color = col.v, linetype = "longdash", size = 1.5) +
  geom_line(aes(group = Group), size=1.2) + 
  xlab("Fiscal Week") + ylab("Conversion %") +
  coord_cartesian(ylim = c(0.3, 0.8)) +
  ggtitle("Change in Conversion") +
  scale_color_manual(values = c(col.c, col.t)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))


p3 <- ggplot(dat, aes(week)) + geom_bar(aes(fill = Group)) +
  geom_vline(aes(xintercept = as.numeric(strftime('2016-9-5', format = "%W"))-1), color = "darkgrey", linetype = "longdash") +
  ggtitle("Count of Quotes") +
  xlab("Week") + ylab("") +
  scale_fill_manual(values = c(col.c, col.t)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))



p4 <- ggplot(gg.dat2, aes(m)) +
  geom_point(aes(y = c, colour = Group)) + 
  geom_line(data = gg.dat3, aes(m, pred, colour = Group), size=1.1) + 
  geom_ribbon(data = subset(gg.dat3, Group = "beforeTest"), aes(ymin = pred.low, ymax = pred.up), alpha = 0.15) +
  scale_color_manual(values = c(col.c, col.t)) +
  xlab("Average Price Level") + ylab("Conversion %") + ggtitle("Price Response Function") + 
  coord_cartesian(xlim = c(0.55, 1.5)) + 
  theme_bw() +
  theme(legend.position="top") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))


p5 <- ggplot(dat.b4, aes(x = priceCut, fill = afterTest)) + geom_bar(fill = col.c) + 
  coord_cartesian(xlim = c(0.55, 1.5)) + 
  xlab("Average Price Level") + ylab("") + ggtitle("Count of Quotes (Before Test)") + 
  theme_bw() +
  #theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))

p6 <- ggplot(dat.af, aes(x = priceCut, fill = afterTest)) + geom_bar(fill = col.t) + 
  coord_cartesian(xlim = c(0.55, 1.5)) + 
  xlab("Average Price Level") + ylab("") + ggtitle("Count of Quotes (After Test)") + 
  theme_bw() +
  #theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5))


#-------------------------------------------------------------------

gg1 = grid.arrange(p1, p2, p3, ncol = 1)

p4 = ggplot_gtable(ggplot_build(p4))
p5 = ggplot_gtable(ggplot_build(p5))
p6 = ggplot_gtable(ggplot_build(p6))

maxWidth = unit.pmax(p4$widths[2:3], p5$widths[2:3], p6$widths[2:3])

p4$widths[2:3] <- maxWidth
p5$widths[2:3] <- maxWidth
p6$widths[2:3] <- maxWidth

gg2 = grid.arrange(grobs = list(p4, p5, p6), layout_matrix = matrix(c(1,1,1,2,3), nrow = 5))



ggsave(
  "sacTest2.png",
  gg2,
  dpi = 1000
)

