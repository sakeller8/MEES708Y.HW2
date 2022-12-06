ano <- read.csv("D:/Fall 2022/MEES708/Homeworks/708Y Homework 2/anoxia.csv")

library(tidyverse)
library(lubridate)
library(ggplot2)

#Question 1 ----

JanAprTNLoad <- ano$JanAprTNLoad
JanMayTNLoad <- ano$JanMayTNLoad

JA1 <- (-.98 + (6.903 * 10^(-6) * JanAprTNLoad))
JM1 <- (-.217 + (5.596 * 10^(-6) * JanMayTNLoad))

JA_df <- data.frame(ano, JA1)
JM_df <- data.frame(ano, JM1)

JA_df2 <- JA_df %>%
    mutate(Date = Year)

JM_df2 <- JM_df %>%
    mutate(Date = Year)

#Question 2 ----
JA_plot <- data.frame(JA_df2$Date, JA_df$EarlySummerAnoxicVol, JA1)
JM_plot <- data.frame(JM_df2$Date, JM_df$LateSummerAnoxicVol, JM1)

plota <- ggplot() +
    geom_line(data=JA_plot, aes(x=JA_df2.Date, y=JA_df.EarlySummerAnoxicVol, group=1),
              size = 1) +
    geom_line(data=JA_plot, aes(x=JA_df2.Date, y=JA1, group=1), 
              size = 1, col = "#0099FF") +
    geom_point(data=JA_plot, aes(x=JA_df2.Date, y=JA1, group=1),
               size = 3, col = "#0099FF") +
    labs(x = NULL, y = expression(Early~summer~anoxic~volume~(km^3))) # for the y axis label

plot1 <- plota + annotate("text", x=1988, y=2.4, label="Fitted", col = "#0099FF") +
    annotate("text", x=1990, y=0, label= "Observed")

plot1

plotb <- ggplot() +
    geom_line(data=JM_plot, aes(x=JM_df2.Date, y=JM_df.LateSummerAnoxicVol, group=1), 
              size = 1) +
    geom_line(data=JM_plot, aes(x=JM_df2.Date, y=JM1, group=1), 
              size = 1, col = "#0099FF") +
    geom_point(data=JM_plot, aes(x=JM_df2.Date, y=JM1, group=1),
               size = 1.5, col = "#0099FF") +
    labs(x = "Year", y = expression(Late~summer~anoxic~volume~(km^3)))

plot2 <- plotb + annotate("text", x=1991, y=1, label="Fitted", col = "#0099FF") +
    annotate("text", x=1990, y=.6, label= "Observed")

plot2

#part 2

#observed - fitted

JA_plot$residuals = JA_plot$JA_df.EarlySummerAnoxicVol - JA_plot$JA1
JM_plot$residuals = JM_plot$JM_df.LateSummerAnoxicVol - JM_plot$JM1

plot1.res <- ggplot() +
    geom_line(data=JA_plot, aes(x=JA_df2.Date, y=JA_plot$residuals, group=1),
              size = 1) +
    geom_point(data=JA_plot, aes(x=JA_df2.Date, y=JA_plot$residuals, group=1),
               size=1.5) +
    labs(x = NULL, y = expression(Residuals~(km^3)))
    
 
plot1.res

plot2.res <- ggplot() +
    geom_line(data=JM_plot, aes(x=JM_df2.Date, y=JM_plot$residuals, group=1),
              size = 1) +
    geom_point(data=JM_plot, aes(x=JM_df2.Date, y=JM_plot$residuals, group=1),
               size=1.5) +
    labs(x = "Year", y = expression(Residuals~(km^3)))

plot2.res

JA_ts <- as.ts(JA_plot)
JA_ts[,"residuals"]
library(bayesforecast)
plot1.acf <- ggacf(na.omit(JA_ts[,"residuals"])) +
    labs(x = NULL, y = "ACF Residuals")

plot1.acf

JM_ts <- as.ts(JM_plot)
JM_ts[,"residuals"]
plot2.acf <- ggacf(na.omit(JM_ts[,"residuals"])) +
    labs(x = "Lag (years)", y = "ACF Residuals")

plot2.acf

#plots all together

cowplot::plot_grid(plot1, plot1.res, plot1.acf, plot2, plot2.res, plot2.acf,
                   labels = c("A", "B", "c", "D", "E", "F"))

#Question 3 ----

library(rpart)
library(rpart.plot)

JA_plot2 <- na.omit(JA_plot)
JM_plot2 <- na.omit(JM_plot)

#classification

m1 = rpart(residuals ~ JA_df2.Date, 
           data = JA_plot2)
rpart.plot(m1)

m2 = rpart(residuals ~ JM_df2.Date, 
           method = "anova",
           data = JM_plot2)
rpart.plot(m2)

#Question 4 ----

library(funtimes)

#Early Summer

JA_plot2a <- JA_plot2[4:29, ]

set.seed(1)
xa <- JA_plot2a$JA_df.EarlySummerAnoxicVol
ya <- JA_plot2a$JA1

ccf(xa, ya) # default CCF with parametric confidence band
tmp <- ccf_boot(xa, ya) # CCF with bootstrap
final.a <- tmp$rP; tmp$rS 
early.summer.value <- final.a[1]
early.summer.value

#Late summer

JM_plot2b <- JM_plot2[17:25, ]

#set.seed(2)
xb <- JM_plot2b$JM_df.LateSummerAnoxicVol
yb <- JM_plot2b$JM1

ccf(xb, yb) # default CCF with parametric confidence band
tmpb <- ccf_boot(xb, yb) # CCF with bootstrap
#receiving an error and cannot complete code
final.b <- tmpb$rP; tmpb$rS 
late.summer.value <- final.b[1]
late.summer.value

#END ASSIGNMENT