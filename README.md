Probability Density Plot
------------------------

### t distribution

[Link to Shiny application](https://loankimrobinson.shinyapps.io/Probability_Density_Plot/)

``` r
library(ggplot2)
library(RColorBrewer)
# t distribution
x <- seq(-4, 4, length.out = 200)
df <- c(1, 3, 7)

# Create a data is easy to work
dist_data  <- data.frame(df = double(),x = double(),t.density = double())

for(i in df){
  t.density <- dt(x, df = i)
  data <- data.frame(df = i,x = x, t.density  = t.density)
  dist_data <- rbind(dist_data ,data)
  norm <- data.frame(df = "N(0,1)", x = x,t.density = dnorm(x, 0, 1))
  dist_data <- rbind(dist_data, norm)
  
}

# Create data for geom_segment, geom_text
# x <- c(0.5,0.68,0.8,0.78)
# 0.68-0.5 = 0.18, so we add exactly 18 for xend, it starts 1.35, next 1.35 + 0.18 = 0.53
# xend <- c(1.35,1.53,1.71,1.89)

color_dt <- data.frame(x = c(0.5,0.68,0.8,0.78),
                       y = c(0.35,0.3,0.25,0.2),
                       xend = c(1.35,1.53,1.71,1.89),
                       yend = c(0.35,0.3,0.25,0.2),
                       color = c("N(0,1)","7","3","1"),
                       text = c("N(0,1)","t(7)","t(3)","t(1)"))

ggplot(dist_data, aes(x = x, y = t.density, color = df)) + 
  geom_line(aes(linetype=df,color=df),size = 1.3) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend,colour = color),
               data = color_dt,size = 1,arrow = arrow(length = unit(0.02, "npc"),ends = "first"))+# ends = c("first","last","both")
  geom_text(aes(x = xend+0.2, y = y+0.005,colour = color, label = text),data = color_dt,size = 5)+
  annotate("segment",x = -4, xend = 4, y = -0.01, yend = -0.01,color = "#6baed6", size=1)+
  scale_linetype_manual(values=c("solid","solid","solid", "dotted"))+
  scale_x_continuous(breaks = seq(-4,4,1))+
  ylab("Probability Density") +  labs(title= "t-Distribution ",subtitle = "vs Standard Normal Distribution") + xlab("")+
  theme_minimal()+
  theme(axis.text.x=element_text(color = "#08306b",size = 12,vjust=10),
        axis.text.y=element_text(color = "#08306b",size = 12),
        axis.ticks.y=element_blank(),
        axis.title.y=element_text(color = "#08306b",size = 18, face = "bold.italic"),
        plot.title = element_text(hjust = 0.5,color = "#08306b", size = 20, face = "bold.italic"),
        plot.subtitle = element_text(hjust = 0.5,color = "#08306b", size = 16, face = "bold.italic"),
        legend.position="none",plot.margin = unit(c(1,1,0.5,1),"lines"))
```

![](README_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
# # View a single RColorBrewer palette by specifying its name
# display.brewer.pal(n = 6, name = 'RdBu')
# # Hexadecimal color specification 
# dput(brewer.pal(n = 6, name = "RdBu"))
```

![plot](t%20distribution.jpeg)

### Chisquare Distribution

``` r
# Chisquare distribution
x <- seq(0, 10, length.out = 200)
df <- c(1, 2, 3, 5, 10)

# # View a single RColorBrewer palette by specifying its name
# display.brewer.pal(n = 6, name = 'RdBu')
# # Hexadecimal color specification 
# dput(brewer.pal(n = 6, name = "RdBu"))
dput(brewer.pal(n = 5,name = 'Accent'))
```

    ## c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0")

``` r
# Create a data is easy to work
dist_data  <- data.frame(df = double(),x = double(),density = double())

for(i in df){
  density <- dchisq(x, df = i)
  data <- data.frame(df = i,x = x, density  = density)
  dist_data <- rbind(dist_data ,data)
}

dist_data$df <- factor(dist_data$df)

color_dt <- data.frame(x = c(1.1,1.8,3,5,8),
                       y = c(0.42,0.27,0.2,0.16,0.12),
                       color = c("1","2","3","5","10"),
                       text = c('df = 1','df = 2','df = 3','df = 5','df = 10'))



ggplot(dist_data, aes(x = x, y = density, color = df)) + 
  geom_line(aes(colour=df),size = 1.1)+
  scale_y_continuous(limits = c(0, 0.5))+
  scale_x_continuous(breaks = seq(0,10,1))+
  scale_colour_manual(values = c("#b2182b", "#4393c3", "#f4a582","#5aae61", "#8c510a"))+
  annotate("text",x = 4.5, y =0.27,color = "gray", size=150,alpha = .3,label = expression(chi),parse = TRUE,family="Times New Roman", fontface="italic")+
  annotate("text",x = 6.7, y =0.45,color = "gray", size=60,alpha = .3,label = "2",parse=TRUE,family="Times New Roman", fontface="italic")+
  geom_text(aes(x = x, y = y,colour = color, label = text,family="Times New Roman", fontface="bold.italic"),data = color_dt, size = 10)+
  
  ylab("Probability Density") + xlab("")+
  theme_minimal()+
                       theme(axis.text.x=element_text(color = "#08306b",size = 12,vjust=10),
                             axis.text.y=element_text(color = "#08306b",size = 12),
                             axis.title.y=element_text(color = "#08306b",size = 18, face = "bold.italic"),
                             plot.title = element_text(hjust = 0.5,color = "#08306b", size = 20, face = "bold.italic"),
                             #plot.subtitle = element_text(hjust = 0.5,color = "#08306b", size = 60, face = "bold.italic",family="Times New Roman"),
                             legend.text=element_text(size=12),
                             #legend.key.width = unit(1.7,"cm"),legend.title.align = 0.5,
                             legend.position="none")
```


![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
                             #guides(fill=guide_colorbar(title.vjust=1),label.hjust = 1)

# windowsFonts()
# install.packages("extrafont")
# library(extrafont)
# font_import()
# loadfonts(device = "win")
```

![plot](chisquare%20distribution.jpeg)

### F Distribution

``` r
# F distribution
x <- seq(0, 4, length.out = 200)
df1 <- c(20, 70, 75, 3, 2)
df2 <- c(20, 5, 1, 3, 1)

# # View a single RColorBrewer palette by specifying its name
# display.brewer.pal(n = 6, name = 'RdBu')
# # Hexadecimal color specification 
# dput(brewer.pal(n = 6, name = "RdBu"))
dput(brewer.pal(n = 7,name = 'Accent'))
```

    ## c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", 
    ## "#BF5B17")

``` r
# Create a data is easy to work
dist_data  <- data.frame(df1 = double(),df2 = double(),x = double(),density = double())

for (i in seq_along(df1)) {
    density <- df(x, df1 = df1[i], df2 = df2[i]) 
    data <- data.frame(df1 = df1[i], df2= df2[i],x = x, density  = density)
    dist_data <- rbind(dist_data ,data)
  }

dist_data$df <- paste(dist_data$df1," ",dist_data$df2, sep="")

color_dt <- data.frame(x = c(0.15,1.15, 0.36, 0.95, 1),
                       y = c(0.9, 0.9, 0.68, 0.7, 0.3),
                       color = c("2 1","20 20","3 3","70 5","75 1"),
                       text = c('F(2,1)','F(20,20)','F(3,3)','F(70,5)','F(75,1)'))


ggplot(dist_data, aes(x = x, y = density, color = df)) + 
  geom_line(aes(colour=df),size = 1.1)+
  scale_y_continuous(breaks = seq(0,1.2,0.2),limits = c(0, 1.2))+
  scale_x_continuous(breaks = seq(0,4,0.5),limits = c(0,2.5))+
  scale_colour_manual(values = c("#b2182b", "#4393c3", "#f4a582","#5aae61", "#8c510a"))+
  geom_text(aes(x = x, y = y,colour = color, 
                label = text,family="Times New Roman", fontface="bold.italic"),
                data = color_dt, size = 7)+
  annotate("text",x = 1.2, y =1.195,color = "gray", size=13,alpha = .5,label = "F Distribution",family="Times New Roman", fontface="bold.italic")+
  annotate("text",x = 1.19, y =1.2,color = "#08306b", size=13,alpha = .9,label = "F Distribution",family="Times New Roman", fontface="bold.italic")+
  ylab("Probability Density") + xlab("")+ #labs(title= "F Distribution") + 
  theme_minimal()+
  theme(axis.text.x=element_text(color = "#08306b",size = 12,vjust=10),
        axis.text.y=element_text(color = "#08306b",size = 12),
        #axis.ticks.x=element_line(color = "#08306b"),
        axis.title.y=element_text(color = "#08306b",size = 18, face = "bold.italic"),
        plot.title = element_text(hjust = 0.5,color = "#08306b", size = 35, face = "bold.italic",
                                  margin=margin(b = -50, unit = "pt")),
        #plot.subtitle = element_text(hjust = 0.5,color = "#08306b", size = 16, face = "bold.italic"),
        text = element_text(family="Times New Roman"),
        legend.position="none",
        #legend.text=element_text(size=12),
        #legend.key.width = unit(1.7,"cm"),legend.title.align = 0.5,
        plot.margin = unit(c(2.5,1.5,1.5,1.5), "lines"),
        legend.title = element_text(color = "#08306b", size = 12, face = "italic"))
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
#guides(fill=guide_colorbar(title.vjust=1),label.hjust = 1)
```

![plot](F%20Distribution.jpeg)
