filename <- list.files(path = "./stat", pattern ="^stat.*.csv", all.files = FALSE,
           full.names = TRUE, recursive = FALSE)

fi <- as.data.frame(filename)

fi["conc"] <- NA

fi$conc <-  sub("./stat/stat([0-9]+).csv", "\\1", fi$filename ,perl=TRUE)

for (i in 1:length(fi$filename)) {

    data <- read.table(toString(fi$filename[i]), header=TRUE,
         sep=",", col.names=c("id", "time", "latency", "concurrency"))

    concurrency <- fi$conc[i]

    rps <- max(data$time)/length(data$time)

    quant <- quantile(data$latency, c(0.99))

    baselatency <- subset(data, latency <= quant[1], select=c(latency))

    slowlatency <- subset(data, latency > quant[1], select=c(latency))

    png(paste("./stat/plot",toString(concurrency),".png", sep = ""), width = 600, height = 800, res=120)

    par(mfrow=c(3,1))



    plot(
        data$time,
        data$latency,
        type="l",
        main="Latency in time",
        ylab="Latency, ms",
        xlab="Time from start, s"
        )


    ##par(mar = c(0, 0,2, 0))
    par(xpd=NA)
    hist(
        baselatency$latency,
        main = "Histogram of remote actor latency, 99% of requests",
        xlab = "Latency, ms",
        yaxt = "n",
        sub = paste("requests latency <= ",toString(quant[1])," ms", sep = ""),
        breaks = 9,
        labels = TRUE,
        freq = TRUE
        )
    axis(
        side = 2,
        at=axTicks(2),
        labels=format(axTicks(2),scientific = FALSE)
        )

    leg1 <- paste("mean = ", round(mean(baselatency$latency), digits = 4))
    leg2 <- paste("sd = ", round(sd(baselatency$latency),digits = 4))
    count <- paste("count = ", length(baselatency$latency))
    legend(x = "topright", c(leg1,leg2,count), bty = "n")


    hist(
        slowlatency$latency,
        main="Histogram of remote actor latency, 1% of requests",
        xlab="Latency, ms",
        yaxt="n",
        sub=paste("requests latency > ",toString(quant[1])," ms", sep = ""),
        labels=TRUE,
        freq = TRUE
        )

    axis(
        side=2,
        at=axTicks(2),
        labels=format(axTicks(2),scientific=FALSE)
        )

    count <- paste("count = ", length(slowlatency$latency))
    legend(x = "topright", c(count), bty = "n")


    dev.off()

    summary(data)
    summary(baselatency)

}

alldata <- data.frame(id= integer(0), time =numeric(0), latency=numeric(0), concurrency=integer(0))

for (i in 1:length(fi$filename)) {

    data <- read.table(toString(fi$filename[i]), header=TRUE,
         sep=",", col.names=c("id", "time", "latency", "concurrency"))

    alldata <- rbind(alldata,  data)
}

png(paste("./stat/plot.png"), width = 600, height = 600, res = 120)

boxplot(latency~concurrency,data=alldata, main="Latency versus Concurrency",
  	 xlab="Concurrency, number of parallel requests", ylab="Latency, ms", log = "y", outline=FALSE)

dev.off()