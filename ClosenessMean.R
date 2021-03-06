closeness.Mean = function(ylimits = c(0,15), size = 30, step_size = 5){
  data = c("sampson", "kapferer", "samplk", "faux.mesa.high")
  lincol = c("#D7191C", "#FDAE61", "#ABD9E9", "#c351e2")
  linewidth = 2
  
  
  ydiffs = diff(ylimits)
  yupper = ylimits[2]
  ylower = ylimits[1]
  
  
  plot.new()
  plot.window(xlim = c(0,100), ylim = c(0,3*ydiffs), xaxs = "i", yaxs = "i")
  #Axes
  {
    axis(1,
         at = seq(0,100, 10),
         labels = c(seq(0, 40, 10), seq(0, 50, 10)),
         las = 1
    )
    axis(2,
         at = seq(0,ydiffs, step_size),
         labels = seq(ylower, yupper, step_size),
         las = 1
    )
    axis(2,
         at = seq(0,ydiffs, step_size) + 2*ydiffs,
         labels = seq(ylower, yupper, step_size),
         las = 1
         
    )
    axis(4,
         at = seq(0,ydiffs, step_size) + ydiffs,
         labels = seq(ylower, yupper, step_size),
         las = 1
    )
  }
  # Horizontal dividing lines:
  abline(h = c(ydiffs, 2*ydiffs), lwd = 1)
  #Titles
  title(xlab = "False Negative Rate", 
        ylab = "Mean", 
        main = paste("Mean Closeness Centrality\nNetwork Size (", size, ")", sep = ""))
  #Plotted lines
  {
    # f.pos.rate 0
    {
      f.pos.rate = 0
      for(i in 1:4){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50, y = unlist(lapply(record$closeness.rec, mean)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.01
    {f.pos.rate = 0.01
      for(i in 1:4){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50 + 50, y = unlist(lapply(record$closeness.rec, mean)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.05
    {f.pos.rate = 0.05
      for(i in 1:4){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50, y = unlist(lapply(record$closeness.rec, mean)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.10
    {f.pos.rate = 0.10
      for(i in 1:4){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50 + 50, y = unlist(lapply(record$closeness.rec, mean)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.15
    {
      f.pos.rate = 0.15
      for(i in 1:4){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50, y = unlist(lapply(record$closeness.rec, mean)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.20
    {
      f.pos.rate = 0.20
      for(i in 1:4){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50 + 50, y = unlist(lapply(record$closeness.rec, mean)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
  }
  
  #Vertical dividing lines and box
  abline(v = 50, lwd = 2)
  box(lwd = 1)
}
#30
pdf("closeness_Mean_30.pdf")
closeness.Mean(ylimits = c(0,0.03), size = 30, step_size = 0.01)
dev.off()
#100
pdf("closeness_Mean_100.pdf")
closeness.Mean(ylimits = c(0,0.008), size = 100, step_size = 0.002)
dev.off()
#250
pdf("closeness_Mean_250.pdf")
closeness.Mean(ylimits = c(0,0.003), size = 250, step_size = 0.001)
dev.off()
#500
pdf("closeness_Mean_500.pdf")
closeness.Mean(ylimits = c(0,0.0015), size = 500, step_size = 0.0005)
dev.off()
#1000
pdf("closeness_Mean_1000.pdf")
closeness.Mean(ylimits = c(0,0.0006), size = 1000, step_size = 0.0001)
dev.off()
