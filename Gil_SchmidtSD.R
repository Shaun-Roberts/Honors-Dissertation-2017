Gil_Schmidt.SD = function(ylimits = c(0,0.5), size = 30, step_size = 0.1){
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
    axis(3,
         at = 50,
         labels = "",
         lwd = 2
    )
    axis(1,
         at = 50,
         labels = "",
         lwd = 2
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
        ylab = "Standard Deviation", 
        main = paste("Gil-Schmidt Power Centrality Standard Deviations\nNetwork Size (", size, ")", sep = ""))
  #Plotted lines
  {
    # f.pos.rate 0
    {
      f.pos.rate = 0
      for(i in 1:4){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50, y = unlist(lapply(record$gil.sch.rec, sd)) + 2*ydiffs  - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.01
    {f.pos.rate = 0.01
      for(i in 1:4){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50 + 50, y = unlist(lapply(record$gil.sch.rec, sd)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.05
    {f.pos.rate = 0.05
      for(i in 1:4){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50, y = unlist(lapply(record$gil.sch.rec, sd)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.10
    {f.pos.rate = 0.10
      for(i in 1:4){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50 + 50, y = unlist(lapply(record$gil.sch.rec, sd)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.15
    {
      f.pos.rate = 0.15
      for(i in 1:4){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50, y = unlist(lapply(record$gil.sch.rec, sd)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.20
    {
      f.pos.rate = 0.20
      for(i in 1:4){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50 + 50, y = unlist(lapply(record$gil.sch.rec, sd)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
  }
  
  #Vertical dividing lines and box
  abline(v = 50, lwd = 2)
  box(lwd = 1)
}

Gil_Schmidt.SD.nofaux = function(ylimits = c(0,0.5), size = 30, step_size = 0.1){
  data = c("sampson", "kapferer", "samplk")
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
    axis(3,
         at = 50,
         labels = "",
         lwd = 2
    )
    axis(1,
         at = 50,
         labels = "",
         lwd = 2
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
        ylab = "Standard Deviation", 
        main = paste("Gil-Schmidt Power Centrality Standard Deviations\nNetwork Size (", size, ")", sep = ""))
  #Plotted lines
  {
    # f.pos.rate 0
    {
      f.pos.rate = 0
      for(i in 1:3){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50, y = unlist(lapply(record$gil.sch.rec, sd)) + 2*ydiffs  - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.01
    {f.pos.rate = 0.01
      for(i in 1:3){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50 + 50, y = unlist(lapply(record$gil.sch.rec, sd)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.05
    {f.pos.rate = 0.05
      for(i in 1:3){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50, y = unlist(lapply(record$gil.sch.rec, sd)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.10
    {f.pos.rate = 0.10
      for(i in 1:3){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50 + 50, y = unlist(lapply(record$gil.sch.rec, sd)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.15
    {
      f.pos.rate = 0.15
      for(i in 1:3){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50, y = unlist(lapply(record$gil.sch.rec, sd)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
    # f.pos.rate 0.20
    {
      f.pos.rate = 0.20
      for(i in 1:3){
        load(paste(data[i], size, f.pos.rate,0.01, sep = "_"))
        lines(x = 0:50 + 50, y = unlist(lapply(record$gil.sch.rec, sd)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      }
    }
  }
  
  #Vertical dividing lines and box
  abline(v = 50, lwd = 2)
  box(lwd = 1)
}

#30
pdf("Gil_Schmidt_SD_30.pdf")
Gil_Schmidt.SD(ylimits = c(0,0.55), size = 30, step_size = 0.1)
dev.off()
#100
pdf("Gil_Schmidt_SD_100_zoomy.pdf")
Gil_Schmidt.SD(ylimits = c(0,0.55), size = 100, step_size = 0.1)
dev.off()
pdf("Gil_Schmidt_SD_100_no_fuax.pdf")
Gil_Schmidt.SD.nofaux(ylimits = c(0,0.05), size = 100, step_size = 0.01)
dev.off()
#250
pdf("Gil_Schmidt_SD_250_no_faux.pdf")
Gil_Schmidt.SD.nofaux(ylimits = c(0,0.04), size = 250, step_size = 0.01)
dev.off()
#500
pdf("Gil_Schmidt_SD_500.pdf")
Gil_Schmidt.SD(ylimits = c(0,0.3), size = 500, step_size = 0.1)
dev.off()
#1000
pdf("Gil_Schmidt_SD_1000.pdf")
Gil_Schmidt.SD(ylimits = c(0,0.4), size = 1000, step_size = 2)
dev.off()