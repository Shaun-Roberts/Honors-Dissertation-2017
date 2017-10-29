#Degree Distribution Plots for Individual origional networks
#Min
degree.dist.1.min = function(ylimits = c(0,70), i, step_size = 10){
  data = c("sampson", "kapferer", "samplk", "faux.mesa.high", "faux.magnolia.high", "ecoli")
  lincol = c("#D7191C", "#FDAE61", "#ABD9E9", "#c351e2", "#c351e2", "#c351e2")
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
        ylab = "Degree", 
        main = paste("min degree.dist ", data[i]))
  #Plotted lines
  {
    min.fun = function(x){
      which(x == x[which(x != 0)[1]])[1]
    }
    # f.pos.rate 0
    {
      f.pos.rate = 0
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.dist.rec, min.fun)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
    }
    # f.pos.rate 0.01
    {f.pos.rate = 0.01
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.dist.rec, min.fun)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
    }
    # f.pos.rate 0.05
    {f.pos.rate = 0.05
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.dist.rec, min.fun)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.10
    {f.pos.rate = 0.10
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.dist.rec, min.fun)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.15
    {
      f.pos.rate = 0.15
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.dist.rec, min.fun)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.20
    {
      f.pos.rate = 0.20
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.dist.rec, min.fun)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
  }
  
  #Vertical dividing lines and box
  abline(v = 50, lwd = 2)
  box(lwd = 1)
}
degree.dist.1.min(ylimits = c(0,10), 1, step_size = 5)
degree.dist.1.min(ylimits = c(0,20), 2, step_size = 10)
degree.dist.1.min(ylimits = c(0,10), 3, step_size = 5)
degree.dist.1.min(ylimits = c(0,60), 4, step_size = 25)
degree.dist.1.min(ylimits = c(0,500), 5, step_size = 100)
degree.dist.1.min(ylimits = c(0,130), 6, step_size = 20)

#Median
degree.dist.1.med = function(ylimits = c(0,70), i, step_size = 10){
  data = c("sampson", "kapferer", "samplk", "faux.mesa.high", "faux.magnolia.high", "ecoli")
  lincol = c("#D7191C", "#FDAE61", "#ABD9E9", "#c351e2", "#c351e2", "#c351e2")
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
        ylab = "Degree", 
        main = paste("Median degree.dist.dist Centrality", data[i]))
  #Plotted lines
  {
    med.fun = function(x){
      which(cumsum(x) >= 0.5)[1]
    }
    # f.pos.rate 0
    {
      f.pos.rate = 0
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.dist.rec, med.fun)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
    }
    # f.pos.rate 0.01
    {f.pos.rate = 0.01
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.dist.rec, med.fun)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
    }
    # f.pos.rate 0.05
    {f.pos.rate = 0.05
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.dist.rec, med.fun)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.10
    {f.pos.rate = 0.10
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.dist.rec, med.fun)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.15
    {
      f.pos.rate = 0.15
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.dist.rec, med.fun)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.20
    {
      f.pos.rate = 0.20
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.dist.rec, med.fun)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
  }
  
  #Vertical dividing lines and box
  abline(v = 50, lwd = 2)
  box(lwd = 1)
}
degree.dist.1.med(ylimits = c(0,15), 1, step_size = 5)
degree.dist.1.med(ylimits = c(0,40), 2, step_size = 10)
degree.dist.1.med(ylimits = c(0,15), 3, step_size = 5)
degree.dist.1.med(ylimits = c(0,100), 4, step_size = 25)
degree.dist.1.med(ylimits = c(0,650), 5, step_size = 100)
degree.dist.1.med(ylimits = c(0,180), 6, step_size = 20)

#Max
degree.dist.1.max = function(ylimits = c(0,70), i, step_size = 10){
  data = c("sampson", "kapferer", "samplk", "faux.mesa.high", "faux.magnolia.high", "ecoli")
  lincol = c("#D7191C", "#FDAE61", "#ABD9E9", "#c351e2", "#c351e2", "#c351e2")
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
        ylab = "Degree", 
        main = paste("Max degree.dist.dist Centrality", data[i]))
  #Plotted lines
  {
    max.fun = function(x){
      length(x)
    }
    # f.pos.rate 0
    {
      f.pos.rate = 0
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.dist.rec, max.fun)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
    }
    # f.pos.rate 0.01
    {f.pos.rate = 0.01
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.dist.rec, max.fun)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
    }
    # f.pos.rate 0.05
    {f.pos.rate = 0.05
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.dist.rec, max.fun)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.10
    {f.pos.rate = 0.10
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.dist.rec, max.fun)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.15
    {
      f.pos.rate = 0.15
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.dist.rec, max.fun)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.20
    {
      f.pos.rate = 0.20
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.dist.rec, max.fun)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
  }
  
  #Vertical dividing lines and box
  abline(v = 50, lwd = 2)
  box(lwd = 1)
}
#30

degree.dist.1.max(ylimits = c(0,25), 1, step_size = 5)
degree.dist.1.max(ylimits = c(0,80), 2, step_size = 10)
degree.dist.1.max(ylimits = c(0,20), 3, step_size = 5)
degree.dist.1.max(ylimits = c(0,130), 4, step_size = 25)
degree.dist.1.max(ylimits = c(0,800), 5, step_size = 100)
degree.dist.1.max(ylimits = c(0,250), 6, step_size = 20)
