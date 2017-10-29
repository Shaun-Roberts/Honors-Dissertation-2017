#Degree Centrality plots for the individual origional networks
#Mean:
degree.1.Mean = function(ylimits = c(0,70), i, step_size = 10){
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
        ylab = "Mean", 
        main = paste("Mean degree Centrality", data[i]))
  #Plotted lines
  {
    # f.pos.rate 0
    {
      f.pos.rate = 0
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.rec, mean)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
    }
    # f.pos.rate 0.01
    {f.pos.rate = 0.01
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.rec, mean)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
    }
    # f.pos.rate 0.05
    {f.pos.rate = 0.05
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.rec, mean)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.10
    {f.pos.rate = 0.10
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.rec, mean)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.15
    {
      f.pos.rate = 0.15
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.rec, mean)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.20
    {
      f.pos.rate = 0.20
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.rec, mean)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
  }
  
  #Vertical dividing lines and box
  abline(v = 50, lwd = 2)
  box(lwd = 1)
}

degree.1.Mean(ylimits = c(0,15), 1, step_size = 2)
degree.1.Mean(ylimits = c(0,18), 2, step_size = 5)
degree.1.Mean(ylimits = c(0,6), 3, step_size = 2)
degree.1.Mean(ylimits = c(0,50), 4, step_size = 10)
degree.1.Mean(ylimits = c(0,400), 5, step_size = 100)
degree.1.Mean(ylimits = c(0,100), 6, step_size = 20)

#Standard Deviation
degree.1.SD = function(ylimits = c(0,70), i, step_size = 10){
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
        ylab = "SD", 
        main = paste("SD degree Centrality", data[i]))
  #Plotted lines
  {
    # f.pos.rate 0
    {
      f.pos.rate = 0
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.rec, sd)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
    }
    # f.pos.rate 0.01
    {f.pos.rate = 0.01
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.rec, sd)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
    }
    # f.pos.rate 0.05
    {f.pos.rate = 0.05
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.rec, sd)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.10
    {f.pos.rate = 0.10
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.rec, sd)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.15
    {
      f.pos.rate = 0.15
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$degree.rec, sd)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.20
    {
      f.pos.rate = 0.20
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$degree.rec, sd)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
  }
  
  #Vertical dividing lines and box
  abline(v = 50, lwd = 2)
  box(lwd = 1)
}
#30

degree.1.SD(ylimits = c(0,5), 1, step_size = 1)
degree.1.SD(ylimits = c(0,6), 2, step_size = 1)
degree.1.SD(ylimits = c(0,2), 3, step_size = 1)
degree.1.SD(ylimits = c(0,10), 4, step_size = 1)
degree.1.SD(ylimits = c(0,20), 5, step_size = 5)
degree.1.SD(ylimits = c(0,10), 6, step_size = 1)
