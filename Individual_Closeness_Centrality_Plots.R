#Closeness Centrality plots for the individual origional networks
#Mean:
closeness.1.Mean = function(ylimits = c(0,70), i, step_size = 10){
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
        main = paste("Mean Betweenness Centrality", data[i]))
  #Plotted lines
  {
    # f.pos.rate 0
    {
      f.pos.rate = 0
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$closeness.rec, mean)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
    }
    # f.pos.rate 0.01
    {f.pos.rate = 0.01
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$closeness.rec, mean)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
    }
    # f.pos.rate 0.05
    {f.pos.rate = 0.05
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$closeness.rec, mean)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.10
    {f.pos.rate = 0.10
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$closeness.rec, mean)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.15
    {
      f.pos.rate = 0.15
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$closeness.rec, mean)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.20
    {
      f.pos.rate = 0.20
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$closeness.rec, mean)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
  }
  
  #Vertical dividing lines and box
  abline(v = 50, lwd = 2)
  box(lwd = 1)
}
closeness.1.Mean(ylimits = c(0,0.05), 1, step_size = 0.01)
closeness.1.Mean(ylimits = c(0,0.02), 2, step_size = 0.01)
closeness.1.Mean(ylimits = c(0,0.05), 3, step_size = 0.01)
closeness.1.Mean(ylimits = c(0,0.003), 4, step_size = 0.001)
closeness.1.Mean(ylimits = c(0,0.0005), 5, step_size = 0.0001)
closeness.1.Mean(ylimits = c(0,0.002), 6, step_size = 0.001)

#SD:
closeness.1.SD = function(ylimits = c(0,70), i, step_size = 10){
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
        main = paste("SD Betweenness Centrality", data[i]))
  #Plotted lines
  {
    # f.pos.rate 0
    {
      f.pos.rate = 0
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$closeness.rec, sd)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
    }
    # f.pos.rate 0.01
    {f.pos.rate = 0.01
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$closeness.rec, sd)) + 2*ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
    }
    # f.pos.rate 0.05
    {f.pos.rate = 0.05
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$closeness.rec, sd)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.10
    {f.pos.rate = 0.10
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$closeness.rec, sd)) + ydiffs - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.15
    {
      f.pos.rate = 0.15
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50, y = unlist(lapply(record$closeness.rec, sd)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
    # f.pos.rate 0.20
    {
      f.pos.rate = 0.20
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(x = 0:50 + 50, y = unlist(lapply(record$closeness.rec, sd)) - ylower, type = "l", col = lincol[i], lwd = linewidth)
      
    }
  }
  
  #Vertical dividing lines and box
  abline(v = 50, lwd = 2)
  box(lwd = 1)
}

closeness.1.SD(ylimits = c(0,0.005), 1, step_size = 0.001)
closeness.1.SD(ylimits = c(0,0.002), 2, step_size = 0.01)
closeness.1.SD(ylimits = c(0,0.005), 3, step_size = 0.01)
closeness.1.SD(ylimits = c(0,0.0003), 4, step_size = 0.001)
closeness.1.SD(ylimits = c(0,0.00002), 5, step_size = 0.000001)
closeness.1.SD(ylimits = c(0,0.0001), 6, step_size = 0.00002)
