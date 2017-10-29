#Gil-Schmidt Centrality plots for the individual origional networks
#Mean
gil.sch.1.Mean = function(ylimits = c(0, 70),
                          i,
                          step_size = 10) {
  data = c("sampson",
           "kapferer",
           "samplk",
           "faux.mesa.high",
           "faux.magnolia.high",
           "ecoli")
  lincol = c("#D7191C",
             "#FDAE61",
             "#ABD9E9",
             "#c351e2",
             "#c351e2",
             "#c351e2")
  linewidth = 2
  
  
  ydiffs = diff(ylimits)
  yupper = ylimits[2]
  ylower = ylimits[1]
  
  
  plot.new()
  plot.window(
    xlim = c(0, 100),
    ylim = c(0, 3 * ydiffs),
    xaxs = "i",
    yaxs = "i"
  )
  #Axes
  {
    axis(
      1,
      at = seq(0, 100, 10),
      labels = c(seq(0, 40, 10), seq(0, 50, 10)),
      las = 1
    )
    axis(
      2,
      at = seq(0, ydiffs, step_size),
      labels = seq(ylower, yupper, step_size),
      las = 1
    )
    axis(
      2,
      
      at = seq(0, ydiffs, step_size) + 2 * ydiffs,
      labels = seq(ylower, yupper, step_size),
      las = 1
    )
    axis(3,
         at = 50,
         labels = "",
         lwd = 2)
    axis(1,
         at = 50,
         labels = "",
         lwd = 2)
    axis(
      4,
      at = seq(0, ydiffs, step_size) + ydiffs,
      labels = seq(ylower, yupper, step_size),
      las = 1
    )
  }
  # Horizontal dividing lines:
  abline(h = c(ydiffs, 2 * ydiffs), lwd = 1)
  #Titles
  title(
    xlab = "False Negative Rate",
    ylab = "Mean",
    main = paste("Mean Gil-Schmidt Centrality", data[i])
  )
  #Plotted lines
  {
    # f.pos.rate 0
    {
      f.pos.rate = 0
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(
        x = 0:50,
        y = unlist(lapply(record$gil.sch.rec, mean)) + 2 * ydiffs - ylower,
        type = "l",
        col = lincol[i],
        lwd = linewidth
      )
    }
    # f.pos.rate 0.01
    {
      f.pos.rate = 0.01
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(
        x = 0:50 + 50,
        y = unlist(lapply(record$gil.sch.rec, mean)) + 2 * ydiffs - ylower,
        type = "l",
        col = lincol[i],
        lwd = linewidth
      )
    }
    # f.pos.rate 0.05
    {
      f.pos.rate = 0.05
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(
        x = 0:50,
        y = unlist(lapply(record$gil.sch.rec, mean)) + ydiffs - ylower,
        type = "l",
        col = lincol[i],
        lwd = linewidth
      )
      
    }
    # f.pos.rate 0.10
    {
      f.pos.rate = 0.10
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(
        x = 0:50 + 50,
        y = unlist(lapply(record$gil.sch.rec, mean)) + ydiffs - ylower,
        type = "l",
        col = lincol[i],
        lwd = linewidth
      )
      
    }
    # f.pos.rate 0.15
    {
      f.pos.rate = 0.15
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(
        x = 0:50,
        y = unlist(lapply(record$gil.sch.rec, mean)) - ylower,
        type = "l",
        col = lincol[i],
        lwd = linewidth
      )
      
    }
    # f.pos.rate 0.20
    {
      f.pos.rate = 0.20
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(
        x = 0:50 + 50,
        y = unlist(lapply(record$gil.sch.rec, mean)) - ylower,
        type = "l",
        col = lincol[i],
        lwd = linewidth
      )
      
    }
  }
  
  #Vertical dividing lines and box
  abline(v = 50, lwd = 2)
  box(lwd = 1)
}
gil.sch.1.Mean(ylimits = c(0, 1.1), 1, step_size = 0.2)
gil.sch.1.Mean(ylimits = c(0, 1), 2, step_size = 0.2)
gil.sch.1.Mean(ylimits = c(0, 1), 3, step_size = 0.2)
gil.sch.1.Mean(ylimits = c(0, 1), 4, step_size = 0.2)
gil.sch.1.Mean(ylimits = c(0, 1), 5, step_size = 0.2)
gil.sch.1.Mean(ylimits = c(0, 1), 6, step_size = 0.2)

#SD:
gil.sch.1.SD = function(ylimits = c(0, 70),
                        i,
                        step_size = 10) {
  data = c("sampson",
           "kapferer",
           "samplk",
           "faux.mesa.high",
           "faux.magnolia.high",
           "ecoli")
  lincol = c("#D7191C",
             "#FDAE61",
             "#ABD9E9",
             "#c351e2",
             "#c351e2",
             "#c351e2")
  linewidth = 2
  
  
  ydiffs = diff(ylimits)
  yupper = ylimits[2]
  ylower = ylimits[1]
  
  
  plot.new()
  plot.window(
    xlim = c(0, 100),
    ylim = c(0, 3 * ydiffs),
    xaxs = "i",
    yaxs = "i"
  )
  #Axes
  {
    axis(
      1,
      at = seq(0, 100, 10),
      labels = c(seq(0, 40, 10), seq(0, 50, 10)),
      las = 1
    )
    axis(
      2,
      at = seq(0, ydiffs, step_size),
      labels = seq(ylower, yupper, step_size),
      las = 1
    )
    axis(
      2,
      
      at = seq(0, ydiffs, step_size) + 2 * ydiffs,
      labels = seq(ylower, yupper, step_size),
      las = 1
    )
    axis(3,
         at = 50,
         labels = "",
         lwd = 2)
    axis(1,
         at = 50,
         labels = "",
         lwd = 2)
    axis(
      4,
      at = seq(0, ydiffs, step_size) + ydiffs,
      labels = seq(ylower, yupper, step_size),
      las = 1
    )
  }
  # Horizontal dividing lines:
  abline(h = c(ydiffs, 2 * ydiffs), lwd = 1)
  #Titles
  title(
    xlab = "False Negative Rate",
    ylab = "SD",
    main = paste("SD Gil-Schmidt Centrality", data[i])
  )
  #Plotted lines
  {
    # f.pos.rate 0
    {
      f.pos.rate = 0
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(
        x = 0:50,
        y = unlist(lapply(record$gil.sch.rec, sd)) + 2 * ydiffs - ylower,
        type = "l",
        col = lincol[i],
        lwd = linewidth
      )
    }
    # f.pos.rate 0.01
    {
      f.pos.rate = 0.01
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(
        x = 0:50 + 50,
        y = unlist(lapply(record$gil.sch.rec, sd)) + 2 * ydiffs - ylower,
        type = "l",
        col = lincol[i],
        lwd = linewidth
      )
    }
    # f.pos.rate 0.05
    {
      f.pos.rate = 0.05
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(
        x = 0:50,
        y = unlist(lapply(record$gil.sch.rec, sd)) + ydiffs - ylower,
        type = "l",
        col = lincol[i],
        lwd = linewidth
      )
      
    }
    # f.pos.rate 0.10
    {
      f.pos.rate = 0.10
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(
        x = 0:50 + 50,
        y = unlist(lapply(record$gil.sch.rec, sd)) + ydiffs - ylower,
        type = "l",
        col = lincol[i],
        lwd = linewidth
      )
      
    }
    # f.pos.rate 0.15
    {
      f.pos.rate = 0.15
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(
        x = 0:50,
        y = unlist(lapply(record$gil.sch.rec, sd)) - ylower,
        type = "l",
        col = lincol[i],
        lwd = linewidth
      )
      
    }
    # f.pos.rate 0.20
    {
      f.pos.rate = 0.20
      
      load(paste("OG", data[i], f.pos.rate, 0.01, sep = "_"))
      lines(
        x = 0:50 + 50,
        y = unlist(lapply(record$gil.sch.rec, sd)) - ylower,
        type = "l",
        col = lincol[i],
        lwd = linewidth
      )
      
    }
  }
  
  #Vertical dividing lines and box
  abline(v = 50, lwd = 2)
  box(lwd = 1)
}
gil.sch.1.SD(ylimits = c(0, 1.1), 1, step_size = 0.2)
gil.sch.1.SD(ylimits = c(0, 1), 2, step_size = 0.2)
gil.sch.1.SD(ylimits = c(0, 1), 3, step_size = 0.2)
gil.sch.1.SD(ylimits = c(0, 1), 4, step_size = 0.2)
gil.sch.1.SD(ylimits = c(0, 1), 5, step_size = 0.2)
gil.sch.1.SD(ylimits = c(0, 1), 6, step_size = 0.2)
