example = function(ylimits = c(0,100), step_size = 25){
  data = c("sampson", "kapferer", "samplk", "faux.mesa.high")
  lincol = c("#D7191C", "#FDAE61", "#ABD9E9", "#c351e2")
  linewidth = 2
  ydiffs = diff(ylimits)
  yupper = ylimits[2]
  ylower = ylimits[1]
  
  layout(c(1,1,1,2))
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
        ylab = "Metric Mean or Standard Deviation", 
        main = "Metric Mean or Standard Deviation \nNetwork Size (n)")
  #Vertical dividing lines and box
  abline(v = 50, lwd = 2)
  box(lwd = 1)
  text(25 , 250,"False positive rate of 0")
  text(75 , 250,"False positive rate of 0.01")
  text(25 , 150,"False positive rate of 0.05")
  text(75 , 150,"False positive rate of 0.10")
  text(25 , 50,"False positive rate of 0.15")
  text(75 , 50,"False positive rate of 0.20")
  # legend("top", legend = data, col = lincol, lty = rep(1,4), pch = "", horiz = T, text.width = 17.3, lwd = 2, cex = 1, x.intersp = 0.2)
  # legend("topleft", legend = data[1:2], col = lincol[1:2], lty = rep(1,2), pch = "", bty = "n", horiz = F, text.width = 10, lwd = 2, cex = 1)#, adj = 0.2)
  # legend("top", legend = data[3:4], col = lincol[3:4], lty = rep(1,2), pch = "", bty = "n", horiz = F, text.width = 10, lwd = 2, cex = 1)#, adj = 0.2)
  # legend("top", legend = data, col = lincol, lty = rep(1,4), pch = "", bty = "n", horiz = T, text.width = 1, lwd = 2)
  plot.new()
  #box()
  legend("top", legend = data, col = lincol, lty = rep(1,4), pch = "", horiz = T, text.width = 0.22, lwd = 6, cex = 1, x.intersp = 0.2)
}
example()
pdf("example_plots.pdf")
par(mar = c(2,4,3,3))
example()
dev.off()
