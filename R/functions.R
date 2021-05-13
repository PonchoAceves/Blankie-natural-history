
### Theme for figures

theme_poncho <- function() {
  theme_classic() +
    theme(axis.title.x = element_text(size=14, face="bold", hjust = 0.5), 
          axis.title.y = element_text(size=14, face="bold", hjust = 0.5), 
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size=12), 
          legend.position="top")
}


norm.interval <-  function(data, variance = var(data, na.rm = T), conf.level = 0.95) {
  z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
  xbar = mean(data, na.rm = T)
  sdx = sqrt(variance/length(data))
  c(xbar - z * sdx, xbar + z * sdx) 
}