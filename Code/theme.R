library(ggplot2)
COLVEC = c("brown","grey","pink","pink3","beige","orange2","darkorange2")
my_theme <- theme_bw() +
  theme(strip.background = element_rect(fill = "white"), text = element_text(face="bold", size=12),
  )
theme_set(my_theme)