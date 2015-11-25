
theme_simple <- function(base_size = 12, base_family = "Helvetica")
  {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
      theme(
            strip.background = element_rect(fill = 'grey90', colour = 'grey90'),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(colour = 'grey95'),
            panel.border = element_rect(fill = NA, colour = 'grey80', linetype = 'solid')
           )
  }

theme_nothing <- function(base_size = 12, base_family = "Helvetica")
  {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
      theme(
            rect             = element_blank(),
            line             = element_blank(),
            text             = element_blank(),
            axis.ticks.margin = unit(0, "lines")
           )
  }
