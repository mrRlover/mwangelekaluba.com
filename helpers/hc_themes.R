my_theme <- hc_theme_merge(
  hc_theme_sandsignika(),
  hc_theme(
    chart = list(
      backgroundColor = "#f4fcfe", 
      style = list(
        fontFamily = "Montserrat"
      )
    )
  )
)

font_theme <- hc_theme_merge(
  hc_theme_ft(),
  hc_theme(
  chart = list(
    style = list(
      fontFamily = "Montserrat"
    )
    )
  )
)

ft_theme <- hc_theme_merge(
  hc_theme_ft(),
  hc_theme(
    chart = list(
      backgroundColor = "#3e3e40", 
      style = list(
        fontFamily = "Montserrat"
      )
    )
  )
)
