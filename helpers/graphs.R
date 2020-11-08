asset_cumulative <- asset_returns_long %>%
  group_by(asset) %>%
  mutate(returns = cumprod(1+returns))

ggplot(asset_cumulative, aes(x = index, y = returns, linetype = asset))+
  geom_line(aes(colour = asset), size = 1)+
  theme_classic()+
  scale_color_manual(values = rep(c("#85a77f", "#eb6b54", "#4d457e", "#F4B400", "#4285F4", "#FF00BF")))+
  scale_x_date(expand = c(0,0), date_breaks = "1 year", date_labels =  "%d-%y")+
  labs(x = "", y = "")+
  theme(legend.title = element_blank(),
        legend.key = element_blank(), legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.margin=margin(c(-25,-25,-5,-5)),
        plot.margin = unit(c(0.2,0.2,0.2,-0.3), "cm"))#+
#  scale_linetype_manual(values = c("ABG" = "solid", "AGL" = "dashed", "GRT" = "solid",
#                                    "MRP" = "solid", "NPN" = "dashed"))
