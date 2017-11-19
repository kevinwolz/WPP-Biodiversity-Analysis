aerials.raw <- read_csv(paste0(inputDataPath, "WPP_Aerials_2014-2016.csv"), col_types = cols())

aerials <- aerials.raw %>%
  select(-Collembola) %>% # Collembola not measured in aerial traps in 2014, so leaving it in would confound between year analyses
  mutate(num.orders = rowSums(select(., Anthophila:Neuroptera) != 0)) %>%
  mutate(num.indivs = rowSums(select(., Anthophila:Neuroptera), na.rm = FALSE)) %>%
  group_by(trt, year, month) %>%
  summarize(num.orders.mean = mean(num.orders),
            num.orders.sem  = sd(num.orders) / sqrt(4),
            num.indivs.mean = mean(num.indivs),
            num.indivs.sem  = sd(num.indivs) / sqrt(4))

aerials$trt <- factor(aerials$trt, labels = c("AF", "MSR"))
aerial.date.labs <- data.frame(year = 2014:2016, month = 4, num.orders.mean = 10, num.indivs.mean = 300)

## NUMBER OF ORDERS
aerial.order.ts <- ggplot(aerials, aes(x = month, y = num.orders.mean)) +
  labs(x = "Month", y = "Number of orders", color = "") +
  facet_wrap(~year, ncol = 1) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), labels = month.abb[unique(aerials$month)]) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(0, 11.2)) +
  geom_line(na.rm = TRUE, aes(color = trt)) +
  geom_point(na.rm = TRUE, aes(color = trt)) +
  geom_errorbar(aes(ymin = (num.orders.mean - num.orders.sem),
                    ymax = (num.orders.mean + num.orders.sem),
                    color = trt), na.rm = TRUE, width = 0.3) +
  scale_color_manual(values = c("black", "grey70")) +
  geom_text(data = aerial.date.labs, aes(label = year), hjust = 0.15, vjust = 0.25, size = 6) +
  theme_ggEHD() +
  theme(legend.position = c(0.26, 0.77),
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())

ggsave_fitmax(paste0(outputPlotPath, "WPP_Aerial_Orders_TS.jpg"),
              aerial.order.ts,
              dpi = 500)

## NUMBER OF INDIVIDUALS
aerial.indiv.ts <- ggplot(aerials, aes(x = month, y = num.indivs.mean)) +
  labs(x = "Month", y = "Abundance", color = "") +
  facet_wrap(~year, ncol = 1) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), labels = month.abb[unique(aerials$month)]) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_line(na.rm = TRUE, aes(color = trt)) +
  geom_point(na.rm = TRUE, aes(color = trt)) +
  geom_errorbar(aes(ymin = (num.indivs.mean - num.indivs.sem),
                    ymax = (num.indivs.mean + num.indivs.sem),
                    color = trt), na.rm = TRUE, width = 0.3) +
  scale_color_manual(values = c("black", "grey70")) +
  geom_text(data = aerial.date.labs, aes(label = year), hjust = 0.1, vjust = 0.5, size = 6) +
  theme_ggEHD() +
  theme(legend.position = c(0.6, 0.9),
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())

ggsave_fitmax(paste0(outputPlotPath, "WPP_Aerial_Abundnace_TS.jpg"),
              aerial.indiv.ts,
              dpi = 500)
