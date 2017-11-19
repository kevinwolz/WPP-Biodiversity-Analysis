bowls.raw <- read_csv(paste0(inputDataPath, "WPP_SI_Bowls_2014-2016.csv"), col_types = cols())

bowls <- bowls.raw %>%
  select(-Collembola) %>% # Collembola not measured in aerial traps in 2014, so leaving it in would confound between year analyses
  mutate(num.orders = rowSums(select(., Anthophila:Araneae) != 0)) %>%
  mutate(num.indivs = rowSums(select(., Anthophila:Araneae), na.rm = FALSE)) %>%
  filter(site != "Fruit Farm")

bowls.wpp <- bowls %>%
  filter(site == "WPP") %>%
  group_by(year, age, month, day, doy, site, trt) %>%
  summarize(num.orders.mean = mean(num.orders, na.rm = TRUE),
            num.orders.sem  = sd(num.orders, na.rm = TRUE) / sqrt(4),
            num.indivs.mean = mean(num.indivs, na.rm = TRUE),
            num.indivs.sem  = sd(num.indivs, na.rm = TRUE) / sqrt(4))

bowls.si <- bowls %>%
  filter(site != "WPP")

bowls.controls <- bowls %>%
  filter(trt == "Row Crop") %>%
  rename(num.orders.control = num.orders) %>%
  rename(num.indivs.control = num.indivs) %>%
  select(year, month, site, block, num.orders.control, num.indivs.control)

bowls.rel <- bowls %>%
  filter(trt != "Row Crop") %>%
  select(year, age, month, day, doy, site, trt, block, num.orders, num.indivs) %>%
  left_join(bowls.controls, by = c("year", "month", "site", "block")) %>%
  mutate(num.orders.rel = num.orders / num.orders.control) %>%
  mutate(num.indivs.rel = num.indivs / num.indivs.control) %>%
  select(-num.orders, -num.indivs, -num.orders.control, -num.indivs.control) %>%
  group_by(year, age, month, day, doy, site, trt) %>%
  summarize(num.orders.mean = mean(num.orders.rel, na.rm = TRUE),
            num.orders.sem  = sd(num.orders.rel, na.rm = TRUE) / sqrt(4),
            num.indivs.mean = mean(num.indivs.rel, na.rm = TRUE),
            num.indivs.sem  = sd(num.indivs.rel, na.rm = TRUE) / sqrt(4))

bowls.rel.wpp <- bowls.rel %>% filter(site == "WPP")
bowls.rel.si <- bowls.rel %>% filter(site != "WPP")

bowls.wpp$trt <- factor(bowls.wpp$trt, labels = c("AF", "MSR"))
bowl.date.labs <- data.frame(year = 2014:2016, month = 5, num.orders.mean = 10, num.indivs.mean = 600)

## NUMBER OF ORDERS
bowl.order.ts <- ggplot(bowls.wpp, aes(x = month, y = num.orders.mean)) +
  labs(x = "Month", y = "Number of orders", color = "") +
  facet_wrap(~year, ncol = 1) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), labels = month.abb[sort(unique(bowls.wpp$month))]) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL), limits = c(0, 11.2)) +
  geom_line(na.rm = TRUE, aes(color = trt)) +
  geom_point(na.rm = TRUE, aes(color = trt)) +
  geom_errorbar(aes(ymin = (num.orders.mean - num.orders.sem),
                    ymax = (num.orders.mean + num.orders.sem),
                    color = trt), na.rm = TRUE, width = 0.3) +
  scale_color_manual(values = c("black", "grey70")) +
  geom_text(data = bowl.date.labs, aes(label = year), hjust = 0.15, vjust = 0.25, size = 6) +
  theme_ggEHD() +
  theme(legend.position = c(0.26, 0.77),
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())

ggsave_fitmax(paste0(outputPlotPath, "WPP_Bowl_Orders_TS.jpg"),
              bowl.order.ts,
              dpi = 500)


## NUMBER OF INDIVIDUALS
bowl.indiv.ts <- ggplot(bowls.wpp, aes(x = month, y = num.indivs.mean)) +
  labs(x = "Month", y = "Abundance", color = "") +
  facet_wrap(~year, ncol = 1) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), labels = month.abb[sort(unique(bowls.wpp$month))]) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_line(na.rm = TRUE, aes(color = trt)) +
  geom_point(na.rm = TRUE, aes(color = trt)) +
  geom_errorbar(aes(ymin = (num.indivs.mean - num.indivs.sem),
                    ymax = (num.indivs.mean + num.indivs.sem),
                    color = trt), na.rm = TRUE, width = 0.3) +
  scale_color_manual(values = c("black", "grey70")) +
  geom_text(data = bowl.date.labs, aes(label = year), hjust = 0.1, vjust = 0.5, size = 6) +
  theme_ggEHD() +
  theme(legend.position = c(0.25, 0.87),
        legend.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())

ggsave_fitmax(paste0(outputPlotPath, "WPP_Bowl_Abundnace_TS.jpg"),
              bowl.indiv.ts,
              dpi = 500)

## ALL TOGETHER BY AGE
bowls.rel.wpp.PLOT <- subset(bowls.rel.wpp, age < 4)
bowls.rel.wpp.PLOT$month <- bowls.rel.wpp.PLOT$month - 0.05

bowls.rel.si.PLOT  <- subset(bowls.rel.si, num.orders.mean < 4 & num.indivs.mean < 20)
bowls.rel.si.PLOT$month <- bowls.rel.si.PLOT$month + 0.05
bowls.rel.si.PLOT$trt <- factor(bowls.rel.si.PLOT$trt, labels = c("AF", "Forest", "Hay"))

bowl.age.labs <- data.frame(age = 1:3,
                            label = paste0("(", c("a", "b", "c"), ") ", 1:3, "yr"),
                            month = 4,
                            num.orders.mean = 2.7,
                            num.indivs.mean = 15)

rel.ofders.age <- ggplot(bowls.rel.wpp.PLOT, aes(x = month, y = num.orders.mean)) +
  labs(x = "Month", y = "Number of orders relative to MSR", color = "") +
  facet_wrap(~age, ncol = 1) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), breaks = 4:9) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(na.rm = TRUE) +
  geom_errorbar(aes(ymin = (num.orders.mean - num.orders.sem), ymax = (num.orders.mean + num.orders.sem)), na.rm = TRUE, width = 0.3) +
  geom_point(data = bowls.rel.si.PLOT, aes(color = trt), na.rm = TRUE)+
  geom_text(data = bowl.age.labs, aes(label = label), hjust = 0.1, vjust = 1.5, size = 5, na.rm = TRUE) +
  scale_color_manual(values = c("grey70", "#E69F00", "#56B4E9")) +
  theme_ggEHD() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank())

ggsave_fitmax(paste0(outputPlotPath, "Bowl_Rel_Orders_by_Age.jpg"),
              rel.ofders.age,
              dpi = 500)

rel.abundance.age <- ggplot(bowls.rel.wpp.PLOT, aes(x = month, y = num.indivs.mean)) +
  labs(x = "Month", y = "Abundance relative to MSR", color = "") +
  facet_wrap(~age, ncol = 1) +
  scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), breaks = 4:9) +
  scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(na.rm = TRUE) +
  geom_errorbar(aes(ymin = (num.indivs.mean - num.indivs.sem), ymax = (num.indivs.mean + num.indivs.sem)), na.rm = TRUE, width = 0.3) +
  geom_point(data = bowls.rel.si.PLOT, aes(color = trt), na.rm = TRUE)+
  geom_text(data = bowl.age.labs, aes(label = label), hjust = 0.1, vjust = 0.6, size = 5, na.rm = TRUE) +
  scale_color_manual(values = c("grey70", "#E69F00", "#56B4E9")) +
  theme_ggEHD() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_blank())

ggsave_fitmax(paste0(outputPlotPath, "Bowl_Rel_Abundance_by_Age.jpg"),
              rel.abundance.age,
              dpi = 500)

## RELATIVE ABUNDANCE BY ORDER
bowls.to.plot <- bowls
bowls.to.plot$Thysanoptera[bowls.to.plot$Thysanoptera > 750] <- NA
bowls.to.plot$Orthoptera[bowls.to.plot$Orthoptera > 30] <- NA
bowls.to.plot$Hymenoptera[bowls.to.plot$Hymenoptera > 200] <- NA
bowls.to.plot$Araneae[bowls.to.plot$Araneae > 14] <- NA

plot_by_order <- function(order.name, bowls) {
  wpp <- si <- bowls
  names(wpp)[names(wpp) == order.name] <- "value"
  names(si)[names(si) == order.name] <- "value.mean"

  wpp <- wpp %>%
    filter(site == "WPP", age < 4) %>%
    #mutate(month = month - 0.1) %>%
    mutate(trt = factor(trt,  labels = c("AF", "MSR"))) %>%
    group_by(year, age, month, day, doy, site, trt) %>%
    summarize(value.mean = mean(value, na.rm = TRUE),
              value.sem  = sd(value, na.rm = TRUE) / sqrt(4))

  si <- si %>%
    filter(site != "WPP") %>%
    #mutate(month = month + 0.1) %>%
    mutate(trt = factor(trt,  labels = c("AF", "Forest", "Hay", "Row Crop")))


  order.plot <- ggplot(wpp, aes(x = doy, y = value.mean)) +
    labs(x = "Month", y = paste0(order.name, " abundance"), color = "WPP", fill = "SI") +
    facet_wrap(~age, ncol = 1) +
    scale_x_continuous(breaks = c(105,135,166,196,227,258), labels = as.character(4:9)) + #sec.axis = sec_axis(~ ., labels = NULL),
    scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
    geom_jitter(data = si, shape = 21, aes(fill = trt), na.rm = TRUE) +
    geom_point(aes(color = trt), na.rm = TRUE, size = 2) +
    geom_errorbar(aes(ymin = (value.mean - value.sem), ymax = (value.mean + value.sem), color = trt), na.rm = TRUE, width = 0.3) +
    geom_point(na.rm = TRUE, shape = 21, size = 2) +
    scale_color_manual(values = c("black", "#D55E00")) +
    scale_fill_manual(values = c("black", "#56B4E9", "#E69F00", "#D55E00")) +
    theme_ggEHD() +
    theme(legend.position = "right")

  ggsave_fitmax(paste0(orderPlotPath, order.name, "_Rel_Abundance_by_Age.jpg"),
                order.plot,
                dpi = 500)
}

orders.to.plot <- c("Anthophila", "Hymenoptera", "Diptera", "Orthoptera", "Hemiptera", "Thysanoptera", "Coleoptera", "Lepidoptera", "Araneae")
purrr::walk(orders.to.plot, plot_by_order, bowls = bowls.to.plot)

 # plot_by_order <- function(order.name, bowls) {
#   bowls.controls <- bowls.rel <- bowls
#   names(bowls.controls)[names(bowls.controls) == order.name] <- "control"
#   names(bowls.rel)[names(bowls.rel) == order.name] <- "value"
#
#   bowls.controls <- bowls.controls %>%
#     filter(trt == "Row Crop") %>%
#     select(year, month, site, block, control)
#
#   bowls.rel <- bowls.rel %>%
#     filter(trt != "Row Crop") %>%
#     select(year, age, month, day, doy, site, trt, block, value) %>%
#     left_join(bowls.controls, by = c("year", "month", "site", "block")) %>%
#     mutate(num.indivs.rel = value / control) %>%
#     select(-value, -control) %>%
#     group_by(year, age, month, day, doy, site, trt) %>%
#     summarize(num.indivs.mean = mean(num.indivs.rel, na.rm = TRUE),
#               num.indivs.sem  = sd(num.indivs.rel, na.rm = TRUE) / sqrt(4))
#
#   wpp <- bowls.rel %>% filter(site == "WPP")
#   si  <- bowls.rel %>% filter(site != "WPP")
#
#   wpp$trt <- factor(wpp$trt, labels = "AF")
#   si$trt  <- factor(si$trt,  labels = c("AF", "Forest", "Hay"))
#
#   rel.order <- ggplot(wpp, aes_string(x = "month", y = num.indivs.mean)) +
#     labs(x = "Month", y = "Number of orders relative to MSR", color = "") +
#     facet_wrap(~age, ncol = 1) +
#     scale_x_continuous(sec.axis = sec_axis(~ ., labels = NULL), breaks = 4:9) +
#     scale_y_continuous(sec.axis = sec_axis(~ ., labels = NULL)) +
#     geom_hline(yintercept = 1, linetype = "dashed") +
#     geom_point(na.rm = TRUE) +
#     geom_errorbar(aes(ymin = (num.indivs.mean - num.indivs.sem), ymax = (num.indivs.mean + num.indivs.sem)), na.rm = TRUE) +
#     geom_point(data = si, aes(color = trt), na.rm = TRUE) +
#     scale_color_manual(values = c("grey70", "#E69F00", "#56B4E9")) +
#     theme_ggEHD() +
#     theme(legend.position = "bottom")
#
#   ggsave_fitmax(paste0(outputPlotPath, order.name, "_Rel_Abundance_by_Age.jpg"),
#                 rel.order,
#                 dpi = 500)
# }
