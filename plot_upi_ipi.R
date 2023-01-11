library(data.table)
library(ggplot2)
library(cowplot)

#### IPI
ipi = fread("ipi.csv")

ipi_misc = cbind(ipi[,c(1,7)], "Miscellaneous")
colnames(ipi_misc) = c("date", "ipi", "Type")
ipi = rbind(cbind(ipi[, c(1,2)], "Central Banks", "Causal"),
            cbind(ipi[, c(1,3)], "News", "Consequences"),
            cbind(ipi[, c(1,4)], "Emerging Markets", "Other"),
            cbind(ipi[, c(1,5)], "Eurozone", "Causal"),
            cbind(ipi[, c(1,6)], "Private Investment", "Consequences"),
            #cbind(ipi[, c(1,7)], "Miscellaneous", "Miscellaneous"),
            cbind(ipi[, c(1,8)], "Financial Markets", "Consequences"),
            cbind(ipi[, c(1,9)], "Companies", "Consequences"),
            cbind(ipi[, c(1,10)], "German Politics", "Causal"),
            cbind(ipi[, c(1,11)], "Raw Materials", "Causal"),
            use.names = FALSE)
colnames(ipi) = c("date", "ipi", "Topic", "Type")
ipi[,Type := factor(Type, levels = c("Causal", "Consequences", "Other"))]

ggList1 = lapply(split(ipi, ipi$Type), function(i) {
  ggplot(i) +
    annotate(geom = "rect",
             xmin = as.Date("2001-01-01"),
             xmax = as.Date("2005-12-31"),
             ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.3) +
    aes(x = date, y = ipi, group = Topic, color = Topic) + 
    geom_line() +
    xlab("") + ylab("") + theme(legend.position = "top", title = element_blank()) +
    guides(colour = guide_legend(nrow = 1)) +
    scale_x_date(breaks = as.Date(c("2001-01-01", "2005-01-01", "2010-01-01",
                                    "2015-01-01", "2020-01-01", "2022-01-01")),
                 date_minor_breaks = "1 year",
                 labels = c(2000+c(1,5,10,15,20,22))) +
    scale_y_continuous(breaks = seq(0,1,0.005), minor_breaks = seq(0,1,0.005), limits = c(0,max(i$ipi))) +
    facet_grid(~Type)
  })
ggList1[[4]] = ggplot(ipi_misc) +
  annotate(geom = "rect",
           xmin = as.Date("2001-01-01"),
           xmax = as.Date("2005-12-31"),
           ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.3) +
  aes(x = date, y = ipi) + 
  geom_line() +
  xlab("") + ylab("") + theme(legend.position = "top", title = element_blank()) +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_date(breaks = as.Date(c("2001-01-01", "2005-01-01", "2010-01-01",
                                  "2015-01-01", "2020-01-01", "2022-01-01")),
               date_minor_breaks = "1 year",
               labels = c(2000+c(1,5,10,15,20,22))) +
  scale_y_continuous(breaks = seq(0,1,0.005), minor_breaks = seq(0,1,0.005), limits = c(0,max(ipi_misc$ipi))) +
  facet_grid(~Type)
ipi_ges = ipi[, .(ipi = sum(ipi)), by = date]
ipi_ges[, Type := "IPI"]
ggList1[[5]] = ggplot(ipi_ges) +
  annotate(geom = "rect",
           xmin = as.Date("2001-01-01"),
           xmax = as.Date("2005-12-31"),
           ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.3) +
  aes(x = date, y = ipi) + 
  geom_line() +
  xlab("") + ylab("") + theme(legend.position = "top", title = element_blank()) +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_date(breaks = as.Date(c("2001-01-01", "2005-01-01", "2010-01-01",
                                  "2015-01-01", "2020-01-01", "2022-01-01")),
               date_minor_breaks = "1 year",
               labels = c(2000+c(1,5,10,15,20,22))) +
  scale_y_continuous(breaks = seq(0,1,0.05), minor_breaks = seq(0,1,0.05), limits = c(0,max(ipi_ges$ipi))) +
  facet_grid(~Type)
ipi_plot = cowplot::plot_grid(plotlist = ggList1[c(5,1:4)], ncol = 1, align = 'v', rel_heights = c(1.5,2,2,2,1.5))


#### UPI
upi_topics = fread("upi.csv")
colnames(upi_topics) = c("date", gsub("Topic [1-9]{1}[0-9]*: ", "", colnames(upi_topics)[2:15]))
colnames(upi_topics)[colnames(upi_topics) == "Energy & Climate Change Mitigation"] = "Energy & Climate"
upi_sub = character(14)
upi_sub[c(1,3,4,11,13)] = "Real Economy"
upi_sub[c(2,5,6,8,12,14)] = "Politics"
upi_sub[c(7,10)] = "Financial Markets"
#upi_sub[9] = "Miscellaneous"
n_tmp = nrow(upi_topics)

upi_topics = melt(upi_topics, id.vars = "date", variable.name = "Topic", value.name = "upi")
upi_topics = cbind(upi_topics, Type = rep(upi_sub, each = n_tmp))
upi_topics[, Topic := factor(Topic, levels = sort(unique(as.character(Topic))))]
upi_topics[, Type := factor(Type, levels = c("Real Economy", "Politics", "Financial Markets"))]

ggList2 = lapply(split(upi_topics[Topic != "Miscellaneous"], upi_topics[Topic != "Miscellaneous", Type]), function(i) {
  ggplot(i) +
    annotate(geom = "rect",
             xmin = as.Date("2001-01-01"),
             xmax = as.Date("2005-12-31"),
             ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.3) +
    aes(x = date, y = upi, group = Topic, color = Topic) + 
    geom_line() +
    xlab("") + ylab("") + theme(legend.position = "top", title = element_blank()) +
    guides(colour = guide_legend(ncol = 3, byrow = TRUE)) +
    scale_x_date(breaks = as.Date(c("2001-01-01", "2005-01-01", "2010-01-01",
                                    "2015-01-01", "2020-01-01", "2022-01-01")),
                 date_minor_breaks = "1 year",
                 labels = c(2000+c(1,5,10,15,20,22))) +
    scale_y_continuous(breaks = seq(0,1,0.002), minor_breaks = seq(0,1,0.002), limits = c(0,max(i$upi))) +
    facet_grid(~Type)
})
upi_misc = upi_topics[Topic == "Miscellaneous"]
ggList2[[4]] = ggplot(upi_misc) +
  annotate(geom = "rect",
           xmin = as.Date("2001-01-01"),
           xmax = as.Date("2005-12-31"),
           ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.3) +
  aes(x = date, y = upi) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(legend.position = "top", title = element_blank()) +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_date(breaks = as.Date(c("2001-01-01", "2005-01-01", "2010-01-01",
                                  "2015-01-01", "2020-01-01", "2022-01-01")),
               date_minor_breaks = "1 year",
               labels = c(2000+c(1,5,10,15,20,22))) +
  scale_y_continuous(breaks = seq(0,1,0.002), minor_breaks = seq(0,1,0.002), limits = c(0,max(upi_misc$upi))) +
  facet_grid(~Topic)
upi_ges = upi_topics[Topic != "Miscellaneous", .(upi = sum(upi)), by = date]
upi_ges[, Type := "UPI"]
ggList2[[5]] = ggplot(upi_ges) +
  annotate(geom = "rect",
           xmin = as.Date("2001-01-01"),
           xmax = as.Date("2005-12-31"),
           ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.3) +
  aes(x = date, y = upi) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(legend.position = "top", title = element_blank()) +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_date(breaks = as.Date(c("2001-01-01", "2005-01-01", "2010-01-01",
                                  "2015-01-01", "2020-01-01", "2022-01-01")),
               date_minor_breaks = "1 year",
               labels = c(2000+c(1,5,10,15,20,22))) +
  scale_y_continuous(breaks = seq(0,1,0.01), minor_breaks = seq(0,1,0.01), limits = c(0,max(upi_ges$upi))) +
  facet_grid(~Type)
upi_plot = cowplot::plot_grid(plotlist = ggList2[c(5,1:4)], ncol = 1, align = 'v', rel_heights = c(1.5,2.12,2.12,1.8,1.5))

cowplot::plot_grid(upi_plot, ipi_plot)
