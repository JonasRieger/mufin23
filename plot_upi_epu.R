library(openxlsx)
library(data.table)
library(ggplot2)

dat = read.xlsx("epu.xlsx")
dat = dat[-nrow(dat),]

dat = data.table(date = as.Date(paste0(dat$Year, "-", dat$Month, "-01")),
                 value = dat$Germany_News_Index)
epu = dat[!is.na(dat$value)]
epu[, type := "EPU"]

upi_topics = fread("upi.csv")
colnames(upi_topics) = c("date", gsub("Topic [1-9]{1}[0-9]*: ", "", colnames(upi_topics)[2:15]))
colnames(upi_topics)[colnames(upi_topics) == "Energy & Climate Change Mitigation"] = "Energy & Climate"
upi_sub = character(14)
upi_sub[c(1,3,4,11,13)] = "Real Economy"
upi_sub[c(2,5,6,8,12,14)] = "Politics"
upi_sub[c(7,10)] = "Financial Markets"
n_tmp = nrow(upi_topics)
upi_topics = melt(upi_topics, id.vars = "date", variable.name = "Topic", value.name = "upi")
upi_topics = cbind(upi_topics, Type = rep(upi_sub, each = n_tmp))
upi_topics[, Topic := factor(Topic, levels = sort(unique(as.character(Topic))))]
upi_topics[, Type := factor(Type, levels = c("Real Economy", "Politics", "Financial Markets"))]

upi_ges = upi_topics[Topic != "Miscellaneous", .(value = sum(upi)), by = date]
multiplikator = max(epu$value) / max(upi_ges$value)
multiplikator = 8000
upi_ges[, value := value*multiplikator]
upi_ges[, type := "UPI"]
upi_ges[, date := as.Date(date)]

ggplot(rbind(epu, upi_ges)) + aes(x = date, y = value, col = type) +
  geom_line() + labs(col = "") + xlab("") + theme(legend.position = "top") +
  scale_y_continuous("EPU", 
                     sec.axis = sec_axis(~ . / multiplikator, name = "UPI"))
