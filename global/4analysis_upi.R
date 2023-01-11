library(ldaPrototype)
library(tosca)
library(data.table)
library(ggplot2)
library(beepr)

lda = readRDS("proto_upi.rds")
id = names(readRDS("docs_upi.rds"))
obj = readRDS("obj_updated_wirtschaft_unsicher.rds")
clean = readRDS("clean_updated_wirtschaft_unsicher.rds")
counts = readRDS("counts.rds")
dates = obj$meta$date[match(id, obj$meta$id)]

saveRDS(counts, "counts_upi.rds")
fwrite(counts, "counts_upi.csv")

labels = c("Topic 1: Central banks",
           "Topic 2: Corporate Culture (+German Economy)",
           "Topic 3: German Politics II",
           "Topic 4: German Politics I",
           "Topic 5: German Economy",
           "Topic 6: Financial Markets I",
           "Topic 7: Geopolitics (+Energy)",
           "Topic 8: Miscellaneous",
           "Topic 9: EU Conflicts",
           "Topic 10: Financial Markets II",
           "Topic 11: Geopolitics",
           "Topic 12: Companies (Cars)",
           "Topic 13: Society",
           "Topic 14: Companies & Markets"
           )

setwd("analysis")
setwd("upi")

counts = counts[type == "words"]

pdf("words.pdf", width = 10, height = 7)
print(ggplot(counts) + aes(x = date, y = n/1e6) + geom_line() +
        ylim(c(0, max(counts$n)/1e6)) +
        xlab("monthly") + ylab("n (in Million)"))
print(ggplot(counts) + aes(x = date, y = n_wirtschaft/n) + geom_line() +
        ylim(c(0, max(counts$n_wirtschaft/counts$n))) +
        xlab("monthly"))
print(ggplot(counts) + aes(x = date, y = n_wirtschaft_unsicher/n_wirtschaft) + geom_line() +
        ylim(c(0, max(counts$n_wirtschaft_unsicher/counts$n_wirtschaft))) +
        xlab("monthly"))
print(ggplot(counts) + aes(x = date, y = n_wirtschaft_unsicher/n) + geom_line() +
        ylim(c(0, max(counts$n_wirtschaft_unsicher/counts$n))) +
        xlab("monthly"))
dev.off()

## topTexts
dir.create("topTexts")
mat = topTexts(getLDA(lda), id, 100, tnames = gsub(":", "", gsub(" |/", "_", labels)))
showTexts(obj, mat, file = "topTexts/")

## topTexts per Month
dir.create("topTextsPerMonth")
for(m in as.character(seq.Date(as.Date("2001-01-01"), max(dates), "month"))){
  m = as.Date(m)
  doc_sums = list(document_sums = getDocument_sums(getLDA(lda))[,dates >= m & dates < m + months(1)])
  mat = topTexts(doc_sums, id[dates >= m & dates < m + months(1)], 10, tnames = gsub(":", "", gsub(" |/", "_", labels)))
  dir.create(file.path("topTextsPerMonth", as.character(substr(m, 0, 7))))
  showTexts(obj, mat, file = paste0("topTextsPerMonth/", as.character(substr(m, 0, 7)), "/"))
}

## topwords
topwords = topWords(getTopics(getLDA(lda)), 200)
prop = round(rowSums(getTopics(getLDA(lda))) / sum(getTopics(getLDA(lda))) * 100, 4)
out = rbind(prop, topwords)
colnames(out) = labels
row.names(out) = c("Proportion (%)", 1:200)
write.csv(out, file = "topwords.csv", fileEncoding = "UTF-8")

## topics
tab = plotTopic(object = obj, ldaresult = getLDA(lda), ldaID = id)
tabrel = plotTopic(object = obj, ldaresult = getLDA(lda), ldaID = id, rel = TRUE)
colnames(tab) = colnames(tabrel) = c("date", labels)

write.csv(tab, file = "topics.csv", fileEncoding = "UTF-8", row.names = FALSE)
write.csv(tabrel, file = "topics_rel.csv", fileEncoding = "UTF-8", row.names = FALSE)

tabrel_wirtschaft = cbind(date = tabrel$date,
                          tabrel[, -1] *
                            counts[match(tabrel$date, date), n_wirtschaft_unsicher/n_wirtschaft])
write.csv(tabrel_wirtschaft, file = "topics_rel_to_wirtschaft.csv",
          fileEncoding = "UTF-8", row.names = FALSE)
tabrel_all = cbind(date = tabrel$date,
                   tabrel[, -1] *
                     counts[match(tabrel$date, date), n_wirtschaft_unsicher/n])
write.csv(tabrel_all, file = "topics_rel_to_entire.csv",
          fileEncoding = "UTF-8", row.names = FALSE)

# delete topic 11 (Misc.) for upi calculation
upi = data.frame(date = tabrel_all$date,
                 upi = rowSums(tabrel_all[,-c(1,12)]),
                 
                 sub_real_economy = rowSums(tabrel_all[, c(14,2,7,5,12)+1]),
                 sub_politics = rowSums(tabrel_all[, c(1,9,11,3,4,13)+1]),
                 sub_financial_markets = rowSums(tabrel_all[, c(6,10)+1]))
write.csv(upi, file = "upi.csv", fileEncoding = "UTF-8", row.names = FALSE)

upi_tmp = upi
upi = unname(upi)
sub = rbind(cbind(a = upi[, c(1,3)], b = "Real Economy"),
            cbind(a = upi[, c(1,4)], b = "Politics"),
            cbind(a = upi[, c(1,5)], b = "Financial Markets"))
colnames(sub) = c("date", "upi", "Subindex")
upi = upi_tmp

pdf("upi.pdf", width = 10, height = 7)
print(ggplot(upi) + aes(x = date, y = upi) + geom_line() + xlab("") + ylab("UPI (Share)"))
print(ggplot(sub) +
        aes(x = date, y = upi, group = Subindex, color = Subindex) +
        geom_line() +
        xlab("") + ylab("Share") + theme(legend.position = "top"))
dev.off()

beep(3)
