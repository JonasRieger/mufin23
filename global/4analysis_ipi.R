library(ldaPrototype)
library(tosca)
library(data.table)
library(ggplot2)
library(beepr)

lda = readRDS("proto_ipi.rds")
id = names(readRDS("docs_ipi.rds"))
obj = readRDS("obj_inflation_updated.rds")
clean = readRDS("clean_inflation_updated.rds")
counts = readRDS("counts.rds")
dates = obj$meta$date[match(id, obj$meta$id)]

tab = cbind(plotScot(clean, type = "docs"), type = "docs")
colnames(tab) = c("date", "n_inflation", "type")
tab2 = cbind(plotScot(clean, type = "words"), type = "words")
colnames(tab2) = c("date", "n_inflation", "type")
tab = rbindlist(list(tab, tab2))
counts = merge(counts, tab)
saveRDS(counts, "counts_ipi.rds")
fwrite(counts, "counts_ipi.csv")

labels = c("Topic 1: German Politics",
           "Topic 2: Raw Materials/News",
           "Topic 3: Private Investment",
           "Topic 4: Financial Markets",
           "Topic 5: Emerging Markets",
           "Topic 6: Companies",
           "Topic 7: Miscellaneous",
           "Topic 8: News",
           "Topic 9: Eurozone",
           "Topic 10: Central Banks")

setwd("analysis")
setwd("ipi")

counts = counts[type == "words"]

pdf("words.pdf", width = 10, height = 7)
print(ggplot(counts) + aes(x = date, y = n/1e6) + geom_line() +
        ylim(c(0, max(counts$n)/1e6)) +
        xlab("monthly") + ylab("n (in Million)"))
print(ggplot(counts) + aes(x = date, y = n_inflation/n) + geom_line() +
        ylim(c(0, max(counts$n_inflation/counts$n))) +
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

tabrel_all = cbind(date = tabrel$date,
                   tabrel[, -1] *
                     counts[match(tabrel$date, date), n_inflation/n])
write.csv(tabrel_all, file = "topics_rel_to_entire.csv",
          fileEncoding = "UTF-8", row.names = FALSE)

# delete topic 7 (Misc.) for ipi calculation
ipi = data.frame(date = tabrel_all$date,
                 ipi = rowSums(tabrel_all[,-c(1,8)]))
write.csv(ipi, file = "ipi.csv", fileEncoding = "UTF-8", row.names = FALSE)
sub = as.data.table(tabrel_all)
sub = rbind(cbind(sub[, c(1,2)], "German Politics", "Causal"),
            cbind(sub[, c(1,3)], "Raw Materials/News", "Causal"),
            cbind(sub[, c(1,4)], "Private Investment", "Consequences"),
            cbind(sub[, c(1,5)], "Financial Markets", "Consequences"),
            cbind(sub[, c(1,6)], "Emerging Markets", "Other"),
            cbind(sub[, c(1,7)], "Companies", "Consequences"),
            cbind(sub[, c(1,9)], "News", "Consequences"),
            cbind(sub[, c(1,10)], "Eurozone", "Causal"),
            cbind(sub[, c(1,11)], "Central Banks", "Causal"),
            use.names = FALSE)
colnames(sub) = c("date", "ipi", "Topic", "Type")

pdf("ipi.pdf", width = 10, height = 7)
print(ggplot(ipi) + aes(x = date, y = ipi) + geom_line() + xlab("") + ylab("IPI (Share)"))
print(ggplot(sub) +
        aes(x = date, y = ipi, group = Topic, color = Topic) +
        geom_line() +
        xlab("") + ylab("Share") + theme(legend.position = "top") +
        facet_wrap(~Type, nrow = 3))
dev.off()

beep(3)
