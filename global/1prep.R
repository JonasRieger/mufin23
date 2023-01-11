library(tosca)

# UPI
obj = readRDS("clean_updated_wirtschaft_unsicher.rds")
wl = makeWordlist(obj$text)
vocab = wl$words[wl$wordtable > 5]
saveRDS(vocab, "vocab_upi.rds")
docs = LDAprep(obj$text, vocab)
saveRDS(docs, "docs_upi.rds")

# IPI
obj = readRDS("clean_inflation_updated.rds")
wl = makeWordlist(obj$text)
vocab = wl$words[wl$wordtable > 5]
saveRDS(vocab, "vocab_ipi.rds")
docs = LDAprep(obj$text, vocab)
saveRDS(docs, "docs_ipi.rds")
