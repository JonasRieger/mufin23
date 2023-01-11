library(ldaPrototype)

K = c(upi = 14, ipi = 10)

for(type in c("upi", "ipi")){
  docs = readRDS(paste0("docs_", type, ".rds"))
  vocab = readRDS(paste0("vocab_", type, ".rds"))
  batch = LDABatch(docs, vocab, id = type, K = K[type],
                   resources = list(walltime = 60*60, memory = 4*1024))
  saveRDS(batch, paste0("batch_", type, ".rds"))
}
