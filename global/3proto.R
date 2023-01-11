library(ldaPrototype)

for(type in c("upi", "ipi")){
  batch = readRDS(paste0("batch_", type, ".rds"))
  setFileDir(batch, type)
  proto = getPrototype(batch, pm.backend = "socket", ncpus = 4)
  saveRDS(proto, paste0("proto_", type, ".rds"))
}
