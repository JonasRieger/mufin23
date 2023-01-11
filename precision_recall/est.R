library(data.table)

tab = fread("labeled.csv")
upi = fread("upi.csv")
ipi = fread("ipi.csv")

n1ipi = table(tab$inflation[tab$id %in% ipi[group == "ipi", id]])
n2ipi = table(tab$inflation[tab$id %in% ipi[group == "outer", id]])
preipi = n1ipi[2]/sum(n1ipi)
recipi = n1ipi[2]/(n1ipi[2] + n2ipi[2])
recipi = n1ipi[2]*53652/100/(n1ipi[2]*53652/100 + n2ipi[2]*(2813454+39554)/(1800+39))

n1upi = table(tab$uncertainty[tab$id %in% upi[group == "upi", id]])
n2upi = table(tab$uncertainty[tab$id %in% upi[group == "outer", id]])
preupi = n1upi[2]/sum(n1upi)
recupi = n1upi[2]/(n1upi[2] + n2upi[2])
recupi = n1upi[2]*39554/100/(n1upi[2]*39554/100 + n2upi[2]*(2813454+53652)/(1800+53))

cat("Precision-95KI IPI: ",
    preipi + c(-1,1)*qnorm(0.975) * sqrt(preipi*(1-preipi)/(sum(n1ipi))),
    "\nRecall-95KI IPI: ",
    recipi + c(-1,1)*qnorm(0.975) * sqrt(recipi*(1-recipi)/(n1ipi[2] + n2ipi[2])),
    "\nPrecision-95KI UPI: ",
    preupi + c(-1,1)*qnorm(0.975) * sqrt(preupi*(1-preupi)/(sum(n1upi))),
    "\nRecall-95KI UPI: ",
    recupi + c(-1,1)*qnorm(0.975) * sqrt(recupi*(1-recupi)/(n1upi[2] + n2upi[2])))
