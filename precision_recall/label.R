library(data.table)

tab = fread("labeled.csv")
tab[, text := gsub("\"+", "\"", text)]
tab[, title := gsub("\"+", "\"", title)]

if(!any(c(is.na(tab$inflation), is.na(tab$uncertainty))))
  stop("All labeled!")
if(!identical(which(is.na(tab$inflation)), which(is.na(tab$uncertainty))))
  stop("Please repair file!")
ind = which(is.na(tab$inflation))

for(i in which(is.na(tab$inflation))){
  cat(tab[i,outlet], " - ", as.character(tab[i,date]), "\n",
      gsub("\"+", "\"", tab[i,title]), "\n", sep = "")
  text = tm::stripWhitespace(tab[i,text])
  text = gsub("unsicher", "UNSICHER", ignore.case = TRUE, text)
  text = gsub("inflation", "INFLATION", ignore.case = TRUE, text)
  text = gsub("preissteigerung", "PREISSTEIGERUNG", ignore.case = TRUE, text)
  text = gsub("geldentwertung", "GELDENTWERTUNG", ignore.case = TRUE, text)
  text = gsub("teuerung", "TEUERUNG", ignore.case = TRUE, text)
  for(j in seq(1, nchar(text), 1000)){
    cat(substr(text, j, min(j+999, nchar(text))))
    readline()
  }
  unc = menu(c("Yes", "No"), title = "Uncertainty?")
  inf = menu(c("Yes", "No"), title = "Inflation?")
  tab[i, inflation := inf == 1L]
  tab[i, uncertainty := unc == 1L]
  fwrite(tab, "labeled.csv")
}
