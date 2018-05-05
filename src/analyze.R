

library(data.table)
library(ggplot2)
library(readxl)
library(stringr)

trimWhite <- function(str) {
	res = str
	res = gsub('^\\s+', '', res)
	res = gsub('\\s+$', '', res)
	res
}


aTbl = read_excel("../raw/Book1.xls")


aTbl = as.data.table(aTbl)
aTbl[,1]

db = list()

aTbl[, .SD
     ][, .(rowOff1=which(grepl("2015-17 Lone Stars Avg", `Lone Stars Baseball`)))
     ][, {db$rowOff1 <<- rowOff1}
     ]

aTbl[, .SD
     ][1:db$rowOff1 - 1
     ][1 
     ][, .(colOff1=which(as.vector(unlist(.SD)) == "PA"))
     ][, {db$colOff1 <<- colOff1}
     ]

aTbl[, .SD
     ][1:(db$rowOff1-1)
     ][, 1:(db$colOff1-1)
     ][2:.N
     ][!is.na(`Lone Stars Baseball`)
     ][, setnames(.SD, 'Lone Stars Baseball', 'player')
     ][, lapply(.SD, as.character)
     ][, melt(.SD, id.vars='player')
     ][, variable := str_extract(variable, '[0-9]+$')
     ][, value := trimWhite(value)
     ][, {bTbl <<- copy(.SD); .SD}
     ]


cTbl = data.table(player=bTbl[, unique(player)])
cTbl

bTbl[, .SD
     ][!is.na(value)
     ][, .N, .(player)
     ][, cTbl[.SD, PA := i.N, on=.(player)]
     ]

bTbl
cTbl

bTbl[, .SD
     ][!is.na(value)
     ][!grepl("B.*B.*B.*B", value)
     ][!grepl("H", value)
     ][!grepl("X", value)
     ][, .N, .(player)
     ][, cTbl[.SD, AB := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("1", value) * 1 + grepl("2", value) * 2 + grepl("3", value) * 3 + grepl("4", value) * 4)), .(player)
     ][, cTbl[.SD, TB := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("1", value) * 1 + grepl("2", value) * 1 + grepl("3", value) * 1 + grepl("4", value) * 1)), .(player)
     ][, cTbl[.SD, H := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("1", value) * 0 + grepl("2", value) * 1 + grepl("3", value) * 1 + grepl("4", value) * 1)), .(player)
     ][, cTbl[.SD, XBH := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("B.*B.*B.*B", value))), .(player)
     ][, cTbl[.SD, BB := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("(S|SR|SC|SP|SCR|SRC|SRP|SPR)$", value))), .(player)
     ][, cTbl[.SD, Ks := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("(K|KR|KC|KP|KCR|KRC|KRP|KPR)$", value))), .(player)
     ][, cTbl[.SD, Kc := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("H", value))), .(player)
     ][, cTbl[.SD, HBP := i.N, on=.(player)]
     ]


bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("X", value))), .(player)
     ][, cTbl[.SD, SAC := i.N, on=.(player)]
     ]


countChars <- function(value, x) {
	Reduce("+", lapply(x, function(i) str_count(value, i)))
}

hasChars <- function(value, x) {
	Reduce("+", lapply(x, function(i) str_count(value, i) > 0))
}


bTbl[, .SD
     ][!is.na(value)
     ][, countChars(value, c("B", "B"))
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(countChars(value, c("B", "K", "S", "F", "A", "G", "X", "H", "1", "2", "3", "4")))), .(player)
     ][, cTbl[.SD, PPerPA := i.N / PA, on=.(player)]
     ]


bTbl[, .SD
     ][!is.na(value)
     ][, num := countChars(value, "F") + hasChars(value, c('1', '2', '3', '4', 'G', 'A', 'X'))
     ][, den := countChars(value, c('F', 'S')) + hasChars(value, c('1', '2', '3', '4', 'G', 'A', 'X'))
     ][, N := sum(num) / sum(den), .(player)
     ][, cTbl[.SD, ConPerc := i.N, on=.(player)]
     ]


bTbl[, .SD
     ][!is.na(value)
     ][, num := countChars(value, "S")
     ][, den := nchar(value) - countChars(value, c('C', 'P', 'R'))
     ][, N := sum(num) / sum(den), .(player)
     ][, cTbl[.SD, SwStrPerc := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, N := sum(hasChars(value, "G")) / sum(hasChars(value, "A")), .(player)
     ][, cTbl[.SD, GOPerAO := i.N, on=.(player)]
     ]

cTbl
