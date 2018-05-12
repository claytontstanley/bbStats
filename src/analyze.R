

library(data.table)
library(ggplot2)
library(readxl)
library(stringr)
options(width=600)

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

bTbl[, .SD
     ][!is.na(value)
     ][, num := hasChars(value, c("1", "2", '3', '4'))
     ][, den := grepl("B.*B.*B.*B", value) + hasChars(value, c('H', 'X'))
     ][, .(N=sum(num) / (.N - sum(den))), .(player)
     ][, cTbl[.SD, BA := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, num := grepl('1', value) * 1 + grepl('2', value) * 2 + grepl('3', value) * 3 + grepl('4', value) * 4
     ][, den := hasChars(value, c('1', '2', '3', '4', 'A', 'G'))
     ][, .(N=sum(num) / sum(den)), .(player)
     ][, cTbl[.SD, SLG := i.N, on=.(player)]
     ]


bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(hasChars(value, c('1', '2', '3', '4', 'H')) + grepl('B.*B.*B.*B', value)) / .N), .(player)
     ][, cTbl[.SD, OBP := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, num1 := sum(grepl('1', value) * 1 + grepl('2', value) * 2 + grepl('3', value) * 3 + grepl('4', value) * 4), .(player)
     ][, num2 := sum(hasChars(value, c('1', '2', '3', '4', 'H')) + grepl('B.*B.*B.*B', value)), .(player)
     ][, den1 := sum(hasChars(value, c('1', '2', '3', '4', 'A', 'G'))), .(player)
     ][, den2 := .N, .(player)
     ][, .(N=num1/den1 + num2/den2), .(player, num1, den1, num2, den2)
     ][, cTbl[.SD, OPS := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, num1 := 1 * grepl('1.*R', value) + 2 * grepl('2.*R', value) + 3 * grepl('3.*R', value) + 4 * grepl('4.*R', value)
     ][, den1 := grepl('[1234AG].*R', value)
     ][, num2 := grepl('[1234H].*R', value) + grepl('B.*B.*B.*B.*R', value)
     ][, den2 := grepl('R', value)
     ][, .(N=sum(num1) / sum(den1) + sum(num2) / sum(den2)), .(player)
     ][, cTbl[.SD, OPS_RISP := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(hasChars(value, 'R'))), .(player)
     ][, cTbl[.SD, `_1` := i.N, on=.(player)]
     ]


bTbl[, .SD
     ][!is.na(value)
     ][, num1 := 1 * grepl('P.*1', value) + 2 * grepl('P.*2', value) + 3 * grepl('P.*3', value) + 4 * grepl('P.*4', value)
     ][, den1 := grepl('P.*[1234AG]', value)
     ][, num2 := grepl('P.*[1234H]', value) + (countChars(value, 'B') == 4) * (countChars(value, 'P') == 1)
     ][, den2 := grepl('P', value)
     ][, .(N=sum(num1) / sum(den1) + sum(num2) / sum(den2)), .(player)
     ][, cTbl[.SD, OPS_Adv := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(hasChars(value, 'P'))), .(player)
     ][, cTbl[.SD, `_2` := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(countChars(value, 'P') - hasChars(value, 'P'))), .(player)
     ][, cTbl[.SD, `_3` := sprintf('+%s', i.N), on=.(player)]
     ]

allCharsCount <- function(value, x) {
	Reduce('*', lapply(1:length(x), function(i) countChars(value, names(x)[i]) == x[i]))
}

sumNums <- function(zTbl) {
	cNames = grep('^num', colnames(zTbl), value=T)
	zTbl[, cNames, with=F][, Reduce('+', .SD)]
}


getTB <- function(bTbl, BCount, KCount, SCount, FCount) {
	bTbl[, .SD
	     ][, num1 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '1'=1))
	     ][, num2 := 2 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '2'=1))
	     ][, num3 := 3 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '3'=1))
	     ][, num4 := 4 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '4'=1))
	     ][, .(N=sum(sumNums(.SD))), .(player)
	     ]
}

getAB <- function(bTbl, BCount, KCount, SCount, FCount) {
	bTbl[, .SD
	     ][, num1 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '1'=1))
	     ][, num2 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '2'=1))
	     ][, num3 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '3'=1))
	     ][, num4 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '4'=1))
	     ][, num5 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, 'A'=1))
	     ][, num6 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, 'G'=1))
	     ][, .(N=sum(sumNums(.SD))), .(player)
	     ]
}
	
getOB <- function(bTbl, BCount, KCount, SCount, FCount) {
	bTbl[, .SD
	     ][, num1 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '1'=1))
	     ][, num2 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '2'=1))
	     ][, num3 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '3'=1))
	     ][, num4 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '4'=1))
	     ][, num5 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, 'H'=1))
	     ][, .(N=sum(sumNums(.SD))), .(player)
	     ]
}

getNB <- function(bTbl, BCount, KCount, SCount, FCount) {
	bTbl[, .SD
	     ][, num1 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '1'=1))
	     ][, num2 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '2'=1))
	     ][, num3 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '3'=1))
	     ][, num4 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '4'=1))
	     ][, num5 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, 'H'=1))
	     ][, num6 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, 'A'=1))
	     ][, num7 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, 'G'=1))
	     ][, num8 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, 'X'=1))
	     ][, .(N=sum(sumNums(.SD))), .(player)
	     ]
}

bTbl[, .SD
     ][!is.na(value)
     ][, getTB(.SD, 0, 0, 0, 0)
     ][, cTbl[.SD, TB_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, getAB(.SD, 0, 0, 0, 0)
     ][, cTbl[.SD, AB_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, getOB(.SD, 0, 0, 0, 0)
     ][, cTbl[.SD, OB_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, getNB(.SD, 0, 0, 0, 0)
     ][, cTbl[.SD, `_4` := i.N, on=.(player)]
     ]

cTbl[, `0_0_OPS` := TB_0/AB_0 + OB_0/`_4`]

bTbl[, .SD
     ][!is.na(value)
     ][, getTB(.SD, 1, 0, 0, 0)
     ][, cTbl[.SD, TB_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, getAB(.SD, 1, 0, 0, 0) 
     ][, cTbl[.SD, AB_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, getOB(.SD, 1, 0, 0, 0)
     ][, cTbl[.SD, OB_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, getNB(.SD, 1, 0, 0, 0)
     ][, cTbl[.SD, `_5` := i.N, on=.(player)]
     ]

cTbl[, `1_0_OPS` := TB_1/AB_1 + OB_1/`_5`]

bTbl[, .SD
     ][!is.na(value)
     ][, getTB(.SD, 2, 0, 0, 0)
     ][, cTbl[.SD, TB_2 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, getAB(.SD, 2, 0, 0, 0) 
     ][, cTbl[.SD, AB_2 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, getOB(.SD, 2, 0, 0, 0)
     ][, cTbl[.SD, OB_2 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, getNB(.SD, 2, 0, 0, 0)
     ][, cTbl[.SD, `_6` := i.N, on=.(player)]
     ]

cTbl[, `2_0_OPS` := TB_2/AB_2 + OB_2/`_6`]

bTbl[, .SD
     ][!is.na(value)
     ][, getTB(.SD, 3, 0, 0, 0)
     ][, cTbl[.SD, TB_3 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, getAB(.SD, 3, 0, 0, 0) 
     ][, cTbl[.SD, AB_3 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, getOB(.SD, 3, 0, 0, 0)
     ][, cTbl[.SD, OB_3 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, getNB(.SD, 3, 0, 0, 0)
     ][, cTbl[.SD, `_7` := i.N, on=.(player)]
     ]

cTbl[, `3_0_OPS` := TB_3/AB_3 + OB_3/`_7`]





















