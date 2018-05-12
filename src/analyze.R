

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
	     ][, sum(sumNums(.SD))
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
	     ][, sum(sumNums(.SD))
	     ]
}
	
getOB <- function(bTbl, BCount, KCount, SCount, FCount) {
	bTbl[, .SD
	     ][, num1 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '1'=1))
	     ][, num2 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '2'=1))
	     ][, num3 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '3'=1))
	     ][, num4 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, '4'=1))
	     ][, num5 := 1 * allCharsCount(value, c(B=BCount, K=KCount, S=SCount, F=FCount, 'H'=1))
	     ][, sum(sumNums(.SD))
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
	     ][, sum(sumNums(.SD))
	     ]
}

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 0, 0, 0, 0)), .(player)
     ][, cTbl[.SD, TB_0_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 0, 0, 0, 0)), .(player)
     ][, cTbl[.SD, AB_0_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 0, 0, 0, 0)), .(player)
     ][, cTbl[.SD, OB_0_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 0, 0, 0, 0)), .(player)
     ][, cTbl[.SD, NB_0_0 := i.N, on=.(player)]
     ]

cTbl[, `0_0_OPS` := TB_0_0/AB_0_0 + OB_0_0/NB_0_0]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 1, 0, 0, 0)), .(player)
     ][, cTbl[.SD, TB_1_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 1, 0, 0, 0)), .(player)
     ][, cTbl[.SD, AB_1_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 1, 0, 0, 0)), .(player)
     ][, cTbl[.SD, OB_1_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 1, 0, 0, 0)), .(player)
     ][, cTbl[.SD, NB_1_0 := i.N, on=.(player)]
     ]

cTbl[, `1_0_OPS` := TB_1_0/AB_1_0 + OB_1_0/NB_1_0]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 2, 0, 0, 0)), .(player)
     ][, cTbl[.SD, TB_2_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 2, 0, 0, 0)), .(player) 
     ][, cTbl[.SD, AB_2_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 2, 0, 0, 0)), .(player)
     ][, cTbl[.SD, OB_2_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 2, 0, 0, 0)), .(player)
     ][, cTbl[.SD, NB_2_0 := i.N, on=.(player)]
     ]

cTbl[, `2_0_OPS` := TB_2_0/AB_2_0 + OB_2_0/NB_2_0]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 3, 0, 0, 0)), .(player)
     ][, cTbl[.SD, TB_3_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 3, 0, 0, 0)), .(player) 
     ][, cTbl[.SD, AB_3_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 3, 0, 0, 0)), .(player)
     ][, cTbl[.SD, OB_3_0 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 3, 0, 0, 0)), .(player)
     ][, cTbl[.SD, NB_3_0 := i.N, on=.(player)]
     ]

cTbl[, `3_0_OPS` := TB_3_0/AB_3_0 + OB_3_0/NB_3_0]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(allCharsCount(value, c(B=4, K=0, S=0, F=0)))), .(player)
     ][, cTbl[.SD, `_4` := NB_3_0 + i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 0, 1, 0, 0) + getTB(.SD, 0, 0, 1, 0) + getTB(.SD, 0, 0, 0, 1)), .(player)
     ][, cTbl[.SD, TB_0_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 0, 1, 0, 0) + getAB(.SD, 0, 0, 1, 0) + getAB(.SD, 0, 0, 0, 1)), .(player)
     ][, cTbl[.SD, AB_0_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 0, 1, 0, 0) + getOB(.SD, 0, 0, 1, 0) + getOB(.SD, 0, 0, 0, 1)), .(player)
     ][, cTbl[.SD, OB_0_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 0, 1, 0, 0) + getNB(.SD, 0, 0, 1, 0) + getNB(.SD, 0, 0, 0, 1)), .(player)
     ][, cTbl[.SD, NB_0_1 := i.N, on=.(player)]
     ]

cTbl[, `0_1_OPS` := TB_0_1/AB_0_1 + OB_0_1/NB_0_1]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 1, 1, 0, 0) + getTB(.SD, 1, 0, 1, 0) + getTB(.SD, 1, 0, 0, 1)), .(player)
     ][, cTbl[.SD, TB_1_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 1, 1, 0, 0) + getAB(.SD, 1, 0, 1, 0) + getAB(.SD, 1, 0, 0, 1)), .(player)
     ][, cTbl[.SD, AB_1_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 1, 1, 0, 0) + getOB(.SD, 1, 0, 1, 0) + getOB(.SD, 1, 0, 0, 1)), .(player)
     ][, cTbl[.SD, OB_1_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 1, 1, 0, 0) + getNB(.SD, 1, 0, 1, 0) + getNB(.SD, 1, 0, 0, 1)), .(player)
     ][, cTbl[.SD, NB_1_1 := i.N, on=.(player)]
     ]

cTbl[, `1_1_OPS` := TB_1_1/AB_1_1 + OB_1_1/NB_1_1]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 2, 1, 0, 0) + getTB(.SD, 2, 0, 1, 0) + getTB(.SD, 2, 0, 0, 1)), .(player)
     ][, cTbl[.SD, TB_2_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 2, 1, 0, 0) + getAB(.SD, 2, 0, 1, 0) + getAB(.SD, 2, 0, 0, 1)), .(player)
     ][, cTbl[.SD, AB_2_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 2, 1, 0, 0) + getOB(.SD, 2, 0, 1, 0) + getOB(.SD, 2, 0, 0, 1)), .(player)
     ][, cTbl[.SD, OB_2_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 2, 1, 0, 0) + getNB(.SD, 2, 0, 1, 0) + getNB(.SD, 2, 0, 0, 1)), .(player)
     ][, cTbl[.SD, NB_2_1 := i.N, on=.(player)]
     ]

cTbl[, `2_1_OPS` := TB_2_1/AB_2_1 + OB_2_1/NB_2_1]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 3, 1, 0, 0) + getTB(.SD, 3, 0, 1, 0) + getTB(.SD, 3, 0, 0, 1)), .(player)
     ][, cTbl[.SD, TB_3_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 3, 1, 0, 0) + getAB(.SD, 3, 0, 1, 0) + getAB(.SD, 3, 0, 0, 1)), .(player)
     ][, cTbl[.SD, AB_3_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 3, 1, 0, 0) + getOB(.SD, 3, 0, 1, 0) + getOB(.SD, 3, 0, 0, 1)), .(player)
     ][, cTbl[.SD, OB_3_1 := i.N, on=.(player)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 3, 1, 0, 0) + getNB(.SD, 3, 0, 1, 0) + getNB(.SD, 3, 0, 0, 1)), .(player)
     ][, cTbl[.SD, NB_3_1 := i.N, on=.(player)]
     ]

cTbl[, `3_1_OPS` := TB_3_1/AB_3_1 + OB_3_1/NB_3_1]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(allCharsCount(value, c(B=4, K=1, S=0, F=0)) + allCharsCount(value, c(B=4, K=0, S=1, F=0)) + allCharsCount(value, c(B=4, K=0, S=0, F=1)))), .(player)
     ][, cTbl[.SD, `_5` := NB_3_1 + i.N, on=.(player)]
     ]

cTbl

















