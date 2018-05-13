

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
     ][, .(rowOff2=which(grepl("2015-17 Opp Team Avg", `Lone Stars Baseball`)))
     ][, {db$rowOff2 <<- rowOff2}
     ]

aTbl[, .SD
     ][, .(rowOff3=which(grepl("Lone Stars Runs", `Lone Stars Baseball`)))
     ][, {db$rowOff3 <<- rowOff3}
     ]

aTbl[, .SD
     ][1:db$rowOff1 - 1
     ][1 
     ][, .(colOff1=which(as.vector(unlist(.SD)) == "PA") - 1)
     ][, {db$colOff1 <<- colOff1}
     ]

rollForward <- function(x) {
	zTbl = data.table(player=x, rowid=1:length(x))
	pTbl = zTbl[!is.na(player)] 
	pTbl[zTbl, .(player=x.player, rowid=i.rowid), on=.(rowid), roll=Inf
	     ][, player
	     ]
}

aTbl[, .SD
     ][, 1:(db$colOff1-1)
     ][, rbind(.SD[2:(db$rowOff1-1)][, year := 1],
	       .SD[(db$rowOff2+1):(db$rowOff3-1)][, year := 2])
#     ][!is.na(`Lone Stars Baseball`)
     ][, setnames(.SD, 'Lone Stars Baseball', 'player')
     ][, lapply(.SD, as.character)
     ][, player := rollForward(player)
     ][, melt(.SD, id.vars=c('player', 'year'))
     ][, variable := str_extract(variable, '[0-9]+$')
     ][, value := trimWhite(value)
     ][, {bTbl <<- copy(.SD); .SD}
     ]

bTbl[, gID := .GRP, .(player, year)]
cTbl = bTbl[, unique(.SD, by='gID')][, .(player, year, gID)]

bTbl[, .SD
     ][!is.na(value)
     ][, .N, .(gID)
     ][, cTbl[.SD, PA := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][!grepl("B.*B.*B.*B", value)
     ][!grepl("H", value)
     ][!grepl("X", value)
     ][, .N, .(gID)
     ][, cTbl[.SD, AB := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("1", value) * 1 + grepl("2", value) * 2 + grepl("3", value) * 3 + grepl("4", value) * 4)), .(gID)
     ][, cTbl[.SD, TB := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("1", value) * 1 + grepl("2", value) * 1 + grepl("3", value) * 1 + grepl("4", value) * 1)), .(gID)
     ][, cTbl[.SD, H := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("1", value) * 0 + grepl("2", value) * 1 + grepl("3", value) * 1 + grepl("4", value) * 1)), .(gID)
     ][, cTbl[.SD, XBH := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("B.*B.*B.*B", value))), .(gID)
     ][, cTbl[.SD, BB := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("(S|SR|SC|SP|SCR|SRC|SRP|SPR)$", value))), .(gID)
     ][, cTbl[.SD, Ks := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("(K|KR|KC|KP|KCR|KRC|KRP|KPR)$", value))), .(gID)
     ][, cTbl[.SD, Kc := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("H", value))), .(gID)
     ][, cTbl[.SD, HBP := i.N, on=.(gID)]
     ]


bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(grepl("X", value))), .(gID)
     ][, cTbl[.SD, SAC := i.N, on=.(gID)]
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
     ][, .(N=sum(countChars(value, c("B", "K", "S", "F", "A", "G", "X", "H", "1", "2", "3", "4")))), .(gID)
     ][, cTbl[.SD, PPerPA := i.N / PA, on=.(gID)]
     ]


bTbl[, .SD
     ][!is.na(value)
     ][, num := countChars(value, "F") + hasChars(value, c('1', '2', '3', '4', 'G', 'A', 'X'))
     ][, den := countChars(value, c('F', 'S')) + hasChars(value, c('1', '2', '3', '4', 'G', 'A', 'X'))
     ][, N := sum(num) / sum(den), .(gID)
     ][, cTbl[.SD, ConPerc := i.N, on=.(gID)]
     ]


bTbl[, .SD
     ][!is.na(value)
     ][, num := countChars(value, "S")
     ][, den := nchar(value) - countChars(value, c('C', 'P', 'R'))
     ][, N := sum(num) / sum(den), .(gID)
     ][, cTbl[.SD, SwStrPerc := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, N := sum(hasChars(value, "G")) / sum(hasChars(value, "A")), .(gID)
     ][, cTbl[.SD, GOPerAO := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, num := hasChars(value, c("1", "2", '3', '4'))
     ][, den := grepl("B.*B.*B.*B", value) + hasChars(value, c('H', 'X'))
     ][, .(N=sum(num) / (.N - sum(den))), .(gID)
     ][, cTbl[.SD, BA := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, num := grepl('1', value) * 1 + grepl('2', value) * 2 + grepl('3', value) * 3 + grepl('4', value) * 4
     ][, den := hasChars(value, c('1', '2', '3', '4', 'A', 'G'))
     ][, .(N=sum(num) / sum(den)), .(gID)
     ][, cTbl[.SD, SLG := i.N, on=.(gID)]
     ]


bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(hasChars(value, c('1', '2', '3', '4', 'H')) + grepl('B.*B.*B.*B', value)) / .N), .(gID)
     ][, cTbl[.SD, OBP := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, num1 := sum(grepl('1', value) * 1 + grepl('2', value) * 2 + grepl('3', value) * 3 + grepl('4', value) * 4), .(gID)
     ][, num2 := sum(hasChars(value, c('1', '2', '3', '4', 'H')) + grepl('B.*B.*B.*B', value)), .(gID)
     ][, den1 := sum(hasChars(value, c('1', '2', '3', '4', 'A', 'G'))), .(gID)
     ][, den2 := .N, .(gID)
     ][, .(N=num1/den1 + num2/den2), .(gID, num1, den1, num2, den2)
     ][, cTbl[.SD, OPS := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, num1 := 1 * grepl('1.*R', value) + 2 * grepl('2.*R', value) + 3 * grepl('3.*R', value) + 4 * grepl('4.*R', value)
     ][, den1 := grepl('[1234AG].*R', value)
     ][, num2 := grepl('[1234H].*R', value) + grepl('B.*B.*B.*B.*R', value)
     ][, den2 := grepl('R', value)
     ][, .(N=sum(num1) / sum(den1) + sum(num2) / sum(den2)), .(gID)
     ][, cTbl[.SD, OPS_RISP := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(hasChars(value, 'R'))), .(gID)
     ][, cTbl[.SD, `_1` := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, num1 := 1 * grepl('P.*1', value) + 2 * grepl('P.*2', value) + 3 * grepl('P.*3', value) + 4 * grepl('P.*4', value)
     ][, den1 := grepl('P.*[1234AG]', value)
     ][, num2 := grepl('P.*[1234H]', value) + (countChars(value, 'B') == 4) * (countChars(value, 'P') == 1)
     ][, den2 := grepl('P', value)
     ][, .(N=sum(num1) / sum(den1) + sum(num2) / sum(den2)), .(gID)
     ][, cTbl[.SD, OPS_Adv := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(hasChars(value, 'P'))), .(gID)
     ][, cTbl[.SD, `_2` := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(countChars(value, 'P') - hasChars(value, 'P'))), .(gID)
     ][, cTbl[.SD, `_3` := sprintf('+%s', i.N), on=.(gID)]
     ]

allCharsCount <- function(value, x) {
	ix = which(x != '.')
	Reduce('*', lapply(ix, function(i) countChars(value, names(x)[i]) == x[i]))
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
     ][, .(N=getTB(.SD, 0, 0, 0, 0)), .(gID)
     ][, cTbl[.SD, TB_0_0 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 0, 0, 0, 0)), .(gID)
     ][, cTbl[.SD, AB_0_0 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 0, 0, 0, 0)), .(gID)
     ][, cTbl[.SD, OB_0_0 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 0, 0, 0, 0)), .(gID)
     ][, cTbl[.SD, NB_0_0 := i.N, on=.(gID)]
     ]

cTbl[, `0_0_OPS` := TB_0_0/AB_0_0 + OB_0_0/NB_0_0]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 1, 0, 0, 0)), .(gID)
     ][, cTbl[.SD, TB_1_0 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 1, 0, 0, 0)), .(gID)
     ][, cTbl[.SD, AB_1_0 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 1, 0, 0, 0)), .(gID)
     ][, cTbl[.SD, OB_1_0 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 1, 0, 0, 0)), .(gID)
     ][, cTbl[.SD, NB_1_0 := i.N, on=.(gID)]
     ]

cTbl[, `1_0_OPS` := TB_1_0/AB_1_0 + OB_1_0/NB_1_0]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 2, 0, 0, 0)), .(gID)
     ][, cTbl[.SD, TB_2_0 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 2, 0, 0, 0)), .(gID) 
     ][, cTbl[.SD, AB_2_0 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 2, 0, 0, 0)), .(gID)
     ][, cTbl[.SD, OB_2_0 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 2, 0, 0, 0)), .(gID)
     ][, cTbl[.SD, NB_2_0 := i.N, on=.(gID)]
     ]

cTbl[, `2_0_OPS` := TB_2_0/AB_2_0 + OB_2_0/NB_2_0]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 3, 0, 0, 0)), .(gID)
     ][, cTbl[.SD, TB_3_0 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 3, 0, 0, 0)), .(gID) 
     ][, cTbl[.SD, AB_3_0 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 3, 0, 0, 0)), .(gID)
     ][, cTbl[.SD, OB_3_0 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 3, 0, 0, 0)), .(gID)
     ][, cTbl[.SD, NB_3_0 := i.N, on=.(gID)]
     ]

cTbl[, `3_0_OPS` := TB_3_0/AB_3_0 + OB_3_0/NB_3_0]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(allCharsCount(value, c(B=4, K=0, S=0, F=0)))), .(gID)
     ][, cTbl[.SD, `_4` := NB_3_0 + i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 0, 1, 0, 0) + getTB(.SD, 0, 0, 1, 0) + getTB(.SD, 0, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, TB_0_1 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 0, 1, 0, 0) + getAB(.SD, 0, 0, 1, 0) + getAB(.SD, 0, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, AB_0_1 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 0, 1, 0, 0) + getOB(.SD, 0, 0, 1, 0) + getOB(.SD, 0, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, OB_0_1 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 0, 1, 0, 0) + getNB(.SD, 0, 0, 1, 0) + getNB(.SD, 0, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, NB_0_1 := i.N, on=.(gID)]
     ]

cTbl[, `0_1_OPS` := TB_0_1/AB_0_1 + OB_0_1/NB_0_1]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 1, 1, 0, 0) + getTB(.SD, 1, 0, 1, 0) + getTB(.SD, 1, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, TB_1_1 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 1, 1, 0, 0) + getAB(.SD, 1, 0, 1, 0) + getAB(.SD, 1, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, AB_1_1 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 1, 1, 0, 0) + getOB(.SD, 1, 0, 1, 0) + getOB(.SD, 1, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, OB_1_1 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 1, 1, 0, 0) + getNB(.SD, 1, 0, 1, 0) + getNB(.SD, 1, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, NB_1_1 := i.N, on=.(gID)]
     ]

cTbl[, `1_1_OPS` := TB_1_1/AB_1_1 + OB_1_1/NB_1_1]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 2, 1, 0, 0) + getTB(.SD, 2, 0, 1, 0) + getTB(.SD, 2, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, TB_2_1 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 2, 1, 0, 0) + getAB(.SD, 2, 0, 1, 0) + getAB(.SD, 2, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, AB_2_1 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 2, 1, 0, 0) + getOB(.SD, 2, 0, 1, 0) + getOB(.SD, 2, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, OB_2_1 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 2, 1, 0, 0) + getNB(.SD, 2, 0, 1, 0) + getNB(.SD, 2, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, NB_2_1 := i.N, on=.(gID)]
     ]

cTbl[, `2_1_OPS` := TB_2_1/AB_2_1 + OB_2_1/NB_2_1]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 3, 1, 0, 0) + getTB(.SD, 3, 0, 1, 0) + getTB(.SD, 3, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, TB_3_1 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 3, 1, 0, 0) + getAB(.SD, 3, 0, 1, 0) + getAB(.SD, 3, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, AB_3_1 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 3, 1, 0, 0) + getOB(.SD, 3, 0, 1, 0) + getOB(.SD, 3, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, OB_3_1 := i.N, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 3, 1, 0, 0) + getNB(.SD, 3, 0, 1, 0) + getNB(.SD, 3, 0, 0, 1)), .(gID)
     ][, cTbl[.SD, NB_3_1 := i.N, on=.(gID)]
     ]

cTbl[, `3_1_OPS` := TB_3_1/AB_3_1 + OB_3_1/NB_3_1]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=sum(allCharsCount(value, c(B=4, K=1, S=0, F=0)) + allCharsCount(value, c(B=4, K=0, S=1, F=0)) + allCharsCount(value, c(B=4, K=0, S=0, F=1)))), .(gID)
     ][, cTbl[.SD, `_5` := NB_3_1 + i.N, on=.(gID)]
     ]

cTbl

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 0, '.', '.', '.')), .(gID)
     ][, cTbl[.SD, TB_0_2 := i.N - TB_0_0 - TB_0_1, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 0, '.', '.', '.')), .(gID)
     ][, cTbl[.SD, AB_0_2 := i.N - AB_0_0 - AB_0_1, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 0, '.', '.', '.')), .(gID)
     ][, cTbl[.SD, OB_0_2 := i.N - OB_0_0 - OB_0_1, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 0, '.', '.', '.')), .(gID)
     ][, cTbl[.SD, NB_0_2 := i.N - NB_0_0 - NB_0_1 + (Ks + Kc) / 4, on=.(gID)]
     ]

cTbl[, `0_2_OPS` := TB_0_2/AB_0_2 + OB_0_2/NB_0_2]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 1, '.', '.', '.')), .(gID)
     ][, cTbl[.SD, TB_1_2 := i.N - TB_1_0 - TB_1_1, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 1, '.', '.', '.')), .(gID)
     ][, cTbl[.SD, AB_1_2 := i.N - AB_1_0 - AB_1_1, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 1, '.', '.', '.')), .(gID)
     ][, cTbl[.SD, OB_1_2 := i.N - OB_1_0 - OB_1_1, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 1, '.', '.', '.')), .(gID)
     ][, cTbl[.SD, NB_1_2 := i.N - NB_1_0 - NB_1_1 + (Ks + Kc) / 4, on=.(gID)]
     ]

cTbl[, `1_2_OPS` := TB_1_2/AB_1_2 + OB_1_2/NB_1_2]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 2, '.', '.', '.')), .(gID)
     ][, cTbl[.SD, TB_2_2 := i.N - TB_2_0 - TB_2_1, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 2, '.', '.', '.')), .(gID)
     ][, cTbl[.SD, AB_2_2 := i.N - AB_2_0 - AB_2_1, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 2, '.', '.', '.')), .(gID)
     ][, cTbl[.SD, OB_2_2 := i.N - OB_2_0 - OB_2_1, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 2, '.', '.', '.')), .(gID)
     ][, cTbl[.SD, NB_2_2 := i.N - NB_2_0 - NB_2_1 + (Ks + Kc) / 4, on=.(gID)]
     ]

cTbl[, `2_2_OPS` := TB_2_2/AB_2_2 + OB_2_2/NB_2_2]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getTB(.SD, 3, '.', '.', '.')), .(gID)
     ][, cTbl[.SD, TB_3_2 := i.N - TB_3_0 - TB_3_1, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getAB(.SD, 3, '.', '.', '.')), .(gID)
     ][, cTbl[.SD, AB_3_2 := i.N - AB_3_0 - AB_3_1, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getOB(.SD, 3, '.', '.', '.') + sum(grepl('B.*B.*B.*B', value))), .(gID)
     ][, cTbl[.SD, OB_3_2 := i.N - OB_3_0 - OB_3_1, on=.(gID)]
     ]

bTbl[, .SD
     ][!is.na(value)
     ][, .(N=getNB(.SD, 3, '.', '.', '.') + sum(grepl('B.*B.*B.*B', value))), .(gID)
     ][, cTbl[.SD, NB_3_2 := i.N - NB_3_0 - NB_3_1 + (Ks + Kc) / 4, on=.(gID)]
     ]

cTbl[, `3_2_OPS` := TB_3_2/AB_3_2 + OB_3_2/NB_3_2]

cTbl[, !(1:15)]
cTbl[, 1:45]












