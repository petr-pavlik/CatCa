PRECHODNA_TABA <- TABA_MARTIN[1:1135,]
upovs = list()
whole_upov = c()

STREAMS_NEW_NEW <- setdiff(PRECHODNA_TABA$FROM, PRECHODNA_TABA$TO)

upovs <- list()
for (i in 1:length(STREAMS_NEW_NEW)) {
  IN = c(); OUT = c() # vynulovat oba pomocne vektory
  j = 0
  IN = as.character(PRECHODNA_TABA[which(PRECHODNA_TABA$FROM == STREAMS_NEW_NEW[i]),]$FROM);IN
  OUT = as.character(PRECHODNA_TABA[which(PRECHODNA_TABA$FROM == STREAMS_NEW_NEW[i]),]$TO);OUT
  while(length(OUT) != 0) {
    j = j + 1
    whole_upov[j] = IN;whole_upov
    IN = OUT;IN
    OUT = as.character(PRECHODNA_TABA[which(PRECHODNA_TABA$FROM == as.character(OUT)),]$TO);OUT
  }
  if (length(whole_upov) == 0) {
    upovs[STREAMS_NEW_NEW[i]] = STREAMS_NEW_NEW[i]
  } else {
    upovs[[whole_upov[1]]] = whole_upov
  }
  whole_upov = c()
}

PRECHODNA_TABA <- as.data.frame(PRECHODNA_TABA)
names(PRECHODNA_TABA) <- c("FROM","TO")
PRECHODNA_TABA$FR_ORD = NA
PRECHODNA_TABA$TO_ORD = NA
#PRECHODNA_TABA_NEW <- PRECHODNA_TABA
PRECHODNA_TABA[which(as.character(PRECHODNA_TABA$FROM) %in% STREAMS_NEW), ]$FR_ORD <- 1
# PRECHODNA_TABA URCENI RADU FROM
for (i in 1:length(upovs)) {
  for (j in 1:length(upovs[[i]])) {
    PAT = upovs[[i]][j]
    LOKACE <- grep(upovs, pattern = PAT, fixed = TRUE) #
    VEC <- c()
    MAX <- c()
    m = 1
    for (l in LOKACE) {
      VEC <- upovs[[l]]
      MAX[m] <- match(PAT, VEC)
      m <- m + 1
    }
    PRECHODNA_TABA[which(PRECHODNA_TABA$FROM == upovs[[i]][j] ),]$FR_ORD <- max(MAX)
    #if (is.na(PRECHODNA_TABA$FR_ORD) == FALSE & PRECHODNA_TABA$FR_ORD < j)
  }
}
# PRECHODNA_TABA URCENI RADU TO
PRECHODNA_TABA <- as.data.table(PRECHODNA_TABA)
for (i in 1:nrow(PRECHODNA_TABA)) {
  if (PRECHODNA_TABA[i, ]$TO != "-9999") {
    PRECHODNA_TABA[i, ]$TO_ORD <- PRECHODNA_TABA[which(PRECHODNA_TABA$FROM == as.character(PRECHODNA_TABA[i,]$TO)), ]$FR_ORD
  } else {
    PRECHODNA_TABA[i, ]$TO_ORD <- "-9999"
  }
}
PRECHODNA_TABA$TO_ORD <- as.numeric(PRECHODNA_TABA$TO_ORD)

# PRODUKCE PRECHODNA_TABB
distances = upovs
PRECHODNA_TABB = data.table()
i = 1
for ( i in 1:length(distances) ) { # pro vsechny vetve
  TO = as.vector(distances[[i]])
  names(TO) = NULL
  WHICH = distances[[i]]
  TO = TO[length(TO)] # posledni recipient
  for ( j in 1:length(distances[[i]]) ) {
    FLOW = as.numeric(length(distances[[i]]):1)
  }
  tempPRECHODNA_TABB = data.table(FROM = WHICH, TO, DIST = FLOW)
  tempTABA_ALL <- data.frame(FROM = character(), TO = character(), DIST = numeric())
  for ( k in 1:nrow(tempPRECHODNA_TABB) ) {
    tempPRECHODNA_TABB_ELEMENT <- tempPRECHODNA_TABB[k,1]
    tempPRECHODNA_TABB_ELEMENT_DIST <- data.table(FROM = character(), TO = character(), DIST = numeric())
    tempPRECHODNA_TABB_ELEMENT_DIST_all <- data.table(FROM = character(), TO = character(), DIST = numeric())
    for ( l in k:nrow(tempPRECHODNA_TABB)) {
      VZD <- tempPRECHODNA_TABB[k,]$DIST - tempPRECHODNA_TABB[l,]$DIST
      tempPRECHODNA_TABB_ELEMENT_DIST <- tempPRECHODNA_TABB_ELEMENT_DIST[, .(FROM = tempPRECHODNA_TABB[k,]$FROM, TO = tempPRECHODNA_TABB[l,]$FROM, DIST = VZD)]
      tempPRECHODNA_TABB_ELEMENT_DIST_all <- rbind(tempPRECHODNA_TABB_ELEMENT_DIST_all, tempPRECHODNA_TABB_ELEMENT_DIST)
    }
    tempTABA_ALL <- rbind(tempTABA_ALL, tempPRECHODNA_TABB_ELEMENT_DIST_all)
  }
  PRECHODNA_TABB = rbind(PRECHODNA_TABB, tempTABA_ALL)
}
PRECHODNA_TABB <- unique(PRECHODNA_TABB)




