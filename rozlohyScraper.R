library(rvest)
library(raster)
library(data.table)

# KONTROLA HOTOVYCH A CHYBEJICI PLOCHY ----
CHARS <- readRDS(file.path(.datadir, 'chmu', 'chars_dopl.rds'))
MISSING <- as.character(CHARS[is.na(A) == TRUE,]$UPOV_ID)

# SCRAPE NA JEZERA Z WEBU ----

LAKES <- grep(TABA$FROM, pattern = "_J")
NAMES_LAKES <- TABA[LAKES]$FROM
for (i in 1:NAMES_LAKES) {
  i = 1
  ID <- NAMES_LAKES[i]
  URL <- paste("http://heis.vuv.cz/data/webmap/isapi.dll?MAP=5881&MU=CS&TYPE=fulltext&GEN=LSTD&TS=36&QY=%7B36%7DC%5BUPOV_ID%5DE",ID, sep = "")
  ENC <- guess_encoding(URL)[1,1]
  AREA <- URL %>%
    read_html(encoding = ENC) %>%
    html_nodes(xpath = "/html/body/div/div[4]/table[1]") %>%
    html_table(header = NA, trim = TRUE, fill = TRUE, dec = ",") %>% as.data.table()
  A <- AREA[AREA$X1 == "Plocha povodí, km2:",2]
  CHARS[which(CHARS$UPOV_ID == ID),]$A <- gsub(pattern = ",",replacement = ".", x = A)
}
#plot(UPOVS[LAKES,])

# SCRAPE NA PLOCHU REK Z WEBU ----

ID <- "HVL_1130"
URL <- paste("http://heis.vuv.cz/data/webmap/isapi.dll?MAP=5881&MU=CS&TYPE=fulltext&GEN=LSTD&TS=35&QY=%7B35%7DC%5BUPOV_ID%5DE",ID, sep = "")
ENC <- guess_encoding(URL)[1,1]
AREA <- URL %>%
  read_html(encoding = ENC) %>%
  html_nodes(xpath = "/html/body/div/div[4]/table[1]") %>%
  html_table(header = NA, trim = TRUE, fill = TRUE, dec = ",") %>% as.data.table()
AREA[AREA$X1 == "Plocha povodí, km2:",2]

# CHYBEJICI DOPOCTU POMOCI FCE AREA Z RASTRU
MISSING_A <- as.character(CHARS[is.na(A) == TRUE,]$UPOV_ID)
for (i in 1:length(MISSING_A)) {
  CHARS[UPOV_ID == MISSING_A[i]]$A <- area(UPOVS[which(UPOVS$UPOV_ID == MISSING_A[i]),])/10^6
}


# CHYBEJICI PRECIPITACE ----
MISSING_Pa <- as.character(CHARS[is.na(Pa) == TRUE,]$UPOV_ID)
for (i in 1:length(MISSING_Pa)) {
  i = 1
  UID <- MISSING_Pa[i]
  UPOV_MET <- readRDS(file.path("C:/Users/PetrP/Documents/Kalibrace/",paste0("meteo/",UID, ".Rds")))
  UPOV_MET <- UPOV_MET[DTM >= "1982-11-01" & DTM <= "2010-10-31"]
  for (i in 1:28) {
    UPOV_MET$P/(i*365)
  }
  INTERVAL.mean <- c()
  INTERVAL.mean
  CHARS[UPOV_ID == MISSING_Pa[i]]$Pa <- mean(UPOV_MET)
}

# CHYBEJICI PRUTOKY ----
MISSING_Qa <- as.character(CHARS[is.na(Qa) == TRUE,]$UPOV_ID)
for (i in 1:length(MISSING_Qa)) {
  CHARS[UPOV_ID == MISSING_Qa[i]]$Qa <- 0.75*mean(as.numeric(CHARS[which(UPOV_ID == MISSING_Qa[i]),14:26]))
  #  UPOVS[which(UPOVS$UPOV_ID == MISSING_Qa[i]),]
}

# SCRAPE NA NAVAZUJICI UPOVY ----
NEXT_UPOV <- as.character(TABA$TO)
for (i in 1:length(NEXT_UPOV)) {
  ID <- as.character(NEXT_UPOV[i])
  URL <- paste("http://heis.vuv.cz/data/webmap/isapi.dll?MAP=5881&MU=CS&TYPE=fulltext&GEN=LSTD&TS=35&QY=%7B35%7DC%5BUPOV_ID%5DE",ID, sep = "")
  ENC <- guess_encoding(URL)[1,1]
  TO <- URL %>%
    read_html(encoding = ENC) %>%
    html_nodes(xpath = "/html/body/div/div[4]/table[1]") %>%
    html_table(header = NA, trim = TRUE, fill = TRUE, dec = ",") %>% as.data.table()
  AREA[AREA$X1 == "Plocha povodí, km2:", 2]
  NEXT_UPOV ==
}







