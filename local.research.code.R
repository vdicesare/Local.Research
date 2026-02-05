library(parallel)
library(tidyverse)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(reshape2)
library(readxl)
library(ggplot2)
library(ggcorrplot)
library(UpSetR)
library(viridisLite)
library(sf)
library(countrycode)
library(cld2)
library("FactoMineR")
library("factoextra")
load("~/Desktop/Local.Research/local.research.data.RData")


### CITS, REFS & PUBS DATA MINING
# read Dimensions file
df <- read.csv2("~/Desktop/Local.Research/dataset_journals.csv", col.names = c("pubs.refs.cits", "source.type", "journal.id", "journal.name", "issn", "eissn", "publisher", "country", "count"))

# subset journals source type
df.journals <- subset(df, df$source.type == "Journal", select = c("pubs.refs.cits", "journal.id", "journal.name", "issn", "eissn", "publisher", "country", "count"))

# convert pubs.refs.cits variable to factor
df.journals$pubs.refs.cits <- as.factor(df.journals$pubs.refs.cits)

# aggregate all variables by journal
df.journals.aggr <- df.journals %>% group_by(journal.id, journal.name, country, pubs.refs.cits) %>% 
  summarise(count = sum(count), .groups = 'drop') %>%
  as.data.frame()

# reshape to wide format in order to separate pubs.refs.cits into individual variables pubs, refs and cits
df.journals.wide <- dcast(df.journals.aggr, journal.id + journal.name + country ~ pubs.refs.cits)

# sum total cits, pubs and refs per journal
df.journals.wide <- df.journals.wide %>%
  group_by(journal.id) %>%
  mutate(cits.n = sum(cits, na.rm = TRUE))

df.journals.wide <- df.journals.wide %>%
  group_by(journal.id) %>%
  mutate(pubs.n = sum(pubs, na.rm = TRUE))

df.journals.wide <- df.journals.wide %>%
  group_by(journal.id) %>%
  mutate(refs.n = sum(refs, na.rm = TRUE))

# compute proportions per journals' cits and refs
df.journals.wide <- df.journals.wide %>%
  group_by(journal.id) %>%
  mutate(cits.prop = cits / cits.n)
df.journals.wide$cits.prop <- sprintf("%.7f", df.journals.wide$cits.prop)

df.journals.wide <- df.journals.wide %>%
  group_by(journal.id) %>%
  mutate(refs.prop = refs / refs.n)
df.journals.wide$refs.prop <- sprintf("%.7f", df.journals.wide$refs.prop)

# count total countries per journal's cits and refs
df.journals.wide <- df.journals.wide %>%
  group_by(journal.id) %>%
  mutate(cits.country.n = n_distinct(country[!is.na(cits)])) %>%
  ungroup()

df.journals.wide <- df.journals.wide %>%
  group_by(journal.id) %>%
  mutate(refs.country.n = n_distinct(country[!is.na(refs)])) %>%
  ungroup()

# set cut-off threshold at >= 30 publications in 3 years (2017-2019 period)
df.journals.final <- filter(df.journals.wide, pubs.n >= 30)

# filter all rows per journal to keep only the countries with maximum cits and refs values
df.journals.max.cits <- df.journals.final %>%
  group_by(journal.id) %>%
  filter(cits == max(cits, na.rm = TRUE)) %>%
  ungroup()
df.journals.max.cits <- select(df.journals.max.cits, c(journal.id, journal.name, country, cits, cits.n, cits.prop, cits.country.n))
df.journals.max.cits$cits.prop <- as.numeric(df.journals.max.cits$cits.prop)

df.journals.max.refs <- df.journals.final %>%
  group_by(journal.id) %>%
  filter(refs == max(refs, na.rm = TRUE)) %>%
  ungroup()
df.journals.max.refs <- select(df.journals.max.refs, c(journal.id, journal.name, country, refs, refs.n, refs.prop, refs.country.n))
df.journals.max.refs$refs.prop <- as.numeric(df.journals.max.refs$refs.prop)

# merge df.journals.max.cits and df.journals.max.refs to store all data about journals together
journals <- merge(df.journals.max.cits, df.journals.max.refs, by = "journal.id", all = TRUE)
journals <- select(journals, c(journal.id, journal.name.x, country.x, cits.prop, country.y, refs.prop))
journals <- journals %>% rename("journal.name" = "journal.name.x",
                                "cits.country" = "country.x",
                                "refs.country" = "country.y")


### AUTHORS' ORIGINS
# filter all rows per journal to keep only the countries with maximum pubs values
df.journals.max.pubs <- df.journals.final %>%
  group_by(journal.id) %>%
  filter(pubs == max(pubs, na.rm = TRUE)) %>%
  ungroup()
df.journals.max.pubs <- select(df.journals.max.pubs, c(journal.id, journal.name, country, pubs, pubs.n, pubs.prop, pubs.country.n))
df.journals.max.pubs$pubs.prop <- as.numeric(df.journals.max.pubs$pubs.prop)

# rename variable
df.journals.max.pubs <- df.journals.max.pubs %>% rename("pubs.country" = "country")

# merge pubs.country and pubs.prop variables to journals dataframe
journals <- merge(journals, df.journals.max.pubs[, c("journal.id", "pubs.country", "pubs.prop")], by = "journal.id", all = TRUE)


### TOPONYMS
# read file
toponyms <- read.csv2("~/Desktop/Local.Research/journals_papers_titles.csv", col.names = c("source.type", "journal.id", "paper.id", "paper.title"))

# divide the original toponyms dataframe into 10 smaller parts to facilitate processing
n_rows <- nrow(toponyms)
chunk_size <- ceiling(n_rows / 10)
toponyms.parts <- list()

for (i in 1:10) {start_index <- (i - 1) * chunk_size + 1
  end_index <- min(i * chunk_size, n_rows)
  current_chunk <- toponyms[start_index:end_index, ]
  toponyms.parts[[i]] <- current_chunk}

# identify languages present in papers' titles
current_chunk <- toponyms.parts[[1]]
current_chunk$title.language <- cld2::detect_language(current_chunk$paper.title)
toponyms.parts[[1]] <- current_chunk

current_chunk <- toponyms.parts[[2]]
current_chunk$title.language <- cld2::detect_language(current_chunk$paper.title)
toponyms.parts[[2]] <- current_chunk

current_chunk <- toponyms.parts[[3]]
current_chunk$title.language <- cld2::detect_language(current_chunk$paper.title)
toponyms.parts[[3]] <- current_chunk

current_chunk <- toponyms.parts[[4]]
current_chunk$title.language <- cld2::detect_language(current_chunk$paper.title)
toponyms.parts[[4]] <- current_chunk

current_chunk <- toponyms.parts[[5]]
current_chunk$title.language <- cld2::detect_language(current_chunk$paper.title)
toponyms.parts[[5]] <- current_chunk

current_chunk <- toponyms.parts[[6]]
current_chunk$title.language <- cld2::detect_language(current_chunk$paper.title)
toponyms.parts[[6]] <- current_chunk

current_chunk <- toponyms.parts[[7]]
current_chunk$title.language <- cld2::detect_language(current_chunk$paper.title)
toponyms.parts[[7]] <- current_chunk

current_chunk <- toponyms.parts[[8]]
current_chunk$title.language <- cld2::detect_language(current_chunk$paper.title)
toponyms.parts[[8]] <- current_chunk

current_chunk <- toponyms.parts[[9]]
current_chunk$title.language <- cld2::detect_language(current_chunk$paper.title)
toponyms.parts[[9]] <- current_chunk

current_chunk <- toponyms.parts[[10]]
current_chunk$title.language <- cld2::detect_language(current_chunk$paper.title)
toponyms.parts[[10]] <- current_chunk

## 1º EN (english) = 7227273 titles = 92,45%
## 2º JA (japanese) = 103017 titles = 1,31%
## 3º PT (portuguese) = 72173 titles = 0,92%
## 4º DE (german) = 63779 titles = 0,81%
## 5º ES (spanish) = 62989 titles = 0,80%
## 6º ID (indonesian) = 59632 titles = 0,76%
## 7º FR (french) = 48892 titles = 0,62%
## 8º RU (russian) = 20690 titles = 0,26%

# create a unique list of toponyms in different languages
toponyms.list <- unique(c(world.map$NAME_DE, world.map$NAME_EN, world.map$NAME_ES, world.map$NAME_FR, world.map$NAME_ID, world.map$NAME_JA, world.map$NAME_PT, world.map$NAME_RU))

# look for the toponyms in different languages in each part of the toponyms dataframe
toponyms.parts[[1]]$toponym <- lapply(toponyms.parts[[1]]$paper.title, function(title) paste(unlist(str_extract_all(title, paste(toponyms.list, collapse = "|"))), collapse = ", "))
toponyms.part1 <- toponyms.parts[[1]]
toponyms.part1$toponym <- unlist(toponyms.part1$toponym)

toponyms.parts[[2]]$toponym <- lapply(toponyms.parts[[2]]$paper.title, function(title) paste(unlist(str_extract_all(title, paste(toponyms.list, collapse = "|"))), collapse = ", "))
toponyms.part2 <- toponyms.parts[[2]]
toponyms.part2$toponym <- unlist(toponyms.part2$toponym)

toponyms.parts[[3]]$toponym <- lapply(toponyms.parts[[3]]$paper.title, function(title) paste(unlist(str_extract_all(title, paste(toponyms.list, collapse = "|"))), collapse = ", "))
toponyms.part3 <- toponyms.parts[[3]]
toponyms.part3$toponym <- unlist(toponyms.part3$toponym)

toponyms.parts[[4]]$toponym <- lapply(toponyms.parts[[4]]$paper.title, function(title) paste(unlist(str_extract_all(title, paste(toponyms.list, collapse = "|"))), collapse = ", "))
toponyms.part4 <- toponyms.parts[[4]]
toponyms.part4$toponym <- unlist(toponyms.part4$toponym)

toponyms.parts[[5]]$toponym <- lapply(toponyms.parts[[5]]$paper.title, function(title) paste(unlist(str_extract_all(title, paste(toponyms.list, collapse = "|"))), collapse = ", "))
toponyms.part5 <- toponyms.parts[[5]]
toponyms.part5$toponym <- unlist(toponyms.part5$toponym)

toponyms.parts[[6]]$toponym <- lapply(toponyms.parts[[6]]$paper.title, function(title) paste(unlist(str_extract_all(title, paste(toponyms.list, collapse = "|"))), collapse = ", "))
toponyms.part6 <- toponyms.parts[[6]]
toponyms.part6$toponym <- unlist(toponyms.part6$toponym)

toponyms.parts[[7]]$toponym <- lapply(toponyms.parts[[7]]$paper.title, function(title) paste(unlist(str_extract_all(title, paste(toponyms.list, collapse = "|"))), collapse = ", "))
toponyms.part7 <- toponyms.parts[[7]]
toponyms.part7$toponym <- unlist(toponyms.part7$toponym)

toponyms.parts[[8]]$toponym <- lapply(toponyms.parts[[8]]$paper.title, function(title) paste(unlist(str_extract_all(title, paste(toponyms.list, collapse = "|"))), collapse = ", "))
toponyms.part8 <- toponyms.parts[[8]]
toponyms.part8$toponym <- unlist(toponyms.part8$toponym)

toponyms.parts[[9]]$toponym <- lapply(toponyms.parts[[9]]$paper.title, function(title) paste(unlist(str_extract_all(title, paste(toponyms.list, collapse = "|"))), collapse = ", "))
toponyms.part9 <- toponyms.parts[[9]]
toponyms.part9$toponym <- unlist(toponyms.part9$toponym)

toponyms.parts[[10]]$toponym <- lapply(toponyms.parts[[10]]$paper.title, function(title) paste(unlist(str_extract_all(title, paste(toponyms.list, collapse = "|"))), collapse = ", "))
toponyms.part10 <- toponyms.parts[[10]]
toponyms.part10$toponym <- unlist(toponyms.part10$toponym)

# create a function for the translation of all toponyms into english language
translate_country <- function(countries) { translated_countries <- character(length(countries))
  for (i in seq_along(countries)) {if (countries[i] %in% c("Afganistán", "Afeganistão", "Afganistan", "アフガニスタン", "Afghanistan")) { translated_countries[i] <- "Afghanistan"
    } else if (countries[i] %in% c("Albanien", "Albanie", "アルバニア", "Albania")) { translated_countries[i] <- "Albania"
    } else if (countries[i] %in% c("Algérie", "Algerien", "Argelia", "Argélia", "Алжир", "Aljazair", "アルジェリア", "Algeria")) { translated_countries[i] <- "Algeria"
    } else if (countries[i] %in% c("Ангола", "アンゴラ", "Angola")) { translated_countries[i] <- "Angola"
    } else if (countries[i] %in% c("Antártida", "Antarctica")) { translated_countries[i] <- "Antarctica"
    } else if (countries[i] %in% c("Argentine", "Argentinien", "Аргентина", "Argentina")) { translated_countries[i] <- "Argentina"
    } else if (countries[i] %in% c("Armenien", "Arménie", "Armenia")) { translated_countries[i] <- "Armenia"
    } else if (countries[i] %in% c("Australien", "Austrália", "Australie", "オーストラリア", "Australia")) { translated_countries[i] <- "Australia"
    } else if (countries[i] %in% c("Österreich", "Autriche", "オーストリア", "Áustria", "Austria")) { translated_countries[i] <- "Austria"
    } else if (countries[i] %in% c("アゼルバイジャン",  "Azerbaijan", "Азербайджан", "Azerbaiyán")) { translated_countries[i] <- "Azerbaijan"
    } else if (countries[i] %in% c("バングラデシュ", "Bangladesh")) { translated_countries[i] <- "Bangladesh"
    } else if (countries[i] %in% c("ベラルーシ", "Belarus")) { translated_countries[i] <- "Belarus"
    } else if (countries[i] %in% c("Belgia", "Belgique", "Bélgica", "Belgien", "ベルギー", "Belgium")) { translated_countries[i] <- "Belgium"
    } else if (countries[i] %in% c("Belice", "Belize")) { translated_countries[i] <- "Belize"
    } else if (countries[i] %in% c("Bénin", "Benim", "ベナン", "Benin")) { translated_countries[i] <- "Benin"
    } else if (countries[i] %in% c("Bolívia", "Bolivien", "Bolivie", "ボリビア", "Bolivia")) { translated_countries[i] <- "Bolivia"
    } else if (countries[i] %in% c("Bosnien und Herzegowina", "Bosnia and Herzegovina")) { translated_countries[i] <- "Bosnia and Herzegovina"
    } else if (countries[i] %in% c("Brasil", "Brasilien", "Brésil", "ブラジル", "Brazil")) { translated_countries[i] <- "Brazil"
    } else if (countries[i] %in% c("ブルネイ", "Brunei")) { translated_countries[i] <- "Brunei"
    } else if (countries[i] %in% c("Botswana")) { translated_countries[i] <- "Botswana"
    } else if (countries[i] %in% c("Burundi")) { translated_countries[i] <- "Burundi"
    } else if (countries[i] %in% c("Bulgária", "Bulgarien", "Bulgarie", "ブルガリア", "Bulgaria")) { translated_countries[i] <- "Bulgaria"
    } else if (countries[i] %in% c("ブルキナファソ", "Burkina Faso")) { translated_countries[i] <- "Burkina Faso"
    } else if (countries[i] %in% c("ブータン", "Bután", "Bhutan")) { translated_countries[i] <- "Bhutan"
    } else if (countries[i] %in% c("Cambodge", "Kamboja", "Camboja", "カンボジア", "Cambodia")) { translated_countries[i] <- "Cambodia"
    } else if (countries[i] %in% c("Cameroun", "Kamerun", "Camarões", "Camerún", "カメルーン", "Cameroon")) { translated_countries[i] <- "Cameroon"
    } else if (countries[i] %in% c("Canadá", "Kanada", "Канада", "カナダ", "Canada")) { translated_countries[i] <- "Canada"
    } else if (countries[i] %in% c("République centrafricaine", "República Centro-Africana", "Central African Republic")) { translated_countries[i] <- "Central African Republic"
    } else if (countries[i] %in% c("Tchad", "Tschad", "チャド", "Chad")) { translated_countries[i] <- "Chad"
    } else if (countries[i] %in% c("Chili", "チリ", "Chile")) { translated_countries[i] <- "Chile"
    } else if (countries[i] %in% c("Colômbia", "Kolumbien", "Colombie", "Kolombia", "コロンビア", "Colombia")) { translated_countries[i] <- "Colombia"
    } else if (countries[i] %in% c("コスタリカ", "Costa Rica")) { translated_countries[i] <- "Costa Rica"
    } else if (countries[i] %in% c("Kroatien", "Croacia", "Croácia", "Croatia")) { translated_countries[i] <- "Croatia"
    } else if (countries[i] %in% c("Kuba", "キューバ", "Куба", "Cuba")) { translated_countries[i] <- "Cuba"
    } else if (countries[i] %in% c("Chypre", "Cyprus")) { translated_countries[i] <- "Cyprus"
    } else if (countries[i] %in% c("República Checa", "Tschechien", "チェコ", "Czech Republic")) { translated_countries[i] <- "Czech Republic"
    } else if (countries[i] %in% c("République démocratique du Congo", "République du Congo", "Republic of the Congo", "República Democrática do Congo", "コンゴ共和国", "コンゴ民主共和国", "Democratic Republic of the Congo")) { translated_countries[i] <- "Democratic Republic of the Congo"
    } else if (countries[i] %in% c("Dinamarca", "Danemark", "Dänemark", "デンマーク", "Denmark")) { translated_countries[i] <- "Denmark"
    } else if (countries[i] %in% c("ジブチ", "Djibouti")) { translated_countries[i] <- "Djibouti"
    } else if (countries[i] %in% c("República Dominicana", "Dominican Republic")) { translated_countries[i] <- "Dominican Republic"
    } else if (countries[i] %in% c("Timor-Leste", "Timor Leste", "Timor Oriental", "東ティモール", "East Timor")) { translated_countries[i] <- "East Timor"
    } else if (countries[i] %in% c("Equador", "Équateur", "Эквадор", "エクアドル", "Ecuador")) { translated_countries[i] <- "Ecuador"
    } else if (countries[i] %in% c("Ägypten", "Egito", "Égypte", "Egipto", "Mesir", "エジプト", "Египет", "Egypt")) { translated_countries[i] <- "Egypt"
    } else if (countries[i] %in% c("Сальвадор", "Salvador", "El Salvador")) { translated_countries[i] <- "El Salvador"
    } else if (countries[i] %in% c("Érythrée", "Eritrea")) { translated_countries[i] <- "Eritrea"
    } else if (countries[i] %in% c("Eswatini")) { translated_countries[i] <- "Eswatini"
    } else if (countries[i] %in% c("Equatorial Guinea")) { translated_countries[i] <- "Equatorial Guinea"
    } else if (countries[i] %in% c("Estonie", "Estland", "エストニア", "Estonia")) { translated_countries[i] <- "Estonia"
    } else if (countries[i] %in% c("Etiopía", "エチオピア", "Äthiopien", "Éthiopie", "Etiópia", "Ethiopia")) { translated_countries[i] <- "Ethiopia"
    } else if (countries[i] %in% c("Islas Malvinas", "Falkland Islands")) { translated_countries[i] <- "Falkland Islands"
    } else if (countries[i] %in% c("Fidji", "フィジー", "Fiji")) { translated_countries[i] <- "Fiji"
    } else if (countries[i] %in% c("French Southern and Antarctic Lands")) { translated_countries[i] <- "French Southern and Antarctic Lands"
    } else if (countries[i] %in% c("Finnland", "フィンランド", "Finlândia", "Finland")) { translated_countries[i] <- "Finland"
    } else if (countries[i] %in% c("França", "Francia", "Prancis", "フランス", "Frankreich", "France")) { translated_countries[i] <- "France"
    } else if (countries[i] %in% c("Gabun", "Gabon")) { translated_countries[i] <- "Gabon"
    } else if (countries[i] %in% c("Gana", "ガーナ", "Ghana")) { translated_countries[i] <- "Ghana"
    } else if (countries[i] %in% c("Deutschland", "Alemanha", "Alemania", "Allemagne", "ドイツ", "Jerman", "Германия", "Germany")) { translated_countries[i] <- "Germany"
    } else if (countries[i] %in% c("Georgien", "Géorgie", "Georgia")) { translated_countries[i] <- "Georgia"
    } else if (countries[i] %in% c("Griechenland", "Grèce", "Grecia", "Grécia", "Yunani", "ギリシャ", "Greece")) { translated_countries[i] <- "Greece"
    } else if (countries[i] %in% c("Groenlandia", "Grönland", "Groenland", "グリーンランド", "Greenland")) { translated_countries[i] <- "Greenland"
    } else if (countries[i] %in% c("Guinée", "Guiné", "Guinea")) { translated_countries[i] <- "Guinea"
    } else if (countries[i] %in% c("Guiana", "ガイアナ", "Guyana")) { translated_countries[i] <- "Guyana"
    } else if (countries[i] %in% c("Guatemala")) { translated_countries[i] <- "Guatemala"
    } else if (countries[i] %in% c("Haïti", "Haití", "Haiti")) { translated_countries[i] <- "Haiti"
    } else if (countries[i] %in% c("ホンジュラス", "Honduras")) { translated_countries[i] <- "Honduras"
    } else if (countries[i] %in% c("Ungarn", "Hungría", "Hungria", "ハンガリー", "Венгрия", "Hungary")) { translated_countries[i] <- "Hungary"
    } else if (countries[i] %in% c("Island", "Islândia", "アイスランド", "Iceland")) { translated_countries[i] <- "Iceland"
    } else if (countries[i] %in% c("インド", "Indien", "Inde", "Índia", "India")) { translated_countries[i] <- "India"
    } else if (countries[i] %in% c("Indonésie", "Indonesien", "インドネシア", "Indonesia")) { translated_countries[i] <- "Indonesia"
    } else if (countries[i] %in% c("Iraq", "イラク", "Irak")) { translated_countries[i] <- "Irak"
    } else if (countries[i] %in% c("イラン", "Irán", "Иран", "Iran")) { translated_countries[i] <- "Iran"
    } else if (countries[i] %in% c("Irland", "アイルランド", "Ireland")) { translated_countries[i] <- "Ireland"
    } else if (countries[i] %in% c("Israël", "イスラエル", "Israel")) { translated_countries[i] <- "Israel"
    } else if (countries[i] %in% c("Italia", "Italien", "Italie", "Itália", "イタリア", "Италия", "Italy")) { translated_countries[i] <- "Italy"
    } else if (countries[i] %in% c("Côte d'Ivoire", "Costa de Marfil", "Ivory Coast")) { translated_countries[i] <- "Ivory Coast"
    } else if (countries[i] %in% c("Jamaika", "ジャマイカ", "Jamaica")) { translated_countries[i] <- "Jamaica"
    } else if (countries[i] %in% c("日本", "Japon", "Japón", "Jepang", "Japão", "Япония", "Japan")) { translated_countries[i] <- "Japan"
    } else if (countries[i] %in% c("Jordanien", "Jordânia", "ヨルダン", "Jordan")) { translated_countries[i] <- "Jordan"
    } else if (countries[i] %in% c("Kasachstan", "Казахстан", "カザフスタン", "Kazakhstan")) { translated_countries[i] <- "Kazakhstan"
    } else if (countries[i] %in% c("Kenia", "ケニア", "Kenya")) { translated_countries[i] <- "Kenya"
    } else if (countries[i] %in% c("Kuwait")) { translated_countries[i] <- "Kuwait"
    } else if (countries[i] %in% c("Kosovo")) { translated_countries[i] <- "Kosovo"
    } else if (countries[i] %in% c("キルギス", "Kyrgyzstan")) { translated_countries[i] <- "Kyrgyzstan"
    } else if (countries[i] %in% c("ラオス", "Laos")) { translated_countries[i] <- "Laos"
    } else if (countries[i] %in% c("Lettland", "Letonia", "Latvia")) { translated_countries[i] <- "Latvia"
    } else if (countries[i] %in% c("Liban", "Líbano", "Lebanon")) { translated_countries[i] <- "Lebanon"
    } else if (countries[i] %in% c("Liberia")) { translated_countries[i] <- "Liberia"
    } else if (countries[i] %in% c("Lesotho")) { translated_countries[i] <- "Lesotho"
    } else if (countries[i] %in% c("Luxemburg")) { translated_countries[i] <- "Luxemburg"
    } else if (countries[i] %in% c("Libia", "Libyen", "Libye", "リビア", "Libya")) { translated_countries[i] <- "Libya"
    } else if (countries[i] %in% c("Litauen", "Литва", "Lituania", "Lituanie", "リトアニア", "Lithuania")) { translated_countries[i] <- "Lithuania"
    } else if (countries[i] %in% c("Luksemburg", "Luxembourg")) { translated_countries[i] <- "Luxembourg"
    } else if (countries[i] %in% c("Madagaskar", "マダガスカル", "Madagascar")) { translated_countries[i] <- "Madagascar"
    } else if (countries[i] %in% c("Malí", "Mali")) { translated_countries[i] <- "Mali"
    } else if (countries[i] %in% c("マラウイ", "Malawi")) { translated_countries[i] <- "Malawi"
    } else if (countries[i] %in% c("Montenegro")) { translated_countries[i] <- "Montenegro"
    } else if (countries[i] %in% c("Malasia", "Malaisie", "マレーシア", "Malaysia")) { translated_countries[i] <- "Malaysia"
    } else if (countries[i] %in% c("Mauretanien", "Mauritanie", "Mauritania")) { translated_countries[i] <- "Mauritania"
    } else if (countries[i] %in% c("México", "Mexique", "Mexiko", "メキシコ", "Mexico")) { translated_countries[i] <- "Mexico"
    } else if (countries[i] %in% c("Moldavie", "Republik Moldau", "Moldavia", "Moldova")) { translated_countries[i] <- "Moldova"
    } else if (countries[i] %in% c("Mongolei", "Mongolie", "モンゴル国", "Mongólia", "Mongolia")) { translated_countries[i] <- "Mongolia"
    } else if (countries[i] %in% c("Maroc", "Marruecos", "Marokko", "Marrocos", "モロッコ", "Марокко", "Morocco")) { translated_countries[i] <- "Morocco"
    } else if (countries[i] %in% c("Moçambique", "Mosambik", "モザンビーク", "Mozambique")) { translated_countries[i] <- "Mozambique"
    } else if (countries[i] %in% c("ミャンマー", "Birmanie", "Birmania", "Myanmar")) { translated_countries[i] <- "Myanmar"
    } else if (countries[i] %in% c("Namíbia", "ナミビア", "Namibia")) { translated_countries[i] <- "Namibia"
    } else if (countries[i] %in% c("ネパール", "Népal", "Nepal")) { translated_countries[i] <- "Nepal"
    } else if (countries[i] %in% c("Países Bajos", "Niederlande", "Belanda", "Países Baixos", "Pays-Bas", "オランダ", "Netherlands")) { translated_countries[i] <- "Netherlands"
    } else if (countries[i] %in% c("Nouvelle-Calédonie", "ニューカレドニア", "New Caledonia")) { translated_countries[i] <- "New Caledonia"
    } else if (countries[i] %in% c("Nouvelle-Zélande", "Neuseeland", "Nova Zelândia", "ニュージーランド", "New Zealand")) { translated_countries[i] <- "New Zealand"
    } else if (countries[i] %in% c("Nicarágua", "Nicaragua")) { translated_countries[i] <- "Nicaragua"
    } else if (countries[i] %in% c("Níger", "Niger")) { translated_countries[i] <- "Niger"
    } else if (countries[i] %in% c("North Macedonia")) { translated_countries[i] <- "North Macedonia"
    } else if (countries[i] %in% c("Nigéria", "ナイジェリア", "Nigeria")) { translated_countries[i] <- "Nigeria"
    } else if (countries[i] %in% c("Nordkorea", "Corea del Norte", "Korea Utara", "Coreia do Norte", "КНДР", "朝鮮民主主義人民共和国", "North Korea")) { translated_countries[i] <- "North Korea"
    } else if (countries[i] %in% c("Norwegia", "Norwegen", "Noruega", "Norvège", "ノルウェー", "Norway")) { translated_countries[i] <- "Norway"
    } else if (countries[i] %in% c("オマーン", "Oman")) { translated_countries[i] <- "Oman"
    } else if (countries[i] %in% c("Palestina", "Palästina", "パレスチナ", "Palestine")) { translated_countries[i] <- "Palestine"
    } else if (countries[i] %in% c("Panamá", "Panama")) { translated_countries[i] <- "Panama"
    } else if (countries[i] %in% c("パプアニューギニア", "Papua Nugini", "Papua New Guinea")) { translated_countries[i] <- "Papua New Guinea"
    } else if (countries[i] %in% c("Paraguai", "パラグアイ", "Paraguay")) { translated_countries[i] <- "Paraguay"
    } else if (countries[i] %in% c("Paquistão", "パキスタン", "Pakistan")) { translated_countries[i] <- "Pakistan"
    } else if (countries[i] %in% c("Filipina", "Filipinas", "フィリピン", "Philippinen", "Philippines")) { translated_countries[i] <- "Philippines"
    } else if (countries[i] %in% c("Polen", "Pologne", "Polonia", "Polónia", "Польша", "ポーランド", "Poland")) { translated_countries[i] <- "Poland"
    } else if (countries[i] %in% c("République populaire de Chine", "Volksrepublik China", "中華人民共和国", "People's Republic of China", "China")) { translated_countries[i] <- "China"
    } else if (countries[i] %in% c("Perú",  "Pérou", "ペルー", "Peru")) { translated_countries[i] <- "Peru"
    } else if (countries[i] %in% c("ポルトガル", "Portugal")) { translated_countries[i] <- "Portugal"
    } else if (countries[i] %in% c("Porto Rico", "プエルトリコ", "Puerto Rico")) { translated_countries[i] <- "Puerto Rico"
    } else if (countries[i] %in% c("Catar", "Katar", "カタール", "Qatar")) { translated_countries[i] <- "Qatar"
    } else if (countries[i] %in% c("Rumänien", "Roumanie", "Rumania", "ルーマニア", "Romania")) { translated_countries[i] <- "Romania"
    } else if (countries[i] %in% c("Russland", "Rússia", "Russie", "Россия", "ロシア", "Rusia", "Russia")) { translated_countries[i] <- "Russia"
    } else if (countries[i] %in% c("Ruanda", "ルワンダ", "Rwanda")) { translated_countries[i] <- "Rwanda"
    } else if (countries[i] %in% c("ソロモン諸島", "Solomon Islands")) { translated_countries[i] <- "Solomon Islands"
    } else if (countries[i] %in% c("España", "Espanha", "Espagne", "Spanien", "Spanyol", "スペイン", "Spain")) { translated_countries[i] <- "Spain"
    } else if (countries[i] %in% c("スリランカ", "Sri Lanka")) { translated_countries[i] <- "Sri Lanka"
    } else if (countries[i] %in% c("Swiss", "Schweiz", "Suisse", "Suiza", "スイス", "Suíça", "Switzerland")) { translated_countries[i] <- "Switzerland"
    } else if (countries[i] %in% c("Sénégal", "Senegal")) { translated_countries[i] <- "Senegal"
    } else if (countries[i] %in% c("Siria", "Syrien", "シリア", "Síria", "Syrie", "Suriah", "Syria")) { translated_countries[i] <- "Syria"
    } else if (countries[i] %in% c("Südsudan", "南スーダン", "South Sudan")) { translated_countries[i] <- "South Sudan"
    } else if (countries[i] %in% c("Korea Selatan", "Coreia do Sul", "Corée du Sud", "Corea del Sur", "South Korea")) { translated_countries[i] <- "South Korea"
    } else if (countries[i] %in% c("Serra Leoa", "Sierra Leone")) { translated_countries[i] <- "Sierra Leone"
    } else if (countries[i] %in% c("Surinam", "Suriname")) { translated_countries[i] <- "Suriname"
    } else if (countries[i] %in% c("Somaliland")) { translated_countries[i] <- "Somaliland"
    } else if (countries[i] %in% c("África do Sul", "Südafrika", "Afrique du Sud", "Sudáfrica", "Afrika Selatan", "南アフリカ共和国", "ЮАР", "South Africa")) { translated_countries[i] <- "South Africa"
    } else if (countries[i] %in% c("Slowakei", "Slovaquie", "スロバキア", "Slovakia")) { translated_countries[i] <- "Slovakia"
    } else if (countries[i] %in% c("Slowenien", "Eslovenia", "スロベニア", "Slovenia")) { translated_countries[i] <- "Slovenia"
    } else if (countries[i] %in% c("Sudán", "Sudão", "Soudan", "スーダン", "Sudan")) { translated_countries[i] <- "Sudan"
    } else if (countries[i] %in% c("Serbien", "Serbie", "Serbia")) { translated_countries[i] <- "Serbia"
    } else if (countries[i] %in% c("Suecia", "Suécia", "スウェーデン", "Suède", "Schweden", "Swedia", "Sweden")) { translated_countries[i] <- "Sweden"
    } else if (countries[i] %in% c("Arabia Saudita", "Arab Saudi", "サウジアラビア", "Saudi Arabia")) { translated_countries[i] <- "Saudi Arabia"
    } else if (countries[i] %in% c("Somália", "Somalia")) { translated_countries[i] <- "Somalia"
    } else if (countries[i] %in% c("Turki", "Türkei", "Turquie", "Turquía", "Turquia", "トルコ", "Turkey")) { translated_countries[i] <- "Turkey"
    } else if (countries[i] %in% c("Tunisie", "Túnez", "Tunesien", "チュニジア", "Tunisia")) { translated_countries[i] <- "Tunisia"
    } else if (countries[i] %in% c("Bahama", "Bahamas", "The Bahamas")) { translated_countries[i] <- "The Bahamas"
    } else if (countries[i] %in% c("Gambie", "Gambia", "The Gambia")) { translated_countries[i] <- "The Gambia"
    } else if (countries[i] %in% c("Togo")) { translated_countries[i] <- "Togo"
    } else if (countries[i] %in% c("Tadschikistan", "Таджикистан", "Tajikistan")) { translated_countries[i] <- "Tajikistan"
    } else if (countries[i] %in% c("中華民国", "Taïwan", "Taiwan")) { translated_countries[i] <- "Taiwan"
    } else if (countries[i] %in% c("タンザニア", "Tansania", "Tanzânia", "Tanzanie", "Tanzania")) { translated_countries[i] <- "Tanzania"
    } else if (countries[i] %in% c("Tailandia", "Thaïlande", "タイ王国", "Tailândia", "Thailand")) { translated_countries[i] <- "Thailand"
    } else if (countries[i] %in% c("Turkmenistán", "Turkmenistan")) { translated_countries[i] <- "Turkmenistan"
    } else if (countries[i] %in% c("Turkish Republic of Northern Cyprus")) { translated_countries[i] <- "Turkish Republic of Northern Cyprus"
    } else if (countries[i] %in% c("Trinidad and Tobago")) { translated_countries[i] <- "Trinidad and Tobago"
    } else if (countries[i] %in% c("Ouganda", "ウガンダ", "Uganda")) { translated_countries[i] <- "Uganda"
    } else if (countries[i] %in% c("Royaume-Uni", "Reino Unido", "イギリス", "Великобритания", "United Kingdom")) { translated_countries[i] <- "United Kingdom"
    } else if (countries[i] %in% c("Estados Unidos", "États-Unis", "アメリカ合衆国", "США", "Amerika Serikat", "United States of America")) { translated_countries[i] <- "United States of America"
    } else if (countries[i] %in% c("Ukraina", "Ucrania", "Ucrânia", "ウクライナ", "Ukraine")) { translated_countries[i] <- "Ukraine"
    } else if (countries[i] %in% c("Ouzbékistan", "Узбекистан", "ウズベキスタン", "Uzbekistan")) { translated_countries[i] <- "Uzbekistan"
    } else if (countries[i] %in% c("United Arab Emirates")) { translated_countries[i] <- "United Arab Emirates"
    } else if (countries[i] %in% c("Uruguai", "Uruguay")) { translated_countries[i] <- "Uruguay"
    } else if (countries[i] %in% c("Vanuatu")) { translated_countries[i] <- "Vanuatu"
    } else if (countries[i] %in% c("Venezuela")) { translated_countries[i] <- "Venezuela"
    } else if (countries[i] %in% c("ベトナム", "Вьетнам", "Vietnam")) { translated_countries[i] <- "Vietnam"
    } else if (countries[i] %in% c("Sahara Occidental", "Western Sahara")) { translated_countries[i] <- "Western Sahara"
    } else if (countries[i] %in% c("Yaman", "Jemen", "Yémen", "イエメン", "Yemen")) { translated_countries[i] <- "Yemen"
    } else if (countries[i] %in% c("ザンビア", "Sambia", "Zambia")) { translated_countries[i] <- "Zambia"
    } else if (countries[i] %in% c("Zimbabwe")) { translated_countries[i] <- "Zimbabwe"
    } else { translated_countries[i] <- NA }}
  return(translated_countries)}

# apply the translation function to each smaller part of the toponyms dataframe, convert the resulting toponym.english variable into character format and clean vectors
toponyms.part1 <- toponyms.part1 %>% mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part1$toponym.english <- as.character(toponyms.part1$toponym.english)

toponyms.part1$toponym.english <- gsub('c\\("', '', toponyms.part1$toponym.english)
toponyms.part1$toponym.english <- gsub('"\\)', '', toponyms.part1$toponym.english)
toponyms.part1$toponym.english <- gsub('", "', ', ', toponyms.part1$toponym.english)

toponyms.part2 <- toponyms.part2 %>% mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part2$toponym.english <- as.character(toponyms.part2$toponym.english)

toponyms.part2$toponym.english <- gsub('c\\("', '', toponyms.part2$toponym.english)
toponyms.part2$toponym.english <- gsub('"\\)', '', toponyms.part2$toponym.english)
toponyms.part2$toponym.english <- gsub('", "', ', ', toponyms.part2$toponym.english)

toponyms.part3 <- toponyms.part3 %>% mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part3$toponym.english <- as.character(toponyms.part3$toponym.english)

toponyms.part3$toponym.english <- gsub('c\\("', '', toponyms.part3$toponym.english)
toponyms.part3$toponym.english <- gsub('"\\)', '', toponyms.part3$toponym.english)
toponyms.part3$toponym.english <- gsub('", "', ', ', toponyms.part3$toponym.english)

toponyms.part4 <- toponyms.part4 %>% mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part4$toponym.english <- as.character(toponyms.part4$toponym.english)

toponyms.part4$toponym.english <- gsub('c\\("', '', toponyms.part4$toponym.english)
toponyms.part4$toponym.english <- gsub('"\\)', '', toponyms.part4$toponym.english)
toponyms.part4$toponym.english <- gsub('", "', ', ', toponyms.part4$toponym.english)

toponyms.part5 <- toponyms.part5 %>% mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part5$toponym.english <- as.character(toponyms.part5$toponym.english)

toponyms.part5$toponym.english <- gsub('c\\("', '', toponyms.part5$toponym.english)
toponyms.part5$toponym.english <- gsub('"\\)', '', toponyms.part5$toponym.english)
toponyms.part5$toponym.english <- gsub('", "', ', ', toponyms.part5$toponym.english)

toponyms.part6 <- toponyms.part6 %>% mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part6$toponym.english <- as.character(toponyms.part6$toponym.english)

toponyms.part6$toponym.english <- gsub('c\\("', '', toponyms.part6$toponym.english)
toponyms.part6$toponym.english <- gsub('"\\)', '', toponyms.part6$toponym.english)
toponyms.part6$toponym.english <- gsub('", "', ', ', toponyms.part6$toponym.english)

toponyms.part7 <- toponyms.part7 %>% mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part7$toponym.english <- as.character(toponyms.part7$toponym.english)

toponyms.part7$toponym.english <- gsub('c\\("', '', toponyms.part7$toponym.english)
toponyms.part7$toponym.english <- gsub('"\\)', '', toponyms.part7$toponym.english)
toponyms.part7$toponym.english <- gsub('", "', ', ', toponyms.part7$toponym.english)

toponyms.part8 <- toponyms.part8 %>% mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part8$toponym.english <- as.character(toponyms.part8$toponym.english)

toponyms.part8$toponym.english <- gsub('c\\("', '', toponyms.part8$toponym.english)
toponyms.part8$toponym.english <- gsub('"\\)', '', toponyms.part8$toponym.english)
toponyms.part8$toponym.english <- gsub('", "', ', ', toponyms.part8$toponym.english)

toponyms.part9 <- toponyms.part9 %>% mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part9$toponym.english <- as.character(toponyms.part9$toponym.english)

toponyms.part9$toponym.english <- gsub('c\\("', '', toponyms.part9$toponym.english)
toponyms.part9$toponym.english <- gsub('"\\)', '', toponyms.part9$toponym.english)
toponyms.part9$toponym.english <- gsub('", "', ', ', toponyms.part9$toponym.english)

toponyms.part10 <- toponyms.part10 %>% mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part10$toponym.english <- as.character(toponyms.part10$toponym.english)

toponyms.part10$toponym.english <- gsub('c\\("', '', toponyms.part10$toponym.english)
toponyms.part10$toponym.english <- gsub('"\\)', '', toponyms.part10$toponym.english)
toponyms.part10$toponym.english <- gsub('", "', ', ', toponyms.part10$toponym.english)

# duplicate rows when there are two or more country names per cell and convert NA characters to NA values
toponyms.part1.1 <- strsplit(toponyms.part1$toponym.english, ", ")
toponyms.part1.1 <- cbind(toponyms.part1[rep(1:nrow(toponyms.part1), lengths(toponyms.part1.1)), -which(names(toponyms.part1) == 'toponym.english')], 
                       toponym.english = unlist(toponyms.part1.1))
toponyms.part1.1 <- toponyms.part1.1 %>% mutate(toponym.english = na_if(toponym.english, "NA"))

toponyms.part2.1 <- strsplit(toponyms.part2$toponym.english, ", ")
toponyms.part2.1 <- cbind(toponyms.part2[rep(1:nrow(toponyms.part2), lengths(toponyms.part2.1)), -which(names(toponyms.part2) == 'toponym.english')], 
                          toponym.english = unlist(toponyms.part2.1))
toponyms.part2.1 <- toponyms.part2.1 %>% mutate(toponym.english = na_if(toponym.english, "NA"))

toponyms.part3.1 <- strsplit(toponyms.part3$toponym.english, ", ")
toponyms.part3.1 <- cbind(toponyms.part3[rep(1:nrow(toponyms.part3), lengths(toponyms.part3.1)), -which(names(toponyms.part3) == 'toponym.english')], 
                          toponym.english = unlist(toponyms.part3.1))
toponyms.part3.1 <- toponyms.part3.1 %>% mutate(toponym.english = na_if(toponym.english, "NA"))

toponyms.part4.1 <- strsplit(toponyms.part4$toponym.english, ", ")
toponyms.part4.1 <- cbind(toponyms.part4[rep(1:nrow(toponyms.part4), lengths(toponyms.part4.1)), -which(names(toponyms.part4) == 'toponym.english')], 
                          toponym.english = unlist(toponyms.part4.1))
toponyms.part4.1 <- toponyms.part4.1 %>% mutate(toponym.english = na_if(toponym.english, "NA"))

toponyms.part5.1 <- strsplit(toponyms.part5$toponym.english, ", ")
toponyms.part5.1 <- cbind(toponyms.part5[rep(1:nrow(toponyms.part5), lengths(toponyms.part5.1)), -which(names(toponyms.part5) == 'toponym.english')], 
                          toponym.english = unlist(toponyms.part5.1))
toponyms.part5.1 <- toponyms.part5.1 %>% mutate(toponym.english = na_if(toponym.english, "NA"))

toponyms.part6.1 <- strsplit(toponyms.part6$toponym.english, ", ")
toponyms.part6.1 <- cbind(toponyms.part6[rep(1:nrow(toponyms.part6), lengths(toponyms.part6.1)), -which(names(toponyms.part6) == 'toponym.english')], 
                          toponym.english = unlist(toponyms.part6.1))
toponyms.part6.1 <- toponyms.part6.1 %>% mutate(toponym.english = na_if(toponym.english, "NA"))

toponyms.part7.1 <- strsplit(toponyms.part7$toponym.english, ", ")
toponyms.part7.1 <- cbind(toponyms.part7[rep(1:nrow(toponyms.part7), lengths(toponyms.part7.1)), -which(names(toponyms.part7) == 'toponym.english')], 
                          toponym.english = unlist(toponyms.part7.1))
toponyms.part7.1 <- toponyms.part7.1 %>% mutate(toponym.english = na_if(toponym.english, "NA"))

toponyms.part8.1 <- strsplit(toponyms.part8$toponym.english, ", ")
toponyms.part8.1 <- cbind(toponyms.part8[rep(1:nrow(toponyms.part8), lengths(toponyms.part8.1)), -which(names(toponyms.part8) == 'toponym.english')], 
                          toponym.english = unlist(toponyms.part8.1))
toponyms.part8.1 <- toponyms.part8.1 %>% mutate(toponym.english = na_if(toponym.english, "NA"))

toponyms.part9.1 <- strsplit(toponyms.part9$toponym.english, ", ")
toponyms.part9.1 <- cbind(toponyms.part9[rep(1:nrow(toponyms.part9), lengths(toponyms.part9.1)), -which(names(toponyms.part9) == 'toponym.english')], 
                          toponym.english = unlist(toponyms.part9.1))
toponyms.part9.1 <- toponyms.part9.1 %>% mutate(toponym.english = na_if(toponym.english, "NA"))

toponyms.part10.1 <- strsplit(toponyms.part10$toponym.english, ", ")
toponyms.part10.1 <- cbind(toponyms.part10[rep(1:nrow(toponyms.part10), lengths(toponyms.part10.1)), -which(names(toponyms.part10) == 'toponym.english')], 
                          toponym.english = unlist(toponyms.part10.1))
toponyms.part10.1 <- toponyms.part10.1 %>% mutate(toponym.english = na_if(toponym.english, "NA"))

# merge all toponyms smaller parts vertically
toponyms <- rbind(toponyms.part1.1, toponyms.part2.1, toponyms.part3.1, toponyms.part4.1, toponyms.part5.1, toponyms.part6.1, toponyms.part7.1, toponyms.part8.1, toponyms.part9.1, toponyms.part10.1)

# add a with.toponym variable to check whether a paper contains or not toponyms in its title
toponyms$with.toponym <- ifelse(is.na(toponyms$toponym.english), 0, 1)

# group the toponyms dataframe per journal.id, counting the number of total papers and papers with toponyms
toponyms.counts <- subset(toponyms, select = c("journal.id", "paper.id", "with.toponym"))
toponyms.counts <- distinct(toponyms.counts, paper.id, .keep_all = TRUE)
toponyms.counts <- toponyms.counts %>% group_by(journal.id) %>%
  summarise(paper.total = n(), paper.with.top = sum(with.toponym))

# compute the proportion of toponyms per journal
toponyms.counts$toponyms.prop <- toponyms.counts$paper.with.top / toponyms.counts$paper.total

# add toponyms.prop variable to journals dataframe
journals <- left_join(journals, toponyms.counts[, c("journal.id", "toponyms.prop")], by = "journal.id")


### MAINSTREAM DATABASES
# read files
Scopus <- read.csv("~/Desktop/Local.Research/Scopus.journals.csv", col.names = c("journal.name", "issn", "eissn", "language", "publisher", sep = ","))
Scopus$journal.name <- toupper(Scopus$journal.name)
WOS <- read.csv("~/Desktop/Local.Research/WOS.journals.csv", col.names = c("journal.name", "issn", "eissn", "publisher", "language", sep = ","))
WOS$journal.name <- toupper(WOS$journal.name)

# merge Scopus and WOS journals data
databases <- merge(Scopus, WOS, by = "journal.name", all = TRUE)

# transform all empty cells into NA values
replace_empty_with_na <- function(x) {x[x == ""] <- NA
  return(x)}
databases <- databases %>% mutate(across(everything(), replace_empty_with_na))

# convert journal.name in journals dataframe to uppercase for case-insensitive comparison with databases
journals$journal.name <- toupper(journals$journal.name)

# check if every journal.name in journals is also present in databases, and store the binary answer in new mainstream.database variable
journals$mainstream.database <- as.integer(journals$journal.name %in% databases$journal.name)


### LANGUAGES
# read file
DOAJ <- read.csv("~/Desktop/Local.Research/DOAJ.journals.csv")
DOAJ <- DOAJ %>% rename("journal.name" = "Journal.title",
                        "language.z" = "Languages.in.which.the.journal.accepts.manuscripts")
DOAJ$journal.name <- toupper(DOAJ$journal.name)

# apply the function to transform all empty cells into NA values to DOAJ before merging
DOAJ <- DOAJ %>% mutate(across(everything(), replace_empty_with_na))

# merge DOAJ to Scopus and WOS journals data already combined in databases dataframe
databases <- merge(databases, DOAJ[, c("journal.name", "language.z")], by = "journal.name", all = TRUE)

# replace ISO codes with languages full names in Scopus data within databases dataframe
databases$language.x <- sapply(databases$language.x, function(iso_codes) {languages <- unlist(strsplit(iso_codes, "; "))
  full_names <- sapply(languages, function(lang) {switch(lang,
           "JPN" = "Japanese", "ENG" = "English", "FRE" = "French", "CAT" = "Catalan", "SPA" = "Spanish", "POR" = "Portuguese",
           "GER" = "German", "ITA" = "Italian", "RUS" = "Russian", "CHI" = "Chinese", "DAN" = "Danish", "SLV" = "Slovenian",
           "LIT" = "Lithuanian", "AFR" = "Afrikaans", "SLO" = "Slovak", "HUN" = "Hungarian", "POL" = "Polish", "CZE" = "Czech",
           "SCR" = "Croatian", "NOR" = "Norwegian", "GRE" = "Greek", "FIN" = "Finnish", "EST" = "Estonian", "DUT" = "Dutch",
           "SCC" = "Serbian", "LAV" = "Latvian", "TUR" = "Turkish", "MAY" = "Malay", "PER" = "Persian", "IND" = "Indonesian",
           "BUL" = "Bulgarian", "ARA" = "Arabic", "RUM" = "Romanian", "AZE" = "Azerbaijani", "KOR" = "Korean", "SWE" = "Swedish",
           "HEB" = "Hebrew", "BAQ" = "Basque", "GLE" = "Irish", "BEL" = "Belarusian", "UKR" = "Ukrainian", "THA" = "Thai",
           "GLG" = "Galician", "GEO" = "Georgian", "ICE" = "Icelandic", "BOS" = "Bosnian", "MAC" = "Macedonian", "MAO" = "Maori",
           "ARM" = "Armenian", "ALB" = "Albanian", lang)})
  return(paste(full_names, collapse = ", "))})

# convert NA characters to NA values in language.x variable
databases$language.x[databases$language.x == "NA"] <- NA

# merge language.x, language.y and language.z into a single language variable
databases$language <- apply(databases, 1, function(row) {
  language_x <- row["language.x"]
  language_y <- row["language.y"]
  language_z <- row["language.z"]
  if (!is.na(language_x) && !is.na(language_y) && !is.na(language_z)) {
    paste(language_x, language_y, language_z, sep = ", ")
  } else if (!is.na(language_x) && !is.na(language_y)) {
    paste(language_x, language_y, sep = ", ")
  } else if (!is.na(language_x) && !is.na(language_z)) {
    paste(language_x, language_z, sep = ", ")
  } else if (!is.na(language_y) && !is.na(language_z)) {
    paste(language_y, language_z, sep = ", ")
  } else if (!is.na(language_x)) {
    language_x
  } else if (!is.na(language_y)) {
    language_y
  } else if (!is.na(language_z)) {
    language_z
  } else {
    NA}})

# remove duplicate languages from the language variable
databases$language <- sapply(databases$language, function(lang_string) {
  unique_languages <- unique(unlist(strsplit(lang_string, ", ")))
  cleaned_language <- paste(unique_languages, collapse = ", ")
  return(cleaned_language)})

# add language variable to journals dataframe and remove duplicates
journals <- left_join(journals, databases[, c("journal.name", "language")], by = "journal.name", keep = FALSE)
journals <- journals[!duplicated(journals), ]

# apply the function to transform all empty cells into NA values, convert NA characters to NA values in language variable
journals <- journals %>% mutate(across(everything(), replace_empty_with_na))
journals$language[journals$language == "NA"] <- NA

# isolate NA rows from journals dataframe in order to complete only those empty cases with language data from papers' titles
journals.language <- journals[is.na(journals$language),]
journals.language <- subset(journals.language, select = c("journal.id", "language"))
journals.language <- unique(journals.language)
journals.language <- subset(journals.language, select = -language)

# isolate language data from toponyms dataframe and process it to replicate the format of journals$language
toponyms.language <- subset(toponyms, select = c("journal.id", "title.language"))
toponyms.language <- unique(toponyms.language)
toponyms.language <- na.omit(toponyms.language)
toponyms.language$title.language <- sapply(toponyms.language$title.language, function(lang) {switch(lang,
  "en" = "English", "es" = "Spanish", "fr" = "French", "pl" = "Polish", "pt" = "Portuguese", "ja" = "Japanese", "de" = "German",
  "gl" = "Galician", "sk" = "Slovak", "ro" = "Romanian", "it" = "Italian", "nl" = "Dutch", "lt" = "Lithuanian", "da" = "Danish",
  "el" = "Greek", "rw" = "Rwandan", "st" = "Southern Sotho", "af" = "Afrikaans", "sw" = "Swahili", "et" = "Estonian", "ca" = "Catalan",
  "ms" = "Malay", "zh-Hant" = "Chinese", "no" = "Norwegian", "jw" = "Javanese", "ru" = "Russian", "mg" = "Malagasy", "ku" = "Kurdish",
  "id" = "Indonesian", "ny" = "Chichewa", "tl" = "Tagalog", "su" = "Sundanese", "cy" = "Welsh", "eu" = "Basque", "tr" = "Turkish",
  "ceb" = "Cebuano", "sq" = "Albanian", "zh" = "Chinese", "uz" = "Uzbek", "ga" = "Irish", "mt" = "Maltese", "sl" = "Slovenian",
  "hu" = "Hungarian", "ht" = "Haitian", "uk" = "Ukranian", "ar" = "Arabic", "sr" = "Serbian", "fi" = "Finnish", "sv" = "Swedish",
  "gd" = "Gaelic", "lg" = "Ganda", "is" = "Icelandic", "hr" = "Croatian", "bs" = "Bosnian", "mk" = "Macedonian", "bg" = "Bulgarian",
  "cs" = "Czech", "az" = "Azerbaijani", "iw" = "Hebrew", "ko" = "Korean", "lv" = "Latvian", "vi" = "Vietnamese", "hmn" = "Mong",
  "ky" = "Kirghiz", "be" = "Belarusian", "th" = "Thai", "fa" = "Persian", "ne" = "Nepali", "ur" = "Urdu", "ta" = "Tamil",
  "kk" = "Kazakh", "hi" = "Hindi", "pa" = "Punjabi", "si" = "Sinhala", "gu" = "Gujarati", "hy" = "Armenian", NA)})

# group by journal.id and concatenate languages into a single cell separated by ", "
toponyms.language <- toponyms.language %>% group_by(journal.id) %>%
  summarise(language = paste(unique(title.language), collapse = ", "))

# merge both subsets in order to keep only the languages of the journals presenting NA values in journals dataframe
journals.toponyms.language <- merge(journals.language, toponyms.language, by = "journal.id", all.x = TRUE)

# fill in the language variable in journals dataframe with the language identified in papers' titles
journals <- left_join(journals, journals.toponyms.language, by = "journal.id") %>%
  mutate(language = coalesce(language.x, language.y)) %>%
  select(-language.x, -language.y)

# create a new binary variable mainstream.language to separate English and Multi-Language (1 = mainstream) from the rest (0 = not mainstream)
journals <- journals %>% mutate(mainstream.language = ifelse(
    grepl("English|Multi-Language", language, ignore.case = TRUE), 1, 0))


### CATEGORIES
# read Dimensions file
categories <- read_excel("~/Desktop/Local.Research/journal-for-division.xlsx")

# add a category and a category.prop variables to journals dataframe
journals <- left_join(journals, categories, by = "journal.id")

# add a variable for categories acronyms
journals <- journals %>% mutate(category.acronym = case_when(
    category == "Agricultural, Veterinary and Food Sciences" ~ "AgriVetFoodSci",
    category == "Biological Sciences" ~ "BiolSci",
    category == "Biomedical and Clinical Sciences" ~ "BiomClinSci",
    category == "Built Environment and Design" ~ "EnvironDes",
    category == "Chemical Sciences" ~ "ChemSci",
    category == "Commerce, Management, Tourism and Services" ~ "ComManTourServ",
    category == "Creative Arts and Writing" ~ "ArtWrit",
    category == "Earth Sciences" ~ "EarthSci",
    category == "Economics" ~ "Econ",
    category == "Education" ~ "Edu",
    category == "Engineering" ~ "Eng",
    category == "Environmental Sciences" ~ "EnvironSci",
    category == "Health Sciences" ~ "HealthSci",
    category == "History, Heritage and Archaeology" ~ "HisHeritArch",
    category == "Human Society" ~ "HumSoc",
    category == "Information and Computing Sciences" ~ "InfCompSci",
    category == "Language, Communication and Culture" ~ "LangCommCult",
    category == "Law and Legal Studies" ~ "LawLegStud",
    category == "Mathematical Sciences" ~ "MathSci",
    category == "Philosophy and Religious Studies" ~ "PhilReligStud",
    category == "Physical Sciences" ~ "PhysSci",
    category == "Psychology" ~ "Psych",
    TRUE ~ NA_character_))

# add a variable to group categories into fields
health.sciences <- c("BiomClinSci", "HealthSci")
humanities <- c("ArtWrit", "HisHeritArch", "LangCommCult", "PhilReligStud")
life.sciences <- c("AgriVetFoodSci", "BiolSci", "EarthSci", "EnvironSci")
physical.sciences <- c("EnvironDes", "ChemSci", "Eng", "InfCompSci", "MathSci", "PhysSci")
social.sciences <- c("ComManTourServ", "Econ", "Edu", "HumSoc", "LawLegStud", "Psych")

journals$field <- ifelse(journals$category.acronym %in% health.sciences, "Health Sciences",
                         ifelse(journals$category.acronym %in% humanities, "Humanities",
                                ifelse(journals$category.acronym %in% life.sciences, "Life Sciences",
                                       ifelse(journals$category.acronym %in% physical.sciences, "Physical Sciences",
                                              ifelse(journals$category.acronym %in% social.sciences, "Social Sciences", NA)))))


### JOURNAL LEVEL SUMMARY DATA
# compute measures of central tendency, non-central position and variability in all continuous variables: cits.prop, refs.prop, pubs.prop and toponyms.prop
journals$cits.prop <- round(journals$cits.prop, digits = 2)
print(mean(journals$cits.prop, na.rm = TRUE))
print(median(journals$cits.prop, na.rm = TRUE))

print(min(journals$cits.prop, na.rm = TRUE))
print(max(journals$cits.prop, na.rm = TRUE))

print(quantile(journals$cits.prop, probs = c(0.25,0.75), na.rm = TRUE))
print(quantile(journals$cits.prop, probs = 0.9, na.rm = TRUE))
print(sd(journals$cits.prop, na.rm = TRUE))

# compute measures of distribution in all categorical variables: mainstream.database, language, mainstream.language and category
print(journals %>% distinct(journal.id, .keep_all = TRUE) %>% summarise(sum(mainstream.database == 0)))

languages <- journals %>% mutate(language = strsplit(language, ", ")) %>% unnest(language)
print(sort(table(languages[!duplicated(languages[c("journal.id", "language")]), "language"]), decreasing = TRUE))
print(sum(table(languages[!duplicated(languages[c("journal.id", "language")]), "language"])))

print(journals %>% distinct(journal.id, .keep_all = TRUE) %>% summarise(sum(mainstream.language == 0)))

print(sort(table(journals[!duplicated(journals[c("journal.id", "category")]), "category"]), decreasing = TRUE))


### LOCAL JOURNALS SUMMARY DATA
# manually build journals overlap vector for plotting
local.journals <- c("Toponyms" = 3056, "Languages" = 52, "Authors" = 233, "Databases" = 1964, "References" = 2158, "Citations" = 233,
                    "Toponyms&Languages" = 27, "Toponyms&Authors" = 105, "Toponyms&Databases" = 1059, "Toponyms&References" = 676, "Toponyms&Citations" = 105,
                    "Languages&Authors" = 30, "Languages&Databases" = 49, "Languages&References" = 7, "Languages&Citations" = 42, "Authors&Databases" = 533,
                    "Authors&References" = 154, "Authors&Citations" = 274, "Databases&References" = 584, "Databases&Citations" = 584, "References&Citations" = 229,
                    "Toponyms&Languages&Authors" = 0, "Toponyms&Languages&Databases" = 57, "Toponyms&Languages&References" = 2, "Toponyms&Languages&Citations" = 16,
                    "Toponyms&Authors&Databases" = 251, "Toponyms&Authors&References" = 11, "Toponyms&Authors&Citations" = 64, "Toponyms&Databases&References" = 218,
                    "Toponyms&Databases&Citations" = 176, "Toponyms&References&Citations" = 64, "Languages&Authors&Databases" = 93, "Languages&Authors&References" = 0,
                    "Languages&Authors&Citations" = 113, "Languages&Databases&References" = 9, "Languages&Databases&Citations" = 160, "Languages&References&Citations" = 8,
                    "Authors&Databases&References" = 157, "Authors&Databases&Citations" = 1548, "Authors&References&Citations" = 271, "Databases&References&Citations" = 231,
                    "Toponyms&Languages&Authors&Databases" = 21, "Toponyms&Languages&Authors&References" = 0, "Toponyms&Languages&Authors&Citations" = 8,
                    "Toponyms&Languages&Databases&References" = 15, "Toponyms&Languages&Databases&Citations" = 38, "Toponyms&Languages&References&Citations" = 3,
                    "Toponyms&Authors&Databases&References" = 34, "Toponyms&Authors&Databases&Citations" = 261, "Toponyms&Authors&References&Citations" = 30,
                    "Toponyms&Databases&References&Citations" = 78, "Languages&Authors&Databases&References" = 26, "Languages&Authors&Databases&Citations" = 464,
                    "Languages&Authors&References&Citations" = 16, "Languages&Databases&References&Citations" = 28, "Authors&Databases&References&Citations" = 557,
                    "Toponyms&Languages&Authors&Databases&References" = 6, "Toponyms&Languages&Authors&Databases&Citations" = 55,
                    "Toponyms&Languages&Authors&References&Citations" = 6, "Toponyms&Languages&Databases&References&Citations" = 11,
                    "Toponyms&Authors&Databases&References&Citations" = 98, "Languages&Authors&Databases&References&Citations" = 131,
                    "Toponyms&Languages&Authors&Databases&References&Citations" = 21)

# plot intersections matrix to represent the overlap of local journals between approaches
figure1 <- upset(fromExpression(local.journals),
                 nintersects = 63, 
                 nsets = 6,
                 sets = c("Toponyms", "Languages", "Authors", "Databases", "References", "Citations"),
                 mainbar.y.label = "Number of journals",
                 main.bar.color = "grey50",
                 sets.x.label = "Set size",
                 point.size = 1.5,
                 matrix.color = "grey50",
                 line.size = 0.5,
                 order.by = "freq", 
                 decreasing = T,
                 show.numbers = "no",
                 mb.ratio = c(0.5, 0.5),
                 queries = list(list(query = intersects, params = list("Toponyms"), color = "#ED7953", active = TRUE),
                                list(query = intersects, params = list("References"), color = "#ED7953", active = TRUE),
                                list(query = intersects, params = list("Databases"), color = "#ED7953", active = TRUE),
                                list(query = intersects, params = list("Authors"), color = "#ED7953", active = TRUE),
                                list(query = intersects, params = list("Citations"), color = "#ED7953", active = TRUE),
                                list(query = intersects, params = list("Languages"), color = "#ED7953", active = TRUE)))
png(filename = "~/Desktop/Local.Research/Figure1.png", width = 6.27, height = 3.14, units = "in", res = 300)
print(figure1)
dev.off()

# compute the distribution of local journals per disciplinary category and approach
local.toponyms.categories <- unique(merge(local.toponyms.q["journal.id"], categories[, c("journal.id", "category")], by = "journal.id"))
local.toponyms.categories <- local.toponyms.categories %>% group_by(category) %>%
  summarise(sum = n()) %>%
  mutate(total = sum(sum))
local.toponyms.categories$prop <- round(local.toponyms.categories$sum / local.toponyms.categories$total, 2)
local.toponyms.categories$approach <- "Toponyms approach"

local.language.categories <- unique(merge(local.language["journal.id"], categories[, c("journal.id", "category")], by = "journal.id"))
local.language.categories <- local.language.categories %>% group_by(category) %>%
  summarise(sum = n()) %>%
  mutate(total = sum(sum))
local.language.categories$prop <- round(local.language.categories$sum / local.language.categories$total, 2)
local.language.categories$approach <- "Languages approach"

local.pubs.categories <- unique(merge(local.pubs.q["journal.id"], categories[, c("journal.id", "category")], by = "journal.id"))
local.pubs.categories <- local.pubs.categories %>% group_by(category) %>%
  summarise(sum = n()) %>%
  mutate(total = sum(sum))
local.pubs.categories$prop <- round(local.pubs.categories$sum / local.pubs.categories$total, 2)
local.pubs.categories$approach <- "Authors approach"

local.database.categories <- unique(merge(local.database["journal.id"], categories[, c("journal.id", "category")], by = "journal.id"))
local.database.categories <- local.database.categories %>% group_by(category) %>%
  summarise(sum = n()) %>%
  mutate(total = sum(sum))
local.database.categories$prop <- round(local.database.categories$sum / local.database.categories$total, 2)
local.database.categories$approach <- "Databases approach"

local.refs.categories <- unique(merge(local.refs.q["journal.id"], categories[, c("journal.id", "category")], by = "journal.id"))
local.refs.categories <- local.refs.categories %>% group_by(category) %>%
  summarise(sum = n()) %>%
  mutate(total = sum(sum))
local.refs.categories$prop <- round(local.refs.categories$sum / local.refs.categories$total, 2)
local.refs.categories$approach <- "References approach"

local.cits.categories <- unique(merge(local.cits.q["journal.id"], categories[, c("journal.id", "category")], by = "journal.id"))
local.cits.categories <- local.cits.categories %>% group_by(category) %>%
  summarise(sum = n()) %>%
  mutate(total = sum(sum))
local.cits.categories$prop <- round(local.cits.categories$sum / local.cits.categories$total, 2)
local.cits.categories$approach <- "Citations approach"

# build all approaches dataframe with average share of local journals by disciplinary category and field
local.approaches.categories <- rbind(local.toponyms.categories, local.language.categories, local.pubs.categories, local.database.categories, local.refs.categories, local.cits.categories)
local.approaches.categories <- local.approaches.categories %>% select(-sum, -total)
local.approaches.categories <- local.approaches.categories %>% rename(value = prop)

# add new variables to the local.approaches.categories dataframe
local.approaches.categories$value.type <- "prop"
local.approaches.categories$field <- ifelse(local.approaches.categories$category %in% c("Biomedical and Clinical Sciences", "Health Sciences"), "Health Sciences",
                                            ifelse(local.approaches.categories$category %in% c("Creative Arts and Writing", "History, Heritage and Archaeology", "Language, Communication and Culture", "Philosophy and Religious Studies"), "Humanities",
                                                   ifelse(local.approaches.categories$category %in% c("Agricultural, Veterinary and Food Sciences", "Biological Sciences", "Earth Sciences", "Environmental Sciences"), "Life Sciences",
                                                          ifelse(local.approaches.categories$category %in% c("Built Environment and Design", "Chemical Sciences", "Engineering", "Information and Computing Sciences", "Mathematical Sciences", "Physical Sciences"), "Physical Sciences",
                                                                 ifelse(local.approaches.categories$category %in% c("Commerce, Management, Tourism and Services", "Economics", "Education", "Human Society", "Law and Legal Studies", "Psychology"), "Social Sciences", NA)))))

# manually add mean values to the local.approaches.categories dataframe
local.approaches.categories.mean.values <- data.frame(approach = c("Toponyms approach", "Toponyms approach", "Toponyms approach", "Toponyms approach", "Toponyms approach",
                                                       "Languages approach", "Languages approach", "Languages approach", "Languages approach", "Languages approach",
                                                       "Authors approach", "Authors approach", "Authors approach", "Authors approach", "Authors approach",
                                                       "Databases approach", "Databases approach", "Databases approach", "Databases approach", "Databases approach",
                                                       "References approach", "References approach", "References approach", "References approach", "References approach",
                                                       "Citations approach", "Citations approach", "Citations approach", "Citations approach", "Citations approach"),
                                          field = c("Health Sciences", "Humanities", "Life Sciences", "Physical Sciences", "Social Sciences",
                                                     "Health Sciences", "Humanities", "Life Sciences", "Physical Sciences", "Social Sciences",
                                                     "Health Sciences", "Humanities", "Life Sciences", "Physical Sciences", "Social Sciences",
                                                     "Health Sciences", "Humanities", "Life Sciences", "Physical Sciences", "Social Sciences",
                                                     "Health Sciences", "Humanities", "Life Sciences", "Physical Sciences", "Social Sciences",
                                                     "Health Sciences", "Humanities", "Life Sciences", "Physical Sciences", "Social Sciences"),
                                          category = c("HSMean", "HMean", "LSMean", "PSMean", "SSMean",
                                                         "HSMean", "HMean", "LSMean", "PSMean", "SSMean",
                                                         "HSMean", "HMean", "LSMean", "PSMean", "SSMean",
                                                         "HSMean", "HMean", "LSMean", "PSMean", "SSMean",
                                                         "HSMean", "HMean", "LSMean", "PSMean", "SSMean",
                                                         "HSMean", "HMean", "LSMean", "PSMean", "SSMean"),
                                          value = c(0.10, 0.03, 0.06, 0.01, 0.07,
                                                     0.11, 0.05, 0.03, 0.02, 0.06,
                                                     0.15, 0.04, 0.03, 0.03, 0.05,
                                                     0.13, 0.05, 0.03, 0.03, 0.05,
                                                     0.09, 0.06, 0.02, 0.02, 0.08,
                                                     0.10, 0.06, 0.02, 0.03, 0.06),
                                          value.type = c("mean", "mean", "mean", "mean", "mean",
                                                         "mean", "mean", "mean", "mean", "mean",
                                                         "mean", "mean", "mean", "mean", "mean",
                                                         "mean", "mean", "mean", "mean", "mean",
                                                         "mean", "mean", "mean", "mean", "mean",
                                                         "mean", "mean", "mean", "mean", "mean"))
local.approaches.categories <- rbind(local.approaches.categories, local.approaches.categories.mean.values)

local.approaches.categories$approach <- factor(local.approaches.categories$approach, levels = c("Toponyms approach", "Languages approach", "Authors approach", "Databases approach", "References approach", "Citations approach"))
local.approaches.categories$value.type <- factor(local.approaches.categories$value.type, levels = c("mean", "prop"))
local.approaches.categories$field <- factor(local.approaches.categories$field, levels = c("Physical Sciences", "Life Sciences", "Health Sciences", "Social Sciences", "Humanities"))

# reduce category variable for plotting purposes
local.approaches.categories$category <- ifelse(local.approaches.categories$category == "Biomedical and Clinical Sciences", "BiomClinSci",
                                      ifelse(local.approaches.categories$category == "Health Sciences", "HealthSci",
                                             ifelse(local.approaches.categories$category == "Creative Arts and Writing", "ArtWrit",
                                                    ifelse(local.approaches.categories$category == "History, Heritage and Archaeology", "HisHeritArch",
                                                           ifelse(local.approaches.categories$category == "Language, Communication and Culture", "LangCommCult",
                                                                  ifelse(local.approaches.categories$category == "Philosophy and Religious Studies", "PhilReligStud",
                                                                         ifelse(local.approaches.categories$category == "Agricultural, Veterinary and Food Sciences", "AgriVetFoodSci",
                                                                                ifelse(local.approaches.categories$category == "Biological Sciences", "BiolSci",
                                                                                       ifelse(local.approaches.categories$category == "Earth Sciences", "EarthSci",
                                                                                              ifelse(local.approaches.categories$category == "Environmental Sciences", "EnvironSci",
                                                                                                     ifelse(local.approaches.categories$category == "Chemical Sciences", "ChemSci",
                                                                                                            ifelse(local.approaches.categories$category == "Engineering", "Eng",
                                                                                                                   ifelse(local.approaches.categories$category == "Built Environment and Design", "EnvironDes",
                                                                                                                          ifelse(local.approaches.categories$category == "Information and Computing Sciences", "InfCompSci",
                                                                                                                                 ifelse(local.approaches.categories$category == "Mathematical Sciences", "MathSci",
                                                                                                                                        ifelse(local.approaches.categories$category == "Physical Sciences", "PhysSci",
                                                                                                                                               ifelse(local.approaches.categories$category == "Commerce, Management, Tourism and Services", "ComManTourServ",
                                                                                                                                                      ifelse(local.approaches.categories$category == "Economics", "Econ",
                                                                                                                                                             ifelse(local.approaches.categories$category == "Education", "Edu",
                                                                                                                                                                    ifelse(local.approaches.categories$category == "Human Society", "HumSoc",
                                                                                                                                                                           ifelse(local.approaches.categories$category == "Law and Legal Studies", "LawLegStud",
                                                                                                                                                                                  ifelse(local.approaches.categories$category == "Psychology", "Psych", NA))))))))))))))))))))))

# convert variables to factor to order the levels and customize how they appear in the plot
local.approaches.categories$category <- factor(local.approaches.categories$category, levels = c("ArtWrit", "HisHeritArch", "PhilReligStud", "LangCommCult", "Econ", "LawLegStud", "Psych", "ComManTourServ", "Edu", "HumSoc", "HealthSci", "BiomClinSci", "EnvironSci", "EarthSci", "AgriVetFoodSci", "BiolSci", "PhysSci", "ChemSci", "EnvironDes", "MathSci", "InfCompSci", "Eng"))
local.approaches.categories$field <- factor(local.approaches.categories$field, levels = c("Humanities", "Social Sciences", "Health Sciences", "Life Sciences", "Physical Sciences"))

# plot proportions distributions per categories
local.approaches.categories %>% filter(value.type == "prop") %>%
  ggplot(aes(x = approach, y = category, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value)), color = "white", size = 3) +
  scale_x_discrete(labels = c("Toponyms approach" = "Toponyms",
    "Languages approach" = "Languages",
    "Journals approach" = "Authors",
    "Databases approach" = "Databases",
    "References approach" = "References",
    "Citations approach" = "Citations")) +
  scale_fill_viridis_c(name = "Publication share",
    na.value = "grey50",
    option = "plasma",
    limits = c(0, 0.80)) +
  labs(x = "Operational approach", y = "Field & Category") +
  facet_grid(field ~ ., scales = "free_y", space = "free_y", switch = "y") +
  theme_minimal() +
  theme(legend.position = "bottom",
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, face = "bold"))
ggsave("~/Desktop/Local.Research/Figure2.png", width = 6.7, height = 6.27, dpi = 300, bg = "white")


### COUNTRY LEVEL SUMMARY DATA
# before working with country data in local journals, compute the total number of publications per country considering all journals
total.pubs.country <- subset(df.journals.final, select = c(country, pubs))
total.pubs.country <- total.pubs.country[complete.cases(total.pubs.country), ]
total.pubs.country <- total.pubs.country %>% group_by(country) %>% 
  summarise(total.pubs = sum(pubs), .groups = 'drop') %>%
  as.data.frame()

# include full country names
total.pubs.country <- total.pubs.country %>% left_join(map.q %>%
                                                         st_drop_geometry() %>%
                                                         select(ISO_A2_EH, NAME),
                                                       by = c("country" = "ISO_A2_EH")) %>%
                                              rename(full.name = NAME)
total.pubs.country <- total.pubs.country %>% distinct(country, total.pubs, full.name, .keep_all = TRUE)

missing_countries <- tribble(~country, ~full.name,
  "AD", "Andorra",
  "AG", "Antigua and Barbuda",
  "AI", "Anguilla",
  "AN", "Netherlands Antilles",
  "AW", "Aruba",
  "AX", "Åland Islands",
  "BB", "Barbados",
  "BH", "Bahrain",
  "BL", "Saint Barthélemy",
  "BM", "Bermuda",
  "BQ", "Bonaire, Sint Eustatius and Saba",
  "CC", "Cocos (Keeling) Islands",
  "CK", "Cook Islands",
  "CS", "Serbia and Montenegro",
  "CV", "Cabo Verde",
  "CW", "Curaçao",
  "CX", "Christmas Island",
  "DM", "Dominica",
  "EH", "Western Sahara",
  "FM", "Micronesia (Federated States of)",
  "FO", "Faroe Islands",
  "GD", "Grenada",
  "GF", "French Guiana",
  "GG", "Guernsey",
  "GI", "Gibraltar",
  "GP", "Guadeloupe",
  "GS", "South Georgia and the South Sandwich Islands",
  "IM", "Isle of Man",
  "IO", "British Indian Ocean Territory",
  "JE", "Jersey",
  "KI", "Kiribati",
  "KM", "Comoros",
  "KN", "Saint Kitts and Nevis",
  "KY", "Cayman Islands",
  "LC", "Saint Lucia",
  "LI", "Liechtenstein",
  "MC", "Monaco",
  "MF", "Saint Martin (French part)",
  "MH", "Marshall Islands",
  "MQ", "Martinique",
  "MS", "Montserrat",
  "MT", "Malta",
  "MU", "Mauritius",
  "MV", "Maldives",
  "NF", "Norfolk Island",
  "NR", "Nauru",
  "NU", "Niue",
  "PF", "French Polynesia",
  "PN", "Pitcairn Islands",
  "PW", "Palau",
  "RE", "Réunion",
  "SC", "Seychelles",
  "SG", "Singapore",
  "SH", "Saint Helena, Ascension and Tristan da Cunha",
  "SJ", "Svalbard and Jan Mayen",
  "SM", "San Marino",
  "ST", "São Tomé and Príncipe",
  "SX", "Sint Maarten (Dutch part)",
  "TC", "Turks and Caicos Islands",
  "TF", "French Southern Territories",
  "TK", "Tokelau",
  "TO", "Tonga",
  "TV", "Tuvalu",
  "VA", "Vatican City",
  "VC", "Saint Vincent and the Grenadines",
  "VG", "British Virgin Islands",
  "WF", "Wallis and Futuna",
  "WS", "Samoa",
  "YT", "Mayotte")

total.pubs.country <- total.pubs.country %>% rows_update(missing_countries, by = "country", unmatched = "ignore") %>%
  bind_rows(anti_join(missing_countries, total.pubs.country, by = "country"))

iso_to_country <- tribble(~country, ~final.country,
  "AD", "Andorra",
  "AE", "United Arab Emirates",
  "AF", "Afghanistan",
  "AL", "Albania",
  "AM", "Armenia",
  "AO", "Angola",
  "AR", "Argentina",
  "AT", "Austria",
  "AU", "Australia",
  "AZ", "Azerbaijan",
  "BA", "Bosnia and Herzegovina",
  "BD", "Bangladesh",
  "BE", "Belgium",
  "BF", "Burkina Faso",
  "BG", "Bulgaria",
  "BI", "Burundi",
  "BJ", "Benin",
  "BN", "Brunei",
  "BO", "Bolivia",
  "BR", "Brazil",
  "BS", "Bahamas",
  "BT", "Bhutan",
  "BW", "Botswana",
  "BY", "Belarus",
  "BZ", "Belize",
  "CA", "Canada",
  "CD", "Democratic Republic of the Congo",
  "CF", "Central African Republic",
  "CG", "Republic of the Congo",
  "CH", "Switzerland",
  "CI", "Ivory Coast",
  "CL", "Chile",
  "CM", "Cameroon",
  "CN", "China",
  "CO", "Colombia",
  "CR", "Costa Rica",
  "CU", "Cuba",
  "CY", "Cyprus",
  "CZ", "Czechia",
  "DE", "Germany",
  "DJ", "Djibouti",
  "DK", "Denmark",
  "DO", "Dominican Republic",
  "DZ", "Algeria",
  "EC", "Ecuador",
  "EE", "Estonia",
  "EG", "Egypt",
  "ER", "Eritrea",
  "ES", "Spain",
  "ET", "Ethiopia",
  "FI", "Finland",
  "FJ", "Fiji",
  "FR", "France",
  "GA", "Gabon",
  "GB", "United Kingdom",
  "GD", "Grenada",
  "GE", "Georgia",
  "GH", "Ghana",
  "GM", "Gambia",
  "GN", "Guinea",
  "GQ", "Equatorial Guinea",
  "GR", "Greece",
  "GT", "Guatemala",
  "GW", "Guinea-Bissau",
  "GY", "Guyana",
  "HN", "Honduras",
  "HR", "Croatia",
  "HT", "Haiti",
  "HU", "Hungary",
  "ID", "Indonesia",
  "IE", "Ireland",
  "IL", "Israel",
  "IN", "India",
  "IQ", "Iraq",
  "IR", "Iran",
  "IS", "Iceland",
  "IT", "Italy",
  "JM", "Jamaica",
  "JO", "Jordan",
  "JP", "Japan",
  "KE", "Kenya",
  "KG", "Kyrgyzstan",
  "KH", "Cambodia",
  "KP", "North Korea",
  "KR", "South Korea",
  "KW", "Kuwait",
  "KZ", "Kazakhstan",
  "LA", "Laos",
  "LB", "Lebanon",
  "LK", "Sri Lanka",
  "LR", "Liberia",
  "LS", "Lesotho",
  "LT", "Lithuania",
  "LU", "Luxembourg",
  "LV", "Latvia",
  "LY", "Libya",
  "MA", "Morocco",
  "MD", "Moldova",
  "ME", "Montenegro",
  "MG", "Madagascar",
  "MK", "North Macedonia",
  "ML", "Mali",
  "MM", "Myanmar",
  "MN", "Mongolia",
  "MR", "Mauritania",
  "MT", "Malta",
  "MU", "Mauritius",
  "MV", "Maldives",
  "MW", "Malawi",
  "MX", "Mexico",
  "MY", "Malaysia",
  "MZ", "Mozambique",
  "NA", "Namibia",
  "NE", "Niger",
  "NG", "Nigeria",
  "NI", "Nicaragua",
  "NL", "Netherlands",
  "NO", "Norway",
  "NP", "Nepal",
  "NZ", "New Zealand",
  "OM", "Oman",
  "PA", "Panama",
  "PE", "Peru",
  "PG", "Papua New Guinea",
  "PH", "Philippines",
  "PK", "Pakistan",
  "PL", "Poland",
  "PS", "Palestine",
  "PT", "Portugal",
  "PY", "Paraguay",
  "QA", "Qatar",
  "RO", "Romania",
  "RS", "Serbia",
  "RU", "Russia",
  "RW", "Rwanda",
  "SA", "Saudi Arabia",
  "SB", "Solomon Islands",
  "SD", "Sudan",
  "SE", "Sweden",
  "SG", "Singapore",
  "SI", "Slovenia",
  "SK", "Slovakia",
  "SL", "Sierra Leone",
  "SM", "San Marino",
  "SN", "Senegal",
  "SO", "Somalia",
  "SR", "Suriname",
  "SS", "South Sudan",
  "SV", "El Salvador",
  "SY", "Syria",
  "SZ", "Eswatini",
  "TD", "Chad",
  "TG", "Togo",
  "TH", "Thailand",
  "TJ", "Tajikistan",
  "TL", "Timor-Leste",
  "TM", "Turkmenistan",
  "TN", "Tunisia",
  "TR", "Turkey",
  "TT", "Trinidad and Tobago",
  "TW", "Taiwan",
  "TZ", "Tanzania",
  "UA", "Ukraine",
  "UG", "Uganda",
  "US", "United States",
  "UY", "Uruguay",
  "UZ", "Uzbekistan",
  "VE", "Venezuela",
  "VN", "Vietnam",
  "VU", "Vanuatu",
  "WS", "Samoa",
  "YE", "Yemen",
  "ZA", "South Africa",
  "ZM", "Zambia",
  "ZW", "Zimbabwe",
  "VA", "Vatican City",
  "XK", "Kosovo",
  "AG", "Antigua and Barbuda",
  "AI", "United Kingdom",
  "AN", "Netherlands",
  "AW", "Netherlands",
  "AX", "Finland",
  "BB", "Barbados",
  "BH", "Bahrain",
  "BL", "France",
  "BM", "United Kingdom",
  "BQ", "Netherlands",
  "CC", "Australia",
  "CK", "New Zealand",
  "CS", "Serbia",
  "CV", "Cabo Verde",
  "CW", "Netherlands",
  "CX", "Australia",
  "DM", "Dominica",
  "EH", "Morocco",
  "FK", "United Kingdom",
  "FM", "Micronesia",
  "FO", "Denmark",
  "GF", "France",
  "GG", "United Kingdom",
  "GI", "United Kingdom",
  "GL", "Denmark",
  "GP", "France",
  "GS", "United Kingdom",
  "GU", "United States",
  "HK", "China",
  "IM", "United Kingdom",
  "IO", "United Kingdom",
  "JE", "United Kingdom",
  "KI", "Kiribati",
  "KM", "Comoros",
  "KN", "Saint Kitts and Nevis",
  "KY", "United Kingdom",
  "LC", "Saint Lucia",
  "LI", "Liechtenstein",
  "MC", "Monaco",
  "MF", "France",
  "MH", "Marshall Islands",
  "MQ", "France",
  "MS", "United Kingdom",
  "NC", "France",
  "NF", "Australia",
  "NR", "Nauru",
  "NU", "New Zealand",
  "PF", "France",
  "PN", "United Kingdom",
  "PW", "Palau",
  "RE", "France",
  "SC", "Seychelles",
  "SG", "Singapore",
  "SH", "United Kingdom",
  "SJ", "Norway",
  "SM", "San Marino",
  "ST", "São Tomé and Príncipe",
  "SX", "Netherlands",
  "TC", "United Kingdom",
  "TF", "France",
  "TK", "New Zealand",
  "TO", "Tonga",
  "TV", "Tuvalu",
  "VC", "Saint Vincent and the Grenadines",
  "VG", "United Kingdom",
  "WF", "France",
  "YT", "France")

total.pubs.country <- total.pubs.country %>% left_join(iso_to_country, by = "country")
total.pubs.country <- total.pubs.country %>% group_by(final.country) %>%
                                             summarise(total.pubs = sum(total.pubs, na.rm = TRUE)) %>%
                                             ungroup()

# isolate local research journals according to the toponyms approach (cut-off thresholds for trial >= 0.14 (3º quartile))
local.toponyms.q <- subset(journals, select = c("journal.id", "journal.name", "toponyms.prop"), toponyms.prop >= 0.14)

# subset the necessary variables to work at country level and remove NA values
local.toponyms.countries.q <- df.journals.final[df.journals.final$journal.id %in% local.toponyms.q$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.toponyms.countries.q <- local.toponyms.countries.q[complete.cases(local.toponyms.countries.q), ]

# add nation to which territories belong
local.toponyms.countries.q <- local.toponyms.countries.q %>% left_join(iso_to_country, by = "country")

# compute each country's publication share in local journals = n pubs per country in local journals / N pubs per country in all journals
local.toponyms.countries.q <- aggregate(pubs ~ final.country, data = local.toponyms.countries.q, FUN = sum)
local.toponyms.countries.q <- local.toponyms.countries.q %>% left_join(total.pubs.country, by = "final.country") %>%
  mutate(pubs.share = pubs / total.pubs)


# isolate local research journals according to the languages approach (= 0)
local.language <- subset(journals, select = c("journal.id", "journal.name", "mainstream.language"), mainstream.language == 0)

# subset the necessary variables to work at country level and remove NA values
local.language.countries <- df.journals.final[df.journals.final$journal.id %in% local.language$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.language.countries <- local.language.countries[complete.cases(local.language.countries), ]

# add nation to which territories belong
local.language.countries <- local.language.countries %>% left_join(iso_to_country, by = "country")

# compute each country's publication share in local journals = n pubs per country in local journals / N pubs per country in all journals
local.language.countries <- aggregate(pubs ~ final.country, data = local.language.countries, FUN = sum)
local.language.countries <- local.language.countries %>% left_join(total.pubs.country, by = "final.country") %>%
  mutate(pubs.share = pubs / total.pubs)


# isolate local research journals according to the authors approach (cut-off thresholds for trial >= 0.82 (3º quartile))
local.pubs.q <- subset(journals, select = c("journal.id", "journal.name", "pubs.prop", "category.acronym", "category.prop", "field"), pubs.prop >= 0.82)

# subset the necessary variables to work at country level and remove NA values
local.pubs.countries.q <- df.journals.final[df.journals.final$journal.id %in% local.pubs.q$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.pubs.countries.q <- local.pubs.countries.q[complete.cases(local.pubs.countries.q), ]

# add nation to which territories belong
local.pubs.countries.q <- local.pubs.countries.q %>% left_join(iso_to_country, by = "country")

# compute each country's publication share in local journals = n pubs per country in local journals / N pubs per country in all journals
local.pubs.countries.q <- aggregate(pubs ~ final.country, data = local.pubs.countries.q, FUN = sum)
local.pubs.countries.q <- local.pubs.countries.q %>% left_join(total.pubs.country, by = "final.country") %>%
  mutate(pubs.share = pubs / total.pubs)


# isolate local research journals according to the databases approach (= 0)
local.database <- subset(journals, select = c("journal.id", "journal.name", "mainstream.database", "category.acronym", "category.prop", "field"), mainstream.database == 0)

# subset the necessary variables to work at country level and remove NA values
local.database.countries <- df.journals.final[df.journals.final$journal.id %in% local.database$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.database.countries <- local.database.countries[complete.cases(local.database.countries), ]

# add nation to which territories belong
local.database.countries <- local.database.countries %>% left_join(iso_to_country, by = "country")

# compute each country's publication share in local journals = n pubs per country in local journals / N pubs per country in all journals
local.database.countries <- aggregate(pubs ~ final.country, data = local.database.countries, FUN = sum)
local.database.countries <- local.database.countries %>% left_join(total.pubs.country, by = "final.country") %>%
  mutate(pubs.share = pubs / total.pubs)


# isolate local research journals according to the references approach (cut-off thresholds for trial >= 0.40 (3º quartile))
local.refs.q <- subset(journals, select = c("journal.id", "journal.name", "refs.prop", "category.acronym", "category.prop", "field"), refs.prop >= 0.40)

# subset the necessary variables to work at country level and remove NA values
local.refs.countries.q <- df.journals.final[df.journals.final$journal.id %in% local.refs.q$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.refs.countries.q <- local.refs.countries.q[complete.cases(local.refs.countries.q), ]

# add nation to which territories belong
local.refs.countries.q <- local.refs.countries.q %>% left_join(iso_to_country, by = "country")

# compute each country's publication share in local journals = n pubs per country in local journals / N pubs per country in all journals
local.refs.countries.q <- aggregate(pubs ~ final.country, data = local.refs.countries.q, FUN = sum)
local.refs.countries.q <- local.refs.countries.q %>% left_join(total.pubs.country, by = "final.country") %>%
  mutate(pubs.share = pubs / total.pubs)


# isolate local research journals according to the citations approach (cut-off thresholds for trial >= 0.50 (3º quartile))
local.cits.q <- subset(journals, select = c("journal.id", "journal.name", "cits.prop", "category.acronym", "category.prop", "field"), cits.prop >= 0.50)

# subset the necessary variables to work at country level and remove NA values
local.cits.countries.q <- df.journals.final[df.journals.final$journal.id %in% local.cits.q$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.cits.countries.q <- local.cits.countries.q[complete.cases(local.cits.countries.q), ]

# add nation to which territories belong
local.cits.countries.q <- local.cits.countries.q %>% left_join(iso_to_country, by = "country")

# compute each country's publication share in local journals = n pubs per country in local journals / N pubs per country in all journals
local.cits.countries.q <- aggregate(pubs ~ final.country, data = local.cits.countries.q, FUN = sum)
local.cits.countries.q <- local.cits.countries.q %>% left_join(total.pubs.country, by = "final.country") %>%
  mutate(pubs.share = pubs / total.pubs)


# compute measures of central tendency, non-central position and variability in all approaches within local research journals, focusing on countries' publication share
print(mean(local.cits.countries.q$pubs.share, na.rm = TRUE))
print(median(local.cits.countries.q$pubs.share, na.rm = TRUE))

print(min(local.cits.countries.q$pubs.share, na.rm = TRUE))
print(max(local.cits.countries.q$pubs.share, na.rm = TRUE))

print(quantile(local.cits.countries.q$pubs.share, probs = c(0.25,0.75), na.rm = TRUE))
print(sd(local.cits.countries.q$pubs.share, na.rm = TRUE))


### WORLD MAPS
map.world <- st_read("~/Desktop/Local.Research/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")

# plot toponyms world map
local.toponyms.countries.q$ISO_A2_EH <- countrycode(local.toponyms.countries.q$final.country, origin = "country.name", destination = "iso2c")
local.toponyms.countries.q$ISO_A2_EH[local.toponyms.countries.q$final.country == "Kosovo"] <- "XK"
local.toponyms.countries.q$ISO_A2_EH[local.toponyms.countries.q$final.country == "Micronesia"] <- "FM"
local.toponyms.map.q <- merge(map.world, local.toponyms.countries.q, by.x = "ISO_A2_EH", by.y = "ISO_A2_EH", all.x = TRUE)

# plot language world map
local.language.countries$ISO_A2_EH <- countrycode(local.language.countries$final.country, origin = "country.name", destination = "iso2c")
local.language.countries$ISO_A2_EH[local.language.countries$final.country == "Kosovo"] <- "XK"
local.language.countries$ISO_A2_EH[local.language.countries$final.country == "Micronesia"] <- "FM"
local.language.map <- merge(map.world, local.language.countries, by.x = "ISO_A2_EH", by.y = "ISO_A2_EH", all.x = TRUE)

# plot pubs world map
local.pubs.countries.q$ISO_A2_EH <- countrycode(local.pubs.countries.q$final.country, origin = "country.name", destination = "iso2c")
local.pubs.countries.q$ISO_A2_EH[local.pubs.countries.q$final.country == "Kosovo"] <- "XK"
local.pubs.countries.q$ISO_A2_EH[local.pubs.countries.q$final.country == "Micronesia"] <- "FM"
local.pubs.map.q <- merge(map.world, local.pubs.countries.q, by.x = "ISO_A2_EH", by.y = "ISO_A2_EH", all.x = TRUE)

# plot database world map
local.database.countries$ISO_A2_EH <- countrycode(local.database.countries$final.country, origin = "country.name", destination = "iso2c")
local.database.countries$ISO_A2_EH[local.database.countries$final.country == "Kosovo"] <- "XK"
local.database.countries$ISO_A2_EH[local.database.countries$final.country == "Micronesia"] <- "FM"
local.database.map <- merge(map.world, local.database.countries, by.x = "ISO_A2_EH", by.y = "ISO_A2_EH", all.x = TRUE)

# plot refs world map
local.refs.countries.q$ISO_A2_EH <- countrycode(local.refs.countries.q$final.country, origin = "country.name", destination = "iso2c")
local.refs.countries.q$ISO_A2_EH[local.refs.countries.q$final.country == "Kosovo"] <- "XK"
local.refs.countries.q$ISO_A2_EH[local.refs.countries.q$final.country == "Micronesia"] <- "FM"
local.refs.map.q <- merge(map.world, local.refs.countries.q, by.x = "ISO_A2_EH", by.y = "ISO_A2_EH", all.x = TRUE)

# plot cits world map
local.cits.countries.q$ISO_A2_EH <- countrycode(local.cits.countries.q$final.country, origin = "country.name", destination = "iso2c")
local.cits.countries.q$ISO_A2_EH[local.cits.countries.q$final.country == "Kosovo"] <- "XK"
local.cits.countries.q$ISO_A2_EH[local.cits.countries.q$final.country == "Micronesia"] <- "FM"
local.cits.map.q <- merge(map.world, local.cits.countries.q, by.x = "ISO_A2_EH", by.y = "ISO_A2_EH", all.x = TRUE)

# create one faceted plot with 6 maps and 1 common legend
local.toponyms.map.q$approach <- "Toponyms approach"
local.language.map$approach <- "Languages approach"
local.pubs.map.q$approach <- "Authors approach"
local.database.map$approach <- "Databases approach"
local.refs.map.q$approach <- "References approach"
local.cits.map.q$approach <- "Citations approach"

map.q <- bind_rows(local.toponyms.map.q, local.language.map, local.pubs.map.q, local.database.map, local.refs.map.q, local.cits.map.q)
map.q$approach <- factor(map.q$approach, levels = c("Toponyms approach", "Languages approach", "Authors approach", "Databases approach", "References approach", "Citations approach"))

ggplot() +
  geom_sf(data = map.q %>% filter(ISO_A2_EH != "AQ"), aes(fill = pubs.share)) +
  scale_fill_viridis_c(name = "Publication share", na.value = "grey50", option = "plasma") +
  facet_wrap(~approach, ncol = 2) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("~/Desktop/Local.Research/Figure4.png", width = 8.27, height = 8.27, dpi = 300, bg = "white")


### REGIONS
df.journals.final <- df.journals.final %>% left_join(categories %>% select(journal.id, category), by = "journal.id", relationship = "many-to-many")

# add UN regions and subregions
df.journals.final$region <- countrycode(df.journals.final$country, origin = "iso2c", destination = "un.region.name")
df.journals.final$subregion <- countrycode(df.journals.final$country, origin = "iso2c", destination = "un.regionsub.name")
df.journals.final <- df.journals.final %>% mutate(region = case_when(country %in% c("US", "CA") ~ "United States and Canada",
                                                                     region == "Americas" ~ "Latin America and the Caribbean", TRUE ~ region))

regions.all.journals <- subset(df.journals.final, select = c(journal.id, region, pubs, category))
regions.all.journals <- regions.all.journals %>% group_by(region, category) %>%
  summarise(all.total.pubs = sum(pubs, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame() %>% na.omit()

## toponyms approach
regions.toponyms.journals <- df.journals.final[df.journals.final$journal.id %in% local.toponyms.q$journal.id, c("journal.id", "region", "pubs", "category")]
regions.toponyms.journals <- regions.toponyms.journals %>% group_by(region, category) %>%
  summarise(tops.total.pubs = sum(pubs, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame() %>% na.omit()
regions.toponyms.journals <- merge(regions.toponyms.journals, regions.all.journals, by = c("region", "category"))
regions.toponyms.journals$pubs.share <- regions.toponyms.journals$tops.total.pubs / regions.toponyms.journals$all.total.pubs
regions.toponyms.journals$pubs.share <- sprintf("%.4f", regions.toponyms.journals$pubs.share)
# add a variable for the specific approach
regions.toponyms.journals$approach <- rep("Toponyms", nrow(regions.toponyms.journals))

## language approach
regions.language.journals <- df.journals.final[df.journals.final$journal.id %in% local.language$journal.id, c("journal.id", "region", "pubs", "category")]
regions.language.journals <- regions.language.journals %>% group_by(region, category) %>%
  summarise(lang.total.pubs = sum(pubs, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame() %>% na.omit()
regions.language.journals <- merge(regions.language.journals, regions.all.journals, by = c("region", "category"))
regions.language.journals$pubs.share <- regions.language.journals$lang.total.pubs / regions.language.journals$all.total.pubs
regions.language.journals$pubs.share <- sprintf("%.4f", regions.language.journals$pubs.share)
# add a variable for the specific approach
regions.language.journals$approach <- rep("Languages", nrow(regions.language.journals))

## pubs approach
regions.pubs.journals <- df.journals.final[df.journals.final$journal.id %in% local.pubs.q$journal.id, c("journal.id", "region", "pubs", "category")]
regions.pubs.journals <- regions.pubs.journals %>% group_by(region, category) %>%
  summarise(pubs.total.pubs = sum(pubs, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame() %>% na.omit()
regions.pubs.journals <- merge(regions.pubs.journals, regions.all.journals, by = c("region", "category"))
regions.pubs.journals$pubs.share <- regions.pubs.journals$pubs.total.pubs / regions.pubs.journals$all.total.pubs
regions.pubs.journals$pubs.share <- sprintf("%.4f", regions.pubs.journals$pubs.share)
# add a variable for the specific approach
regions.pubs.journals$approach <- rep("Authors", nrow(regions.pubs.journals))

## database approach
regions.database.journals <- df.journals.final[df.journals.final$journal.id %in% local.database$journal.id, c("journal.id", "region", "pubs", "category")]
regions.database.journals <- regions.database.journals %>% group_by(region, category) %>%
  summarise(data.total.pubs = sum(pubs, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame() %>% na.omit()
regions.database.journals <- merge(regions.database.journals, regions.all.journals, by = c("region", "category"))
regions.database.journals$pubs.share <- regions.database.journals$data.total.pubs / regions.database.journals$all.total.pubs
regions.database.journals$pubs.share <- sprintf("%.4f", regions.database.journals$pubs.share)
# add a variable for the specific approach
regions.database.journals$approach <- rep("Databases", nrow(regions.database.journals))

## refs approach
regions.refs.journals <- df.journals.final[df.journals.final$journal.id %in% local.refs.q$journal.id, c("journal.id", "region", "pubs", "category")]
regions.refs.journals <- regions.refs.journals %>% group_by(region, category) %>%
  summarise(refs.total.pubs = sum(pubs, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame() %>% na.omit()
regions.refs.journals <- merge(regions.refs.journals, regions.all.journals, by = c("region", "category"))
regions.refs.journals$pubs.share <- regions.refs.journals$refs.total.pubs / regions.refs.journals$all.total.pubs
regions.refs.journals$pubs.share <- sprintf("%.4f", regions.refs.journals$pubs.share)
# add a variable for the specific approach
regions.refs.journals$approach <- rep("References", nrow(regions.refs.journals))

## cits approach
regions.cits.journals <- df.journals.final[df.journals.final$journal.id %in% local.cits.q$journal.id, c("journal.id", "region", "pubs", "category")]
regions.cits.journals <- regions.cits.journals %>% group_by(region, category) %>%
  summarise(cits.total.pubs = sum(pubs, na.rm = TRUE), .groups = 'drop') %>%
  as.data.frame() %>% na.omit()
regions.cits.journals <- merge(regions.cits.journals, regions.all.journals, by = c("region", "category"))
regions.cits.journals$pubs.share <- regions.cits.journals$cits.total.pubs / regions.cits.journals$all.total.pubs
regions.cits.journals$pubs.share <- sprintf("%.4f", regions.cits.journals$pubs.share)
# add a variable for the specific approach
regions.cits.journals$approach <- rep("Citations", nrow(regions.cits.journals))

# merge all regions to plot
regions <- rbind(regions.toponyms.journals %>% select(region, category, pubs.share, approach),
                 regions.language.journals %>% select(region, category, pubs.share, approach),
                 regions.pubs.journals %>% select(region, category, pubs.share, approach),
                 regions.database.journals %>% select(region, category, pubs.share, approach),
                 regions.refs.journals %>% select(region, category, pubs.share, approach),
                 regions.cits.journals %>% select(region, category, pubs.share, approach))

# reduce category variable for plotting purposes
regions$category <- ifelse(regions$category == "Biomedical and Clinical Sciences", "BiomClinSci",
                                              ifelse(regions$category == "Health Sciences", "HealthSci",
                                                     ifelse(regions$category == "Creative Arts and Writing", "ArtWrit",
                                                            ifelse(regions$category == "History, Heritage and Archaeology", "HisHeritArch",
                                                                   ifelse(regions$category == "Language, Communication and Culture", "LangCommCult",
                                                                          ifelse(regions$category == "Philosophy and Religious Studies", "PhilReligStud",
                                                                                 ifelse(regions$category == "Agricultural, Veterinary and Food Sciences", "AgriVetFoodSci",
                                                                                        ifelse(regions$category == "Biological Sciences", "BiolSci",
                                                                                               ifelse(regions$category == "Earth Sciences", "EarthSci",
                                                                                                      ifelse(regions$category == "Environmental Sciences", "EnvironSci",
                                                                                                             ifelse(regions$category == "Chemical Sciences", "ChemSci",
                                                                                                                    ifelse(regions$category == "Engineering", "Eng",
                                                                                                                           ifelse(regions$category == "Built Environment and Design", "EnvironDes",
                                                                                                                                  ifelse(regions$category == "Information and Computing Sciences", "InfCompSci",
                                                                                                                                         ifelse(regions$category == "Mathematical Sciences", "MathSci",
                                                                                                                                                ifelse(regions$category == "Physical Sciences", "PhysSci",
                                                                                                                                                       ifelse(regions$category == "Commerce, Management, Tourism and Services", "ComManTourServ",
                                                                                                                                                              ifelse(regions$category == "Economics", "Econ",
                                                                                                                                                                     ifelse(regions$category == "Education", "Edu",
                                                                                                                                                                            ifelse(regions$category == "Human Society", "HumSoc",
                                                                                                                                                                                   ifelse(regions$category == "Law and Legal Studies", "LawLegStud",
                                                                                                                                                                                          ifelse(regions$category == "Psychology", "Psych", NA))))))))))))))))))))))

# add new variable field to the regions dataframe
regions$field <- ifelse(regions$category %in% c("BiomClinSci", "HealthSci"), "Health Sciences",
                        ifelse(regions$category %in% c("ArtWrit", "HisHeritArch", "LangCommCult", "PhilReligStud"), "Humanities",
                               ifelse(regions$category %in% c("AgriVetFoodSci", "BiolSci", "EarthSci", "EnvironSci"), "Life Sciences",
                                      ifelse(regions$category %in% c("EnvironDes", "ChemSci", "Eng", "InfCompSci", "MathSci", "PhysSci"), "Physical Sciences",
                                             ifelse(regions$category %in% c("ComManTourServ", "Econ", "Edu", "HumSoc", "LawLegStud", "Psych"), "Social Sciences", NA)))))

# convert variables to factor to order the levels and customize how they appear in the plot
regions$approach <- factor(regions$approach, levels = c("Toponyms", "Languages", "Authors", "Databases", "References", "Citations"))
regions$category <- factor(regions$category, levels = c("PhilReligStud", "LangCommCult", "HisHeritArch", "ArtWrit", "ComManTourServ", "Psych", "LawLegStud", "Econ", "Edu", "HumSoc",
                                                        "BiomClinSci", "HealthSci", "BiolSci", "AgriVetFoodSci", "EnvironSci", "EarthSci", "ChemSci", "MathSci", "PhysSci", "Eng", "InfCompSci", "EnvironDes"))
regions <- regions %>% mutate(field_label = case_when(field == "Humanities" ~ "Humanities", field == "Social Sciences" ~ "Social\nSciences", field == "Health Sciences" ~ "Health\nSciences",
                                                      field == "Life Sciences" ~ "Life\nSciences", field == "Physical Sciences" ~ "Physical\nSciences", TRUE ~ field))
regions <- regions %>% mutate(field_label = factor(field_label, levels = c("Humanities", "Social\nSciences", "Health\nSciences", "Life\nSciences", "Physical\nSciences")))
regions <- regions %>% mutate(region_label = case_when(region == "Europe" ~ "Europe", region == "Africa" ~ "Africa", region == "United States and Canada" ~ "United States\nand Canada",
                                                       region == "Latin America and the Caribbean" ~ "Latin America\nand the Caribbean", region == "Oceania" ~ "Oceania", region == "Asia" ~ "Asia", TRUE ~ field))
regions <- regions %>% mutate(region_label = factor(region_label, levels = c("Oceania", "United States\nand Canada", "Europe", "Asia", "Latin America\nand the Caribbean", "Africa")))

# convert variable to numeric
regions$pubs.share <- as.numeric(regions$pubs.share)

# plot regions per categories
ggplot(regions, aes(x = approach, y = category, fill = pubs.share)) +
  geom_tile() +
  facet_grid(field_label ~ region_label, scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_viridis_c(name = "Publication share", na.value = "grey50", option = "plasma") +
  labs(x = "Operational approach", y = "Field & Category") +
  theme_minimal() +
  theme(legend.position = "bottom",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 90, face = "bold"))
ggsave("~/Desktop/Local.Research/Figure5.png", width = 8.27, height = 9.27, dpi = 300, bg = "white")


### CORRELATIONS
# country level with 3ºQ data
corr.local.toponyms.q <- subset(local.toponyms.countries.q, select = c(final.country, pubs.share))
corr.local.toponyms.q <- corr.local.toponyms.q %>% rename("toponyms.pubs.share" = "pubs.share")

corr.local.language <- subset(local.language.countries, select = c(final.country, pubs.share))
corr.local.language <- corr.local.language %>% rename("language.pubs.share" = "pubs.share")

corr.local.pubs.q <- subset(local.pubs.countries.q, select = c(final.country, pubs.share))
corr.local.pubs.q <- corr.local.pubs.q %>% rename("pubs.pubs.share" = "pubs.share")

corr.local.database <- subset(local.database.countries, select = c(final.country, pubs.share))
corr.local.database <- corr.local.database %>% rename("database.pubs.share" = "pubs.share")

corr.local.refs.q <- subset(local.refs.countries.q, select = c(final.country, pubs.share))
corr.local.refs.q <- corr.local.refs.q %>% rename("refs.pubs.share" = "pubs.share")

corr.local.cits.q <- subset(local.cits.countries.q, select = c(final.country, pubs.share))
corr.local.cits.q <- corr.local.cits.q %>% rename("cits.pubs.share" = "pubs.share")

corr.local.q <- Reduce(function(x, y) merge(x, y, by = "final.country", all = TRUE), list(corr.local.toponyms.q, corr.local.language, corr.local.pubs.q, corr.local.database, corr.local.refs.q, corr.local.cits.q))
corr.local.q <- corr.local.q %>% rename("Tops prop" = "toponyms.pubs.share", "Non-Eng pub" = "language.pubs.share", "Pub prop" = "pubs.pubs.share", "Non-W/S index" = "database.pubs.share", "Ref prop" = "refs.pubs.share", "Cit prop" = "cits.pubs.share")

corr.local.matrix.q <- cor(corr.local.q[, c("Tops prop", "Non-Eng pub", "Pub prop", "Non-W/S index", "Ref prop", "Cit prop")], use = "pairwise.complete.obs")

ggcorrplot(corr.local.matrix.q,
           type = "lower",
           ggtheme = ggplot2::theme_minimal,
           lab = TRUE) +
  scale_fill_gradientn(colours = viridis(10, option = "plasma")) +
  labs(fill = "Value")
ggsave("~/Desktop/Local.Research/Figure3.png", width = 6.27, height = 5.27, dpi = 300, bg = "white")


### PCA
# add Global North / Global South categories to the countries listed in df total.pubs.country
total.pubs.country <- total.pubs.country %>% mutate(global.divide = case_when(
                                             final.country %in% c("Australia", "Austria", "Belgium", "Canada", "Switzerland", "Czechia", "Germany", "Denmark", "Spain", "Estonia",
                                                                  "Finland", "France", "United Kingdom", "Greece", "Hungary", "Ireland", "Iceland", "Israel", "Italy", "Japan",
                                                                  "South Korea", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Norway", "New Zealand", "Poland", "Portugal",
                                                                  "Slovakia", "Slovenia", "Sweden", "United States", "Andorra", "Bulgaria", "Bosnia and Herzegovina", "Belarus",
                                                                  "Cyprus", "Croatia", "Liechtenstein", "Monaco", "Moldova", "North Macedonia", "Malta", "Montenegro", "Romania",
                                                                  "Russia", "San Marino", "Serbia", "Ukraine", "Vatican City") ~ "Global North", TRUE ~ "Global South"))

# filter out incomplete cases and countries with less than 3000 publications in the period
pca.countries <- corr.local.q %>% filter(complete.cases(select(., -final.country))) %>%
                                  inner_join(total.pubs.country %>% filter(total.pubs >= 3000), by = "final.country") #%>%

# keep relevant numeric variables only
pca.data <- pca.countries %>% select(-final.country, -total.pubs, -global.divide)

# run PCA analysis
pca.analysis <- prcomp(pca.data, scale. = TRUE)
summary(pca.analysis)
pca.analysis$rotation

# extract scores and attach countries
pca.scores <- as.data.frame(pca.analysis$x) %>% mutate(country = pca.countries$final.country)

# create correlation circle
pca.vars <- pca.countries[, c("Tops prop", "Non-Eng pub", "Pub prop", "Non-W/S index", "Ref prop", "Cit prop")]
pca.result <- PCA(pca.vars, scale.unit = TRUE, graph = FALSE)
pca.coords <- as.data.frame(pca.result$ind$coord)
pca.coords$country <- pca.countries$final.country
pca.coords <- pca.coords %>% left_join(total.pubs.country, by = c("country" = "final.country"))

# add ISO codes and UN regions directly from country names
pca.coords <- pca.coords %>% mutate(iso2c   = countrycode(country, origin = "country.name", destination = "iso2c"),
                                    region  = countrycode(country, origin = "country.name", destination = "un.region.name"),
                                    subregion = countrycode(country, origin = "country.name", destination = "un.regionsub.name")) %>%
  mutate(region = case_when(iso2c %in% c("US", "CA") ~ "United States and Canada",
                            region == "Americas" ~ "Latin America and the Caribbean",
                            country == "Taiwan" ~ "Asia", TRUE ~ region))

pca.coords$region <- factor(pca.coords$region, levels = c("Latin America and the Caribbean", "Europe", "Africa", "Oceania", "Asia", "United States and Canada"))

# plot PCA by country and region
arrow_scale <- 3
loadings <- as.data.frame(pca.result$var$coord)
loadings$varname <- rownames(loadings)
loadings <- loadings %>% mutate(varname = case_when(varname == "Tops prop" ~ "Toponyms",
                                                    varname == "Non-Eng pub" ~ "Languages",
                                                    varname == "Pub prop" ~ "Authors",
                                                    varname == "Non-W/S index"  ~ "Databases",
                                                    varname == "Ref prop" ~ "References",
                                                    varname == "Cit prop" ~ "Citations", TRUE ~ varname))

ggplot() +
  geom_point(data = pca.coords, aes(x = Dim.1, y = Dim.2, color = region), size = 2, alpha = 0.9) +
  geom_text(data = subset(pca.coords, total.pubs >= 9000), aes(x = Dim.1, y = Dim.2, label = iso2c, color = region), vjust = -0.7, size = 3, show.legend = FALSE) +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = Dim.1 * arrow_scale, yend = Dim.2 * arrow_scale),
               arrow = arrow(length = unit(0.2, "cm")), color = "grey30", linewidth = 0.6) +
  geom_text(data = loadings, aes(x = Dim.1 * arrow_scale, y = Dim.2 * arrow_scale, label = varname),
            color = "grey30", size = 3, vjust = -0.7) +
  labs(x = "Principal Component 1", y = "Principal Component 2", color = "World\nregion") +
  scale_color_manual(values = c("Latin America and the Caribbean" = "#FFDF3D",
                                "Africa" =  "#FCA636",
                                "Asia" = "#FC7300" ,
                                "Europe" = "#9C179E", 
                                "Oceania" = "#6A00A8",
                                "United States and Canada" = "#0D0887")) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text = element_text(size = 10), axis.title = element_text(size = 11),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "grey80", linewidth = 0.3))
ggsave("~/Desktop/Local.Research/Figure6.png", width = 8.27, height = 6.27, dpi = 300, bg = "white")

### SAVE DATAFRAMES
save.image("~/Desktop/Local.Research/local.research.data.RData")