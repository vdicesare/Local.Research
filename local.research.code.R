library(parallel)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(reshape2)
library(readxl)
library(ggplot2)
library(ggcorrplot)
library(viridisLite)
library(sf)
library(countrycode)
library(cld2)
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

for (i in 1:10) {
  start_index <- (i - 1) * chunk_size + 1
  end_index <- min(i * chunk_size, n_rows)
  
  current_chunk <- toponyms[start_index:end_index, ]
  
  toponyms.parts[[i]] <- current_chunk
}

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
  for (i in seq_along(countries)) {
    if (countries[i] %in% c("Afganistán", "Afeganistão", "Afganistan", "アフガニスタン", "Afghanistan")) { translated_countries[i] <- "Afghanistan"
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
toponyms.part1 <- toponyms.part1 %>%
  mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part1$toponym.english <- as.character(toponyms.part1$toponym.english)

toponyms.part1$toponym.english <- gsub('c\\("', '', toponyms.part1$toponym.english)
toponyms.part1$toponym.english <- gsub('"\\)', '', toponyms.part1$toponym.english)
toponyms.part1$toponym.english <- gsub('", "', ', ', toponyms.part1$toponym.english)

toponyms.part2 <- toponyms.part2 %>%
  mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part2$toponym.english <- as.character(toponyms.part2$toponym.english)

toponyms.part2$toponym.english <- gsub('c\\("', '', toponyms.part2$toponym.english)
toponyms.part2$toponym.english <- gsub('"\\)', '', toponyms.part2$toponym.english)
toponyms.part2$toponym.english <- gsub('", "', ', ', toponyms.part2$toponym.english)

toponyms.part3 <- toponyms.part3 %>%
  mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part3$toponym.english <- as.character(toponyms.part3$toponym.english)

toponyms.part3$toponym.english <- gsub('c\\("', '', toponyms.part3$toponym.english)
toponyms.part3$toponym.english <- gsub('"\\)', '', toponyms.part3$toponym.english)
toponyms.part3$toponym.english <- gsub('", "', ', ', toponyms.part3$toponym.english)

toponyms.part4 <- toponyms.part4 %>%
  mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part4$toponym.english <- as.character(toponyms.part4$toponym.english)

toponyms.part4$toponym.english <- gsub('c\\("', '', toponyms.part4$toponym.english)
toponyms.part4$toponym.english <- gsub('"\\)', '', toponyms.part4$toponym.english)
toponyms.part4$toponym.english <- gsub('", "', ', ', toponyms.part4$toponym.english)

toponyms.part5 <- toponyms.part5 %>%
  mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part5$toponym.english <- as.character(toponyms.part5$toponym.english)

toponyms.part5$toponym.english <- gsub('c\\("', '', toponyms.part5$toponym.english)
toponyms.part5$toponym.english <- gsub('"\\)', '', toponyms.part5$toponym.english)
toponyms.part5$toponym.english <- gsub('", "', ', ', toponyms.part5$toponym.english)

toponyms.part6 <- toponyms.part6 %>%
  mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part6$toponym.english <- as.character(toponyms.part6$toponym.english)

toponyms.part6$toponym.english <- gsub('c\\("', '', toponyms.part6$toponym.english)
toponyms.part6$toponym.english <- gsub('"\\)', '', toponyms.part6$toponym.english)
toponyms.part6$toponym.english <- gsub('", "', ', ', toponyms.part6$toponym.english)

toponyms.part7 <- toponyms.part7 %>%
  mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part7$toponym.english <- as.character(toponyms.part7$toponym.english)

toponyms.part7$toponym.english <- gsub('c\\("', '', toponyms.part7$toponym.english)
toponyms.part7$toponym.english <- gsub('"\\)', '', toponyms.part7$toponym.english)
toponyms.part7$toponym.english <- gsub('", "', ', ', toponyms.part7$toponym.english)

toponyms.part8 <- toponyms.part8 %>%
  mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part8$toponym.english <- as.character(toponyms.part8$toponym.english)

toponyms.part8$toponym.english <- gsub('c\\("', '', toponyms.part8$toponym.english)
toponyms.part8$toponym.english <- gsub('"\\)', '', toponyms.part8$toponym.english)
toponyms.part8$toponym.english <- gsub('", "', ', ', toponyms.part8$toponym.english)

toponyms.part9 <- toponyms.part9 %>%
  mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
toponyms.part9$toponym.english <- as.character(toponyms.part9$toponym.english)

toponyms.part9$toponym.english <- gsub('c\\("', '', toponyms.part9$toponym.english)
toponyms.part9$toponym.english <- gsub('"\\)', '', toponyms.part9$toponym.english)
toponyms.part9$toponym.english <- gsub('", "', ', ', toponyms.part9$toponym.english)

toponyms.part10 <- toponyms.part10 %>%
  mutate(toponym.english = sapply(strsplit(toponym, ", "), function(x) translate_country(x)))
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
toponyms.counts <- toponyms.counts %>%
  group_by(journal.id) %>%
  summarise(paper.total = n(),
            paper.with.top = sum(with.toponym))

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
replace_empty_with_na <- function(x) {
  x[x == ""] <- NA
  return(x)
}
databases <- databases %>%
  mutate(across(everything(), replace_empty_with_na))

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
DOAJ <- DOAJ %>%
  mutate(across(everything(), replace_empty_with_na))

# merge DOAJ to Scopus and WOS journals data already combined in databases dataframe
databases <- merge(databases, DOAJ[, c("journal.name", "language.z")], by = "journal.name", all = TRUE)

# replace ISO codes with languages full names in Scopus data within databases dataframe
databases$language.x <- sapply(databases$language.x, function(iso_codes) {
  languages <- unlist(strsplit(iso_codes, "; "))
  full_names <- sapply(languages, function(lang) {
    switch(lang,
           "JPN" = "Japanese", "ENG" = "English", "FRE" = "French", "CAT" = "Catalan", "SPA" = "Spanish", "POR" = "Portuguese",
           "GER" = "German", "ITA" = "Italian", "RUS" = "Russian", "CHI" = "Chinese", "DAN" = "Danish", "SLV" = "Slovenian",
           "LIT" = "Lithuanian", "AFR" = "Afrikaans", "SLO" = "Slovak", "HUN" = "Hungarian", "POL" = "Polish", "CZE" = "Czech",
           "SCR" = "Croatian", "NOR" = "Norwegian", "GRE" = "Greek", "FIN" = "Finnish", "EST" = "Estonian", "DUT" = "Dutch",
           "SCC" = "Serbian", "LAV" = "Latvian", "TUR" = "Turkish", "MAY" = "Malay", "PER" = "Persian", "IND" = "Indonesian",
           "BUL" = "Bulgarian", "ARA" = "Arabic", "RUM" = "Romanian", "AZE" = "Azerbaijani", "KOR" = "Korean", "SWE" = "Swedish",
           "HEB" = "Hebrew", "BAQ" = "Basque", "GLE" = "Irish", "BEL" = "Belarusian", "UKR" = "Ukrainian", "THA" = "Thai",
           "GLG" = "Galician", "GEO" = "Georgian", "ICE" = "Icelandic", "BOS" = "Bosnian", "MAC" = "Macedonian", "MAO" = "Maori",
           "ARM" = "Armenian", "ALB" = "Albanian", lang)})
  return(paste(full_names, collapse = ", "))
})

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
    NA
  }
})

# remove duplicate languages from the language variable
databases$language <- sapply(databases$language, function(lang_string) {
  unique_languages <- unique(unlist(strsplit(lang_string, ", ")))
  cleaned_language <- paste(unique_languages, collapse = ", ")
  return(cleaned_language)
})

# add language variable to journals dataframe and remove duplicates
journals <- left_join(journals, databases[, c("journal.name", "language")], by = "journal.name", keep = FALSE)
journals <- journals[!duplicated(journals), ]

# apply the function to transform all empty cells into NA values, convert NA characters to NA values in language variable
journals <- journals %>%
  mutate(across(everything(), replace_empty_with_na))
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
toponyms.language <- toponyms.language %>%
  group_by(journal.id) %>%
  summarise(language = paste(unique(title.language), collapse = ", "))

# merge both subsets in order to keep only the languages of the journals presenting NA values in journals dataframe
journals.toponyms.language <- merge(journals.language, toponyms.language, by = "journal.id", all.x = TRUE)

# fill in the language variable in journals dataframe with the language identified in papers' titles
journals <- left_join(journals, journals.toponyms.language, by = "journal.id") %>%
  mutate(language = coalesce(language.x, language.y)) %>%
  select(-language.x, -language.y)

# create a new binary variable mainstream.language to separate English and Multi-Language (1 = mainstream) from the rest (0 = not mainstream)
journals <- journals %>%
  mutate(mainstream.language = ifelse(
    grepl("English|Multi-Language", language, ignore.case = TRUE), 1, 0))


### CATEGORIES
# read Dimensions file
categories <- read_excel("~/Desktop/Local.Research/journal-for-division.xlsx")

# add a category and a category.prop variables to journals dataframe
journals <- left_join(journals, categories, by = "journal.id")

# add a variable for categories acronyms
journals <- journals %>% 
  mutate(category.acronym = case_when(
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
    category == "Law and Legal Studies" ~ "LawLegSci",
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
social.sciences <- c("ComManTourServ", "Econ", "Edu", "HumSoc", "LawLegSci", "Psych")

journals$field <- ifelse(journals$category.acronym %in% health.sciences, "Health Sciences",
                         ifelse(journals$category.acronym %in% humanities, "Humanities",
                                ifelse(journals$category.acronym %in% life.sciences, "Life Sciences",
                                       ifelse(journals$category.acronym %in% physical.sciences, "Physical Sciences",
                                              ifelse(journals$category.acronym %in% social.sciences, "Social Sciences", NA)))))


### JOURNAL LEVEL SUMMARY TABLES
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

# compute the mean distribution of journals per category, per variable: cits.prop, refs.prop, pubs.prop, toponyms.prop, mainstream.database and mainstream.language
journals %>% 
  na.omit() %>%
  distinct(journal.id, .keep_all = TRUE) %>% 
  group_by(category.acronym) %>% 
  summarise(avg_cits_prop = mean(cits.prop)) %>%
  print(n = Inf)

journals %>%
  na.omit() %>%
  distinct(journal.id, .keep_all = TRUE) %>%
  group_by(category.acronym) %>%
  summarise(avg_mainstream_yes = mean(mainstream.language, na.rm = TRUE)) %>%
  print(n = Inf)


### COUNTRY LEVEL SUMMARY TABLE
# before working with country data in local journals, compute the total number of publications per country considering all journals
total.pubs.country <- subset(df.journals.final, select = c(country, pubs))
total.pubs.country <- total.pubs.country[complete.cases(total.pubs.country), ]
total.pubs.country <- total.pubs.country %>%
  group_by(country) %>% 
  summarise(total.pubs = sum(pubs), .groups = 'drop') %>%
  as.data.frame()


# isolate local research journals according to the toponyms approach (cut-off thresholds for trial >= 0.14 (3º quartile) & >= 0.30 (9º decile))
local.toponyms.q <- subset(journals, select = c("journal.id", "journal.name", "toponyms.prop"), toponyms.prop >= 0.14)
local.toponyms.d <- subset(journals, select = c("journal.id", "journal.name", "toponyms.prop"), toponyms.prop >= 0.30)

# subset the necessary variables to work at country level and remove NA values
local.toponyms.countries.q <- df.journals.final[df.journals.final$journal.id %in% local.toponyms.q$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.toponyms.countries.q <- local.toponyms.countries.q[complete.cases(local.toponyms.countries.q), ]
local.toponyms.countries.d <- df.journals.final[df.journals.final$journal.id %in% local.toponyms.d$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.toponyms.countries.d <- local.toponyms.countries.d[complete.cases(local.toponyms.countries.d), ]

# compute each country's publication share in local journals = n pubs per country in local journals / N pubs per country in all journals
local.toponyms.countries.q <- aggregate(pubs ~ country, data = local.toponyms.countries.q, FUN = sum)
local.toponyms.countries.q <- local.toponyms.countries.q %>%
  left_join(total.pubs.country, by = "country") %>%
  mutate(pubs.share = pubs / total.pubs)
local.toponyms.countries.d <- aggregate(pubs ~ country, data = local.toponyms.countries.d, FUN = sum)
local.toponyms.countries.d <- local.toponyms.countries.d %>%
  left_join(total.pubs.country, by = "country") %>%
  mutate(pubs.share = pubs / total.pubs)


# isolate local research journals according to the languages approach (= 0)
local.language <- subset(journals, select = c("journal.id", "journal.name", "mainstream.language"), mainstream.language == 0)

# subset the necessary variables to work at country level and remove NA values
local.language.countries <- df.journals.final[df.journals.final$journal.id %in% local.language$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.language.countries <- local.language.countries[complete.cases(local.language.countries), ]

# compute each country's publication share in local journals = n pubs per country in local journals / N pubs per country in all journals
local.language.countries <- aggregate(pubs ~ country, data = local.language.countries, FUN = sum)
local.language.countries <- local.language.countries %>%
  left_join(total.pubs.country, by = "country") %>%
  mutate(pubs.share = pubs / total.pubs)


# isolate local research journals according to the journals approach (cut-off thresholds for trial >= 0.82 (3º quartile) & >= 0.95 (9º decile))
local.pubs.q <- subset(journals, select = c("journal.id", "journal.name", "pubs.prop", "category.acronym", "category.prop", "field"), pubs.prop >= 0.82)
local.pubs.d <- subset(journals, select = c("journal.id", "journal.name", "pubs.prop", "category.acronym", "category.prop", "field"), pubs.prop >= 0.95)

# subset the necessary variables to work at country level and remove NA values
local.pubs.countries.q <- df.journals.final[df.journals.final$journal.id %in% local.pubs.q$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.pubs.countries.q <- local.pubs.countries.q[complete.cases(local.pubs.countries.q), ]
local.pubs.countries.d <- df.journals.final[df.journals.final$journal.id %in% local.pubs.d$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.pubs.countries.d <- local.pubs.countries.d[complete.cases(local.pubs.countries.d), ]

# compute each country's publication share in local journals = n pubs per country in local journals / N pubs per country in all journals
local.pubs.countries.q <- aggregate(pubs ~ country, data = local.pubs.countries.q, FUN = sum)
local.pubs.countries.q <- local.pubs.countries.q %>%
  left_join(total.pubs.country, by = "country") %>%
  mutate(pubs.share = pubs / total.pubs)
local.pubs.countries.d <- aggregate(pubs ~ country, data = local.pubs.countries.d, FUN = sum)
local.pubs.countries.d <- local.pubs.countries.d %>%
  left_join(total.pubs.country, by = "country") %>%
  mutate(pubs.share = pubs / total.pubs)


# isolate local research journals according to the databases approach (= 0)
local.database <- subset(journals, select = c("journal.id", "journal.name", "mainstream.database", "category.acronym", "category.prop", "field"), mainstream.database == 0)

# subset the necessary variables to work at country level and remove NA values
local.database.countries <- df.journals.final[df.journals.final$journal.id %in% local.database$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.database.countries <- local.database.countries[complete.cases(local.database.countries), ]

# compute each country's publication share in local journals = n pubs per country in local journals / N pubs per country in all journals
local.database.countries <- aggregate(pubs ~ country, data = local.database.countries, FUN = sum)
local.database.countries <- local.database.countries %>%
  left_join(total.pubs.country, by = "country") %>%
  mutate(pubs.share = pubs / total.pubs)


# isolate local research journals according to the references approach (cut-off thresholds for trial >= 0.40 (3º quartile) & >= 0.53 (9º decile))
local.refs.q <- subset(journals, select = c("journal.id", "journal.name", "refs.prop", "category.acronym", "category.prop", "field"), refs.prop >= 0.40)
local.refs.d <- subset(journals, select = c("journal.id", "journal.name", "refs.prop", "category.acronym", "category.prop", "field"), refs.prop >= 0.53)

# subset the necessary variables to work at country level and remove NA values
local.refs.countries.q <- df.journals.final[df.journals.final$journal.id %in% local.refs.q$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.refs.countries.q <- local.refs.countries.q[complete.cases(local.refs.countries.q), ]
local.refs.countries.d <- df.journals.final[df.journals.final$journal.id %in% local.refs.d$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.refs.countries.d <- local.refs.countries.d[complete.cases(local.refs.countries.d), ]

# compute each country's publication share in local journals = n pubs per country in local journals / N pubs per country in all journals
local.refs.countries.q <- aggregate(pubs ~ country, data = local.refs.countries.q, FUN = sum)
local.refs.countries.q <- local.refs.countries.q %>%
  left_join(total.pubs.country, by = "country") %>%
  mutate(pubs.share = pubs / total.pubs)
local.refs.countries.d <- aggregate(pubs ~ country, data = local.refs.countries.d, FUN = sum)
local.refs.countries.d <- local.refs.countries.d %>%
  left_join(total.pubs.country, by = "country") %>%
  mutate(pubs.share = pubs / total.pubs)


# isolate local research journals according to the citations approach (cut-off thresholds for trial >= 0.50 (3º quartile) & >= 0.86 (9º decile))
local.cits.q <- subset(journals, select = c("journal.id", "journal.name", "cits.prop", "category.acronym", "category.prop", "field"), cits.prop >= 0.50)
local.cits.d <- subset(journals, select = c("journal.id", "journal.name", "cits.prop", "category.acronym", "category.prop", "field"), cits.prop >= 0.86)

# subset the necessary variables to work at country level and remove NA values
local.cits.countries.q <- df.journals.final[df.journals.final$journal.id %in% local.cits.q$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.cits.countries.q <- local.cits.countries.q[complete.cases(local.cits.countries.q), ]
local.cits.countries.d <- df.journals.final[df.journals.final$journal.id %in% local.cits.d$journal.id, c("journal.id", "journal.name", "country", "pubs")]
local.cits.countries.d <- local.cits.countries.d[complete.cases(local.cits.countries.d), ]

# compute each country's publication share in local journals = n pubs per country in local journals / N pubs per country in all journals
local.cits.countries.q <- aggregate(pubs ~ country, data = local.cits.countries.q, FUN = sum)
local.cits.countries.q <- local.cits.countries.q %>%
  left_join(total.pubs.country, by = "country") %>%
  mutate(pubs.share = pubs / total.pubs)
local.cits.countries.d <- aggregate(pubs ~ country, data = local.cits.countries.d, FUN = sum)
local.cits.countries.d <- local.cits.countries.d %>%
  left_join(total.pubs.country, by = "country") %>%
  mutate(pubs.share = pubs / total.pubs)


# compute measures of central tendency, non-central position and variability in all approaches within local research journals, focusing on countries' publication share
print(mean(local.cits.countries.d$pubs.share, na.rm = TRUE))
print(median(local.cits.countries.d$pubs.share, na.rm = TRUE))

print(min(local.cits.countries.d$pubs.share, na.rm = TRUE))
print(max(local.cits.countries.d$pubs.share, na.rm = TRUE))

print(quantile(local.cits.countries.d$pubs.share, probs = c(0.25,0.75), na.rm = TRUE))
print(sd(local.cits.countries.d$pubs.share, na.rm = TRUE))


### WORLD MAPS
map.world <- st_read("~/Desktop/Local.Research/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")

# plot toponyms world map
local.toponyms.map.q <- merge(map.world, local.toponyms.countries.q, by.x = "ISO_A2_EH", by.y = "country", all.x = TRUE)
local.toponyms.map.q <- local.toponyms.map.q[complete.cases(local.toponyms.map.q$pubs.share), ]
local.toponyms.map.d <- merge(map.world, local.toponyms.countries.d, by.x = "ISO_A2_EH", by.y = "country", all.x = TRUE)
local.toponyms.map.d <- local.toponyms.map.d[complete.cases(local.toponyms.map.d$pubs.share), ]

# plot language world map
local.language.map <- merge(map.world, local.language.countries, by.x = "ISO_A2_EH", by.y = "country", all.x = TRUE)
local.language.map <- local.language.map[complete.cases(local.language.map$pubs.share), ]

# plot pubs world map
local.pubs.map.q <- merge(map.world, local.pubs.countries.q, by.x = "ISO_A2_EH", by.y = "country", all.x = TRUE)
local.pubs.map.q <- local.pubs.map.q[complete.cases(local.pubs.map.q$pubs.share), ]
local.pubs.map.d <- merge(map.world, local.pubs.countries.d, by.x = "ISO_A2_EH", by.y = "country", all.x = TRUE)
local.pubs.map.d <- local.pubs.map.d[complete.cases(local.pubs.map.d$pubs.share), ]

# plot database world map
local.database.map <- merge(map.world, local.database.countries, by.x = "ISO_A2_EH", by.y = "country", all.x = TRUE)
local.database.map <- local.database.map[complete.cases(local.database.map$pubs.share), ]

# plot refs world map
local.refs.map.q <- merge(map.world, local.refs.countries.q, by.x = "ISO_A2_EH", by.y = "country", all.x = TRUE)
local.refs.map.q <- local.refs.map.q[complete.cases(local.refs.map.q$pubs.share), ]
local.refs.map.d <- merge(map.world, local.refs.countries.d, by.x = "ISO_A2_EH", by.y = "country", all.x = TRUE)
local.refs.map.d <- local.refs.map.d[complete.cases(local.refs.map.d$pubs.share), ]

# plot cits world map
local.cits.map.q <- merge(map.world, local.cits.countries.q, by.x = "ISO_A2_EH", by.y = "country", all.x = TRUE)
local.cits.map.q <- local.cits.map.q[complete.cases(local.cits.map.q$pubs.share), ]
local.cits.map.d <- merge(map.world, local.cits.countries.d, by.x = "ISO_A2_EH", by.y = "country", all.x = TRUE)
local.cits.map.d <- local.cits.map.d[complete.cases(local.cits.map.d$pubs.share), ]

## 3ºQ
# create one faceted plot with 6 maps and 1 common legend
local.toponyms.map.q$variable <- "Toponyms proportion"
local.language.map$variable <- "Non-English publishing"
local.pubs.map.q$variable <- "Publishing proportion"
local.database.map$variable <- "Non-mainstream indexing"
local.refs.map.q$variable <- "Referenced proportion"
local.cits.map.q$variable <- "Citing proportion"

map.q <- rbind(local.toponyms.map.q, local.language.map, local.pubs.map.q, local.database.map, local.refs.map.q, local.cits.map.q)
map.q$variable <- factor(map.q$variable, levels = c("Toponyms proportion", "Non-English publishing", "Publishing proportion", "Non-mainstream indexing", "Referenced proportion", "Citing proportion"))

ggplot() +
  geom_sf(data = map.q, aes(fill = pubs.share)) +
  scale_fill_viridis_c(name = "Publication share", na.value = "grey90", option = "plasma") +
  facet_wrap(~variable, ncol = 2) +
  theme_minimal() +
  theme(legend.position = "bottom")

## 9ºD
# create one faceted plot with 6 maps and 1 common legend
local.toponyms.map.d$variable <- "Toponyms proportion"
local.language.map$variable <- "Non-English publishing"
local.pubs.map.d$variable <- "Publishing proportion"
local.database.map$variable <- "Non-mainstream indexing"
local.refs.map.d$variable <- "Referenced proportion"
local.cits.map.d$variable <- "Citing proportion"

map.d <- rbind(local.toponyms.map.d, local.language.map, local.pubs.map.d, local.database.map, local.refs.map.d, local.cits.map.d)
map.d$variable <- factor(map.d$variable, levels = c("Toponyms proportion", "Non-English publishing", "Publishing proportion", "Non-mainstream indexing", "Referenced proportion", "Citing proportion"))

ggplot() +
  geom_sf(data = map.d, aes(fill = pubs.share)) +
  scale_fill_viridis_c(name = "Publication share", na.value = "grey90", option = "plasma") +
  facet_wrap(~variable, ncol = 2) +
  theme_minimal() +
  theme(legend.position = "bottom")


### CASE STUDIES
# considering local journals where pubs.prop >= 0.51, isolate the countries selected for case study: United States, China, Germany, Indonesia, Brazil and South Africa
case.studies <- subset(journals, select = c("pubs.prop", "pubs.country", "category"),
                       pubs.prop >= 0.51 & pubs.country %in% c("US", "CN", "DE", "ID", "BR", "ZA"))

# drop NA values
case.studies <- case.studies[complete.cases(case.studies), ]

# compute the total pubs.prop per country
case.studies <- case.studies %>%
  group_by(pubs.country) %>%
  mutate(pubs.country.total = sum(pubs.prop)) %>%
  ungroup()

# compute the total pubs.prop per discipline per country
case.studies <- case.studies %>%
  group_by(discipline.acronym, pubs.country) %>%
  mutate(pubs.discipline.total = sum(pubs.prop)) %>%
  ungroup()

# compute the share of publications per discipline per country
case.studies$pubs.share <- case.studies$pubs.discipline.total / case.studies$pubs.country.total

# drop the original pubs.prop variable and keep only unique cases
case.studies <- subset(case.studies, select = -pubs.prop)
case.studies <- case.studies %>% distinct()

# plot the publication share per discipline per country within local research journals
ggplot(case.studies, aes(x = pubs.share, y = discipline.acronym)) +
  geom_bar(stat = "identity", aes(fill = pubs.country)) +  
  facet_wrap(~ pubs.country, scales = "free_y", ncol = 2,
             labeller = labeller(pubs.country = c("BR" = "Brazil", "CN" = "China", "DE" = "Germany", "ID" = "Indonesia", "US" = "United States", "ZA" = "South Africa"))) +
  labs(x = "Publishing proportion", y = "Discipline") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_viridis_d(option = "plasma") +
  guides(fill = FALSE)


### CORRELATIONS
# country level with 3ºQ data
corr.local.toponyms.q <- subset(local.toponyms.countries.q, select = c(country, pubs.share))
corr.local.toponyms.q <- corr.local.toponyms.q %>% rename("toponyms.pubs.share" = "pubs.share")

corr.local.language <- subset(local.language.countries, select = c(country, pubs.share))
corr.local.language <- corr.local.language %>% rename("language.pubs.share" = "pubs.share")

corr.local.pubs.q <- subset(local.pubs.countries.q, select = c(country, pubs.share))
corr.local.pubs.q <- corr.local.pubs.q %>% rename("pubs.pubs.share" = "pubs.share")

corr.local.database <- subset(local.database.countries, select = c(country, pubs.share))
corr.local.database <- corr.local.database %>% rename("database.pubs.share" = "pubs.share")

corr.local.refs.q <- subset(local.refs.countries.q, select = c(country, pubs.share))
corr.local.refs.q <- corr.local.refs.q %>% rename("refs.pubs.share" = "pubs.share")

corr.local.cits.q <- subset(local.cits.countries.q, select = c(country, pubs.share))
corr.local.cits.q <- corr.local.cits.q %>% rename("cits.pubs.share" = "pubs.share")

corr.local.q <- Reduce(function(x, y) merge(x, y, by = "country", all = TRUE), list(corr.local.toponyms.q, corr.local.language, corr.local.pubs.q, corr.local.database, corr.local.refs.q, corr.local.cits.q))
corr.local.q <- corr.local.q %>% rename("Tops prop" = "toponyms.pubs.share", "Eng pub" = "language.pubs.share", "Pub prop" = "pubs.pubs.share", "W/S index" = "database.pubs.share", "Ref prop" = "refs.pubs.share", "Cit prop" = "cits.pubs.share")

corr.local.matrix.q <- cor(corr.local.q[, c("Tops prop", "Eng pub", "Pub prop", "W/S index", "Ref prop", "Cit prop")], use = "pairwise.complete.obs")

ggcorrplot(corr.local.matrix.q,
           type = "lower",
           ggtheme = ggplot2::theme_minimal,
           lab = TRUE) +
  scale_fill_gradientn(colours = viridis(10, option = "plasma"))

# country level with 9ºD data
corr.local.toponyms.d <- subset(local.toponyms.countries.d, select = c(country, pubs.share))
corr.local.toponyms.d <- corr.local.toponyms.d %>% rename("toponyms.pubs.share" = "pubs.share")

corr.local.language <- subset(local.language.countries, select = c(country, pubs.share))
corr.local.language <- corr.local.language %>% rename("language.pubs.share" = "pubs.share")

corr.local.pubs.d <- subset(local.pubs.countries.d, select = c(country, pubs.share))
corr.local.pubs.d <- corr.local.pubs.d %>% rename("pubs.pubs.share" = "pubs.share")

corr.local.database <- subset(local.database.countries, select = c(country, pubs.share))
corr.local.database <- corr.local.database %>% rename("database.pubs.share" = "pubs.share")

corr.local.refs.d <- subset(local.refs.countries.d, select = c(country, pubs.share))
corr.local.refs.d <- corr.local.refs.d %>% rename("refs.pubs.share" = "pubs.share")

corr.local.cits.d <- subset(local.cits.countries.d, select = c(country, pubs.share))
corr.local.cits.d <- corr.local.cits.d %>% rename("cits.pubs.share" = "pubs.share")

corr.local.d <- Reduce(function(x, y) merge(x, y, by = "country", all = TRUE), list(corr.local.toponyms.d, corr.local.language, corr.local.pubs.d, corr.local.database, corr.local.refs.d, corr.local.cits.d))
corr.local.d <- corr.local.d %>% rename("Tops prop" = "toponyms.pubs.share", "Eng pub" = "language.pubs.share", "Pub prop" = "pubs.pubs.share", "W/S index" = "database.pubs.share", "Ref prop" = "refs.pubs.share", "Cit prop" = "cits.pubs.share")

corr.local.matrix.d <- cor(corr.local.d[, c("Tops prop", "Eng pub", "Pub prop", "W/S index", "Ref prop", "Cit prop")], use = "pairwise.complete.obs")

ggcorrplot(corr.local.matrix.d,
           type = "lower",
           ggtheme = ggplot2::theme_minimal,
           lab = TRUE) +
  scale_fill_gradientn(colours = viridis(10, option = "plasma"))

# regional level with 3ºQ data


### SAVE DATAFRAMES
save.image("~/Desktop/Local.Research/local.research.data.RData")
