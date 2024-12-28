# Phillip Thomas
# Setup

# Data understanding

library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)
projectdata <- read.csv("Projectdata.csv")
View(projectdata)
attach(projectdata)

dim(projectdata)
# 4917 values, columns.
str(projectdata)
# All 3 columns in character format.
colSums(is.na(projectdata))
sum(complete.cases(projectdata))
table(is.na(projectdata))
# No NAs detected yet.
table(is.null(projectdata))
unique(Student.name)


coursenames <- as.data.frame(sort(unique(projectdata$coursename)))
View(coursenames)
string <- paste(coursenames$`sort(unique(projectdata$coursename))`, collapse = ", ")
string

dictionary <- c(", 1000 YRS MUSICAL LISTENG: 1000 YRS MUSICAL LISTENG, 19TH-CENT NOVEL, 19TH-CENTURY BRITISH LITERATURE, 1ST YR CLASSICAL CHIN II, 20TH-CENT POETRY, 20th Century Russian Literature: Fiction and Reality, A WORLD AT WAR, ABNORMAL PSYCHOLOGY, ACCEL INTERMD PORTUGUESE, ACCEL INTERMEDIATE SPAN, ACCELERATED HINDI, ACCELERATED INTERMD GRMN, AESTHETICS, AFGHANISTAN & ISLAMISM: AFGHANISTAN & ISLAMISM, AFRICAN-AMERICAN LIT, AFRICAN-AMERICAN LIT: AFRICAN-AMER LIT:CHANGE, AFRICAN LANG. & CULTURE, AFRO AMER HIST 1876-PRES, AMER POST-1800: BF SEM: MODERN AMERICAN CITIES, AMER REVOLUTION, AMERICA IN THE 1960S, AMERICAN FOREIGN POLICY, AMERICAN HEALTH POLICY, AMERICAN MUSICAL THEATRE, AMERICAN POETRY, AMERICAN SOCIETY, AMERICAN SOUTH 1861-PRES, ANAL METH ECON, LAW MED, ANALYTICAL MECHANICS, ANALYZING THE POL WORLD, ANCIENT ROME, ANIMAL BEHAVIOR, ANIMAL COGNITION, ANTH & THE MODERN WORLD, ANTH OF GLOBAL LABOR: Anthropology of Global Labor, ANTHROPOLOGY AND CINEMA, APPLIED MUSIC, ARCHAEOL GREC-ROM RELIG, ARCHAEOL OF PRIVATE LIFE, ARCHAEOLOGY OF THE INCA, ART - ancient to 1945, ART AND BUSINESS OF FILM, ART AND RELIGION, ART: ancient to 1945, ASIAN AMER COMM FLD WRK, AUGUSTAN CULTRL REVOL, BECOMING HUMAN, BEG RDG/WRTG CHINESE II, BEHAVIORAL ECON & PSYCH, BEHAVIORAL PHARMACOLOGY, BEING HUMAN: Being Human: Biology, Culture & Human Diversity, BIBLE IN TRANSLATION: Proverbs, Ecclesiastes, and Job, BIOCHEMISTRY RESEARCH, BIOLOGICAL CHEMISTRY II, BRITAIN SINCE 1945, BRITISH POETRY 1660-1914, Business German: A Micro Perspective, CEL and BIO and BIOCHEMISTRY, CEL BIO BIOCHEMISTRY, CELL and BIO and BIOCHEMISTRY, CELL BIOL & BIOCHEM, CELL BIOLOGY & BIOCHEM, CELL BIOLOGY and BIOCHEM, CELL. BIOL. & BIOCHEM., COGNITIVE PSYCHOLOGY, COMM and  THE PRESIDENCY, CHILDHOOD & PLAY, COMMUNICATION BEHAVIOR, COMMUNICATIONS INTERNSHP, Community Based Research on Health Disparities, COMMUNITY MATH TEACH PRO, COMP POL. WELFARE STATE, COMPARATIVE POLITICS, COMPUTER LINEAR ALGEBRA, CONTEMP ART - 1945 to PRESENT, CONTEMPORARY AFRICAN ART, CONTEMPORARY POL.THOUGHT, CONTEMPORARY SOCIO THEORY, CONTMP POL,POLICY,JOURN, CONVERSATION & WRITING, CONVERSATION AND COMPOSI, CREAT.NON-FICTION WRIT, CREAT.NON-FICTION WRIT: PEER TUTORING, CREATIVE WRITING: CREAT. WRITE: FICT/POET, CREATIVE WRITING: CREAT./POETRY & NON-FICT, CREATIVE WRITING: WRITING PERSONAL ESSAY, CRIME AND DETECTION: Dark Deeds, CURR ISSUES GLOBAL HEALT, DANTE'S DIVINE COMEDY, DEVIL'S PACT LIT/FILM, DIGITAL MEDIA & THE CITY, DIGITAL PHOTOGRAPHY, DNA, DIET, AND DISEASE: The Biology of Food, DOCUMENTARY WRITING, DRAMATURGY, DRUGS, BRAIN AND MIND, DRUGS, BRAIN, AND MIND, EARLY CINEMA, EARLY MESOPOTAM HIST/SOC, ECONOMETRIC FORECASTING, ECONOMETRICS, EDGAR ALLAM POE'S SCIEN, ELECTROMAG II, ELEM BIBLICAL HEBREW II, ELEM CLASSICAL GREEK II, ELEM MODERN HEBREW I, ELEM MODERN HEBREW II, ELEM PERSIAN II, ELEMENTARY ARABIC II, ELEMENTARY FRENCH I, ELEMENTARY FRENCH II, ELEMENTARY GERMAN 1, ENERGY,OIL&GLOBAL WARMIN, Environmental Case Studies, ENVIRONMENTAL GEOLOGY, Environmental Studies Research Seminar for Juniors, Environmental Studies Research Seminar Junior Level, ENVIRONMENTAL SYSTEMS II, EPIC TRADITIONS OF INDIA, EUR PRE-1800: BF SEM: UTOPIA, EURO ART & CIV > 1400: RENAISSANCE TO CONTEMP, EURO INT'L REL SINCE WW One, EURO INT'L REL SINCE WW1, EURO INTELL HIST 18 C., EUROPE IN A WIDER WORLD, EVIDENCED BASED CRIME AND JUSTICE POLICY, EXPERIMENTAL WRITING SEM: The Ecology of Poetry, EYE, MIND AND IMAGE, Feminist Theory: Feminism, Activism, and the Body, FICTION WRITING WORKSHOP, FOOD/FEAST ARCH OF TABLE, FORENSIC ANTHROPOLOGY, FORMAL LOGIC I, FORMAL SEM AND COG SCI, FR FOR PROFESSIONS I, FR FOR PROFESSIONS II, FR LIT OF THE 19TH C: STUDIES IN THE 19TH C., FRANCE & THE EUROP.UNION, FRANCE AND ITS OTHERS: Anthropology and French Modernism, FREEDOM OF EXPRESSION, FRENCH PHONETICS, French Thought Since 1945, FRESHWATER ECOLOGY")

#library(stringdist)
# Using stringdist package
#clean_coursedata <- function(projectdata, dictionary, maxDist = 5) {
#  dictionary[amatch(projectdata, dictionary, maxDist = maxDist)]
#}

# Using base R
#clean_coursedata2 <- function(projectdata, dictionary, maxDist = 5) {
#  sm <- adist(projectdata, dictionary)
#  sm[sm > maxDist] <- NA
#  dictionary[apply(sm, 1, which.min)]
#}

#clean_coursedata(projectdata$coursename, dictionary)
#clean_coursedata2(projectdata$coursename, dictionary)

coursedata2 <- as.data.frame(sort(unique(coursename)))
View(coursedata2)

attach(projectdata)

projectdata <- projectdata %>% mutate(Student.name = recode(Student.name,
                                         'DRichard M. Daley' = 'Richard M. Daley',
                                         'EBarbara Eden' = 'Barbara Eden',
                                         'Elvira (Cassandra Peterson)' = 'Cassandra Peterson',
                                         'FNanette Fabray' = 'Nanette Fabray',
                                         'Gene Tierney (Lee)' = 'Gene Tierney',
                                         'GJohn Kenneth Galbraith' = 'John Kenneth Galbraith',
                                         'HAlexander Haig, Jr.' = 'Alexander Haig, Jr.',
                                         'Ilee lacocca' = 'Lee lacocca',
                                         'JBo Jackson' = 'Bo Jackson',
                                         'Julie Christie 1995' = 'Julie Christie',
                                         'Julie Christie 1999' = 'Julie Christie',
                                         'Ken Follett 2001' = 'Ken Follett',
                                         'Ken Follett 2004' = 'Ken Follett',
                                         'KEwing Kauffman' = 'Ewing Kauffman',
                                         'LCheryl Ladd' = 'Cheryl Ladd',
                                         'MYo-Yo Ma' = 'Yo-Yo Ma',
                                         'N-ORalph Nader' = 'Ralph Nader',
                                         'Olympia Snow, senator' = 'Olympia Snowe',
                                         'Olympia Snowe,' = 'Olympia Snowe',
                                         'PPatti Page' = 'Patti Page',
                                         'QMarilyn Quayle' = 'Marilyn Quayle',
                                         'RSally Jesse Raphael' = 'Sally Jesse Raphael',
                                         'Sean Faircloth,' = 'Sean Faircloth',
                                         'SMorley Safer' = 'Morley Safer',
                                         'state representative' = 'State Representative',
                                         'TJessica Tandy' = 'Jessica Tandy'))

# Data cleaning

# Aligning course names with those in external file - allows for join

projectdata <- projectdata %>% mutate(coursename = recode(coursename,
                                                          '19TH-CENT BRITISH LIT' = '19TH-CENTURY BRITISH LITERATURE',
                                                          'AFRICAN-AMERICAN LIT' = 'AFRICAN-AMERICAN LIT: AFRICAN-AMER LIT:CHANGE',
                                                          'AMERICAN HEALT POLICY' = 'AMERICAN HEALTH POLICY',
                                                          'ART & RELIGION' = 'ART AND RELIGION',
                                                          'ART - ancient to 1945' = 'ART: ancient to 1945',
                                                          'ART - from ancient to 1945' = 'ART: ancient to 1945',
                                                          'ART ancient to 1945' = 'ART: ancient to 1945',
                                                          'ART, from ancient to 1945' = 'ART: ancient to 1945',
                                                          'AUGUSTAN CULTRL REVOL' = 'AUGUSTAN CULTRAL REVOLUTION',
                                                          'Business German - Micro Perspective' = 'Business German: A Micro Perspective',
                                                          'Business German A Micro Perspective' = 'Business German: A Micro Perspective',
                                                          'Business German, A Micro Perspective' = 'Business German: A Micro Perspective',
                                                          'CEL and BIO and BIOCHEMISTRY' = 'CELL. BIOL. & BIOCHEM.',
                                                          'CEL BIO BIOCHEMISTRY' = 'CELL. BIOL. & BIOCHEM.',
                                                          'CELL and BIO and BIOCHEMISTRY' = 'CELL. BIOL. & BIOCHEM.',
                                                          'CELL BIOL & BIOCHEM' = 'CELL. BIOL. & BIOCHEM.',
                                                          'CELL BIOLOGY & BIOCHEM' = 'CELL. BIOL. & BIOCHEM.',
                                                          'CELL BIOLOGY and BIOCHEM' = 'CELL. BIOL. & BIOCHEM.',
                                                          'CELL BIOLOGY and BIOCHEMISTRY' = 'CELL. BIOL. & BIOCHEM.',
                                                          'CELL. BIOL. And BIOCHEM.' = 'CELL. BIOL. & BIOCHEM.',
                                                          'COMM & THE PRESIDENCY' = 'COMM and  THE PRESIDENCY',
                                                          'COMMUNICATIONS INTERNSHiP' = 'COMMUNICATIONS INTERNSHP',
                                                          'COMPUT LINEAR ALGEBRA' = 'COMPUTER LINEAR ALGEBRA',
                                                          'CONTEMP ART - 1945 to today' = 'CONTEMP ART - 1945 to PRESENT',
                                                          'CONTEMP ART - since 1945' = 'CONTEMP ART - 1945 to PRESENT',
                                                          'CONTEMP ART:1945 to PRESENT' = 'CONTEMP ART - 1945 to PRESENT',
                                                          'CONTEMPORARY AFRICAN-ART' = 'CONTEMPORARY AFRICAN ART',
                                                          'CONTEMPORARY SOCIO THEOR' = 'CONTEMPORARY SOCIO THEORY',
                                                          'EARLY MESOPOTAM HIST/SOC' = 'EARLY MESOPOTAM HISTORY/SOCIETY',
                                                          'EARLY MESOPOTAM HIST - SOC' = 'EARLY MESOPOTAM HISTORY/SOCIETY',
                                                          'EARLY MESOPOTAMIAN HIST - SOC' = 'EARLY MESOPOTAM HISTORY/SOCIETY',
                                                          'ELEMENTARY GERMAN I' = 'ELEMENTARY GERMAN 1',
                                                          'Environmental Studies Research Seminar for Juniors' = 'Environmental Studies Research Seminar Junior Level',
                                                          'EVIDENCED BASED CRIME & JUSTICE POLICY' = 'EVIDENCED BASED CRIME AND JUSTICE POLICY',
                                                          'EXPERIMENTAL WRITING SEM' = 'EXPERIMENTAL WRITING SEM: The Ecology of Poetry'))
                                                          

coursedata3 <- as.data.frame(sort(unique(coursename)))
View(coursedata3)    

# Removing rows with blanks

dim(projectdata)

projectdata$coursename[projectdata$coursename == ""] <- NA

colSums(is.na(projectdata))

projectdata <- projectdata[!is.na(projectdata$coursename),]

dim(projectdata)

coursedata4 <- as.data.frame(sort(unique(coursename)))
View(coursedata4) 
                                                          
# Duplicate row removal

# FIX COURSE NAMES FIRST

nrow(projectdata[duplicated(projectdata), ])
projectdata <- projectdata[!duplicated(projectdata), ]
nrow(projectdata)

# Allows correct course names to appear in R data frame
newdata <- projectdata
View(newdata)

coursenames2 <- as.data.frame(sort(unique(newdata$coursename)))
View(coursenames2)

string2 <- paste(coursenames2$`sort(unique(newdata$coursename))`, collapse = "|")
string2

#dictionary <- c('19TH-CENTURY BRITISH LITERATURE|20th Century Russian Literature: Fiction and Reality|A WORLD AT WAR|AESTHETICS|AFRICAN-AMERICAN LIT: AFRICAN-AMER LIT:CHANGE|AMERICAN HEALTH POLICY|AMERICAN SOUTH 1861-PRES|ANALYTICAL MECHANICS|ANALYZING THE POL WORLD|ART AND RELIGION|ART: ancient to 1945|AUGUSTAN CULTRAL REVOLUTION|BECOMING HUMAN|BEHAVIORAL PHARMACOLOGY|BRITISH POETRY 1660-1914|Business German: A Micro Perspective|CELL. BIOL. & BIOCHEM.|COMMUNICATIONS INTERNSHP|COMPARATIVE POLITICS|COMPUTER LINEAR ALGEBRA|CONTEMP ART - 1945 to PRESENT|CONTEMPORARY AFRICAN ART|CONTEMPORARY POL.THOUGHT|CONTEMPORARY SOCIO THEORY|DEVIL\'S PACT LIT/FILM|EARLY MESOPOTAM HISTORY/SOCIETY|ELEMENTARY ARABIC II|ELEMENTARY GERMAN 1|Environmental Studies Research Seminar Junior Level|ENVIRONMENTAL SYSTEMS II|EUROPE IN A WIDER WORLD|EVIDENCED BASED CRIME AND JUSTICE POLICY|EXPERIMENTAL WRITING SEM: The Ecology of Poetry|FOOD/FEAST ARCH OF TABLE|FRANCE & THE EUROP.UNION|French Thought Since 1945|FRESHWATER ECOLOGY'))
courselist <- read.csv("Alphabetical course names.csv")
View(courselist)
combined <- as.data.frame(merge(projectdata, courselist, by = "coursename"))
View(combined)
dim(combined)
colSums(is.na(combined))
length(complete.cases(combined))

write.csv(combined, "D:\\SWENG 545\\final_combined_project_data.csv", row.names=TRUE)

# Duplicate row removal
combined <- combined[!duplicated(combined), ]
nrow(combined)

# Filter
newdata <- newdata %>% filter(grepl('19TH-CENTURY BRITISH LITERATURE|20th Century Russian Literature: Fiction and Reality|A WORLD AT WAR|AESTHETICS|AFRICAN-AMERICAN LIT: AFRICAN-AMER LIT:CHANGE|AMERICAN HEALTH POLICY|AMERICAN SOUTH 1861-PRES|ANALYTICAL MECHANICS|ANALYZING THE POL WORLD|ART AND RELIGION|ART: ancient to 1945|AUGUSTAN CULTRAL REVOLUTION|BECOMING HUMAN|BEHAVIORAL PHARMACOLOGY|BRITISH POETRY 1660-1914|Business German: A Micro Perspective|CELL. BIOL. & BIOCHEM.|COMM and  THE PRESIDENCY|COMMUNICATIONS INTERNSHP|COMPARATIVE POLITICS|COMPUTER LINEAR ALGEBRA|CONTEMP ART - 1945 to PRESENT|CONTEMPORARY AFRICAN ART|CONTEMPORARY POL.THOUGHT|CONTEMPORARY SOCIO THEORY|DEVIL\'S PACT LIT/FILM|EARLY MESOPOTAM HISTORY/SOCIETY|ELEMENTARY ARABIC II|ELEMENTARY GERMAN 1|Environmental Studies Research Seminar Junior Level|ENVIRONMENTAL SYSTEMS II|EUROPE IN A WIDER WORLD|EVIDENCED BASED CRIME AND JUSTICE POLICY|EXPERIMENTAL WRITING SEM: The Ecology of Poetry|FOOD/FEAST ARCH OF TABLE|FRANCE & THE EUROP.UNION|French Thought Since 1945|FRESHWATER ECOLOGY',coursename))

# After filtering irrelevant courses 

dim(newdata)
# 1755 rows.

sort(unique(newdata$coursename))
sort(unique(combined$coursename))

# All should match Word doc exactly.

# Converting each student to numeric ID
combined$Student.name <- as.factor(combined$Student.name)
combined$Student.name <- as.numeric(combined$Student.name)

# Sorting student IDs in ascending order
combined <- combined[order(combined$Student.name),]


