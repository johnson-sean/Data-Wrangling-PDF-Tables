# Libraries ----
library(readr)
library(stringr)
library(pdftools)
library(dplyr)
library(tidyr)
library(tibble)
library(Thematic)

# user selected values ----
pages <- as.character(c(1:20))

# read in pdf file  and manipulate with pdftools ----
# http://www.fapri.missouri.edu/

## pull down file ----
temp.file <- paste(tempfile(),".pdf",sep = "")
download.file("https://www.fapri.missouri.edu/wp-content/uploads/2022/09/2022-Farm-Income-Update.pdf", temp.file, mode = "wb")
file <- temp.file
rm(temp.file)

# It is easy to run with multiple similar files with the purrr package.
# However make sure to double check col values with distinct after joining with
# row_bind() as there may be words/symbol differences that can be addressed with
# the regex

## pdftool converting text string in to a usable form ----
text <- pdf_text(file) 
text <- unlist(str_split(text, "[\\r\\n]+"))
text <- str_split_fixed(str_trim(text), "\\s{2,}", 9)

# text <- str_split_fixed(str_trim(unlist(str_split(pdf_text(file), "[\\r\\n]+"))), "\\s{2,}", 9)

# convert to data frame ----
df<-as.data.frame(text)

## manipulations to get tables
pdf<-df%>%
  mutate(across(V1,str_replace,"U.S. government outlays ...........","x"))%>%
  filter(row_number() >= which(V1=='U.S. government outlays'))%>%
  filter(!(V1 == ""|
             str_detect(string= V1, pattern = "FAPRI‐MU ")|
             str_detect(string= V1, pattern = "Note: ")|
             str_detect(string= V1, pattern = "Coronavirus ")|
             V1 %in% pages))%>%
  mutate(V0 = case_when(str_detect(V1,"Fiscal") ~"Fiscal",
                        str_detect(V1,"Calendar") ~"Calendar",
                        TRUE~as.character(NA)))%>%
  mutate_at("V0",list(~lead(.,n=1)))%>%
  fill(V0)%>%fill(V0, .direction = "up")%>%
  mutate(V2 = case_when((V1 =="(Million dollars)"|V1 =="(Billion dollars)") ~"Unit",
                        V1=="Non‐CCC programs" ~"(Million dollars)",
                        V1=="Farm debt" ~"(Billion dollars)",
                        TRUE~V2),
         Table = case_when(V2 == "" ~ V1,
                           TRUE ~ as.character(NA)))%>%
  fill(Table)%>%  
  mutate(Values = case_when(V1 =="(Million dollars)"~ "Million USD",
                            V1 =="(Billion dollars)"~ "Billion USD",
                            V2 =="(Million dollars)"~ "Million USD",
                            V2 =="(Billion dollars)"~ "Billion USD",
                            TRUE~as.character(NA)))%>%
  mutate_at("Values", list(~lead(.,n = 2)))%>%
  fill(Values)%>%fill(Values, .direction="up")%>%
  filter(!(str_detect(string= V1, pattern = "(Million dollars)")|
             str_detect(string= V1, pattern = "(Billion dollars)")))%>%
  mutate(SubTable = case_when((V2 =="(Million dollars)"|V2 =="(Billion dollars)") ~V1,
                              V2 == "" ~"",
                              TRUE~as.character(NA)),
         V2 = case_when((V2 =="(Million dollars)"|V2 =="(Billion dollars)"|V2 =="Unit")~"",
                        TRUE~V2))%>%
  fill(SubTable)%>%
  mutate(SubTable = case_when(SubTable=="" ~ "Del", # work around for row_to_names no forcing a blank for col name
                                     TRUE ~ SubTable))%>%
  janitor::row_to_names(row_number = 2)%>%
  rename_with(.cols=c(1,10,11,12,13), ~c("Field","Period","Table","Values","SubTable"))%>%
  mutate(SubTable = case_when(SubTable == "Del"~"",
                                     TRUE~SubTable))%>%
  filter(!(Field==SubTable |
             Field==Table |
             str_detect(string = Field, pattern = "Fiscal")|
             str_detect(string = Field, pattern = "Calendar")))%>%
  pivot_longer(!c("Table","SubTable","Field","Period","Values"),
               names_to = "Year",values_to = "Value")%>%
  rename(Unit = Values)%>%
  select(Table,
         SubTable,
         Field,
         Value,
         Unit,
         Year,
         Period)

pdf%>%
  filter(Table == "U.S. government outlays",
         Field == "Total CCC programs")%>%
  Thematic::tabGT()
