library(tidyverse)
library(sampleSelection)
df <- base_final %>%
  select(decisao, material_formal,  turma, ato, governo1, classe, desfecho)

df <- df %>%
     mutate(ato = relevel(ato, ref = "Decreto"),
            governo1 = relevel(governo1, ref = "PT"),
            classe = relevel(classe, ref = "ADI"),
            material_formal = relevel(material_formal, ref = "F")
            )



df <- df %>%
      mutate(across(c(desfecho, decisao), .fns = as.character))

df <- df %>%
      mutate(decisao = case_when(
        decisao == "improcedente" ~ 0L,
        decisao == "procedente" ~ 1L,
        TRUE ~ NA_integer_

      ))



df <- df %>%
     mutate(desfecho = case_when(
       desfecho == "decisão" ~ 1L,
       TRUE ~ 0L

     ))


df <- df %>%
   mutate_at(c("decisao","desfecho"), as.factor)

m5 <- heckit(
   selection = desfecho ~ material_formal +  turma + governo1 + classe + ato,
   outcome = decisao ~ material_formal +  turma + governo1,
  data = df,
  method = "ml"
)




summary(m5)

stargazer::stargazer(m5, type = "html", out = "modelo.htm")
