library(tidyverse)


sicor_operacao_basica_estado_2022 <- read_delim("SICOR_OPERACAO_BASICA_ESTADO_2022.csv", 
                                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

glimpse(sicor_operacao_basica_estado_2022)


sicor_operacao_basica_estado_2022 <- janitor::clean_names(sicor_operacao_basica_estado_2022)

empreendimento <- read_csv("Empreendimento.csv", 
                           locale = locale(encoding = "LATIN1"))

empreendimento <- janitor::clean_names(empreendimento)


glimpse(empreendimento)



names(empreendimento)[1]<- "cd_empreendimento"

operacao_empreendimento<-
  sicor_operacao_basica_estado_2022%>%
  inner_join(empreendimento)


operacao_empreendimento %>%
  ggplot() +
  geom_boxplot(aes(x=modalidade, y=vl_receita_bruta_esperada))

operacao_empreendimento %>%
  summarise(receita_bruta_esperada = sum(vl_receita_bruta_esperada, na.rm = TRUE),
            .by = modalidade) %>%
  ungroup() %>%
  mutate(modalidade = reorder(modalidade, receita_bruta_esperada)) %>%
  ggplot()+
  geom_col(aes(y=modalidade, x=receita_bruta_esperada))




operacao_empreendimento %>%
  ggplot() +
  geom_boxplot(aes(x=atividade, y=vl_receita_bruta_esperada)) +
  scale_y_log10()

operacao_empreendimento %>%
  summarise(receita_bruta_esperada = sum(vl_receita_bruta_esperada, na.rm = TRUE),
            .by = modalidade) %>%
  ungroup() %>%
  mutate(modalidade = reorder(modalidade, receita_bruta_esperada)) %>%
  ggplot()+
  geom_col(aes(y=modalidade, x=receita_bruta_esperada))


glimpse(operacao_empreendimento)

operacao_empreendimento %>%
  distinct(cd_empreendimento,
           cd_tipo_cultura) %>%
  summarise(n(),
            .by = cd_tipo_cultura)
  

NROW(operacao_empreendimento) *0.01

operacao_empreendimento %>%
  slice_sample(n= 20000) %>%
  ggplot() +
  geom_boxplot(aes(x=atividade, y=vl_receita_bruta_esperada)) +
  scale_y_log10()
  
tam_slice<- 20000

operacao_empreendimento %>%
  slice_sample(n= tam_slice) %>%
  summarise( n(),
             n()/tam_slice,
            .by = atividade)

prop.test(9098, 20000)

operacao_empreendimento %>%
  summarise( n(),
             n()/NROW(operacao_empreendimento),
             .by = atividade)


operacao_empreendimento %>%
  slice_sample(n= tam_slice) %>%
  summarise( quantidade = n(),
             proporcao = n()/tam_slice,
             .by = modalidade) %>%
  arrange(desc(quantidade))

prop.test(115, 20000)

0.00575 * NROW(operacao_empreendimento)
0.004770099 * NROW(operacao_empreendimento)
0.006924239 * NROW(operacao_empreendimento)


operacao_empreendimento %>%
  summarise( quantidade = n(),
             proporcao = n()/NROW(operacao_empreendimento),
             .by = modalidade)

set.seed(1972)
amostra_20000_operacoes_basica_estado<-
  operacao_empreendimento %>%
  slice_sample(n= tam_slice)


soma_receita_bruta_amostra <- sum(amostra_20000_operacoes_basica_estado$vl_receita_bruta_esperada )
soma_receita_bruta <- sum(operacao_empreendimento$vl_receita_bruta_esperada)

