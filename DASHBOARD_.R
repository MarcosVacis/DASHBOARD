# Lista de pacotes e carregamento
pacotes <- c("dplyr", "lubridate", "openxlsx", "data.table", "readr", "magrittr", "stringr", "tidyr", "readxl")
lapply(pacotes, library, character.only = TRUE)

# Diretório da pasta onde os arquivos estão
caminho <- "C:/Users/marco/Downloads/DATASET"

# Lista os arquivos .xlsx na pasta
arquivos <- list.files(caminho, pattern = "\\.csv$", full.names = TRUE)

# Lê todos os arquivos em uma lista usando read_xlsx() para arquivos Excel
lista_dados <- lapply(arquivos, read.csv, sep = ";")

# Acessar cada tabela individualmente
PAGAMENTOS_ <- lista_dados[[1]]
PLANOS_DE_CONTAS <- lista_dados[[2]]
RECEBIMENTOS_ <- lista_dados[[3]]

# Fazendo join entre PAGAMENTOS e palno contas

PagamentosxContas <- PAGAMENTOS_ %>% 
  inner_join(PLANOS_DE_CONTAS, by = c("ID" = "id"))

#Realizando join entre recebimentos  x plano contas

RecebimentosxPlano_contas <- RECEBIMENTOS_ %>% 
  inner_join(PLANOS_DE_CONTAS, by = c("plano_contas_id" = "id"))

Consolidado <- PagamentosxContas %>% 
  inner_join(RecebimentosxPlano_contas, by = c("ID" = "plano_contas_id"))

#Começando as medidas

#RECEITA
Consolidado  <- Consolidado %>% 
  mutate(RECEITA = sum(valor_recebido))

#SAIDAS
Consolidado <- Consolidado %>% 
  mutate(SAIDAS = sum(Valor))

#receitas por contas
Consolidado <- Consolidado %>% 
  group_by(conta.x) %>% 
  mutate(Receita_por_contas = sum(valor_recebido[conta.x %in% c("Operacional", "Não Operacional")], na.rm = TRUE)) %>% 
  ungroup()

#Receitas por clientes 
Consolidado <- Consolidado %>%
  group_by(cliente) %>% 
  mutate(Receita_cliente = sum(RECEITA, na.rm = TRUE)/ n_distinct(cliente)) %>% 
  ungroup()

#receita por tipo 
Consolidado <- Consolidado %>%
  group_by(tipo.x) %>% 
  mutate(Receita_tipo = sum(Valor[tipo.x%in% c("Receita", "Fixo", "Variavel")], na.rm = TRUE)) %>% 
ungroup()

#Custo
Consolidado <- Consolidado %>% 
  group_by(lancamento.x) %>% 
  mutate(Custo = sum(SAIDAS[lancamento.x %in%c("Custo")], na.rm = TRUE)) %>% 
ungroup()


#margem bruta 
Consolidado <- Consolidado %>% 
  mutate(Margem_bruta = RECEITA - Custo)

#Despensas 
Consolidado <- Consolidado %>% 
  group_by(lancamento.x) %>% 
  mutate(Despesas = sum(SAIDAS[lancamento.x %in%("Despesa")], na.rm = TRUE)) %>% 
ungroup()

#Lucro
Consolidado <- Consolidado %>% 
  mutate(Lucro = RECEITA - Custo - Despesas)

#Lucro em %
Consolidado <- Consolidado %>% 
  mutate(Lucro_em_percentual = Lucro / RECEITA)

#Extraindo mes para calculado
Consolidado <- Consolidado %>%
  mutate(Data = ymd(Data.x),
         ano = year(Data.x),
         mes = month(Data.x),
         mes_nome = format(Data, "%B"))
#pagamento por tipo e mes

Consolidado <- Consolidado %>% 
  group_by(mes) %>% 
  mutate(Pagamento_tipo_mes = sum(Receita_tipo, na.rm = TRUE))

# Dashboard <- Consolidado %>% 
#   dplyr:: select(RECEITA,Custo,Despesas,Lucro,ano, mes,conta.x,tipo.x,Receita_cliente, cliente  )

write.xlsx(Consolidado, "C:/Users/marco/Downloads/Consolidado.xlsx")
