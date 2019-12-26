# OBJETIVO: MAPEAR TODOS SCRIPTS DE UM DIRETÓRIO E CRIAR CÓPIAS EM OUTRO (COM ENDEREÇO REFERENCIADO NO NOME ARQUIVO)

setwd("")                                 # DIRETÓRIO BASE
arquivos <- list.files(pattern="\\.R", recursive = T)
arquivos <- arquivos[!grepl(pattern = ".RData", arquivos)]
nomesArquivos <- gsub(pattern = "\\/", replacement = "__", arquivos)

diretorioCopia <- ""                        # DIRETÓRIO DESTINO

for (w in seq_along(arquivos)) {
  file.copy(from = arquivos[w], to = paste(diretorioCopia, nomesArquivos[w], sep = ""))
}
