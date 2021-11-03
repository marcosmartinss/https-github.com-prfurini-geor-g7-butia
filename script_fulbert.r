atlantic.birds <- read.csv("ATLANTIC_BIRDS_quantitative.csv", encoding = "latin1" )
head(atlantic.birds)
# verificar o diretorio
getwd()

# listar os arquivos
dir()

# 6. importar dados ------------------------------------------------------
# ler uma planilha eletronica (.csv)
read.csv("ATLANTIC_BIRDS_quantitative.csv", encoding = "latin1")

# ler e atribuir uma planilha eletronica (.csv) a um objeto
da <- read.csv("ATLANTIC_BIRDS_quantitative.csv", encoding = "latin1")

# ver os dados
da
# conferir a classe
class(da)
