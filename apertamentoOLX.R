
extrairAnuncios <- function(url_pagina, info_adicional) {
  
  library(magrittr) # não vivo sem esse pacote
  library(rvest) # principal pacote para web-scraping
  library(readr) # usado para extrair numeros de texto
  library(stringr) # usado para o data cleaning
  library(curl) # usado como suporte para o rvest
  library(tidyr) # data cleaning
  library(dplyr) # data cleaning

  Base <- NULL
  for (i in 1:length(url_pagina)) {
    
    mycurl <- curl(url_pagina[i], handle = curl::new_handle("useragent" = "Mozilla/5.0"))
    mycurl <- read_html(mycurl)
    
    x <- mycurl %>% html_nodes(".OLXad-list-link")
    
    # extrair link do anuncio
    col_links <- mycurl %>% html_nodes(".OLXad-list-link") %>% html_attr("href")
    # extrair titulo do anuncio
    col_titles <- mycurl %>% html_nodes(".OLXad-list-link") %>% html_attr("title")
    # extrair preço
    precos <- lapply(x, . %>% html_nodes(".col-3"))
    precos %<>% lapply(html_text)
    precos %<>% unlist()
    precos <- str_remove_all(precos,"\t")
    precos <- str_remove_all(precos, "\n")
    precos <- str_trim(precos)
    precos <- str_remove_all(precos, "[:punct:]")
    precos <- parse_number(precos)
    col_precos <- precos
    # extrair bairros
    bairros <- mycurl %>% html_nodes(".OLXad-list-line-2") %>% html_text()
    bairros %<>% str_replace_all("[\t]", "")
    bairros %<>% str_replace_all("[\n]", "")
    bairros %<>% str_replace_all("Apartamentos", "")
    bairros %<>% str_replace_all("Aluguel de quartos", "")
    bairros %<>% str_replace_all("Anúncio Profissional", "")
    bairros %<>% str_replace_all("Casas", "")
    bairros %<>% str_replace("-", "")
    bairros %<>% str_trim()
    col_bairros <- bairros
    # extrair informações adicionais de apartamento
    
    if (info_adicional) {
      adicional <- mycurl %>% html_nodes(".detail-specific") %>% html_text()
      adicional %<>% str_replace_all("[\t]", "")
      adicional %<>% str_replace_all("[\n]", "")
      adicional <- str_trim(adicional)
      col_adicionais <- adicional
      
    }
  
   basex <- (data.frame(link = col_links,
                      titulo = col_titles,
                      preco = col_precos,
                      bairro = col_bairros,
                      adicional = col_adicionais,
                      stringsAsFactors = FALSE))
   
   Base <- rbind(Base,basex)
  }
  
  return(Base)
}  

url_apt <- "http://pr.olx.com.br/regiao-de-curitiba-e-paranagua/imoveis/aluguel"
number_pages <- 100 #numero de paginas
# Criar vetor com todos os urls para as páginas do olx
lista_urls <- paste0(url_apt, "?o=", 1:number_pages)

url_pagina <- lista_urls[1]

Base <- extrairAnuncios(lista_urls,TRUE)
