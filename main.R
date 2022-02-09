# Biblioteca
library(shiny)
library(officer)
library(magrittr)


# UI ----------------------------------------------------------------------
ui <- fluidRow(
tags$head(HTML("<title>Cadastro</title>")),
 includeCSS("www/css.css"),
  h1("Ficha Cadastral", id = "titulo"),
  p("Complete suas informações", id = "subtitulo"),
tags$fieldset(class = "grupo",
  tags$div(class = "campo",
    shiny::textInput("nome", "Nome:")), 
  tags$div(class = "campo", 
  shiny::textInput("cpf_id", "CPF")), 
  tags$div(class = "campo", 
           shiny::textInput("cep_id", "CEP:")),
  tags$div(class = "campo",
           shiny::textInput("prof_id", "Profissão:")), 
  tags$div(class = "campo",  
           shiny::textInput("doc_id", "Doc de Identificação:")),
  tags$div(class = "campo", 
           shiny::textInput("org_id", "Org. Expedidor:")),
  tags$div(class = "campo", 
           shiny::textInput("dt_nasc_id", "Data Nascimento:")),
  tags$div(class = "campo", 
           shiny::textInput("Prof_id", "Profissão:")),
  tags$div(class = "campo", 
           shiny::textInput("nac_id", "Nacionalidade:")),
  tags$div(class = "campo", 
           shiny::textInput("end_resid" , "Endereço Residencial")),
  tags$div(class = "campo", 
           shiny::textInput("Num_id", "Número:")),
  tags$div(class = "campo", 
           shiny::textInput("Comple_id", "Complemento:")),
  tags$div(class = "campo", 
           shiny::textInput("bairro_id", "Bairro:")),
  tags$div(class = "campo", 
           shiny::textInput("city_id", "Cidade:")),
  tags$div(class = "campo", 
           shiny::textInput("uf_id", "UF:")),
  tags$div(class = "campo", 
           shiny::textInput("fone_id", "Telefone Residencial:")),
  tags$div(class = "campo", 
           shiny::textInput("fone_ceclular_id", "Telefone Celular:")),
  tags$div(class = "campo", 
           shiny::textInput("email_id", "Email:"))
  ),
    downloadButton("baixar", "Baixar Documento")
)



# Server ------------------------------------------------------------------
server <- function(session, input, output) {
  

# Logica do CEP para cidade -----------------------------------------------
  shiny::observe({
    
    req(input$cep_id)
    
    x <- input$cep_id
    
    if (stringi::stri_length(x) == 8){
      
    lista_output_cep <- cepR::busca_cep(x ,"3dd9411dfe4ae483ebb85dd6f35082f7") 
    
    shiny::updateTextInput(session, "city_id",
                           value = lista_output_cep %>% dplyr::pull(cidade)) 
    
    shiny::updateTextInput(session, "uf_id",
                           value = lista_output_cep %>% dplyr::pull(estado)) 
    
    shiny::updateTextInput(session, "bairro_id",
                           value = lista_output_cep %>% dplyr::pull(bairro))
    
    shiny::updateTextInput(session, "end_resid",
                           value = lista_output_cep %>% dplyr::pull(logradouro))
    
} else{
  
  shiny::updateTextInput(session, "city_id",
                             value = "")
  
  shiny::updateTextInput(session, "uf_id",
                         value = "") 
  
  shiny::updateTextInput(session, "bairro_id",
                         value = "")
  
  shiny::updateTextInput(session, "end_resid",
                         value = "")
  
}
      
})



  
  output$baixar <- downloadHandler(
    filename = function() {  
      glue::glue("{input$nome}_cadastro_bexs.docx")
    },
    content = function(file) {
      
      doc <- officer::read_docx("www/Ficha_PF.docx")
      
      out <- doc %>% 
        officer::body_replace_all_text("name", input$nome) %>% 
        officer::body_replace_all_text("Doc_id", input$doc_id) %>% 
        officer::body_replace_all_text("Cep_number", input$cep_id) %>% 
        officer::body_replace_all_text("Cpf_number", input$cpf_id) %>% 
        officer::body_replace_all_text("job", input$prof_id) %>% 
        officer::body_replace_all_text("org_exp", input$org_id) %>% 
        officer::body_replace_all_text("org_exp", input$org_id) %>% 
        officer::body_replace_all_text("org_exp", input$org_id) %>% 
        officer::body_replace_all_text("org_exp", input$org_id) %>% 
        officer::body_replace_all_text("org_exp", input$org_id) %>% 
        officer::body_replace_all_text("org_exp", input$org_id) %>% 
        officer::body_replace_all_text("org_exp", input$org_id) %>% 
        officer::body_replace_all_text("org_exp", input$org_id) 
      
      print(out, target = file)
    }
  )
}

shinyApp(ui, server)