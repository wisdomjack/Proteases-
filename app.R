library(shiny)

#Serine Proteases 3
#catdog <- list("DHFBJF")



  

chymotrypsin <- c("F","W", "Y", "f", "w", 'y')
#chymotripsinButton1 <- "Chymotripsin"
#Trypsin: C- terminal side of Lys, Arg
trypsin <- c("K", "R", "k", "r")
#trypsinButton2 <- "Trypsin"

#Elastase: C- terminal to Ala, Gly
elastase <- c("A", "G", "a", "g")
#elastaseButton3 <- "Elastase"

#Cyanogen Bromide: C- terminal to Met (not a serine protease)
cnbr <- c("M", "m")
#cnbrButton4 <-"CNBr"

proteases <- c( Chymotrypsin = chymotrypsin,
                Trypsin = trypsin,
                Elastase = elastase,
                CNBr = cnbr)

#proteases <-c("Chymotripsin","Trypsin", "Elastase", "CNBr")
proteasesLegend <-c("Chymotripsin (F, W, Y)","Trypsin (K, R)", "Elastase (A, G)", "CNBr (M)")


get_protease <-function(radioButton_Protease){
  
#   for (i in (1:length(proteasesLegend))) {
#     print("FOR")
#     print(i)
#     if (radioButton_Protease == proteasesLegend[i]){
#       print(proteases[i])
#       return(proteases[i])
#     }
#   }
# }
  
  if (radioButton_Protease == proteasesLegend[1]){
    return(chymotrypsin)

  } else if (radioButton_Protease == proteasesLegend[2]){
    return(trypsin)

  } else if (radioButton_Protease == proteasesLegend[3]){
    return(elastase)

  } else if (radioButton_Protease == proteasesLegend[4]){
    return(cnbr)
  }


}






#This function traverse trough peptide and save the indsis of the cut sites and save it in a vector
get_cut_sites <- function(peptide, enzyme){
  cut_sites <- which(peptide %in% enzyme)
  
  #if statment that will only appened that last amino acid into cut targets if cut target != cut site
  last_amino_acid_bool <- FALSE
  for (cut_site in enzyme) {
    
    if(peptide[length(peptide)]==cut_site){
      last_amino_acid_bool <- TRUE
    }
  }
  
  if(last_amino_acid_bool==FALSE){
    cut_sites <- append(cut_sites, length(peptide))
  }
  #print(cut_site)
  cut_sites
}

digest <- function (peptide="MGAAMSPKR", enzyme="K"){
  
  enzyme <- get_protease(enzyme) 
  cut_sites <-get_cut_sites(peptide, enzyme)
  digestedPeptide<-NULL
  previous_cut_site <- 1

  
  for(cut_site in cut_sites){
    
    digestedPeptide <-append(digestedPeptide, peptide[previous_cut_site:cut_site])
    digestedPeptide <-append(digestedPeptide, "----")
    previous_cut_site <- cut_site + 1

    
  } 
  
  toupper(digestedPeptide)
}




ui <- fluidPage(
  
  
  div(h1("Serine Proteases Simulator")),
  br(),
  
  #Inputs
  sidebarLayout(
    
    
    
    sidebarPanel(
      
      
      
      div("Serine proteases 
      are enzymes that cleave peptide bonds in proteins,
      in which serine serves as the nucleophilic amino acid at the enzyme active site."),
      
      br(),
      
      div("instructions:"),
      div("1. Input a peptide sequence below"),
      div("2. Select serine protease"),
      
      br(),
      
      
      
      
      textInput(inputId = "user_peptide",
                label = h4("Peptide"),
                value = "",
                width = "400px",
                placeholder = "Example: MGAAMSPKR"),
      

      
      radioButtons(inputId = "Proteases",
                   label = "Proteases",
                   choices = proteasesLegend)

      
      
      

      
      
    ),
    
    mainPanel(
      
      
      
      
      
      #Outputs
      titlePanel("Digested Peptide"),
      textOutput("digested_peptide"), 

      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br()

      
      # imageOutput("Cat_Butt")
      
                 
      )
      
    )
  
  
)

server <- function(input, output){
  
  
  
  output$digested_peptide <- renderText({
    
    #convert string into list and list into vector
    peptide <-strsplit(input$user_peptide, "")
    peptide <-unlist(peptide)
      
    req(input$user_peptide)
    digest(peptide,input$Proteases)
  
  })
# 
#   output&Cat_Butt <- renderImage({
#   
#     if(input$user_peptide == "cat butt"){
#       img(src = "cat-rear.jpg", height = 140, width = 400)
#     }  
#   
#   })
  
}


shinyApp(ui = ui, server = server)