#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

term1 = c("I","am","at","in","a","is","on","it","up","can","mum","dad","us","an","boy","girl","to","my","with","you","no","so","he","she","be","go","the","like")
term2 = c("me","by","we","and","of","this","was","went")
week = c("see","get","do","said")

getSightWords <- function(wordlist){
    if(wordlist == "Term 1"){ return(term1) }  
    if(wordlist == "Term 2"){ return(c(term2,week)) }  
    if(wordlist == "This week"){ return(week) }  
    return(c(term1,term2,week))
}

ui = fluidPage(
    #numericInput("num", label = "Make changes", value = 1),
    sidebarLayout(
        sidebarPanel(
            selectInput("wordlist","Word list:",c("All","Term 1","Term 2","This week")),
            actionButton("update" ,"New word", icon("refresh"),
                         class = "btn btn-primary"),
            helpText("When you click the button above, you should see",
                     "a new sight word, right:")
        ),
        
        mainPanel(
            #verbatimTextOutput("value"),
            htmlOutput("htmlword"),
            helpText("Now type the sight word in the box below:"),
            textInput("yourturn","Your turn:"),
            htmlOutput("assess")
        )
    )
)

server = function(input, output, session) {
    
    newWord <- reactiveVal("me")
    oldWord <- reactiveVal("me")
    
    observeEvent(input$update,{
        updateTextInput(session, "yourturn", value="")
    })
    
    observeEvent(input$yourturn,{
        oldWord(newWord())
    })
    
    output$htmlword <- renderUI({
        input$update
        sightwords = getSightWords(input$wordlist)
        isolate({
            newWord(sightwords[as.integer(runif(1,1,length(sightwords))+0.5)])
        })
        isolate(HTML(paste0("<p><font size=36 color=\"blue\">", newWord(), "</font></p><hr>")))
        # isolate(HTML(paste0("<p><font size=36 color=\"blue\">", sightwords[as.integer(runif(1,1,length(sightwords))+0.5)], "</font></p><hr>")))
    })
    
    output$assess <- renderUI({
        attempt = tolower(input$yourturn)
        myword = tolower(newWord())
        if(attempt == myword){
            HTML("<p><font size=36 color=\"green\">Yes!</font></p><hr>")
        }else{
            if(input$yourturn == ""){
                HTML("<p><font size=24 color=\"grey\">Have a go!</font></p><hr>")
            }else{
                HTML("<p><font size=36 color=\"red\">?</font></p><hr>")
            }
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
