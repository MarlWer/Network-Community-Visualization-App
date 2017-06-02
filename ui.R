# SET WORKING DIRECTORY!
library("shiny")

shinyUI(
  fluidPage(
    
    #####################
    # Application title #
    #####################
    
    titlePanel(title = h1(tags$b("Network Community Detection"))),
    
    #################
    # SidebarLayout #
    #################
    
    sidebarLayout(
      
      ################
      # SidebarPanel #
      ################
      
      sidebarPanel(position = "right",
                   p("The options below specify the datafile. 
                     If you do not have data yourself but want to try out 
                     the app, you can select the Demo dataset below.
                     Then, you can plot a network based on the 25 NEO-PI-R
                     items (5 items per trait: neuroticism, extroversion,
                     openness, conscientiousness, and agreableness). 
                     Make sure you specify 1:25 in the columnrange below,
                     if you use the Demo dataset."),
                   tags$a(href = "http://personality-project.org/r/psych",
                          "Thanks to the psych package for the demo data!"),
                   br(),
                   br(),
                   p("Need help using this app?",
                   tags$a(href = "https://github.com/MarlWer/Network-Community-Visualization-App",
                          "Download its manual from
                          GitHub.")),
                   br(),
                   p("Suggestions or found a bug?",
                   tags$a(href = "https://github.com/MarlWer/Network-Community-Visualization-App/issues",
                          "Use the GitHub Issue Tracker.")),
                   
                   ######################################
                   # Specify if demo data is to be used #
                   ######################################
                   
                   checkboxInput("demo",
                                 label = "Demo Version",
                                 value = FALSE),
                   
                   # Specify type of data
                   
                   selectInput("typedata", 
                               label = "Specify your type of data here:",
                               choices = list(".csv",
                                              ".sav",
                                              ".txt"
                                             # ".xls",
                                             # ".xlsx"
                                              ),
                               selected = ".csv"),
                   
                   # Upload file
                   fileInput("file", 
                             "Choose file"),
                   
                   fluidRow(
                     column(4,
                            # First row = variable labels?
                            checkboxInput(inputId = "header", 
                                          label = "Header",
                                          value = FALSE)),
                     column(4,
                            # Are string to be coded as factor objects?
                            checkboxInput(inputId = "stringfactors", 
                                          label = "Strings as factors", 
                                          value = FALSE)),
                     column(4,
                            selectInput(inputId = "sep",
                                        label = "Separator",
                                        choices = c(Comma = ",",
                                                    Semicolon = ";",
                                                    Tab = "\t",
                                                    Space = " "
                                                    )))
                   ),
                   
                   # Specify the columns that are to be used
                   fluidRow(
                     column(6,
                            numericInput("columnrangefrom",
                                         label = "From column",
                                         min = 1,
                                         value = 1)),
                     column(6,
                            numericInput("columnrangeto",
                                         label = "To column",
                                         min = 2,
                                         value = 2))),
                   
                   tags$hr(),
                   
                   ####################################
                   # Choose network estimation method #
                   ####################################

                   selectInput("method",
                               label = "Network Estimation Method:",
                               choices = c("Partial Correlation",
                                           "GLASSO",
                                           "IsingSampler",
                                           "IsingFit",
                                         # "MixedGraphicalModel",
                                           "Huge"),
                               selected = "Partial Correlation"),
                   
                   tags$hr(),
                   
                   h4(tags$b("Authors")),
                   p("Marlene Werner <m[dot]a[dot]werner[at]student[dot]auc[dot]nl>"),
                   p("The creation of this app would not have been possible without
                     the help of Sacha Epskamp and Claire Stevenson."),
                   p("Additional thanks goes to Jolanda Kossakowski and her Network App,
                     from which I learned a lot during the implementation of this app."),
                   tags$hr()

      ), # exit sidebarpanel
      
      #############
      # MainPanel #
      #############   
      
      mainPanel(
        tabsetPanel(type = "tab",
                    tabPanel("Network",
                             br(),
                             br(),
                             
                             plotOutput("networkplot", 
                                        width = "60%", 
                                        height = "600px"),
                             
                             tags$hr(),
                             
                             fluidRow(

                             column(4,
                                    actionButton(inputId = "goNw", 
                                                 label = "Plot/Update Network!")),
                             column(4,
                                    downloadButton("downloadnw", 
                                                   "Download PDF"))
                             
                             ),

                             tags$hr(),
                             br(),
                             
                             ################
                             # PCOR NETWORK #
                             ################
                             
                             conditionalPanel(
                               condition = "input.method == 'Partial Correlation'",
                               
                               fluidRow(
                                 column(4,
                                        
                                        # Select method of association
                                        selectInput("corMethodPcor",
                                                    label = "Chose Type of Association:",
                                                    choices = c("cor_auto", 
                                                                "cov",
                                                                "cor",
                                                                "npn"),
                                                    selected = "cor")),
                                 column(4,
                                        
                                        # Select network layout
                                        selectInput("layoutPcor",
                                                    label = "Network Layout:",
                                                    choices = c("Circle", 
                                                                "Spring"),
                                                    selected = "Spring"))),
                               
                               fluidRow(
                                 column(4),
                                 column(4,
                                        # Select width edge
                                        sliderInput("edgesizePcor",
                                                    label = "Edge Size:",
                                                    min = 0,
                                                    max = 25,
                                                    value = 5))),
                               
                               fluidRow(
                                 column(4),
                                 column(4,
                                        # Select size of nodes
                                        sliderInput("nodesizePcor",
                                                    label = "Node Size:",
                                                    min = 0,
                                                    max = 25,
                                                    value = 6.1)))),
                            ##################
                            # GLASSO NETWORK #
                            ##################
                             
                              conditionalPanel(
                               condition = "input.method == 'GLASSO'",
                               
                               fluidRow(
                                 column(4,
                                        
                                        # Select method of association
                                        selectInput("corMethodEG",
                                                    label = "Chose Type of Association:",
                                                    choices = c("cor_auto", 
                                                                "cov",
                                                                "cor",
                                                                "npn"),
                                                    selected = "cor")),
                                 column(4,
                                        
                                        # Select network layout
                                        selectInput("layoutEG",
                                                    label = "Network Layout:",
                                                    choices = c("Circle", 
                                                                "Spring"),
                                                    selected = "Spring"))),
                               
                               fluidRow(
                                 column(4,
                                        # Select tuning parameter
                                        sliderInput("tuningEG",
                                                  label = "Chose Tuning Parameter:",
                                                  min = 0,
                                                  max = 1,
                                                  value = 0.5)),
                                 column(4,
                                        # Select width edge
                                        sliderInput("edgesizeEG",
                                                    label = "Edge Size:",
                                                    min = 0,
                                                    max = 25,
                                                    value = 5))),
                               
                               fluidRow(
                                 column(4,
                                        checkboxInput("refitEG", "Refit without regularization", FALSE)),
                                 column(4,
                                        
                                        # Select size of nodes
                                        sliderInput("nodesizeEG",
                                                    label = "Node Size:",
                                                    min = 0,
                                                    max = 25,
                                                    value = 6.1)))),
                            
                            ########################
                            # IsingSampler NETWORK #
                            ########################
                             
                             conditionalPanel(
                               condition = "input.method == 'IsingSampler'",
                               
                               fluidRow(
                                 column(4,
                                        # Select method of association
                                        selectInput("splitMethodIS",
                                                    label = "How to binarize values:",
                                                    choices = c("mean", 
                                                                "median"),
                                                    selected = "median")),
                                 column(4,
                                        # Select network layout
                                        selectInput("layoutIS",
                                                    label = "Network Layout:",
                                                    choices = c("Circle", 
                                                                "Spring"),
                                                    selected = "Spring")),
                                 column(4,
                                        helpText("Note: If you have more than 10 variables",
                                                 "in your dataset, i.e. nodes in your network,",
                                                 "the Ising Models can take a long time to estimate."))),
                               
                               fluidRow(
                                 column(4,
                                        # Select tuning parameter
                                        selectInput("estimationMethodIS",
                                                    label = "How to estimate the model:",
                                                    choices = c("default",
                                                                "loglinear model",
                                                                "pseudolikelihood estimation", 
                                                                "sequential univariate regressions",
                                                                "bivariate regressions"),
                                                    selected = "default")),
                                 column(4,
                                        # Select width edge
                                        sliderInput("edgesizeIS",
                                                    label = "Edge Size:",
                                                    min = 0,
                                                    max = 25,
                                                    value = 5))),
                               
                               fluidRow(
                                 column(4),
                                 column(4,
                                        
                                        # Select size of nodes
                                        sliderInput("nodesizeIS",
                                                    label = "Node Size:",
                                                    min = 0,
                                                    max = 25,
                                                    value = 6.1)))),
                            
                            ####################
                            # IsingFit NETWORK #
                            ####################
                             
                             conditionalPanel(
                               condition = "input.method == 'IsingFit'",
                               
                               fluidRow(
                                 column(4,
                                        
                                        # Select method of association
                                        selectInput("splitMethodIF",
                                                    label = "How to binarize values:",
                                                    choices = c("mean", 
                                                                "median"),
                                                    selected = "median")),
                                 column(4,
                                        
                                        # Select network layout
                                        selectInput("layoutIF",
                                                    label = "Network Layout:",
                                                    choices = c("Circle", 
                                                                "Spring"),
                                                    selected = "Spring")),
                                 column(4,
                                        helpText("Note: If you have more than 10 variables",
                                                 "in your dataset, i.e. nodes in your network,",
                                                 "the Ising Models can take a long time to estimate."))),
                               
                               fluidRow(
                                 column(4,
                                        # Select tuning parameter
                                        selectInput("ruleIF",
                                                    label = "Chose rule to select edges in nodewise estimation:",
                                                    choices = c("AND",
                                                                "OR"),
                                                    selected = "OR")),
                                 column(4,
                                        # Select width edge
                                        sliderInput("edgesizeIF",
                                                    label = "Edge Size:",
                                                    min = 0,
                                                    max = 25,
                                                    value = 5))),
                               
                               fluidRow(
                                 column(4,
                                        # Select tuning parameter
                                        sliderInput("tuningIF",
                                                    label = "Chose Tuning Parameter:",
                                                    min = 0,
                                                    max = 1,
                                                    value = 0.5)),

                                 column(4,
                                        
                                        # Select size of nodes
                                        sliderInput("nodesizeIF",
                                                    label = "Node Size:",
                                                    min = 0,
                                                    max = 25,
                                                    value = 6.1)))),
                            
                            ###############
                            # MGM NETWORK #
                            ###############
                            # 
                            # conditionalPanel(
                            #   condition = "input.method == 'MixedGraphicalModel'",
                            #   
                            #   fluidRow(
                            #     column(4,
                            #            
                            #            # Select method of association
                            #            textInput("type",
                            #                      label = "Types of Variable:")),
                            #     column(4,
                            #            
                            #            # Select network layout
                            #            selectInput("layoutMgm",
                            #                        label = "Network Layout:",
                            #                        choices = c("Circle", 
                            #                                    "Spring"),
                            #                        selected = "Spring"))),
                            #   
                            #   fluidRow(
                            #     column(4,
                            #            textInput("level",
                            #                      label = "Number of levels for each variable:")
                            #     ),
                            #    column(4,
                            #            # Select width edge
                            #            sliderInput("edgesizeMgm",
                            #                        label = "Edge Size:",
                            #                        min = 0,
                            #                        max = 25,
                            #                        value = 5))),
                            #   
                            #  fluidRow(
                            #     column(4,
                            #            # Select tuning parameter
                            #            selectInput("ruleMgm",
                            #                        label = "Chose rule to select edges in nodewise estimation:",
                            #                        choices = c("AND",
                            #                                    "OR"),
                            #                        selected = "OR")),
                            #     column(4,
                            #            
                            #            # Select size of nodes
                            #            sliderInput("nodesizeMgm",
                            #                        label = "Node Size:",
                            #                        min = 0,
                            #                        max = 25,
                            #                        value = 6.1))),
                            #   fluidRow(
                            #     column(4,
                            #            # Select tuning parameter 
                            #           sliderInput("tuningMgm",
                            #                        label = "Chose Tuning Parameter:",
                            #                        min = 0,
                            #                        max = 1,
                            #                        value = 0.5))),

                            #  fluidRow(
                            #     column(4,
                            #            selectInput("criterionMgm",
                            #                       label = "Chose criterion used in model selection:",
                            #                        choices = c("EBIC",
                            #                                    "CV"),
                            #                        selected = "EBIC")))),
                            
                            ################
                            # HUGE NETWORK #
                            ################
                            
                             conditionalPanel(
                               condition = "input.method == 'Huge'",
                               
                               fluidRow(
                                 
                                 column(4,
                                        # Select tuning parameter
                                        sliderInput("tuningH",
                                                    label = "Chose Tuning Parameter:",
                                                    min = 0,
                                                    max = 1,
                                                    value = 0.5)),
                                 column(4,
                                        # Select network layout
                                        selectInput("layoutH",
                                                    label = "Network Layout:",
                                                    choices = c("Circle", 
                                                                "Spring"),
                                                    selected = "Spring"))),
                               
                               fluidRow(
                                 column(4,
                                        selectInput("criterionH",
                                                    label = "Chose criterion used in model selection:",
                                                    choices = c("EBIC",
                                                                "RIC",
                                                                "STARS"),
                                                    selected = "EBIC")),
                                 column(4,
                                        # Select width edge
                                        sliderInput("edgesizeH",
                                                    label = "Edge Size:",
                                                    min = 0,
                                                    max = 25,
                                                    value = 5))),
                               
                               fluidRow(
                                 column(4),
                                 column(4,
                                        # Select size of nodes
                                        sliderInput("nodesizeH",
                                                    label = "Node Size:",
                                                    min = 0,
                                                    max = 25,
                                                    value = 6.1))))
                             
                    ), # exit network tabpanel
                    
                    tabPanel("Community Detection", 
                             br(),
                             br(),
                             
                             plotOutput("comNetwork", 
                                        width = "60%", 
                                        height = "600px"),
                             
                             tags$hr(),

                             
                             fluidRow(
                               column(4,
                                    actionButton(inputId = "goCom", 
                                                 label = "Plot/Update Communities!")),
                               column(4, 
                                    downloadButton("downloadcomnetwork", 
                                                   "Download PDF"))),
                             
                             tags$hr(),
                             br(),
                             
                             fluidRow(
                               column(4,                          
                                      # Select community algorithm
                                      selectInput("typeCA",
                                                  "Select community detection algorithm:", 
                                                  choices = c("Walktrap", "Spinglass"))),
                               column(2,
                                      # Visualize choice for spinglass
                                      checkboxInput("negEdges", 
                                                    label = "Account for negative edges",
                                                    value = FALSE)),
                               column(4,
                                      # change size of cloud
                                      sliderInput("size",
                                                  "Change size of community clouds:",
                                                  min = 1,
                                                  max = 8,
                                                  value = 4))),
                             
                             fluidRow(
                               column(4,
                                      # Visualize widget for number of steps and spins
                                      sliderInput("numberOfStepsSpins", 
                                                  "Select the number of steps/spins taken by the algorithm:", 
                                                  min = 2, 
                                                  max = 500, 
                                                  value = 15)),
                               column(2,
                                      helpText("Note: accounting for negative edges",
                                               "is only possible in spinglass community",
                                               "detection; hence, nothing will change if you",
                                               "check this box and run the walktrap algorithm.")),
                               column(4, 
                                      # Chose color for clouds
                                      selectInput("colCom",
                                                  "Select colorpalette of community clouds:",
                                                  choices = c("rainbow", 
                                                              "heat.colors", 
                                                              "cm.colors", 
                                                              "topo.colors", 
                                                              "terrain.colors", 
                                                              "rainbow_hcl", 
                                                              "diverge_hcl", 
                                                              "terrain_hcl", 
                                                              "sequential_hcl")))),
                             fluidRow(
                               column(4,
                                      # Notification for spinglass
                                      helpText("Note: Be patient if you use spinglass with 
                                               a high number of iterations (>100). It takes
                                               some time until you see the result.")),
                               column(2),
                               column(4,
                                      # Chose whether nodes should be distinguished by gray palette
                                      checkboxInput("nodeColored", 
                                                    label = "Select whether nodes should be colored",
                                                    value = FALSE)))
                    ) # exit community tabpanel
                    ) # exit tabsetpanel
      ) # exit mainpanel
    ) # sidebarlayout
    ) # fluidpage
  ) # shiny exit
