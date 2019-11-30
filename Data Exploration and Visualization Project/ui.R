source("chooser.R")

shinyUI(
  dashboardPage(
    
    dashboardHeader(
      title = "Financial Data of
        Chinese Companies by Industry",
      titleWidth = 650
    ),
    
    dashboardSidebar(
     
    h3(HTML('&nbsp;'),"Visualization Project"),
    br(),
      
      sidebarMenu(
        
        #style = "position: fixed; overflow: visible;",
        menuItem(
          "TreeMap", 
          tabName = "tab2",
          icon = icon("th-large")
        ),
        
        menuItem(
          "Classification", 
          tabName = "tab1",
          icon = icon("project-diagram")
        ),
        
        menuItem(
          "Comparison", 
          tabName = "tab3",
          icon = icon("chart-line")
        ),
        
        menuItem(
          "Help", 
          tabName = "tab0",
          icon = icon("question")
        )
        
      ),
      br(),
      
      
      
      sliderInput(
        "yearRange",
        label = em("Chooes the range of year:",
                   style = "text-align:center;
                 color:#FFA319;font-size:100%"),
        min = 2009,
        max = 2018,
        value = c(2009, 2018),
        step = 1,
        sep = "",
        width = 200,
        animate = animationOptions(interval = 800,
                                   loop = TRUE)
      )
      
      
    ),
    
    dashboardBody(
      
      tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, 
        "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
      '))),
      
      tabItems(
        
        tabItem(
          tabName = "tab0",
          
          h1("Introduction of the Project",
             style = "text-align:center;
                color:blue;font-size:200%"),
          h4("* This Shiny App is used to view 
             the financial data of 
             Chinese listed companies."),
          h3("Purpose:"),
          h4("As we know, China is one of the 
             developing countries with repaid 
             development in recent years. 
             Through researching these questions, 
             people can realize the factors that 
             contribute the rapid development in 
             economics. Also, people might find the 
             chance to develop their own business in 
             the future by analyzing every industry."),
          
          h3("Data:"),
          h4("The data used in this app is downloaded 
             from China Stock Market & Accounting 
             Research Database (CSMAR)."),
          h4("There are totally 1808 companies in 
             this dataset."),
          h4("The data is selected from 1 / 1 / 2009 
             to 31 / 12 / 2018, totally ten years."),
          
          h3("Special terms:"),
          h4(strong("ROA"),
             br("The return on assets (ROA) shows 
                the percentage of how profitable a 
                company's assets are in generating 
                revenue.")),
          h4(strong("Growth Rate"),
             br("The ratio that the asset in current 
                year compared with that in 2009, which 
                shows development speed.")),
          
          h3("Components:"),
          h4(strong("Tree map"),
             br("This page shows a tree map according 
                to the industry type user chose. 
                The size and color of the block 
                represent the value of ROA for this 
                industry. By clicking any of the block, 
                a bar plot will show up about the 
                changes of ROA in recent ten years.")),
          h4(strong("Classification"),
             br("This page shows a tree diagram which 
                enable user view the classification of 
                industries. The node size represents 
                the value of this class. By clicking 
                one of the nodes, a series of sub-nodes 
                will show up. If the node turns white, 
                that means this object cannot be 
                divided further.")),
          h4(strong("Comparison"),
             br("This page shows two point plots of 
                selected industries. In the bottom, 
                user can pick any industry they want
                and the data of the selected industry 
                will show on the plot. By selecting 
                multiple industries, user can clearly 
                see the comparison among them.")),
          h4(strong("Range of year"),
             br("This is a control bar. User can drag 
                the sliding block to change the range 
                of time. Once the range is changed, all
                the plots will automatically adjust the 
                time with selected range. By clicking 
                the play button, user can see the 
                dynamic changes of the financial data 
                in the plot."))
          
        ),
        
        tabItem(
          tabName = "tab1",
          h1("Data by industry classification",
             style = "text-align:center;
                color:blue;font-size:200%"),
          
          
          radioButtons(
            "column",
            label = em("Choose a value type:",
                       style = "text-align:center;
                   color:#FFA319;font-size:150%"),
            list("Average ROA" = "ROA",
                 "Average growth rate" = "Rate")
          ),
          br(),
          collapsibleTreeOutput("tree",
                                width = 800,
                                height = 600)
        ),
        
        tabItem(
          tabName = "tab3",
          h3("ROA of selected industries",
             style = "text-align:center;
             color:blue;font-size:200%"),
          plotOutput("plot1"),
          br(),
          h3("Growth Rate of selected industries",
             style = "text-align:center;
             color:blue;font-size:200%"),
          plotOutput("plot2"),
          br(),
          chooserInput(
            "mychooser",
            "Available frobs",
            "Selected frobs",
            tree$Industry,
            c(),
            size = 10,
            multiple = TRUE
          )
        ),
        
        tabItem(
          tabName = "tab2",
          
          selectizeInput(
            "region",
            label = em("Select Industry Type",
                       style = "text-align:center;
                   color:#FFA319;font-size:150%"),
            tree$Type,
            selected = 'Manufacturing',
            multiple = TRUE
          ),
          
          conditionalPanel(
            condition = "output.condition1 == 1",
            
            tags$h1("Average ROA by Industry Type 
                    in Ten Years",
                    style = "text-align:center;
                    color:blue;font-size:200%"),
            
            tags$p("Click On Any Industry To Get 
                   The Detailed Information",
                   style = "text-align:center;
                   color:purple"),
            
            plotOutput("treemap_industry",
                       height = "600px",
                       click = "click_treemap_industry")
          ),
          
          conditionalPanel(
            condition = "output.condition1 == 0",
            br(),
            br(),
            plotlyOutput("roa_year"),
            uiOutput("zoomout")
          )
        )
        
      )
    )
  )
)