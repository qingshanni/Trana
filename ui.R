library(shinydashboard)

sidebar <- dashboardSidebar(width = "300px",
  sidebarMenu(
    menuItem("UploadFiles", tabName = "tab_upload", icon = icon("upload"),selected=TRUE),
    menuItem("Single Analysis", icon = icon("compass"),
             menuSubItem("TRBV/TRBJ usage", tabName = "tab_V_J_usage", icon = icon("eye")),
             menuSubItem("TRBV and TRBJ usage", tabName = "tab_VJ_usage", icon = icon("eye")),
             menuSubItem("CDR3 length distribution", tabName = "tab_CDR3_length", icon = icon("eye")),
             menuSubItem("Clonotypes frequency distribution", tabName = "tab_clone_freq_distr", icon = icon("eye")),
             menuSubItem("Clonotypes frequency analysis", tabName = "tab_clone_freq_analy", icon = icon("eye")),
             menuSubItem("Degeneracy analysis", tabName = "tab_degeneracy", icon = icon("eye"))
             # menuSubItem("CDR3 rarecurve", tabName = "tab_CDR3_rarecurve", icon = icon("eye"))
    ),
    menuItem("Pair Analysis", icon = icon("compass"),
             menuSubItem("Overlapping analysis", tabName = "tab_overlapping", icon = icon("eye")),
             menuSubItem("Un-overlapping analysis", tabName = "tab_unoverlapping", icon = icon("eye"))
    ),
    menuItem("Multi Analysis", icon = icon("compass"),
             menuSubItem("Clonal space homeostasis", tabName = "tab_clone_space_hom", icon = icon("eye")),
             menuSubItem("Most abundant clonotypes", tabName = "tab_most_abundant_clone", icon = icon("eye")),
             menuSubItem("Clonal cumulative frequency", tabName = "tab_clone_cumul_freq", icon = icon("eye")),
             menuSubItem("Diversity analysis", tabName = "tab_diversity", icon = icon("eye")),
             menuSubItem("Similarity analysis", tabName = "tab_similarity", icon = icon("eye")),
             menuSubItem("Share clones distribution", tabName = "tab_share_clone_distr", icon = icon("eye")),
             menuSubItem("High-frequency clones track", tabName = "tab_high_freq_clone_track", icon = icon("eye")),
             menuSubItem("High-frequency share clones variation", tabName = "tab_high_freq_share_clone_var", icon = icon("eye"))
             #menuSubItem("Search Clonotypes"), tabName = "tab_search_clonotypes", icon = icon("eye"))
             
    )
    
  )
)  

boardpage <-  dashboardBody(
  tabItems(
    tabItem(tabName = "tab_upload", uiOutput("ui_upload")),
    ### Single Analysis ###
    tabItem(tabName = "tab_V_J_usage", uiOutput("ui_V_J_usage")),
    tabItem(tabName = "tab_VJ_usage", uiOutput("ui_VJ_usage")),
    tabItem(tabName = "tab_CDR3_length", uiOutput("ui_CDR3_length")),
    tabItem(tabName = "tab_clone_freq_distr", uiOutput("ui_clone_freq_distr")), 
    tabItem(tabName = "tab_clone_freq_analy", uiOutput("ui_clone_freq_analy")),
    tabItem(tabName = "tab_degeneracy", uiOutput("ui_degeneracy")),
    tabItem(tabName = "tab_CDR3_rarecurve", uiOutput("ui_CDR3_rarecurve")),
    ### Pair Analysis ###
    tabItem(tabName = "tab_overlapping", uiOutput("ui_overlapping")),
    tabItem(tabName = "tab_unoverlapping", uiOutput("ui_unoverlapping")),
    ### Multi Analysis ###
    tabItem(tabName = "tab_clone_space_hom", uiOutput("ui_clone_space_hom")),
    tabItem(tabName = "tab_most_abundant_clone", uiOutput("ui_most_abundant_clone")),
    tabItem(tabName = "tab_clone_cumul_freq", uiOutput("ui_clone_cumul_freq")),
    tabItem(tabName = "tab_diversity", uiOutput("ui_diversity")),
    tabItem(tabName = "tab_similarity", uiOutput("ui_similarity")),
    tabItem(tabName = "tab_share_clone_distr", uiOutput("ui_share_clone_distr")),
    tabItem(tabName = "tab_high_freq_clone_track", uiOutput("ui_high_freq_clone_track")),
    tabItem(tabName = "tab_high_freq_share_clone_var", uiOutput("ui_high_freq_share_clone_var"))
    #tabItem(tabName = "tab_search_clonotypes", uiOutput("ui_search_clonotypes"))
    
  )
)


dashboardPage(
  dashboardHeader(title = "Trana"),
  sidebar,
  boardpage,
  title = "TCR Analysis",
  skin = "black"
)

