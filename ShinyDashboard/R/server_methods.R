#####################################
## @Description: 
## @version: 
## @Author: Li Kangguo
## @Date: 2026-03-10 14:02:09
## @LastEditors: Li Kangguo
## @LastEditTime: 2026-03-10 14:02:30
#####################################
methods_server <- function(input, output, session) {
  output$study_flow_table <- shiny::renderTable({
    study_summary
  }, striped = TRUE, bordered = FALSE, width = "100%")
}