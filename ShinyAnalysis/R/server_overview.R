#####################################
## @Description: 
## @version: 
## @Author: Li Kangguo
## @Date: 2026-03-10 14:02:09
## @LastEditors: Li Kangguo
## @LastEditTime: 2026-03-10 14:02:09
#####################################
overview_server <- function(input, output, session) {
  output$burden_plot <- shiny::renderPlot({
    burden_plot(input$burden_metric)
  }, res = 110)

  output$status_group_plot <- shiny::renderPlot({
    status_group_plot
  }, res = 110, height = function() 440)
}