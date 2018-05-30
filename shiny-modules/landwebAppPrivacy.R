landwebAppPrivacyUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("appPrivacyUI"))
}

landwebAppPrivacy <- function(input, output, session) {
  output$appPrivacyUI <- renderUI({
    ns <- session$ns

    tagList(
      fluidRow(
        shinydashboard::box(
          title = "Privacy Statement", status = "success",
          solidHeader = TRUE, collapsible = TRUE, width = 12,

          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce augue tortor, ultricies ac placerat quis, consectetur ut velit. Pellentesque sagittis ultrices imperdiet. Cras viverra sit amet lectus sit amet ullamcorper. Aliquam venenatis eget tortor quis ultricies. In molestie porta purus, ut dapibus massa efficitur ac. Phasellus non erat maximus, fringilla libero eu, iaculis turpis. Praesent eu venenatis enim, ut posuere augue. Maecenas venenatis euismod fringilla. Nunc congue vehicula tellus, eu condimentum ante malesuada at. Vivamus convallis euismod elit. Phasellus enim neque, posuere ut nunc non, congue rutrum enim. Fusce luctus interdum felis, et hendrerit risus efficitur nec."),
          p("Pellentesque consequat convallis lectus eget ullamcorper. Mauris sagittis accumsan massa eu malesuada. Nam a porta justo. In vitae aliquet odio. Nulla molestie tempor mollis. Sed in ultrices odio, nec vulputate urna. Quisque quis erat ac nisl dapibus commodo fringilla quis ante. Sed sed sapien blandit, hendrerit lectus sit amet, sollicitudin lacus. Nam nec tempus urna, ac ullamcorper enim. Quisque metus tortor, suscipit quis tortor sit amet, facilisis dignissim magna. Mauris sed nisl id diam gravida sagittis."),
          p("Mauris tempor leo augue, sed vehicula ipsum porttitor ac. Pellentesque vestibulum, ligula a accumsan auctor, odio metus malesuada ligula, vel pharetra nulla justo in nibh. Suspendisse potenti. Nullam eget neque ac mauris posuere sollicitudin sit amet et lorem. Maecenas ornare id urna et porttitor. In sagittis tincidunt facilisis. In ornare porttitor erat, et lacinia nibh scelerisque pellentesque. Nulla faucibus, velit non venenatis porttitor, nibh erat vestibulum nulla, et feugiat justo ligula eu diam. Vivamus ut lorem semper nisl hendrerit dictum sit amet vitae arcu. Quisque vitae elit in arcu egestas sollicitudin eget nec odio. Nulla sit amet eros sit amet lorem molestie condimentum. Donec dui eros, feugiat vel congue sed, blandit et mauris. Nullam sed neque augue."),
          p("Nunc in faucibus libero, eu feugiat arcu. Donec elementum pulvinar eros ut rhoncus. Donec vel nunc sit amet ipsum lacinia fringilla sed eget elit. Quisque consectetur consequat elementum. Fusce vel consequat libero. Donec dapibus sem leo, eu pretium urna vulputate ut. Ut bibendum eleifend justo, sit amet congue purus pulvinar in. Curabitur volutpat consequat sapien sed hendrerit. Fusce turpis ex, malesuada ut ligula vitae, sagittis lacinia mauris. Duis lacinia sed diam vel pharetra. Etiam mollis lectus commodo dictum convallis. In vitae sem arcu. Aliquam ut turpis sed diam iaculis consectetur. Praesent et vestibulum mauris, quis dictum est."),
          p("In at mollis est, eget tempor ligula. Morbi ornare blandit posuere. Aenean malesuada lectus ac tortor elementum efficitur. Quisque porttitor volutpat ante in rutrum. Donec consequat nibh vitae sagittis pellentesque. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Donec quis mi a ipsum lacinia vulputate ut et neque. Quisque at est eu quam tincidunt semper vehicula a nisi. Vestibulum iaculis dui ut dui cursus dapibus. Curabitur in neque arcu. Quisque sem nisl, venenatis ac ligula nec, pulvinar egestas elit. Nam vel ligula nulla. Donec interdum erat erat, sed congue enim facilisis sodales.")
        )
      )
    )
  })
}
