# this message is printed on several lines (one per path) to make multiple paths
# easier to spot
# parentDir <- "R:/PICS/06_biopathways/04_code_in_progress/dist"
message('library paths:\n', paste('... ', .libPaths(), sep=' ', collapse='\n'))#, getwd()))

# chrome.portable = file.path(parentDir,
#                             'GoogleChromePortable/App/Chrome-bin/chrome.exe')
# print(chrome.portable)
# 
# launch.browser = function(appUrl, browser.path=chrome.portable) {
#   message('Browser path: ', browser.path)
#   shell(sprintf('"%s" --app=%s', browser.path, appUrl))
# }

.libPaths(.libPaths()[2])
message('library paths:\n', paste('... ', .libPaths(), sep=' ', collapse='\n'))#, getwd()))
shiny::runApp(launch.browser = TRUE)
