detect_language=function(x){
library(httr)
library(jsonlite)

key='027aa188db114ffab73a51e03b639e46'

request_body <- data.frame(
  id = "1",
  text = x,
  stringsAsFactors = FALSE
)
request_body$stringsAsFactors = FALSE
request_body_json=toJSON(list(documents = request_body), auto_unbox = TRUE)

result=POST("https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/languages?numberOfLanguagesToDetect=1", body = request_body_json, add_headers(.headers = c("Content-Type"="application/json","Ocp-Apim-Subscription-Key"=key)))
lang=content(result)
lang$documents[[1]]$detectedLanguages[[1]]$name
}