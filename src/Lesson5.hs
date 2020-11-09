module Lesson5 where

getRequestUrl host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

getHostRequestBuilder host = (\apiKey resource id -> getRequestUrl host apiKey resource id)

getApiKeyRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)

getResourceRequestBuilder hostBuilder apiKey resource = (\id -> hostBuilder apiKey resource id)

