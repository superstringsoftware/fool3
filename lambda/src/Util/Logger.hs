module Util.Logger where


data LogMessages = LogError {
    message :: String
} | LogWarning {
    message :: String
} | LogInfo {
    message :: String
} | LogDebug {
    message :: String
} | LogTrace {
    message :: String
} deriving (Show, Eq, Ord)