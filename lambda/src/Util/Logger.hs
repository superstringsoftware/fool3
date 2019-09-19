module Util.Logger where


data LogMessage a = LogError {
    message :: String,
    payload :: a
} | LogWarning {
    message :: String,
    payload :: a
} | LogInfo {
    message :: String,
    payload :: a
} | LogDebug {
    message :: String,
    payload :: a
} | LogTrace {
    message :: String,
    payload :: a
} deriving (Show, Eq, Ord)