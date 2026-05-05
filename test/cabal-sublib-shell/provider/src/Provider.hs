module Provider (mainMessage) where

import Provider.Slib (slibMessage)

mainMessage :: String
mainMessage = "provider main: " <> slibMessage
