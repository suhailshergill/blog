import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withBlog)
import Prelude              (IO)

main :: IO ()
main = defaultMain fromArgs withBlog