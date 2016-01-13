import Neovim

import qualified Omnisharp.Neovim.Plugin as Omnisharp
import qualified Fibonacci as Fibonacci

main :: IO()
main = neovim defaultConfig
    { plugins = plugins defaultConfig ++ [ Fibonacci.plugin, Omnisharp.plugin ]
    }
