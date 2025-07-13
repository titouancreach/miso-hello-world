{-# LANGUAGE OverloadedStrings #-}

module HeaderComponent where

import Miso
import Miso.Lens
import Miso.String

generateLink :: MisoString -> View action
generateLink name = a_ [class_ "text-sm/6 font-semibold text-gray-900", href_ "#"] [text name]

viewModel :: () -> View action
viewModel _ =
  header_
    [class_ "bg-white"]
    [ nav_
        [class_ "mx-auto flex max-w-7xl items-center justify-between p-6 lg:px-8"]
        [ div_
            [class_ "flex items-center gap-x-12"]
            [ a_
                [href_ "#", class_ "-m-1.5 p-1.5"]
                [ img_
                    [src_ "https://tailwindcss.com/plus-assets/img/logos/mark.svg?color=indigo&shade=600", class_ "h-8 w-auto"]
                ],
              div_
                [class_ "hidden lg:flex lg:gap-x-12"]
                [ generateLink "Product",
                  generateLink "Feature",
                  generateLink "Marketplace"
                ]
            ],

          div_
            [ class_ "hidden lg:flex"]
            [
              a_
              [ href_ "#", class_ "text-sm/6 font-semibold text-gray-900"]
              [
                text "Log in",
                span_ [] [text "&rarr;"]
              ]
            ]
        ]
    ]
