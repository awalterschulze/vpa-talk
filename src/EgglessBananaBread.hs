module EgglessBananaBread ( example ) where

import TreeConstructors
import Data.Tree

example :: [Tree String]
example = [
    node "Ingredients" [
      node "0" [
        strField "Description" "180 g / 1½ cups buckwheat flour*, GF flour mix OR all purpose wheat flour",
        strField "Name" "Buckwheat Flour",
        node "Quantity" [
          numField "Amount" 180,
          strField "Kind" "g"
        ]
      ],
      node "1" [
        strField "Description" "1 tsp of baking powder",
        strField "Name" "Baking Powder",
        node "Quantity" [
          numField "Amount" 1,
          strField "Kind" "tsp"
        ]
      ],
      node "2" [
        strField "Description" "¾ tsp of baking soda",
        strField "Name" "Baking Soda",
        node "Quantity" [
          numField "Amount" 0.75,
          strField "Kind" "tsp"
        ]
      ],
      node "3" [
        strField "Description" "1½ tsp of cinnamon",
        strField "Name" "Cinnamon",
        node "Quantity" [
          numField "Amount" 1.5,
          strField "Kind" "tsp"
        ]
      ],
      node "4" [
        strField "Description" "360 g / 1½ cups mashed up, overripe bananas + 1 banana for decoration (optional)",
        strField "Name" "Bananas",
        numField "Number" 4
      ],
      node "5" [
        strField "Description" "60 ml / ¼ cup mild tasting oil (I used mild olive oil)",
        strField "Name" "Olive Oil",
        node "Quantity" [
          numField "Amount" 60,
          strField "Kind" "ml"
        ]
      ],
      node "6" [
        strField "Description" "90 ml / 1/3 cup + 2 tsp almond milk (or other thin plant milk)",
        strField "Name" "Almond Milk",
        node "Quantity" [
          numField "Amount" 90,
          strField "Kind" "ml"
        ]
      ],
      node "7" [
        strField "Description" "2 tsp lemon or lime juice",
        strField "Name" "Lemon Juice",
        node "Quantity" [
          numField "Amount" 2,
          strField "Kind" "tsp"
        ]
      ],
      node "8" [
        strField "Description" "125 g/ ½ cup + 2 tbsp coconut sugar or demerara sugar**",
        strField "Name" "Coconut Sugar",
        node "Quantity" [
          numField "Amount" 125,
          strField "Kind" "g"
        ]
      ],
      node "9" [
        strField "Description" "2 tsp maple syrup, for glaze (optional)",
        strField "Name" "Maple Syrup",
        node "Quantity" [
          numField "Amount" 2,
          strField "Kind" "tsp"
        ]
      ]
    ],
    strField "Reference" "https://www.lazycatkitchen.com/eggless-banana-bread/"
  ]