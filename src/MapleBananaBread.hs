module MapleBananaBread ( example ) where

import TreeConstructors
import Data.Tree

example :: [Tree String]
example = [
    node "Ingredients" [
      node "0" [
        strField "Description" "1/2 cup white sugar",
        strField "Name" "White Sugar",
        node "Quantity" [
          numField "Amount" 0.5,
          strField "Kind" "cup"
        ]
      ],
      node "1" [
        strField "Description" "1/2 cup brown sugar",
        strField "Name" "Brown Sugar",
        node "Quantity" [
          numField "Amount" 0.5,
          strField "Kind" "cup"
        ]
      ],
      node "2" [
        strField "Description" "1 1/2 cups butter, soft",
        strField "Name" "Butter",
        node "Quantity" [
          numField "Amount" 1.5,
          strField "Kind" "cup"
        ]
      ],
      node "3" [
        strField "Description" "2 eggs",
        strField "Name" "Eggs",
        numField "Number" 2
      ],
      node "4" [
        strField "Description" "3–4 large bananas, mashed",
        strField "Name" "Bananas",
        numField "Number" 3
      ],
      node "5" [
        strField "Description" "1 teaspoon maple flavoring",
        strField "Name" "Maple Flavoring",
        node "Quantity" [
          numField "Amount" 1,
          strField "Kind" "tsp"
        ]
      ],
      node "6" [
        strField "Description" "1 1/2 cups flour",
        strField "Name" "Flour",
        node "Quantity" [
          numField "Amount" 1.5,
          strField "Kind" "cup"
        ]
      ],
      node "7" [
        strField "Description" "1/2 cup oatmeal",
        strField "Name" "Oatmeal",
        node "Quantity" [
          numField "Amount" 0.5,
          strField "Kind" "cup"
        ]
      ],
      node "8" [
        strField "Description" "1 teaspoon baking soda",
        strField "Name" "Baking Soda",
        node "Quantity" [
          numField "Amount" 1,
          strField "Kind" "tsp"
        ]
      ]
    ],
    strField "Reference" "https://pinchofyum.com/maple-banana-bread"
  ]
