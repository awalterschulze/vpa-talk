module NuttyBananaBread ( example ) where

import TreeConstructors
import Data.Tree

example :: [Tree String]
example = [
    node "Ingredients" [
      node "0" [
        strField "Description" "2 cups (250g) all-purpose flour (spoon & leveled)",
        strField "Name" "All Purpose Flour",
        node "Quantity" [
          numField"Amount" 2,
          strField "Kind" "cup"
        ]
      ],
      node "1" [
        strField "Description" "1 teaspoon baking soda",
        strField "Name" "Baking Soda",
        node "Quantity" [
          numField"Amount" 1,
          strField "Kind" "tsp"
        ]
      ],
      node "2" [
        strField "Description" "1/4 teaspoon salt",
        strField "Name" "Salt",
        node "Quantity" [
          numField"Amount" 0.25,
          strField "Kind" "tsp"
        ]
      ],
      node "3" [
        strField "Description" "1/2 teaspoon ground cinnamon",
        strField "Name" "Cinnamon",
        node "Quantity" [
          numField"Amount" 0.5,
          strField "Kind" "tsp"
        ]
      ],
      node "4" [
        strField "Description" "1/2 cup (1 stick or 115g) unsalted butter, softened to room temperature",
        strField "Name" "Unsalted Butter",
        node "Quantity" [
          numField"Amount" 0.5,
          strField "Kind" "cup"
        ]
      ],
      node "5" [
        strField "Description" "3/4 cup (150g) packed light or dark brown sugar",
        strField "Name" "Brown Sugar",
        node "Quantity" [
          numField"Amount" 0.75,
          strField "Kind" "cup"
        ]
      ],
      node "6" [
        strField "Description" "2 large eggs, at room temperature",
        strField "Name" "Eggs",
        numField"Number" 2
      ],
      node "7" [
        strField "Description" "1/3 cup (80g) plain yogurt or sour cream, at room temperature (I use Greek yogurt)",
        strField "Name" "Sour Cream",
        node "Quantity" [
          numField"Amount" 80,
          strField "Kind" "g"
        ]
      ],
      node "8" [
        strField "Description" "2 cups mashed bananas (about 4 large ripe bananas)",
        strField "Name" "Bananas",
        numField"Number" 4
      ],
      node "9" [
        strField "Description" "1 teaspoon pure vanilla extract",
        strField "Name" "Vanilla Extract",
        node "Quantity" [
          numField"Amount" 1,
          strField "Kind" "tsp"
        ]
      ],
      node "10" [
        strField "Description" "optional: 3/4 cup (100g) chopped pecans or walnuts",
        strField "Name" "Chopped Pecans",
        node "Quantity" [
          numField"Amount" 0.75,
          strField "Kind" "tsp"
        ]
      ]
    ],
    strField "Reference" "https://sallysbakingaddiction.com/best-banana-bread-recipe/"
  ]
