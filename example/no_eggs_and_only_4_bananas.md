## I have a vegan guest coming over

```
.Ingredients:
  !(
    ._:.Name == "Eggs"
   )
```

## I only have 3 bananas

```
.Ingredients:
  ._:{
    Name == "Bananas";
    Number <= 3;
    *
  }
```

## A vegan guest is coming over and I only have 4 bananas

```
.Ingredients:
  (
    !(
	  ._:.Name == "Eggs"
     ) &
   	 ._:(
       .Name == "Bananas" &
       .Number <= 4
     )
  )
```