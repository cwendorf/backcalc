# [`backcalc`](https://github.com/cwendorf/backcalc)

## Custom Print Method for backcalc Objects

**Aliases:**

- `print.backcalc`

### Description

This function provides a tailored print method for objects of class backcalc.
It optionally displays additional attributes such as notes and approximation messages
alongside the main data output.

### Usage

```r
printbackcalc(x, ...)
```

### Arguments

- **`x`**: An object of class backcalc, typically a matrix or data frame with
attached attributes "Notes" and "Approximations".
- **`...`**: Additional arguments passed to the base print function.

### Details

If the attribute "attr" is set to TRUE on the object, this method
prints the object along with its "Notes" and "Approximations" attributes.
Otherwise, it prints only the main data content.

### Value

Invisibly returns the original object x.

