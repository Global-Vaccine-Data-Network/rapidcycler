# Extract data from line of text

For use in reading data from options file. Finds line starting with
'title', then extracts everything after the ':' and splits by spaces.
Results are stored in a vector.

## Usage

``` r
extract_and_split_line(title, lines)
```

## Arguments

- title:

  Name or title of line in text, will search for 'title:' at the start
  of the line.

- lines:

  Character vector containing separate lines of text

## Value

Character vector containing information from line of text
