# Create table for demographic variables

Input should be a vector with a demographic variable such as gender or
age, and the desired label, enclosed in double quotes.

## Usage

``` r
RIdemographics(dif.var, diflabel, ...)
```

## Arguments

- dif.var:

  A vector with a demographic variable

- diflabel:

  What the variable represents (sex/age/etc), in quotes

- ...:

  Options for table, see
  [`kbl_rise()`](https://pgmj.github.io/easyRasch/reference/kbl_rise.md)

## Details

Sample use: RIdemographics(dif.gender, "Gender", width = 40)
