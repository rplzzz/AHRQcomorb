# AHRQComorb change log

## Version 1.0.1

Fix bugs related to cases with no comorbidities.

* Cases with no comorbidities now return a vector of zeros instead of
  being dropped.  
* Tables with ID variable not in lexical order were being returned with
  rows permuted into lexical order.  Now order is preserved.  
* Fix documentation for comorb() to reflect actual behavior when id
  variable is omitted.  

## Version 1.0

Update to AHRQ v.2023.1
