# masstimes

A small wrapper around to fetch results from the masstimes.org website.

```common-lisp
MASSTIMES> (first (retrieve-churches-from-lat-lon 40.741 -74.001))
#<CHURCH {10019375D3}>
```

Results from the API are automatically wrapped in classes.
