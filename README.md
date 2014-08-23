# clj-hpack

[![Build Status](https://travis-ci.org/masaori335/clj-hpack.svg?branch=master)](https://travis-ci.org/masaori335/clj-hpack)

A Clojure library of HPACK draft-09 implementation.

- http://tools.ietf.org/html/draft-ietf-httpbis-header-compression-09

Work in progress.

# Current Features

- Decode binaries without Huffman

# Example

## Encode Headers

```clojure
(ns example
    (:require [clj-hpack.encode]
              [clj-hpack.header-table])
    (:import [clj_hpack.header_table HeaderTable]))

(def header-table (HeaderTable. '()))

(encode! header-table [[":method" "GET"]])
```

## Decode Headers

```clojure
(ns example
    (:require [clj-hpack.decode]
              [clj-hpack.header-table])
    (:import [clj_hpack.header_table HeaderTable]))

(def header-table (HeaderTable. '()))

(decode! header-table [0x82])
```

## License

Copyright © 2014 Masaori Koshiba

The MIT License (http://opensource.org/licenses/MIT)
