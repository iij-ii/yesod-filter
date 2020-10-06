# yesod-filter

[![Build Status](https://travis-ci.org/iij-ii/yesod-filter.svg?branch=master)](https://travis-ci.org/iij-ii/yesod-filter)

yesod-filter is a library that automatically generates [Filter](https://hackage.haskell.org/package/persistent-2.10.5.2/docs/Database-Persist-Types.html#t:Filter) and [SelectOpt](https://hackage.haskell.org/package/persistent-2.10.5.2/docs/Database-Persist-Types.html#t:SelectOpt) from URL query string.
yesod-filter is inspired by [django-filter](https://github.com/carltongibson/django-filter).

## Usage

### Example

```haskell
{-
# Suppose the model is defined as follows:
Pet json
    name Text
    age Int
    deriving Eq
    deriving Show

# And the route is defined as follows:
/pets PetsR GET
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Pet where

import           Import
import           Yesod.Filter.TH

-- Define the query options to be available.
$(mkFilterGenerator "Pet" defaultOptions
    { filtering = defaultFiltering
        { filterDefs =
            [ FilterDef "name" defaultFilterParams
            , FilterDef "age"  defaultFilterParams
            ]
        }
    , sorting = defaultSorting
        { sortFields = ["name", "age"]
        , defaultOrdering = ORDERBY "id" ASC
        }
    }
 )

getPetsR :: Handler Value
getPetsR = do
    -- The list of Filter and SelectOpts are automatically converted from query parameters.
    filters' <- $(mkFilters)
    selectOpts <- $(mkSelectOpts)
    pets <- runDB $ selectList filters' selectOpts
    returnJson pets
```

The above handler definition generates the following endpoint.

```sh
# Without query strings
$ curl -s "http://localhost:3000/pets" | jq .
[
  {
    "age": 5,
    "name": "John",
    "id": 1
  },
  {
    "age": 3,
    "name": "Charlie",
    "id": 2
  },
  {
    "age": 10,
    "name": "Jack",
    "id": 3
  }
]

# Filter: WHERE AGE >= 3
$ curl -s "http://localhost:3000/pets?age__gt=3" | jq .
[
  {
    "age": 5,
    "name": "John",
    "id": 1
  },
  {
    "age": 10,
    "name": "Jack",
    "id": 3
  }
]

# SelectOpt: ORDER BY name
$ curl -s "http://localhost:3000/pets?sort=name" | jq .
[
  {
    "age": 3,
    "name": "Charlie",
    "id": 2
  },
  {
    "age": 10,
    "name": "Jack",
    "id": 3
  },
  {
    "age": 5,
    "name": "John",
    "id": 1
  }
]
```

## LICENCE

Copyright (c) IIJ Innovation Institute Inc.

Licensed under The 3-Clause BSD License.
