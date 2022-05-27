let upstream = https://github.com/dfinity/vessel-package-set/releases/download/mo-0.6.21-20220215/package-set.dhall sha256:b46f30e811fe5085741be01e126629c2a55d4c3d6ebf49408fb3b4a98e37589b
let aviate_labs = https://github.com/aviate-labs/package-set/releases/download/v0.1.4/package-set.dhall sha256:30b7e5372284933c7394bad62ad742fec4cb09f605ce3c178d892c25a1a9722e
let Package =
    { name : Text, version : Text, repo : Text, dependencies : List Text }

let additions =
    [
      { name = "format"
      , repo = "https://github.com/tomijaga/format.mo"
      , version = "main"
      , dependencies = [ "base" ]
      },
      { name = "op"
      , repo = "https://github.com/tomijaga/op.mo"
      , version = "main"
      , dependencies = [ "base" , "fmt", "encoding"]
      },
      { name = "http"
      , repo = "https://github.com/aviate-labs/http.mo"
      , version = "v0.1.0"
      , dependencies = [ "base" ]
      },
      { name = "httpie"
      , version = "development"
      , repo = "https://github.com/tomijaga/http-parser.mo"
      , dependencies = ["base", "json", "array", "encoding", "http" ] : List Text
      }
    ] : List Package

in  upstream # aviate_labs # additions
