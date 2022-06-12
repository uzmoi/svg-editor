{ name = "svg-editor"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "exceptions"
  , "halogen"
  , "halogen-svg-elems"
  , "maybe"
  , "prelude"
  , "random"
  , "transformers"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
