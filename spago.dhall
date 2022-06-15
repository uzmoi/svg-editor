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
  , "numbers"
  , "prelude"
  , "random"
  , "transformers"
  , "unsafe-coerce"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
