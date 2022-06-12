{ name = "svg-editor"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "exceptions"
  , "halogen"
  , "halogen-svg-elems"
  , "maybe"
  , "prelude"
  , "transformers"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
