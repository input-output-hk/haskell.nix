module Main where

-- Distribution.Types.PackageName Data.Yaml Data.String Data.Text Data.Text Distribution.Types.PackageId Data.Text Distribution.Text

-- let showPkg = (\(Just (PackageIdentifier pkg ver)) -> putStrLn $ "    " <> quoted (show (disp pkg)) <> " = hsPkgs."<> quoted (show (disp pkg)) <> "." <> quoted (show (disp ver)) <> ";")
-- let extraDeps = (simpleParse . unpack :: Data.Text.Text -> Maybe PackageIdentifier) <$> (value ^. key "extra-deps" . _Array <&> (^. _String))
-- traverse showPkg extraDeps
