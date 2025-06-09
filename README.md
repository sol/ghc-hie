This library can parse HIE files generated with GHC versions:
 - `9.12.2`
 - `9.12.1`
 - `9.10.2`
 - `9.10.1`
 - `9.8.4`
 - `9.8.3`
 - `9.8.2`
 - `9.8.1`

Notes:
 - GHC versions prior to `9.12.1` do not provide [`hie_entity_infos :: NameEntityInfo`][hie_entity_infos] via HIE files.
   - As a consequence `hie_entity_infos` is only available for HIE files that were generated with GHC versions `9.12.1` or later.
   - For HIE files generated with earlier versions `hie_entity_infos` is always `mempty`.
   - It is still entirely possible to e.g. use this library with GHC version `9.10.1` and read HIE files that were generated with GHC version `9.12.2`, including inspecting the `hie_entity_infos` of such HIE files.

- This library reads *wired-in* names as *external* names.
  - The main difference between the two is that wired-in names have `TyThing`s attached, while external names have not.
  - GHC needs this information during type checking, but it is not commonly useful when working with HIE files.  `TyThing` is a somewhat largish data structure, supporting it across multiple versions of GHC is non-trivial.
  - By omitting `TyThing`, wired-in names are effectively treated as external, with the consequence that you cannot access the `TyThing` for wired-in names (`wiredInNameTyThing_maybe` always returns `Nothing`).

[hie_entity_infos]: https://hackage-content.haskell.org/package/ghc-hie-0.0.2/docs/GHC-Iface-Ext-Types.html#v:hie_entity_infos
