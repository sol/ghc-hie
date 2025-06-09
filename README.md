This library can parse HIE files generated with GHC:
 - 9.12.2
 - 9.12.1
 - 9.10.2
 - 9.10.1
 - 9.8.4
 - 9.8.3
 - 9.8.2
 - 9.8.1

Notes:
 - GHC versions prior to `9.12.1` do not provide [`hie_entity_infos :: NameEntityInfo`][hie_entity_infos] via HIE files.
   - As a consequence `hie_entity_infos` is only available for HIE files that were generated with GHC versions `9.12.1` or later.
   - For HIE files generated with earlier versions `hie_entity_infos` is always `mempty`.
   - It is still entirely possible to e.g. use this library with GHC version `9.10.1` and read HIE files that were generated with GHC version `9.12.2`, including inspecting the `hie_entity_infos` of such HIE files.

Note that **wired-in** names are read as **external** names.  As a consequence
you cannot access the `TyThing` for wired-in names (`wiredInNameTyThing_maybe`
always returns `Nothing`).

[hie_entity_infos]: https://hackage-content.haskell.org/package/ghc-hie-0.0.2/docs/GHC-Iface-Ext-Types.html#v:hie_entity_infos
