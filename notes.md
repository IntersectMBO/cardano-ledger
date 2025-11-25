# Points to remember in everything you do for the typeclass-based implementation of huddle specifications to auto-generate .cddl files.

1. Never introduce comments of your liking without confirming with me first.
2. Never add haddocks without confirming with me first.
3. Never delete any files without confirming with me first.
4. Never delete any existing huddle comments from the older testlib CDDL modules without confirming with me first.
5. All instances are type-applied to the era to which the module belongs, and no other era.
6. All smart-constructors are completely polymorphic over era and always reside in the era that introduced their concept.
7. All smart-constructors should in-turn call huddleRule of the children rules and never call another smart-constructor directly, this way the correct instance should be picked for the constituents.
8. All smart-constructors should be placed first in the source file and instances should be last.
9. All huddle comments that are field-specific should be converted into field-comments and only those must be removed from the rule-level comment. Rule-level comments that are truly rule-level must remain as they are.
10. There should be no superfluous exports that are never used anywhere else, in any modules other.
11. The import chain is always: core -> shelley -> allegra -> mary -> alonzo -> babbage -> conway -> dijkstra. Each era should only need to import from its immediate predecessor and not from any other module. Each module should reexport its predecessor module.
12. If a module needs to redefine something introduced in its non-immediate predecessor, we just hide that in the imports list to redefine and export this new definition for later eras to use.
13. In your verification step while checking the diff for the auto-generated .cddl file, make sure that the difference is only and only in comments if at all, and not in the rule definitions.
