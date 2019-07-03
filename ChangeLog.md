# Changelog for Functive

Terminology:
- _stable_: implemented in master branch
- _unstable_: implemented in dev branch
- _planned_: described and hopfully prototyped in a sub-dev branch
- _idea_: only described

## Stable Features

## Unstable Features

## Planned Features

## Ideas for Features

- remove primitive types, so that everything is described functionally. this focusses the language on Church encodings in a unique way, because symbolic variables are implicitly assumed. For example, the Church booleans `true := fun x y = x` and `false := fun x y = y` can be tested by applying to symbolic terms, e.g. `true_val' := true true_val false_val` to see if  indeed`true_check = true_val`.
