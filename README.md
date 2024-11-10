# elm-review-predefine

The [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule `Review.Predefine` which reports values that are constructed lazily but could be defined at the module level to save calculations and allocations.

```bash
elm-review --template lue-bird/elm-review-predefine/preview
```

**This will not be released** because it inherently has lots of false positives, makes your code ugly and is rarely necessary in the first place. However, feel free to use the template for a quick manual review or adapt with more restrictions for when it reports.
Keep in mind:

  - if you use a function with arguments _exclusively in fully applied style_ and not lazily, pre-defining has no benefit. This rule makes no effort to check for cases like that. Ignore it in that case!
  - certain values like simple calculations _might be fine_ to construct again and again on the fly. Ignore it in that case!
  - the rule does not detect or suggest moving let value/function declarations. For that, use [`NoPrematureLetComputation`](https://dark.elm.dmy.fr/packages/jfmengels/elm-review-common/latest/NoPrematureLetComputation)

## add to your project
Copy `src/Review/Predefine.elm` over to your `review/src/` and add

```elm
module ReviewConfig exposing (config)

import Review.Predefine
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ Review.Predefine.rule
    ]
```
