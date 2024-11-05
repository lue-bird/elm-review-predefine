# elm-review-predefine

The [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule `Review.Predefine` which reports values that are constructed lazily but could be defined at the module level to save allocations.

```bash
elm-review --template lue-bird/elm-review-predefine/preview
```

**I will never release this** because it inherently has lots of false positives, makes your code ugly and is rarely applicable in the first place. However, feel free to use the template for a quick manual review or adapt with more restrictions for when it reports.

Keep in mind:

  - certain things like functions (e.g. `Result.andThen identity`) are _perfectly fine_ to construct again and again on the fly. Since this rule neither has type information nor does any analysis, it will report stuff like that. Ignore it in that case!
  - if you use a function with arguments _exclusively in fully applied style_ and not lazily there's no issue either. This rule makes no effort to check for cases like that. Ignore it in that case!
  - the rule only operates on expression-level and therefore does not detect or suggest moving let value/function declarations
  - I did not write tests for this because it's a semi-throwaway rule and works fine for my purpose. But feel free to add some or report bugs


## configure
After copying the source file over to your review config, add

```elm
module ReviewConfig exposing (config)

import Review.Predefine
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ Review.Predefine.rule
    ]
```
