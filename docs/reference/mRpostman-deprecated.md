# Deprecated functions and methods

As of the 2026 refactoring (3.0.0), the expression-based
[`ImapCon$query()`](#method-query) is the canonical search interface,
and criteria combine with R's own operators (`&`, `|`, `!`). The forms
below keep working but signal a deprecation warning once per session:

  - the `search_*()` method family - each call has a direct `query()`
    spelling, e.g. `search_before("02-Jan-2020")` is `query(date <
    "2020-01-02")`;

  - `AND(...)` and `OR(...)` - write `crit1 & crit2` and `crit1 | crit2`
    instead.
