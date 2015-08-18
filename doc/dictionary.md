Dictionary
==========

Icicle and Ivory will require a new dictionary input format (and new output format, but that has different requirements)
in order to support the icicle expression language.

Currently, dictionaries are line oriented, which is sub-optimal, namespaces, features, and facts are interspersed in an ad-hoc manner,
struct and value definitions are ad-hoc, (while struct ingest and output are json, which is slightly better).

Requirements
------------
 * Human readable and writable
 * Machine readable
 * Import of the standard libraries and custom libraries
 * Simple definition of concrete facts
 * Sensible namespacing of facts and features
 * Icicle feature expression support
 * Ivory's deprecated expression support

Proposal
--------

TOML is the current default human readable format at Ambiata. So let's have a look...

```TOML
title = "Demonstration dictionary"

version = 2

# The default namespace for this dictionary.
namespace="demographics"

# imports an array of icicle libs, search path of libs dir, then std libs dir.
import = [
  "prelude.icicle", # not sure about the extension yet.
  "super-customer.icicle",
]

# Optionally, import addition dictionary files, specifying sub dictionaries.
# Should help namespace separation. A Master dictionary could contain just this, title, and version.
chapter = [ ]

# Set the default tombstone and missing value indicator.
tombstone = "NA"

# definition is a fact
# namespace is demographics
# feature is age

[fact.age]
  encoding="int"
  # Override the tombstone value for this fact
  tombstone="-"

[fact.salary]
  encoding="int"
  mode="set"

# When TOML v4 is supported, in-line tables will also be possible. e.g.,
# fact.salary = { namespace="demographics", encoding="int", mode="set" }

[fact.gender]
  encoding="string"

[fact.injury]
  # Override the namespace for this feature
  namespace="injuries"
  # Struct encodings can be written in old style, or as a TOML table.
  # encoding="(location:string,severity:int,refnum:int,action:string*)"
  [fact.injury.encoding]
    location="string"
    severity="int"
    action="string*"

[feature.mean_salary]
  expression="""
    feature salary ~> sum value / count
  """

[feature.salary_standard_deviation]
  expression="""
    feature salary ~>
      let ss = (sum value) ~>
        (((sum (value * value) * count) - (ss * ss)) / (count * (count - 1)))^(0.5)
  """

[deprecated.mean_salary]
  source="salary"
  expression="mean"


```
