# odeqcdr
 ODEQ Continous Data Review

A R package that provides QA/QC support for review of continuous data entered into the Oregon Department of Environmental Quality's Continuous Water Quality Data Template v2.03.

## Install

```R
devtools::install_github('DEQrmichie/odeqcdr', host = 'https://api.github.com', 
                         force = TRUE, upgrade='never')
```

## General Workflow

This outlines the general workflow using the odeqcdr functions. Not all steps are included.

See `example.R` for a specfic example.


```R
#- Import the xlsx template -
odeqcdr::contin_import()

#- Completeness Pre checks -
odeqcdr::pre_checks()

#- Datetime Checks

# timezone check

# Daylight savings check
odeqcdr::dst_check()

# Make a datetime
odeqcdr::dt_combine()

# Apply datetime corrections and make comments
odeqcdr::dt_parts()

#- Unit Conversion -

#- DQL grading -

odeqcdr::dql_accuracy()

odeqcdr::dql_precision()

# Final DQL

# Flag potential anomalies
odeqcdr::anomaly_check()

# Launch Shiny app for further review.
odeqcdr::launch_shiny()

#- Make DQL, Result Status, and Result Comment edits -

#- Export back to xlsx -
odeqcdr::contin_export()

```
