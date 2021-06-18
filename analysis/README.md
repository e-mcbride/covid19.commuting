
## Script naming protocol

Below is the format used:

    ##_##_descriptive-name.R

Underscores (`_`) separate the sections of the script name. Each section
of the script name has a purpose:

-   **First number:** category of work being done in the script

    | Number | Description                          |
    |:------:|--------------------------------------|
    |  `01`  | Data cleaning and preparation        |
    |  `02`  | Data analysis                        |
    |  `03`  | Data visualization                   |
    |  `9#`  | Scripts for supplementary materials. |

-   **Second number:** script run order within each category. Important
    for possible dependencies between scripts.

-   **Descriptive name:** describes what each script does in words
    separated by dashes (`-`)
