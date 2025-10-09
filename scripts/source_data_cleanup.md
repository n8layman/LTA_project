## App Modification Request Prompt

"I have updated the `tick_data.csv` to include separate columns for **`Transect`**, **`Type`**, **`Line`**, and **`Segment`**, derived from the original `Code` column.

The original data structure used in the server logic assumed the `Code` column was the lowest level of detail. I need to modify the `app.R` script, focusing primarily on the Mianus River Gorge logic, to leverage this new structure.

### **Required Changes to `app.R`**

1.  **Simplify `summarize_mianus_hierarchical` Logic:**

    - The existing `summarize_mianus_hierarchical` helper function (which is called in `output$mianus_map`) is currently complex, as it relies on string manipulation (like `sub`) on the `Code` column to define the `Transect_Num` and `Line` levels.
    - **Goal:** Modify the **`summarize_mianus_hierarchical`** function (in `helpers.R`) to use the new, pre-parsed columns (`Transect`, `Type`, `Line`, `Segment`) for grouping instead of string manipulation on the `Code`.

    - The new grouping logic should look like this:
      - **`Segment` Level:** Group by `Code`, `Segment`, `Line`, `Type`, `Transect` (the lowest level).
      - **`Line` Level:** Group by **`Transect`** and **`Line`** (to get summary for the line regardless of segment, e.g., MRG-1-Fen-4).
      - **`Transect` Level:** Group by **`Transect`** (to get summary for the whole transect, e.g., MRG-1).

2.  **Update Mianus Map Tooltip Generation:**

    - In the `output$mianus_map` block, the logic for calculating `t_adult/nymph/total` (Transect level) and `l_adult/nymph/total` (Line level) currently uses string variables like `transect_num` and `line_code` derived from the `SegmCode` property.
    - **Goal:** Adjust the iteration logic within `output$mianus_map` to directly use the simpler grouping variables (`Transect`, `Line`, `Segment`) that are available in the updated `summaries` (from step 1) when filtering the summary tables.

    Specifically, inside the `for` loop that iterates over `transects_data$features`:

    - The `feature$properties` now contain the parsed values (e.g., a property named `Transect`, `Line`, etc.). Assume the GeoJSON conversion script has added these as properties. The key variable is still **`segm_code`** from the GeoJSON.
    - The `segm_code` must now be mapped to the new columns.

    - **Revised Transect/Line lookup logic:**
      - **Transect level:** Lookup should match the `Transect` value from the data to the segment's **Transect ID** (e.g., `1` for MRG-1).
      - **Line level:** Lookup should match the **`Transect`** and **`Line`** values.
