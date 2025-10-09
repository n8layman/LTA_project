### Summary – Problems Fixed in Mianus River Gorge GeoJSON

1. **Duplicate Segment Codes**

   - Multiple features shared the same `SegmCode` (e.g., all labeled “A”).
   - Fixed by assigning correct sequential letters (A–F, etc.) based on feature order.
   - **Affected lines:** MRG-1-Fen-4, MRG-1-Fen-3, MRG-2-Fen-0.
   - Note: Incorrect spatial order (Issue 4) was likely a downstream effect of these duplicates.

2. **Remaining Duplicate Labels**

   - Additional mislabeled features found after the first pass.
   - Corrected specific feature indices for **MRG-1-Fen-3** and **MRG-2-Fen-0**.

3. **Final Duplicate Cleanup**

   - Two remaining mislabeled segments in **MRG-2-Fen-0** fixed (renamed to **H** and **I**).
   - Result: No remaining duplicate `SegmCode` values.

4. **Incorrect Spatial Order (Result of Duplicates)**

   - Some segment labels (A–F) were out of geographic order due to earlier duplicate labeling.
   - Reordered features by longitude and reassigned letters to restore correct west–east sequencing.
   - **Affected lines:** MRG-1-Fen-3 and MRG-1-Fen-4.

5. **Missing Segments in GIS Data**

   - Tick data referenced segments (e.g., **G**, **H**, and **Trailside**) not present in the shapefile.
   - Added one missing feature (`MRG-1-Fen-2-G`) and reassigned it to segment **F**.
   - Remaining unmapped segments documented for future GIS updates.

6. **Verification and Validation**
   - Confirmed all duplicates resolved, spatial order corrected, and transect hierarchy consistent.
   - GeoJSON feature count increased from **141 → 142** after fixes.

---

**Summary of Fixes:**

- 19 duplicate labels corrected
- 12 segments spatially reordered (a result of duplicate corrections)
- 1 missing segment added and reassigned
