# Audit diagrams in TeX PDFs

Treat compilation, visual geometry, and semantic correctness as three independent gates. A clean Tectonic log cannot detect a wrong arrow target, reversed flow, drifting labels, overlapping annotations, unreadable edge labels, or a note pointing at the wrong region.

## Build an inventory

Before claiming completion:

1. Enumerate every `figure`, `tikzpicture`, included graphic, chart, and diagram-like table in the TeX tree.
2. Record each visual's label/caption, final PDF page, purpose, and authoritative source for its semantics.
3. Track separate results for source semantics, rendered geometry, and final post-fix inspection.
4. Reconcile the inventory count with the final PDF. Do not let migrated floats, duplicate labels, or missing includes silently change the set.

Use representative pages only for ordinary prose. Inspect every diagram and graph individually, even in a long document. A montage or reduced-size grayscale sheet may help triage, but it is never the sole inspection surface.

## Audit meaning before appearance

For each visual:

- Verify that every node, state, component, range, count, and ordering matches the source data or implementation.
- Trace every arrow from source to destination. Confirm its direction, endpoint, label, and meaning against the caption and prose; do not infer correctness from a plausible-looking layout.
- Confirm that pointer annotations land on the named object, not merely near it.
- Check that repeated rows, addresses, bit ranges, timelines, and proportional widths remain aligned from first item to last.
- Verify that captions and legends describe what is drawn. Treat a mismatch as a semantic defect even when the page looks polished.
- Re-evaluate any in-document audit/status table after inspecting the final render. Never mark a visual `pass` from source review, semantic assertions, or compiler output alone.
- For generated or data-backed visuals, add focused assertions for totals, membership, edge sets, ranges, or sample traces when feasible. Automated semantic checks complement visual inspection; they do not replace it.

## Construct robust geometry

- Name nodes and attach arrows/labels to node anchors or borders.
- Prefer relative positioning, matrices, chains, and `fit` nodes. Avoid raw polar angles and hand-computed centres when the target already has a node.
- Never place a series of boxes relatively while placing its labels with independent absolute coordinates; small spacing differences accumulate into visible drift.
- Route return edges, self-loops, and bidirectional flows explicitly. Reject zero-length legs, dangling arrowheads, arrows terminating under labels, or edges that cross unrelated labels.
- Keep annotations within their panel and the text block. Include arrowheads and curved control points when checking bounds.
- Chain variable-width cells from their actual west/east anchors instead of assuming text widths.
- If edge labels collide or require tiny type, split the diagram, layer it, replace a dense subgraph with state cards or a table, or move detail into a companion figure. Do not solve structural congestion with global scaling alone.

## Inspect the final render

Open the final PDF render—not only the TeX/SVG source and not a pre-fix PDF—and check each visual at normal reading scale and at a zoom sufficient to inspect edge endpoints.

Check all of the following:

- text or arrowheads outside the page, panel, node, or crop box;
- labels touching borders, other labels, lifelines, nodes, or arrows;
- clipped legends, edge labels, annotations, and cell contents;
- arrowheads aimed at the wrong node, wrong side, or empty space;
- reversed directions, ambiguous bidirectional flows, and malformed self-loops;
- edge crossings that obscure topology or make labels attach to the wrong edge;
- inconsistent alignment, spacing, baselines, widths, and accumulated row drift;
- unreadable type at the PDF's normal page scale;
- colors or line styles that lose meaning in grayscale or for common color-vision deficiencies;
- figures floating before their introduction, stranded headings, separated captions, and unexpected blank space.

Inspect every page in a short document. In a long document, inspect every visual page plus the cover, contents, densest table/prose pages, and final page. Open each visual individually; contact sheets are navigation aids only.

## Close the audit loop

1. Fix the source by anchoring or redesigning the visual.
2. Recompile with the bundled checker.
3. Confirm the final PDF changed and reopen every changed visual.
4. Re-run semantic assertions and recount the inventory.
5. If a shared TikZ style, font, page geometry, or layout macro changed, reopen every visual it can affect.
6. Report the exact number of figures/diagrams and pages inspected, the semantic checks performed, and any residual limitation.

Never use “compiles cleanly,” “the preview passes,” or “representative pages look correct” as a synonym for a completed diagram audit.
