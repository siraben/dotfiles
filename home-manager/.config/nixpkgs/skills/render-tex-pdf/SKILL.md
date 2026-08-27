---
name: render-tex-pdf
description: Create or revise polished PDF documents from source material using LaTeX/TeX and Tectonic, usually through a Nix environment. Use for requests to turn notes, research, analyses, incident timelines, technical explanations, or existing documents into a `.tex` source and rendered `.pdf`; to add tables, diagrams, or graphs to a report; or to audit, rebuild, and visually verify an existing TeX/PDF deliverable, especially figure, TikZ, arrow, clipping, overlap, or diagram-layout correctness.
---

# Render a TeX PDF

Produce both an editable TeX source and a verified PDF. Treat the PDF as the deliverable and the TeX as its reproducible source.

## Establish the deliverable

1. Read the supplied material, nearby project instructions, and any existing report/build conventions.
2. Honor an explicit output directory. Otherwise, prefer an existing project `reports/` or `doc/` directory; if none exists, use `~/doc`.
3. Choose a descriptive hyphenated basename. Keep the `.tex`, `.pdf`, and retained `.log` together unless the project already specifies a build directory.
4. Infer audience, scope, and document length from the request and evidence. Ask only when a missing choice would materially change the result.
5. Preserve sources and unrelated files. When revising an existing document, inspect the current TeX and generated assets before editing.

## Research and organize

- Ground claims in the supplied files and, when requested or necessary, authoritative external sources. Cite sources close to the claims they support.
- Clearly separate observed facts, calculations, assumptions, and inference—especially in forensic, financial, or technical reports.
- Build a coherent narrative rather than pasting raw findings. Put the conclusion or executive summary first, then evidence, methods, limitations, and reproducibility details as appropriate.
- Generate calculations, tables, and charts from source data when feasible. Keep the generator or derived data beside the report when reproducibility matters.
- Use a table for repeated exact mappings or comparisons; a flow/timeline for sequences; a diagram for architecture or hierarchy; and a graph only when it communicates a real quantitative relationship.

## Author the TeX

- If the document contains diagrams or graphs, read [references/diagram-audit.md](references/diagram-audit.md) before authoring or revising them and follow its complete audit protocol.
- Create a self-contained main `.tex` file unless the project already uses a multi-file structure.
- Prefer a restrained modern style: readable typography, consistent spacing, a small color palette, clear hierarchy, and generous margins. Use `booktabs` and `tabularx`/`longtable` for tables, `hyperref` for links, and `microtype` when supported.
- Use TikZ for compact code-native diagrams. Use generated PDF/PNG figures for data plots. Give every figure and table a useful caption and reference it from the prose.
- Anchor labels and arrows to named nodes. Prefer relative positioning and node borders over raw coordinates, polar angles, or separately computed label positions. Never mix relatively placed rows with absolute label coordinates.
- Escape TeX-sensitive text and paths. Avoid fragile macros, digit-bearing command names, forced `[h]` floats, manual line breaks used as layout patches, and packages unsupported by the available Tectonic bundle.
- Keep dense tables and diagrams legible at normal page scale. Redesign or split a dense graph instead of merely shrinking it; prefer landscape pages, long tables, layered diagrams, or state cards over tiny type and crossing edges.
- Include provenance, data dates, units, formulas, important assumptions, and rebuild instructions when they affect interpretation.

## Compile and repair

Run the bundled checker from the skill directory:

```sh
scripts/compile_and_check.sh /absolute/path/to/report.tex
```

The script compiles from the TeX file's directory, preferring an existing `tectonic`, then the nearest ancestor Nix flake, then `nix shell nixpkgs#tectonic`. It requires a nonempty PDF, rejects TeX/layout/reference warnings, checks PDF metadata, and verifies that text can be extracted. These checks prove build integrity, not visual or semantic diagram correctness.

Fix every compilation error and warning instead of merely reporting it. Re-run the checker after each material layout change. Do not claim success from a stale PDF or a failed log.

## Verify the rendered document

After the automated check:

1. Confirm expected headings, numbers, and captions in `pdftotext` output.
2. Render pages to images with `pdftoppm`, `mutool draw`, or the available PDF viewer.
3. Inventory every figure, diagram, and graph. Inspect every final rendered instance individually at normal reading scale; never substitute one example per style, a contact sheet, a thumbnail montage, source inspection, or a clean log for this pass.
4. Check semantics separately from geometry. Trace every arrow direction, endpoint, label, state transition, pointer, range, and annotation back to the source data/code and the caption. Then check clipping, overlap, crossings, containment, alignment, accumulated coordinate drift, malformed loops, label readability, and float placement.
5. Inspect every page for a short document. For a long document, inspect every page containing a visual plus the cover/title page, table of contents if present, densest prose/table pages, and final page.
6. Correct defects by re-layout or simplification, not indiscriminate scaling. Recompile, then reopen every changed visual from the final PDF. If a shared style or layout macro changed, reopen all affected visuals.

Do not claim that a document or its diagrams were visually audited unless the final post-fix render was actually opened and checked. Report how many visuals and pages were inspected.

For sensitive reports, preserve restrictive existing permissions or set the source, PDF, and log to mode `0600`. Do not weaken directory or artifact privacy just to simplify rendering.

## Hand off

Lead with completion. Link both the `.pdf` and `.tex` using absolute paths. State the page count and the checks actually performed. Mention important assumptions or remaining limitations, but do not bury the artifact paths.
