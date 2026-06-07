# mBatchNet Revision Issue Matrix

Working principle: revise the paper around a coherent Application Note story, not around a visible reviewer checklist. Main-text additions should grow naturally from the product narrative. Reviewer-specific clarifications can live in the response letter, supplementary material, or website documentation.

Core revised claim:

> mBatchNet operationalizes established microbiome batch-effect methods in a web-server workflow with input validation, study-design diagnostics, interpretable benchmarking summaries, and reproducible output records.

## Placement Rules

- **Main text**: only claims that support the Application Note story: workflow, safeguards, guidance, case-study interpretation, availability.
- **Supplement**: technical depth, benchmark tables, method assumptions, diagnostic definitions, validation policy, full compatible-method metrics.
- **Website/server**: concrete user-facing functionality reviewers asked for.
- **Response only**: reviewer-specific explanations, scope limits, and comparisons that would make the paper awkward or defensive.
- **Main-text admission test**: add an item to the manuscript only when it improves the reader's understanding of the mBatchNet workflow or case-study logic. Do not insert reviewer-answer fragments that read like patches.
- **App-first items**: user-facing controls, tooltips, validation behavior, runtime guards, output summaries, reproducibility bundles, and detailed interpretation help should primarily live in the app and be cited only briefly in the manuscript if they support the narrative.
- **Response-only comparisons**: BatchServer/MBECS positioning should be handled in the response unless a single natural sentence is needed in the Introduction. Do not force a comparison table or standalone competitor paragraph into the main paper or supplement.

## Editorial and Submission Requirements

These requirements come from the 2026-05-20 Bioinformatics major-revision decision email and current Bioinformatics/OUP author instructions.

- Revision timeline: major revisions are requested ideally within one month of the decision email, and ScholarOne allows revision submission within 90 days of the original decision date.
- Response file: summarize changes for the editor, including changes made and any requested changes not made with reasons. This can be uploaded as a Response to Reviewers file or entered in Author Centre.
- Main revised manuscript markup: mark changes using track changes or red text. For the LaTeX route, use red text in the revised manuscript PDF/source.
- Revised manuscript file route: submit either a `.doc`/`.rtf` file with tables, figures, schemes, and equations inserted, or all LaTeX files required by the typesetter, including class/style, bibliography, `.bst`, figure/source assets, and a complete manuscript PDF.
- Template/class check: the decision email specifically names `bioinfo.cls`, while the current local manuscript uses `oup-authoring-template.cls`. Confirm the accepted Bioinformatics LaTeX class/template route before final submission rather than silently switching templates.
- Supplement final format: upload final supplementary materials **without any changes marked**. The final supplement should be PDF or Word, not LaTeX. Red-highlighted supplement source can be used only as a working draft if needed; the submitted supplement must be clean.
- Page-limit discipline: Bioinformatics strongly discourages exceeding recommended page limits; manuscripts exceeding the limit by 20% or more may be returned immediately. For an Application Note, keep the main paper within the 4-page / approximately 2,600-word expectation.
- Code archive DOI: before final submission, archive the exact revised code snapshot on Zenodo, Figshare, Software Heritage, CRAN, or Bioconductor, and include the archival DOI in the Availability statement along with the code repository.
- Open access/APC: accepted manuscripts publish under an open-access license and are subject to APC handling.
- Conflict of interest: corresponding author must confirm conflicts for all authors through the submission process; if published, the conflict statement appears in the paper.
- Figure accessibility and quality: main-article figures should be embedded, have white background, readable labels, and include `Alt text:` descriptions under figure legends.

## Issue Matrix

| ID | Reviewer concern | Decision | Natural home | Implementation / evidence | Notes for response |
|---|---|---|---|---|---|
| R01 | The manuscript may imply a new batch-correction algorithm. | Must clarify. | Main text + response | Rewrite Summary/Introduction/Conclusion around web-server integration, diagnostics, reproducibility, and decision support. | State explicitly that no new statistical correction algorithm is claimed because the manuscript is an Application Note. |
| R02 | Paper should not become a review article or broad method landscape. | Resist scope expansion. | Response only; minimal main-text transition | Keep only a concise existing-resource paragraph. | Explain page-limit and article-type constraints. |
| R03 | BatchServer / MBECS distinction is unclear. | Address without disrupting paper. | Response only unless a single natural introduction sentence is useful | Response: direct comparison bullets. Main text: avoid standalone competitor discussion unless it improves the narrative. | No comparison table in main paper or supplement. Keep paper centered on mBatchNet. |
| R04 | Method applicability scenarios and parameter meanings are missing. | Must implement. | Website/server + supplement if needed | Method explanations, parameter tooltips, and optional supplementary method-assumption table. | Emphasize decision support, not universal method recommendation. Do not force detailed parameter prose into the main paper. |
| R05 | Users need guidance for choosing methods. | Must address carefully. | Website/server + response; main text only if narrative needs it | Add interpretation guidance based on batch attenuation, phenotype preservation, confounding, input compatibility, and runtime. | Avoid “best method recommender” language. Main paper should stay fluid. |
| R06 | Server computational limits, runtime, memory, and upload-size limits are unclear. | Must implement and measure. | Website/server + response; supplement if benchmark table is useful | Input dimension/file-size pre-checks; runtime/memory benchmark under stated hardware; add privacy-preserving successful-run runtime telemetry for future estimates. | Report recommended and hard limits only after measuring. Runtime history must store aggregate dimensions only, not uploaded data/IDs/names. |
| R07 | Public server needs protection from oversize input/abuse. | Must implement. | Website/server + response | Hard file-size/table-size/method-count guards; clear warning before submission; runtime estimate from baseline benchmarks and aggregate successful-run history. | Tie to public-server availability and UX. |
| R08 | Phenotype-batch confounding needs clear warning. | Must implement as advisory warning only. | Website/server + response; main text only as natural study-design safeguard wording | Extend mosaic diagnostic with Cramer's V / chi-square or Fisher-style fallback; warn on imbalance/structural confounding. | Do not block workflow by default. State this flags design risk; it cannot solve confounding. Method-specific failures remain method-stage errors. |
| R09 | NA, Inf, abnormal values, outliers, malformed files. | Must harden. | Website/server + response; supplement only for validation policy | Validate missing abundance/metadata, nonnumeric values, duplicated IDs, sample mismatch, all-zero rows/columns, extreme sparsity, library-size outliers. | mBatchNet is a correction/assessment server for valid, clean inputs, not a primary data-cleaning server. |
| R10 | Built-in imputation/listwise deletion requested. | Partially address with validation, not silent data repair. | Website/server + response | Block invalid core inputs; warn on risky but valid inputs; allow only transparent filtering where scientifically safe, such as all-zero feature removal. | No default abundance imputation. Explain compositional microbiome data risk and valid-clean input boundary. |
| R11 | Export Script requested. | Replace with web-server-centered reproducibility bundle. | Website/server + response | Add a reproducibility bundle containing original inputs, machine-readable run configuration, app/code version, validation metadata, and settings. Upload page can reload it to restore settings and rerun in mBatchNet. | Do not require/provide local execution script as the main feature. Mention GitHub has complete source and installation instructions for users who prefer local execution. |
| R12 | Output summary needed. | Must implement as separate output bundle. | Website/server + response; main text only if it fits workflow sentence | Output bundle contains corrected matrices, figures, metric tables, logs, validation report, output summary, runtime summary, and parameter manifest. | Keep output bundle separate from reproducibility bundle. |
| R13 | Case-study dataset description insufficient. | Must revise. | Main text + supplement | Add concise dataset details: source, n samples/features, target, batch, covariate, batch-target balance. | Current manuscript already has some; refine and keep concise. |
| R14 | Case-study interpretation not deep enough. | Must revise. | Main text + supplement | Tie each figure/metric category to trade-off interpretation. | Keep main narrative fluid; details in supplement. |
| R15 | Corrected effect sizes and p-values needed. | Must generate for displayed case-study methods. | Figure/caption + main text + response | Add ANOSIM/PERMANOVA R/R2 and p-values for uncorrected and displayed corrected outputs; use figure annotations where layout allows and caption/text for key values. | Avoid overcrowding Figure 1; caption can carry concise numerical notes. |
| R16 | More numerical/statistical evidence in figures. | Address within page limit. | Figure/caption + main text | Add compact p-value/effect-size annotation in available panel space where feasible; discuss key values in Results. | Keep figure readable. |
| R17 | Website supports more correction methods than main text evaluates. | Explain selection logic rather than over-expanding figure. | Main text briefly + response | State the AD case study focuses on representative methods explicitly designed for microbiome batch-effect correction; server supports additional general-purpose methods. | Response carries the detailed explanation. Do not force all methods into main figure. |
| R18 | Shotgun metagenomics applicability. | Clarify conservatively and smoke-test format. | Main text limitation + website note + response | State server accepts compatible feature-abundance tables from 16S or shotgun-derived taxonomic/functional profiles after upstream profiling; add dummy shotgun-derived table smoke test during server revision. | Do not claim full shotgun biological validation. State raw reads are out of scope. |
| R19 | Explain sparsity, compositionality, zero inflation. | Revise concise explanation. | Main text + supplement | Add 1-2 natural sentences in Introduction; expanded note in supplement. | Keep readable for Application Note audience. |
| R20 | Explain Aitchison distance and Bray-Curtis use. | Revise. | Main text briefly + supplement | Define centered log-ratio (CLR), Aitchison, and relative-abundance Bray-Curtis in workflow terms. | Avoid long methods lecture in main text. |
| R21 | Explain diagnostic categories and interpretation. | Revise. | Main text briefly + supplement + website help | Group diagnostics by purpose: ordination/distance tests, variance attribution, neighborhood mixing/separation. | This strengthens product guidance. |
| R22 | Terminology confusion around variables/parameters/covariates. | Fix globally. | Main text + website + response | Use “metadata variables,” “target,” “batch,” “covariates,” “method parameters” consistently. | Straightforward language polish. |
| R23 | Minor wording and formatting: source code grammar, URL consistency, CLR definition, MBECS capitalization. | Fix globally. | Main text + supplement + website | Use canonical URL `https://mbatchnet.com/` everywhere; fix grammar/capitalization and first-use definitions. | Also include code archive DOI after deposit. |
| R24 | Code archival DOI required/recommended. | Must complete before final submission. | Availability/Data availability + response | Archive exact revised code snapshot in Zenodo/Figshare/Software Heritage and cite DOI plus GitHub. | Do after code changes are stable. |

## Execution Order

1. Lock scope from this matrix.
2. Implement server-visible reviewer-risk reduction:
   - validation/pre-checks,
   - confounding diagnostic,
   - runtime estimator and aggregate runtime telemetry,
   - method guide/parameter help,
   - output bundle,
   - reproducibility bundle reload.
3. Generate evidence:
   - runtime/memory benchmark,
   - AD displayed-method metrics,
   - p-values/effect sizes,
   - dummy shotgun-format smoke test.
4. Revise manuscript and supplementary materials.
5. Fill response-to-reviewers placeholders with exact locations, figures/tables, and DOI.

## Current Implementation Status

Updated 2026-06-07 after restoring the Dash app as the active UI and applying the server-side reviewer-facing revisions.

| ID | Current status | Implemented code / artifact | Remaining work |
|---|---|---|---|
| R01 | Pending outside code | Scope decision locked in this matrix. | Manuscript Summary/Introduction/Conclusion wording. |
| R02 | Pending outside code | Scope decision locked in this matrix. | Response-letter framing and manuscript restraint. |
| R03 | Pending outside code | Canonical URL decision locked as `https://mbatchnet.com/`. | Manuscript/response positioning versus BatchServer/MBECS. |
| R04 | Implemented in server with final UI adjustment | Dash correction table now has `Config` controls with `?` tooltips and an `Explanation` expansion using method/package/citation/reference fields. A separate `Method guide` card was removed by final UI decision. | Supplementary method-assumption table, if still needed for manuscript package. |
| R05 | Partially implemented | Website/help text now gives neutral assessment interpretation and avoids subjective recommendation language. | Manuscript/supplement interpretation guidance. |
| R06 | Partially implemented | Public upload limits and runtime summaries are implemented; `Time (s)` is displayed from current-session run logs/session summary. | Baseline runtime/memory benchmark table and aggregate successful-run telemetry/estimator, if still desired. |
| R07 | Implemented for current public server | Hard public upload limits are in the app; download callbacks require explicit clicks. | Optional runtime estimator polish after benchmark evidence. |
| R08 | Implemented | Upload validation warns on batch-target imbalance/confounding risk. | Manuscript/supplement wording that this is an advisory design-risk warning, not a correction for confounding. |
| R09 | Implemented | Upload validation covers malformed tables, row mismatch, nonnumeric values, NA/Inf, all-zero samples/features, high sparsity/negative values, dimensions, and file-size limits. | Manuscript/supplement validation-policy description. |
| R10 | Implemented as validation-only policy | Invalid core inputs are blocked; risky valid inputs produce warnings; no silent imputation is added. | Response wording explaining no default abundance imputation. |
| R11 | Implemented in server | `Repro bundle` exports reloadable server-side reproducibility state; Upload page can restore `reproducibility_bundle.zip`; no local script is required as a main feature. | Response wording mentioning GitHub source/install path for local execution. |
| R12 | Implemented | `Download outputs` is separate from `Repro bundle`; output/runtime/parameter/repro manifests are generated for downloads. | Manuscript/supplement mention if space allows. |
| R13 | Pending outside code | Dataset fields can be reported from existing data once manuscript tables are revised. | AD dataset description in main text/supplement. |
| R14 | Pending outside code | Server help now has neutral diagnostic descriptions. | Case-study interpretation in manuscript/supplement. |
| R15 | Pending figure/evidence work | Server supports metric generation. | Add ANOSIM/PERMANOVA effect sizes and p-values to Figure 1/caption/text for displayed methods. |
| R16 | Pending figure/evidence work | Server supports metric generation. | Add compact numerical/statistical evidence without overcrowding Figure 1. |
| R17 | Pending manuscript/response | Server supports broader method set; method explanations are available in UI. | Explain representative microbiome-oriented method selection for AD case study. |
| R18 | Implemented for server format; manuscript pending | Upload/help text clarifies profiled feature-table input; dummy shotgun-style feature-table validation/correction smoke is covered by tests. | Manuscript limitation/response wording: feature-table compatibility, not full shotgun benchmark. |
| R19 | Partially implemented | Help/assessment text now describes sparsity/composition-related diagnostics neutrally. | Main-text/supplement explanation. |
| R20 | Partially implemented | Help text uses CLR/Aitchison and Bray-Curtis terminology. | Main-text/supplement definitions. |
| R21 | Partially implemented | Website/help groups and explains diagnostic outputs with neutral interpretation language. | Main-text/supplement diagnostic-category explanation. |
| R22 | Partially implemented | Server UI uses clearer terms: batch, target, covariates, method parameters, mappings. | Global manuscript/supplement terminology pass. |
| R23 | Partially implemented | Canonical URL decision is `https://mbatchnet.com/`; website/help text has been updated in the app where touched. | Full manuscript/supplement/reference consistency pass. |
| R24 | Pending final release | No archival DOI should be minted until revised code is stable. | Create GitHub release/tag, archive with Zenodo/Figshare/Software Heritage, update availability statement and response. |

## Main-Text Tone Guardrails

- Do not insert a table comparing competitors in the main text.
- Do not write reviewer-driven one-off sentences that interrupt the paper.
- Do not claim universal best-method recommendation.
- Do not imply mBatchNet solves phenotype-batch confounding.
- Do not imply a new statistical correction algorithm.
- Do not imply mBatchNet cleans invalid primary data; it validates valid-clean correction inputs.
- Do not imply local scripts are required for reproducibility; reproducibility is server-centered through reloadable bundles.
- Keep the main text centered on mBatchNet as a guided, reproducible, browser-based workflow.
