# Response to Reviewers

Manuscript ID: BIOINF-2026-0942

Title: mBatchNet: an interactive web server for diagnosis, correction and benchmarking of batch effects in microbiome data

Dear Editor and Reviewers,

We thank the Associate Editor and reviewers for the constructive comments. We have revised the manuscript, supplementary information, and web server to improve the clarity of the Application Note, strengthen user guidance, and provide additional quantitative evidence for the case study. Changes in the revised manuscript are marked in red text. The supplementary material is prepared as a clean file for final submission, as requested by the journal.

Major changes include:

- We clarified that mBatchNet integrates established batch-correction methods in a unified web-server workflow and does not claim a new statistical correction algorithm.
- We clarified the data scope: the case study and manuscript-level validation use a 16S rRNA OTU table, while the server accepts compatible preprocessed microbiome feature tables, including shotgun-derived taxonomic or functional profiles after upstream profiling. We also state that raw-read processing, taxonomic profiling, assembly, and functional annotation are outside the scope of mBatchNet.
- We revised the Methods/Features text to describe all 12 correction methods currently supported by the server and to group them by practical implementation category.
- We expanded the explanation of diagnostic categories, including ordination, distance-matrix tests, variance partitioning, and neighborhood or embedding-based metrics.
- We added concise definitions of CLR/Aitchison and Bray-Curtis analyses in workflow terms, including zero handling before CLR transformation.
- We expanded the anaerobic digestion case-study description, including sample/feature counts, target variable, batch variable, covariate, and batch-target balance.
- We added numerical ANOSIM/PERMANOVA effect sizes and p-values for the displayed case-study outputs in the Results and Supplementary Table S1.
- We strengthened server-side validation and user-facing guidance for upload limits, malformed inputs, missing/invalid values, all-zero samples/features, high sparsity warnings, and batch-target imbalance warnings. The public-server limits are now documented as 10 MB per CSV file, no more than 1,000 samples, 1,000 features, 1,000,000 matrix cells, and five metadata columns.
- We added method explanations and parameter help in the web interface, as well as separate output and reproducibility bundles. The manuscript/supplement now anchors the corresponding server outputs, including validation_report.json, output_summary.json, runtime_summary.json, parameter_manifest.json, and reproducibility_manifest.json.
- We added a supplementary method-coverage table listing all 12 supported correction methods and indicating which methods are shown in the compact case-study figure or supplementary statistics.
- We standardized terminology across the manuscript and interface, using "metadata variables", "target", "batch", "covariates", and "method parameters" consistently.
- We standardized the public URL as https://mbatchnet.com/ and corrected minor wording/capitalization issues.

One remaining item before final resubmission is to insert measured runtime and peak-memory values into the response letter and supplement. We will complete these measurements under a stated hardware/software environment before submission rather than reporting unmeasured estimates.

Before final resubmission, we will archive the exact revised code snapshot and insert the archival DOI in the Availability statement as requested by Bioinformatics. Placeholder: [ARCHIVAL DOI TO BE INSERTED].

## Reviewer 1

### Major comment 1

Reviewer comment: The reviewer suggested adding a clear statement about computational capabilities, upload limits, runtime estimates, and input size pre-checks, especially for computationally intensive methods such as ConQuR.

Response: We agree. We have revised the server to make public-server limits explicit and to validate uploaded files before preprocessing or method execution. The upload page now states file-size, sample, feature, matrix-cell, and metadata-column limits. Uploaded matrices and metadata are checked before analysis so oversized or malformed inputs fail early with an informative message. The correction page also reports elapsed time from the current session and displays available average elapsed-time information from previous successful runs. In the manuscript, we now describe the session-scoped workflow, logs, downloadable outputs, and method-dependent runtime behavior more clearly.

The current public-server limits are 10 MB per CSV file, no more than 1,000 samples, 1,000 features, or 1,000,000 matrix cells, and no more than five metadata columns. These limits are written to validation_report.json together with the uploaded matrix dimensions. In response to the request for measured performance, we will add a compact supplementary performance table with the bundled example dataset and two representative larger matrices, reporting runtime and peak memory under the final benchmark environment. We will not rely only on the website runtime display in the final resubmission.

Changes made: Upload limits and validation are implemented in the web server; runtime information is displayed in the correction interface; validation_report.json and runtime_summary.json are included in downloadable outputs; manuscript and supplement text now document the server limits and runtime/output summaries. Before final submission, measured runtime and peak-memory values will be inserted into the performance table.

### Major comment 2

Reviewer comment: Phenotype-batch confounding can be serious when phenotype and batch are correlated; users should receive a clear heads-up when they are confounded.

Response: We agree. We added an upload-stage study-design warning that flags strong association between the selected batch and target metadata variables. We kept this as an advisory warning rather than a hard stop, because imbalance/confounding is a study-design issue and cannot be solved automatically by a correction server. The server also produces a batch-versus-target mosaic plot so users can inspect this structure before correction.

Changes made: The upload module now reports batch-target association warnings; the manuscript and supplement now describe the mosaic plot and case-study batch-target balance.

### Minor comment 1

Reviewer comment: The abstract should say "source code is available" rather than "source code are available."

Response: Corrected.

Changes made: The Availability and implementation statement now uses "Source code is available".

### Minor comment 2

Reviewer comment: The manuscript used both https://www.mbatchnet.com and https://mbatchnet.com; this should be consistent.

Response: Corrected. We use https://mbatchnet.com/ consistently.

Changes made: The URL was standardized in the manuscript and related documentation.

## Reviewer 2

### Major comment 1

Reviewer comment: The website/tutorial should outline applicable scenarios for each method and explain adjustable parameters and their impact.

Response: We agree that users need practical method and parameter guidance. We expanded the correction interface so each method includes an explanation panel with citation/source information and a concise description. Method-specific configurable parameters now include help text explaining the meaning of the parameter and how it affects the run. We also revised the manuscript and Supplementary Note 1 to group all supported methods by practical implementation category.

Changes made: The web interface now includes method explanations and parameter help. The main text now lists all 12 supported correction methods and classifies them as microbiome-oriented or general-purpose/expression-derived frameworks. Supplementary Note 1 provides additional category mapping by input scale and modeling family.

### Major comment 2

Reviewer comment: The example dataset is clean, whereas real-world microbiome data may contain missing values or outliers; the server should include robust handling or clear error prompts.

Response: We agree that robust validation and clear prompts are important. We revised the upload workflow to validate uploaded data before preprocessing. The server now blocks invalid core inputs, including missing upload files, malformed numeric matrices, blank/non-numeric/NA/NaN/Inf matrix values, metadata row-count mismatch, missing selected batch or target variables, insufficient batch or target levels, identical batch and target selections, all-zero sample rows, and matrices exceeding public-server limits. It also reports warnings for large uploads, all-zero feature columns, high sparsity, negative or transformed-looking values, and strong batch-target association.

We did not add silent imputation or automatic outlier removal. mBatchNet is intended for processed microbiome count or abundance tables with matching metadata, not as a primary data-cleaning server. Automatic imputation or outlier removal could change compositional structure and downstream correction behavior without the user's explicit scientific decision.

Changes made: Input validation was hardened in the web server. Supplementary Note 2 now explains the validation policy and data-cleaning boundary.

### Minor comment

Reviewer comment: The website should provide an "Export Script" function to reproduce the selected workflow locally.

Response: We agree with the goal of reproducibility. Instead of generating a standalone script that may not reproduce the exact server environment, we added a server-centered reproducibility bundle. The bundle records session inputs, selected metadata variables, method parameters, validation metadata, run metadata, and session state, and can be uploaded back to mBatchNet to restore and rerun the analysis. For users who prefer local execution, the full source code remains available through GitHub.

Changes made: The web server now provides separate "Download outputs" and "Repro bundle" actions. The output bundle contains corrected matrices, diagnostic figures, metric tables, logs, validation_report.json, output_summary.json, runtime_summary.json, parameter_manifest.json, and reproducibility_manifest.json. The upload page can restore a previously exported reproducibility_bundle.zip.

## Reviewer 3

### General comment

Reviewer comment: If positioned as a review article, the Introduction should discuss the landscape of batch-correction methods and mechanisms; additional datasets may be needed.

Response: We have revised the manuscript to clarify that this is an Application Note describing a web server, not a review article. The Introduction now gives a concise motivation for representative microbiome-oriented methods and existing resources, while the Features/Implementation section lists all supported correction methods and explains their practical grouping. We avoided expanding the manuscript into a broad method review in order to stay within the Application Note format and page limit.

Changes made: The Summary, Introduction, Features/Implementation, and Conclusion were revised to emphasize web-server integration, standardized diagnostics, reproducible outputs, and user guidance.

### Major comment 1

Reviewer comment: The anaerobic digestion 16S rRNA dataset should be described clearly within the article.

Response: We agree. We expanded the case-study description in the main text and supplement.

Changes made: The Results now state that the dataset contains 231 OTUs across 75 samples, identifies the target variable as initial phenol concentration, identifies the batch variable as five processing dates, reports target group sizes and batch-size range, and notes that treatment duration was used as an optional covariate where supported. Supplementary Figure S1 further summarizes batch-target composition.

### Major comment 2

Reviewer comment: As an Application Note, the article is acceptable, but it does not provide a novel algorithm comparable to existing methods such as ConQuR, MMUPHin, or PLSDA-batch.

Response: We agree that mBatchNet does not claim to introduce a new statistical batch-correction algorithm. We revised the manuscript to avoid implying methodological novelty at the algorithm level. The contribution is an accessible web server that integrates established correction methods with a consistent upload, correction, assessment, visualization, and export workflow.

Changes made: The Summary, Introduction, and Conclusion now emphasize integration, benchmarking, and reproducible workflow rather than a new correction algorithm.

### Minor comment

Reviewer comment: Some terminology is unclear, for example "variables" in software development versus computational biology usage.

Response: We agree. We standardized terminology throughout the revised manuscript and interface.

Changes made: We now use "metadata variables", "target", "batch", "covariates", and "method parameters" consistently.

## Reviewer 4

### General comment 1

Reviewer comment: The server should offer guidance or recommendations for choosing suitable methods or models.

Response: We agree that users need guidance, but we avoided adding a universal "best method" recommendation because the appropriate correction depends on data type, study design, method assumptions, covariate availability, and the desired balance between batch attenuation and target preservation. Instead, we added neutral guidance that helps users interpret method outputs and check method compatibility.

Changes made: The correction interface includes method explanations and parameter help. The manuscript and Supplementary Note 1 now describe method categories and diagnostic categories, and the Results explain how the case-study outputs should be interpreted jointly rather than as a single ranking.

### General comment 2

Reviewer comment: Case-study interpretations should link general metric definitions to the actual figure results.

Response: We agree. We revised the Results to connect each metric family to the case-study interpretation.

Changes made: The Results now explicitly links ordination, distance-based statistics, variance partitioning, and neighborhood metrics to residual batch separation, phenotype separation, variance attribution, and local cross-batch mixing. Supplementary Note 1 expands these interpretation principles.

### General comment 3

Reviewer comment: More numerical and statistical evidence should be included, including corrected effect sizes and p-values.

Response: We agree. We regenerated and added ANOSIM and PERMANOVA statistics for the uncorrected data and the displayed corrected outputs.

Changes made: The Results now reports Bray-Curtis ANOSIM R and p-values and PERMANOVA R^2 and p-values for the uncorrected and displayed corrected outputs. Supplementary Table S1 provides the full table of these statistics. Figure 1B caption now points readers to the numerical statistics in the Results and Supplementary Table S1.

### General comment 4

Reviewer comment: The manuscript should clarify the advantages of mBatchNet over BatchServer.

Response: We revised the Introduction to clarify the relationship among existing resources without adding a disruptive comparison table. BatchServer is a general omics web interface centered on ComBat-based correction and a limited diagnostic set, whereas MBECS provides R-package functionality for microbiome correction and evaluation. mBatchNet specifically targets microbiome abundance data through a browser-based workflow, exposes recent microbiome-oriented correction methods together with general-purpose frameworks, and harmonizes pre- and post-correction diagnostics and downloadable outputs.

Changes made: The Introduction includes a concise positioning sentence. We keep the fuller explanation in this response to avoid interrupting the Application Note narrative.

### General comment 5

Reviewer comment: The server should provide an output summary so users can quickly locate corrected data and other files.

Response: We agree. We separated the downloadable server outputs into a user-facing output bundle and a reproducibility bundle.

Changes made: The output bundle includes generated outputs such as corrected matrices, figures, metric tables, logs, validation report, runtime summary, and parameter/reproducibility manifests. The reproducibility bundle is separately scoped for restoring and rerunning a session.

### General comment 6

Reviewer comment: The web server supports more correction methods than those discussed and evaluated in the manuscript; please explain why and provide comparative results for other methods.

Response: We agree that the manuscript should not appear to omit server-supported methods. We revised the main text to describe all 12 correction methods currently supported by mBatchNet: ConQuR, MMUPHin, PLSDA-batch, DEBIAS-M, MetaDICT, ComBat, limma, ComBat-seq, FAbatch, RUV-III-NB, FSQN, and BMC. We also added references for RUV-III-NB, FSQN, and BMC.

The displayed anaerobic digestion case study focuses on a compact set of correction outputs to keep the main figure readable within the Application Note page limit. To make the relationship between the website and manuscript explicit, we added a supplementary method-coverage table listing all 12 supported methods and indicating whether each method is displayed in the main figure, included in supplementary case-study statistics, or documented as a supported method that can be run when its input assumptions are met. We also include numerical statistics for the displayed outputs in the main Results and Supplementary Table S1.

Changes made: The Summary now states "12 supported" correction methods; the Features/Implementation section lists all 12 methods and groups them; Supplementary Note 1 maps method categories; Supplementary Table S2 documents method coverage; reference entries were added for RUV-III-NB, FSQN, and BMC.

### Minor comment 1

Reviewer comment: CLR should be defined before first use.

Response: Corrected.

Changes made: The assessment description now defines the centered log-ratio (CLR) transformation and explains that Aitchison distance is Euclidean distance between CLR-transformed profiles.

### Minor comment 2

Reviewer comment: Full names of abbreviations should be available for readers.

Response: We revised the manuscript and supplement to improve the explanation of the assessment abbreviations and metric categories.

Changes made: The main text now defines CLR and groups diagnostics; Supplementary Note 1 provides expanded explanations for PCA, PCoA, NMDS, ANOSIM, PERMANOVA, pRDA, PVCA, alignment, entropy of batch mixing, and silhouette.

### Minor comment 3

Reviewer comment: "Mbecs" should be "MBECS."

Response: Corrected.

Changes made: The manuscript now uses "MBECS".

## Reviewer 5

### Comment 1

Reviewer comment: The Summary may imply that the authors developed a new batch-correction method; clarify software integration versus methodological innovation.

Response: We agree. We revised the Summary and Introduction to state that mBatchNet integrates established methods for diagnosis, correction, and assessment in a single web-server workflow.

Changes made: The Summary now describes mBatchNet as integrating established methods and 12 supported correction methods; the Introduction and Conclusion emphasize web-server workflow, harmonized diagnostics, and exportable outputs.

### Comment 2

Reviewer comment: Clarify whether the server and protocol are applicable to shotgun metagenomic datasets.

Response: We clarified the input boundary conservatively. mBatchNet operates on processed feature tables and matching metadata. Therefore, it can accept compatible taxonomic or functional abundance/count tables derived from 16S or shotgun metagenomic profiling after upstream processing. It does not process raw sequencing reads and the current illustrative benchmark is a 16S rRNA amplicon dataset.

Changes made: The manuscript now states that the case study and manuscript-level validation are based on a 16S rRNA OTU table, while compatible preprocessed microbiome feature tables, including shotgun-derived taxonomic or functional profiles, can be used as inputs when method assumptions are met. The upload guidance and validation policy clarify that raw sequencing reads and upstream profiling steps are outside the scope of mBatchNet. We avoided claiming full biological validation on a shotgun benchmark not included in the manuscript.

### Comment 3

Reviewer comment: Explain how sparsity, compositionality, and zero inflation complicate batch-effect detection and correction.

Response: We agree. We added a concise explanation in the Introduction.

Changes made: The Introduction now states that sparsity and zero inflation make feature detection uneven across samples, while compositional scaling means technical shifts in one set of features can affect the relative abundance of others.

### Comment 4

Reviewer comment: Specify that "microbiome-oriented methods" refers to microbiome-oriented batch-correction methods.

Response: Corrected.

Changes made: The Introduction now uses "microbiome-oriented batch-effect correction methods."

### Comment 5

Reviewer comment: Clarify why differences in input formats/requirements constitute a practical challenge.

Response: We revised the manuscript to be more specific. The challenge is not file format alone, but heterogeneous assumptions, required input scale, covariate support, method parameters, output formats, and evaluation workflows.

Changes made: The Introduction and Features/Implementation section now explain that methods differ in whether they require count-like inputs or transformed abundance profiles, whether they support covariates, and whether target labels enter the adjustment model.

### Comment 6

Reviewer comment: Clarify the distinction between mBatchNet and existing resources, particularly MBECS.

Response: We agree. We added concise positioning in the Introduction while keeping the main paper focused on mBatchNet.

Changes made: The Introduction now distinguishes BatchServer as a general omics web interface centered on ComBat-based correction and MBECS as an R package for microbiome correction/evaluation routines. It then states the gap addressed by mBatchNet: a browser-based platform targeting microbiome abundance data while exposing recent microbiome-specific correction methods and harmonized pre-/post-correction diagnostics.

### Comment 7

Reviewer comment: Provide information on scalability, performance, expected runtime, memory usage, and practical constraints.

Response: We added practical public-server constraints to the upload workflow and runtime information to the correction workflow. The current public server enforces 10 MB per CSV file, 1,000 samples, 1,000 features, 1,000,000 matrix cells, and five metadata columns. It also reports current-session elapsed times for method runs, with average elapsed-time information where available. This gives users a concrete pre-check before analysis and a runtime reference during method execution. In addition, before final submission we will include a small measured performance table reporting runtime and peak memory for the bundled example dataset and representative larger matrices under a stated hardware/software environment.

Changes made: The upload interface now states public-server limits and validates file/table size before preprocessing. The correction interface reports elapsed time from session logs and available average elapsed-time information. The supplement now documents the public-server limits and runtime/output summary files; measured runtime and peak-memory values will be filled after benchmark completion.

### Comment 8

Reviewer comment: Explain Aitchison distance and why it is appropriate for compositional microbiome data.

Response: We added a concise workflow definition and explicit zero-handling detail.

Changes made: The assessment section now states that abundances are transformed using CLR and that Aitchison distance is Euclidean distance between CLR-transformed profiles. Supplementary Note 1 states that non-negative count or abundance matrices are first converted to relative-abundance profiles and that zeros are replaced with a small positive value before log transformation and centering.

### Comment 9

Reviewer comment: Clarify that Bray-Curtis analyses operate on relative abundances.

Response: Corrected.

Changes made: The assessment section now states that Bray-Curtis dissimilarity is computed on relative-abundance profiles.

### Comment 10

Reviewer comment: Explain how diagnostic methods contribute to evaluating batch correction performance.

Response: We agree. We revised the manuscript and supplement to group diagnostics by the question they address.

Changes made: The main text now groups diagnostics into four categories: ordination plots summarize dominant sample structure; distance-matrix tests and heatmaps quantify batch-associated separation; variance-partitioning analyses summarize variation attributed to batch, target, overlap, and residual components; and neighborhood or embedding metrics summarize local cross-batch mixing and target-label separation. Supplementary Note 1 provides expanded interpretation guidance.

## Editorial and Submission Items

We also addressed the submission instructions in the decision letter as follows:

- Revised manuscript changes are marked in red text.
- The supplementary material is prepared cleanly, without marked changes, for final submission.
- The public URL is standardized as https://mbatchnet.com/.
- The Availability statement includes the GitHub source-code repository and will include the archival DOI once the exact revised code snapshot is archived before final submission.

We again thank the reviewers for their helpful comments.
