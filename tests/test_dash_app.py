import unittest
import base64
import csv
import tempfile
import shutil
import re
import zipfile
from io import BytesIO
from pathlib import Path

from dash import Dash, html

import server
from _0_main import _build_download_bundle
from _0_main import _download_bundle_kind_from_click
from _2_utils import _DETAILS_INTERPRETATION
from _2_utils import write_session_manifests
from _2_utils import METHOD_REFERENCE_BY_CODE
from _2_utils import archive_assessment_figure_outputs
from _2_utils import clear_assessment_outputs
from _2_utils import clear_session_derived_outputs
from _2_utils import PREVIEW_SENTINEL
from _1_components import build_navbar
from _5_assessment import _expected_figure_files
from _6_correction import _PARAMETER_CONFIG
from _6_correction import _build_parameter_layout
from _6_correction import _header_with_tooltip
from _6_correction import _build_method_explanation_layout
from _6_correction import _parameter_input
from _6_correction import correction_layout
from _7_description import HELP_MODAL_SECTIONS
from _7_description import HELP_SECTION_TOC
from _4_upload import upload_layout
from _4_upload import validate_session_inputs
from _4_upload import _restore_repro_bundle
from _4_upload import _first_non_empty_level
from _4_upload import MAX_FEATURES
from _4_upload import MAX_MATRIX_CELLS
from _4_upload import MAX_METADATA_COLUMNS
from _4_upload import MAX_SAMPLES
from _4_upload import MAX_UPLOAD_BYTES


def _component_text(component):
    if isinstance(component, str):
        return component
    children = getattr(component, "children", None)
    if children is None:
        return ""
    if isinstance(children, (list, tuple)):
        return " ".join(_component_text(child) for child in children)
    return _component_text(children)


def _read_csv_records_for_test(path):
    with Path(path).open("r", encoding="utf-8", newline="") as fh:
        reader = csv.DictReader(fh)
        return list(reader.fieldnames or []), list(reader)


class DashAppTests(unittest.TestCase):
    def test_server_entrypoint_serves_dash_app(self):
        self.assertIsInstance(server.app, Dash)

        client = server.server.test_client()
        response = client.get("/")

        self.assertEqual(response.status_code, 200)
        self.assertIn(b"_dash", response.data)

    def test_upload_layout_contains_original_example_flow(self):
        text = _component_text(upload_layout("/upload"))

        self.assertIn("Input requirements", text)
        self.assertIn("Quick Start: Example Data", text)
        self.assertIn("Preview rows", text)
        self.assertIn("Preview columns", text)
        self.assertIn("Load Selected Example", text)
        self.assertIn("Restore from Repro bundle", text)
        self.assertIn("Upload Repro bundle", text)
        self.assertIn("reproducibility_bundle.zip", text)
        self.assertIn("How to get a Repro bundle", text)
        self.assertIn("Click Repro bundle in the navbar", text)
        self.assertIn("Repro bundle", text)
        self.assertIn("Process", text)
        self.assertIn("shotgun metagenomics", text)
        self.assertIn("samples in rows", text)
        self.assertIn("1000 samples", text)
        self.assertIn("1000 features", text)
        self.assertIn("10.0 MB per CSV", text)
        self.assertIn("1,000,000 matrix cells", text)
        self.assertIn("Metadata limit: 5 columns or fewer", text)
        self.assertIn("Confounding check", text)
        self.assertIn("batch removal may also remove biological signal", text)

    def test_public_upload_limits_match_server_contract(self):
        self.assertEqual(MAX_SAMPLES, 1000)
        self.assertEqual(MAX_FEATURES, 1000)
        self.assertEqual(MAX_UPLOAD_BYTES, 10 * 1024 * 1024)
        self.assertEqual(MAX_MATRIX_CELLS, 1_000_000)
        self.assertEqual(MAX_METADATA_COLUMNS, 5)

    def test_navbar_exposes_two_download_entries(self):
        text = _component_text(build_navbar("/post"))

        self.assertIn("Download outputs", text)
        self.assertIn("Repro bundle", text)
        self.assertNotIn("Full " + "session", text)

    def test_download_requires_explicit_button_click(self):
        self.assertIsNone(_download_bundle_kind_from_click("download-results-btn", None, None))
        self.assertIsNone(_download_bundle_kind_from_click("download-results-btn", 0, None))
        self.assertIsNone(_download_bundle_kind_from_click("download-reproducibility-btn", None, 0))
        self.assertEqual(_download_bundle_kind_from_click("download-results-btn", 1, None), "outputs")
        self.assertEqual(
            _download_bundle_kind_from_click("download-reproducibility-btn", None, 1),
            "reproducibility",
        )

    def test_correction_layout_omits_method_guide(self):
        text = _component_text(correction_layout("/correction"))

        self.assertNotIn("Method guide", text)
        self.assertNotIn("Phenotype-aware correction", text)
        self.assertIn("package or source reference", text)

    def test_help_modal_matches_repro_bundle_download_flow(self):
        text = _component_text(html.Div(HELP_MODAL_SECTIONS))

        self.assertIn("Repro bundle", text)
        self.assertIn("Upload a reproducibility_bundle.zip exported from mBatchNet", text)
        self.assertIn("Use Download outputs", text)
        self.assertIn("restore the session", text)
        self.assertNotIn("session bundle", text)
        self.assertNotIn("Download results", text)
        self.assertNotIn("recommended Batch", text)
        self.assertNotIn("best balances", text)

    def test_help_modal_lists_correction_methods_and_parameters(self):
        text = _component_text(html.Div(HELP_MODAL_SECTIONS))
        toc_titles = []
        for item in HELP_SECTION_TOC:
            toc_titles.append(item["title"])
            toc_titles.extend(child["title"] for child in item.get("children", []))
        toc_text = " ".join(toc_titles)

        self.assertIn("Methods and parameters", toc_text)
        self.assertIn("Methods and parameters", text)
        self.assertIn("DEBIAS-M", text)
        self.assertIn("MetaDICT", text)
        self.assertIn("ComBat-seq", text)
        self.assertIn("alpha", text)
        self.assertIn("par.prior", text)
        self.assertIn("gene.subset.n", text)
        self.assertIn("No method-specific parameters are exposed", text)
        self.assertIn("method package or citation records", text)

    def test_readme_documents_correction_methods_and_parameters(self):
        text = Path("README.md").read_text(encoding="utf-8")

        self.assertIn("## Correction Methods and Parameters", text)
        self.assertIn("assets/doc/methods.csv", text)
        self.assertIn("### DEBIAS-M", text)
        self.assertIn("### MetaDICT", text)
        self.assertIn("### ComBat-seq", text)
        self.assertIn("`alpha` (default: `0.05`)", text)
        self.assertIn("`par.prior` (default: `False`)", text)
        self.assertIn("Exposed parameters: none.", text)

    def test_user_facing_descriptions_avoid_subjective_method_guidance(self):
        banned_terms = (
            "recommend",
            "suggest",
            "suitable",
            "best",
            "better",
            "advantage",
            "should",
            "prefer",
            "preferred",
            "useful",
            "may help",
            "favorable",
        )
        method_text = " ".join(
            str(metadata.get("description", "")) for metadata in METHOD_REFERENCE_BY_CODE.values()
        )
        parameter_text = " ".join(
            str(spec.get("description", ""))
            for specs in _PARAMETER_CONFIG.values()
            for spec in specs
        )
        help_text = _component_text(html.Div(HELP_MODAL_SECTIONS))
        interpretation_text = " ".join(
            point
            for payload in _DETAILS_INTERPRETATION.values()
            for point in payload.get("points", ())
        )
        visible_text = " ".join([method_text, parameter_text, help_text, interpretation_text]).lower()

        for term in banned_terms:
            self.assertNotIn(term, visible_text)

    def test_time_header_explains_elapsed_time_source(self):
        text = _component_text(
            _header_with_tooltip(
                "Time (s)",
                "Elapsed seconds from the current session's completed method run, parsed from run.log or session_summary.json.",
                "method-time-help-test",
            )
        )

        self.assertIn("Time (s)", text)
        self.assertIn("?", text)
        self.assertIn("Elapsed seconds", text)
        self.assertIn("run.log", text)
        self.assertIn("session_summary.json", text)

    def test_method_explanation_uses_reference_fields(self):
        text = _component_text(
            _build_method_explanation_layout(
                "ComBat",
                {
                    "description": "ComBat uses empirical Bayes frameworks for adjusting known batch effects in genomic data.",
                    "package": "https://rdrr.io/bioc/sva/man/ComBat.html",
                    "citation": "Johnson WE, Li C, Rabinovic A. <i>Biostatistics.</i> 2007;8(1):118-127.",
                    "url": "https://academic.oup.com/nargab/article/2/3/lqaa078/5909519",
                },
            )
        )

        self.assertIn("Method Explanation", text)
        self.assertIn("Description", text)
        self.assertIn("empirical Bayes", text)
        self.assertIn("Package", text)
        self.assertIn("Citation", text)
        self.assertIn("Reference", text)
        self.assertIn("Johnson WE", text)
        self.assertNotIn("Suggested methods", text)

    def test_correction_table_does_not_repeat_citation_column(self):
        source = Path("_6_correction.py").read_text(encoding="utf-8")

        self.assertNotIn('html.Th("Citation", className="text-center")', source)
        self.assertNotIn("citation_cell", source)
        self.assertIn('html.Th("Explanation"', source)

    def test_method_reference_csv_provides_descriptions(self):
        for method_code, metadata in METHOD_REFERENCE_BY_CODE.items():
            description = metadata.get("description", "")

            self.assertTrue(description, method_code)
            self.assertGreaterEqual(len(description), 180, method_code)
            self.assertGreaterEqual(description.count("."), 3, method_code)
            self.assertIn("In mBatchNet", description, method_code)

    def test_pre_post_assessment_figures_use_distinct_stage_outputs(self):
        pre_outputs = set(_expected_figure_files("pre", "pca"))
        post_outputs = set(_expected_figure_files("post", "pca"))

        self.assertIn("pca_batch_pre.tif", pre_outputs)
        self.assertIn("pca_target_pre.tif", pre_outputs)
        self.assertIn("pca_batch_post.tif", post_outputs)
        self.assertIn("pca_target_post.tif", post_outputs)
        self.assertNotIn("pca_batch.tif", pre_outputs)
        self.assertNotIn("pca_batch.tif", post_outputs)
        self.assertTrue(pre_outputs.isdisjoint(post_outputs))

    def test_archiving_post_assessment_figure_preserves_pre_output(self):
        with tempfile.TemporaryDirectory() as tmp:
            session_dir = Path(tmp)
            (session_dir / "pca_batch_pre.tif").write_text("pre", encoding="utf-8")
            (session_dir / "pca_batch.tif").write_text("post", encoding="utf-8")

            archived = archive_assessment_figure_outputs(
                session_dir,
                "post",
                ["pca_batch.tif"],
            )

            self.assertEqual(archived, 1)
            self.assertEqual((session_dir / "pca_batch_pre.tif").read_text(encoding="utf-8"), "pre")
            self.assertEqual((session_dir / "pca_batch_post.tif").read_text(encoding="utf-8"), "post")
            self.assertFalse((session_dir / "pca_batch.tif").exists())

    def test_clearing_post_assessment_preserves_pre_assessment(self):
        with tempfile.TemporaryDirectory() as tmp:
            session_dir = Path(tmp)
            (session_dir / "pca_batch_pre.tif").write_text("pre", encoding="utf-8")
            (session_dir / "pca_batch_post.tif").write_text("post", encoding="utf-8")
            (session_dir / "permanova_raw_assessment_post.csv").write_text("post", encoding="utf-8")
            (session_dir / "method_ranking.csv").write_text("ranking", encoding="utf-8")
            (session_dir / PREVIEW_SENTINEL).write_text("preview", encoding="utf-8")

            removed = clear_assessment_outputs(session_dir, stage="post")

            self.assertGreaterEqual(removed, 3)
            self.assertTrue((session_dir / "pca_batch_pre.tif").exists())
            self.assertFalse((session_dir / "pca_batch_post.tif").exists())
            self.assertFalse((session_dir / "permanova_raw_assessment_post.csv").exists())
            self.assertFalse((session_dir / "method_ranking.csv").exists())
            self.assertFalse((session_dir / PREVIEW_SENTINEL).exists())

    def test_clearing_session_derived_outputs_preserves_uploaded_inputs_only(self):
        with tempfile.TemporaryDirectory() as tmp:
            session_dir = Path(tmp)
            (session_dir / "raw.csv").write_text("raw", encoding="utf-8")
            (session_dir / "metadata_origin.csv").write_text("metadata", encoding="utf-8")
            (session_dir / "metadata.csv").write_text("derived", encoding="utf-8")
            (session_dir / "normalized_limma_tss.csv").write_text("method", encoding="utf-8")
            (session_dir / "reproducibility_bundle.zip").write_text("zip", encoding="utf-8")

            removed = clear_session_derived_outputs(session_dir, preserve_inputs=True)

            self.assertEqual(removed, 3)
            self.assertTrue((session_dir / "raw.csv").exists())
            self.assertTrue((session_dir / "metadata_origin.csv").exists())
            self.assertFalse((session_dir / "metadata.csv").exists())
            self.assertFalse((session_dir / "normalized_limma_tss.csv").exists())
            self.assertFalse((session_dir / "reproducibility_bundle.zip").exists())

    def test_no_parameter_methods_use_objective_session_settings_message(self):
        text = _component_text(_build_parameter_layout("ZINBWaVE"))

        self.assertIn("No method-specific parameters are exposed", text)
        self.assertIn("uploaded matrix", text)
        self.assertNotIn("No configurable parameters available", text)

    def test_correction_parameters_match_r_scripts_and_have_tooltips(self):
        method_files = {
            "ComBat": "correction/methods/ComBat.R",
            "ConQuR": "correction/methods/ConQuR.R",
            "FAbatch": "correction/methods/FAbatch.R",
            "MetaDICT": "correction/methods/MetaDICT.R",
            "MMUPHin": "correction/methods/MMUPHin.R",
            "PLSDA": "correction/methods/PLSDA.R",
            "RUV": "correction/methods/RUV.R",
        }
        for method, path in method_files.items():
            text = Path(path).read_text(encoding="utf-8")
            script_params = set(re.findall(r'get_param\("([^"]+)"', text))
            ui_params = {str(spec["name"]) for spec in _PARAMETER_CONFIG.get(method, [])}
            self.assertEqual(script_params, ui_params, method)
            for spec in _PARAMETER_CONFIG[method]:
                self.assertTrue(spec.get("description"), f"{method}:{spec['name']}")
                rendered = _component_text(_parameter_input(method, spec))
                self.assertIn("?", rendered)

        mmuphin_conv = next(spec for spec in _PARAMETER_CONFIG["MMUPHin"] if spec["name"] == "conv")
        self.assertEqual(mmuphin_conv["default"], 0.000001)

    def test_example_dataset_passes_upload_validation(self):
        with tempfile.TemporaryDirectory() as tmp:
            session_dir = Path(tmp)
            shutil.copyfile("assets/example/raw_ad.csv", session_dir / "raw.csv")
            shutil.copyfile("assets/example/metadata_ad.csv", session_dir / "metadata_origin.csv")

            report = validate_session_inputs(
                session_dir,
                batch_col="Batch",
                target_col="Initial Phenol Concentration",
            )

            self.assertTrue(report["valid"], report)
            self.assertEqual(report["dimensions"]["metadata_rows"], report["dimensions"]["samples"])

    def test_example_study_settings_use_metadata_levels(self):
        header, rows = _read_csv_records_for_test("assets/example/metadata_ad.csv")

        self.assertIn("Batch", header)
        self.assertIn("Initial Phenol Concentration", header)
        self.assertEqual(_first_non_empty_level(rows, "Batch"), "Batch 1")
        self.assertEqual(_first_non_empty_level(rows, "Initial Phenol Concentration"), "0-0.5")

    def test_reproducibility_bundle_upload_restores_session_files(self):
        with tempfile.TemporaryDirectory() as tmp:
            session_dir = Path(tmp)
            bundle = BytesIO()
            with zipfile.ZipFile(bundle, "w") as zf:
                zf.writestr("raw.csv", "1,2\n3,4\n")
                zf.writestr("metadata_origin.csv", "Batch,Phenotype\nA,case\nB,control\n")
                zf.writestr("metadata.csv", "batch,Phenotype\nA,case\nB,control\n")
                zf.writestr("raw_tss.csv", "0.25,0.75\n0.43,0.57\n")
                zf.writestr("raw_clr.csv", "-0.55,0.55\n-0.14,0.14\n")
                zf.writestr("session_config.json", '{"label_column":"Phenotype"}')
                zf.writestr("run.log", "preprocess complete\n")
            encoded = base64.b64encode(bundle.getvalue()).decode("ascii")
            contents = "data:application/zip;base64," + encoded

            ok, _status, preprocess_ready = _restore_repro_bundle(contents, session_dir)

            self.assertTrue(ok)
            self.assertTrue(preprocess_ready)
            self.assertEqual((session_dir / "raw.csv").read_text(encoding="utf-8"), "1,2\n3,4\n")
            self.assertTrue((session_dir / "metadata_origin.csv").exists())
            self.assertTrue((session_dir / "raw_tss.csv").exists())
            self.assertTrue((session_dir / "raw_clr.csv").exists())
            self.assertTrue((session_dir / "run.log").exists())

    def test_reproducibility_bundle_upload_revalidates_bundled_inputs(self):
        with tempfile.TemporaryDirectory() as tmp:
            session_dir = Path(tmp)
            bundle = BytesIO()
            with zipfile.ZipFile(bundle, "w") as zf:
                zf.writestr("raw.csv", "f1,f2\n1,2\n3,4\n")
                zf.writestr("metadata_origin.csv", "Batch,Phenotype\nA,case\n")
                zf.writestr("session_config.json", '{"label_column":"Phenotype"}')
                zf.writestr("validation_report.json", '{"valid": true, "errors": []}')
            encoded = base64.b64encode(bundle.getvalue()).decode("ascii")
            contents = "data:application/zip;base64," + encoded

            ok, status, preprocess_ready = _restore_repro_bundle(contents, session_dir)

            self.assertFalse(ok)
            self.assertFalse(preprocess_ready)
            self.assertIn("row count", _component_text(status))

    def test_invalid_metadata_row_count_is_blocked(self):
        with tempfile.TemporaryDirectory() as tmp:
            session_dir = Path(tmp)
            (session_dir / "raw.csv").write_text("1,2,3\n4,5,6\n", encoding="utf-8")
            (session_dir / "metadata_origin.csv").write_text(
                "Batch,Phenotype\nA,case\nB,control\nA,case\n",
                encoding="utf-8",
            )

            report = validate_session_inputs(session_dir, batch_col="Batch", target_col="Phenotype")

            self.assertFalse(report["valid"])
            self.assertTrue(any("row count" in err for err in report["errors"]))

    def test_metadata_column_limit_is_enforced(self):
        with tempfile.TemporaryDirectory() as tmp:
            session_dir = Path(tmp)
            (session_dir / "raw.csv").write_text("f1,f2\n1,2\n3,4\n", encoding="utf-8")
            (session_dir / "metadata_origin.csv").write_text(
                "Batch,Phenotype,Cov1,Cov2,Cov3,Cov4\nA,case,1,2,3,4\nB,control,1,2,3,4\n",
                encoding="utf-8",
            )

            report = validate_session_inputs(session_dir, batch_col="Batch", target_col="Phenotype")

            self.assertFalse(report["valid"])
            self.assertTrue(any("5 columns or fewer" in err for err in report["errors"]))

    def test_strong_batch_target_confounding_is_warned(self):
        with tempfile.TemporaryDirectory() as tmp:
            session_dir = Path(tmp)
            (session_dir / "raw.csv").write_text(
                "f1,f2\n1,2\n2,3\n3,4\n4,5\n",
                encoding="utf-8",
            )
            (session_dir / "metadata_origin.csv").write_text(
                "Batch,Phenotype\nA,case\nA,case\nB,control\nB,control\n",
                encoding="utf-8",
            )

            report = validate_session_inputs(session_dir, batch_col="Batch", target_col="Phenotype")

            self.assertTrue(report["valid"], report)
            self.assertEqual(report["dimensions"].get("batch_target_cramers_v"), 1.0)
            self.assertTrue(any("strongly associated" in warning for warning in report["warnings"]))

    def test_shotgun_profile_table_contract_is_accepted(self):
        with tempfile.TemporaryDirectory() as tmp:
            session_dir = Path(tmp)
            (session_dir / "raw.csv").write_text(
                "sample_id,gene_a,gene_b,gene_c\nS1,10,0,4\nS2,0,5,3\nS3,7,1,0\nS4,2,8,9\n",
                encoding="utf-8",
            )
            (session_dir / "metadata_origin.csv").write_text(
                "Sample,Batch,Phenotype\nS1,A,case\nS2,A,control\nS3,B,case\nS4,B,control\n",
                encoding="utf-8",
            )

            report = validate_session_inputs(session_dir, batch_col="Batch", target_col="Phenotype")

            self.assertTrue(report["valid"], report)
            self.assertTrue(report["dimensions"]["header_detected"])
            self.assertTrue(report["dimensions"]["sample_id_column"])

    def test_session_manifests_are_written_for_download_bundle(self):
        with tempfile.TemporaryDirectory() as tmp:
            session_dir = Path(tmp)
            (session_dir / "raw.csv").write_text("1,2\n3,4\n", encoding="utf-8")
            (session_dir / "metadata_origin.csv").write_text(
                "Batch,Phenotype\nA,case\nB,control\n",
                encoding="utf-8",
            )
            validate_session_inputs(session_dir, batch_col="Batch", target_col="Phenotype")

            write_session_manifests(session_dir)

            self.assertTrue((session_dir / "output_summary.json").exists())
            self.assertTrue((session_dir / "runtime_summary.json").exists())
            self.assertTrue((session_dir / "parameter_manifest.json").exists())
            self.assertTrue((session_dir / "reproducibility_manifest.json").exists())

    def test_download_bundles_have_distinct_scopes(self):
        with tempfile.TemporaryDirectory() as tmp:
            session_dir = Path(tmp)
            (session_dir / "raw.csv").write_text("1,2\n3,4\n", encoding="utf-8")
            (session_dir / "metadata_origin.csv").write_text(
                "Batch,Phenotype\nA,case\nB,control\n",
                encoding="utf-8",
            )
            (session_dir / "normalized_limma_tss.csv").write_text("1,2\n3,4\n", encoding="utf-8")
            validate_session_inputs(session_dir, batch_col="Batch", target_col="Phenotype")
            write_session_manifests(session_dir)

            output_zip = _build_download_bundle(session_dir, "outputs")
            repro_zip = _build_download_bundle(session_dir, "reproducibility")

            import zipfile
            with zipfile.ZipFile(output_zip) as zf:
                output_names = set(zf.namelist())
            with zipfile.ZipFile(repro_zip) as zf:
                repro_names = set(zf.namelist())

            self.assertIn("normalized_limma_tss.csv", output_names)
            self.assertNotIn("raw.csv", output_names)
            self.assertIn("raw.csv", repro_names)
            self.assertIn("reproducibility_manifest.json", repro_names)


if __name__ == "__main__":
    unittest.main()
