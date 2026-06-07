import unittest
import base64
import tempfile
import shutil
import re
import zipfile
from io import BytesIO
from pathlib import Path

from dash import Dash

import server
from _0_main import _build_download_bundle
from _0_main import _download_bundle_kind_from_click
from _2_utils import write_session_manifests
from _1_components import build_navbar
from _6_correction import _PARAMETER_CONFIG
from _6_correction import _build_method_explanation_layout
from _6_correction import _parameter_input
from _6_correction import correction_layout
from _4_upload import upload_layout
from _4_upload import validate_session_inputs
from _4_upload import _restore_repro_bundle


def _component_text(component):
    if isinstance(component, str):
        return component
    children = getattr(component, "children", None)
    if children is None:
        return ""
    if isinstance(children, (list, tuple)):
        return " ".join(_component_text(child) for child in children)
    return _component_text(children)


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
        self.assertIn("Open Download and choose Repro bundle", text)
        self.assertIn("Repro bundle", text)
        self.assertIn("Process", text)
        self.assertIn("shotgun metagenomics", text)
        self.assertIn("samples in rows", text)

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

    def test_method_explanation_uses_reference_fields(self):
        text = _component_text(
            _build_method_explanation_layout(
                "ComBat",
                {
                    "package": "https://rdrr.io/bioc/sva/man/ComBat.html",
                    "citation": "Johnson WE, Li C, Rabinovic A. <i>Biostatistics.</i> 2007;8(1):118-127.",
                    "url": "https://academic.oup.com/nargab/article/2/3/lqaa078/5909519",
                },
            )
        )

        self.assertIn("Method Explanation", text)
        self.assertIn("Package", text)
        self.assertIn("Citation", text)
        self.assertIn("Reference", text)
        self.assertIn("Johnson WE", text)
        self.assertNotIn("Suggested methods", text)

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
