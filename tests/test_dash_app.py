import unittest
import tempfile
import shutil
from pathlib import Path

from dash import Dash

import server
from _2_utils import write_session_manifests
from _6_correction import correction_layout
from _4_upload import upload_layout
from _4_upload import validate_session_inputs


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

        self.assertIn("Quick Start: Example Data", text)
        self.assertIn("Preview rows", text)
        self.assertIn("Preview columns", text)
        self.assertIn("Load Selected Example", text)
        self.assertIn("Process", text)
        self.assertIn("shotgun metagenomics", text)
        self.assertIn("samples in rows", text)

    def test_correction_layout_contains_method_guide(self):
        text = _component_text(correction_layout("/correction"))

        self.assertIn("Method guide", text)
        self.assertIn("Phenotype-aware correction", text)

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


if __name__ == "__main__":
    unittest.main()
