from pathlib import Path
import tempfile
import unittest

import pandas as pd

from mbatchnet.validation import validate_uploaded_inputs


class ValidationTests(unittest.TestCase):
    def test_validate_uploaded_inputs_accepts_clean_example_shape(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            raw = root / "raw.csv"
            meta = root / "metadata_origin.csv"
            pd.DataFrame([[1, 0, 3], [0, 2, 4], [5, 1, 0]]).to_csv(raw, index=False, header=False)
            pd.DataFrame(
                {
                    "sample_id": ["S1", "S2", "S3"],
                    "Batch": ["A", "B", "A"],
                    "Phenotype": ["control", "case", "case"],
                }
            ).to_csv(meta, index=False)

            report = validate_uploaded_inputs(raw, meta, batch_column="Batch", target_column="Phenotype")

            self.assertEqual(report.n_samples, 3)
            self.assertEqual(report.n_features, 3)
            self.assertIn(report.status, {"ok", "warning"})
            self.assertFalse(any(item.level == "error" for item in report.messages))

    def test_validate_uploaded_inputs_blocks_missing_matrix_value(self):
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            raw = root / "raw.csv"
            meta = root / "metadata_origin.csv"
            raw.write_text("1,2\n3,\n", encoding="utf-8")
            pd.DataFrame({"Batch": ["A", "B"], "Phenotype": ["x", "y"]}).to_csv(meta, index=False)

            report = validate_uploaded_inputs(raw, meta, batch_column="Batch", target_column="Phenotype")

            self.assertEqual(report.status, "error")
            self.assertTrue(any(item.code == "matrix_non_numeric_or_missing" for item in report.messages))


if __name__ == "__main__":
    unittest.main()
