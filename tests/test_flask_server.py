import unittest
import shutil

from server import app
from mbatchnet.runtime import get_session_dir


class FlaskServerTests(unittest.TestCase):
    def test_home_and_upload_pages_render(self):
        client = app.test_client()

        home = client.get("/")
        upload = client.get("/upload")
        example = client.get("/upload?tab=example")
        help_page = client.get("/help")

        self.assertEqual(home.status_code, 200)
        self.assertIn(b"mBatchNet", home.data)
        self.assertIn(b"Everything needed for practical batch-effect evaluation", home.data)
        self.assertEqual(upload.status_code, 200)
        self.assertIn(b"Manual Upload", upload.data)
        self.assertIn(b"Count matrix (CSV)", upload.data)
        self.assertEqual(example.status_code, 200)
        self.assertIn(b"Quick Start: Example Data", example.data)
        self.assertEqual(help_page.status_code, 200)
        self.assertIn(b"Help and tutorials", help_page.data)

    def test_pipeline_routes_do_not_dead_end(self):
        client = app.test_client()

        for path, expected in (
            ("/pre", b"Pre-correction Assessment"),
            ("/correction", b"Batch Effect Correction"),
            ("/post", b"Post-correction Assessment"),
        ):
            response = client.get(path)
            self.assertEqual(response.status_code, 200)
            self.assertIn(expected, response.data)

    def test_session_workflow_pages_render(self):
        client = app.test_client()
        session_id = "test-session-workflow"
        session_dir = get_session_dir(session_id)
        (session_dir / "raw.csv").write_text("1,2\n3,4\n", encoding="utf-8")
        (session_dir / "metadata_origin.csv").write_text(
            "sample_id,batch,phenotype\ns1,a,x\ns2,b,y\n",
            encoding="utf-8",
        )
        (session_dir / "metadata.csv").write_text(
            "sample_id,batch,target_binary\ns1,a,0\ns2,b,1\n",
            encoding="utf-8",
        )

        for path, expected in (
            (f"/sessions/{session_id}/pre", b"Run all assessments"),
            (f"/sessions/{session_id}/correction", b"Run Correction"),
            (f"/sessions/{session_id}/post", b"Post-correction Assessment"),
        ):
            response = client.get(path)
            self.assertEqual(response.status_code, 200)
            self.assertIn(expected, response.data)
        shutil.rmtree(session_dir, ignore_errors=True)


if __name__ == "__main__":
    unittest.main()
