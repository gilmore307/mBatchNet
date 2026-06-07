import unittest

from server import app


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


if __name__ == "__main__":
    unittest.main()
