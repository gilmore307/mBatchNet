import unittest

from server import app


class FlaskServerTests(unittest.TestCase):
    def test_home_and_upload_pages_render(self):
        client = app.test_client()

        home = client.get("/")
        upload = client.get("/upload")

        self.assertEqual(home.status_code, 200)
        self.assertIn(b"mBatchNet", home.data)
        self.assertEqual(upload.status_code, 200)
        self.assertIn(b"Manual Upload", upload.data)


if __name__ == "__main__":
    unittest.main()
