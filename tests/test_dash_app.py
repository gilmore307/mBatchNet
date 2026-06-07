import unittest

from dash import Dash

import server
from _4_upload import upload_layout


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


if __name__ == "__main__":
    unittest.main()
