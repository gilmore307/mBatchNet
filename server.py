from __future__ import annotations

import os

from _0_main import app, server


if __name__ == "__main__":
    default_host = "127.0.0.1" if os.name == "nt" else "0.0.0.0"
    host = os.getenv("HOST", default_host)
    try:
        port = int(os.getenv("PORT", "8050"))
    except ValueError:
        port = 8050
    debug = os.getenv("DASH_DEBUG", "1") == "1"
    app.run(debug=debug, host=host, port=port, use_reloader=False)
