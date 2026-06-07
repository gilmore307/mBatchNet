from __future__ import annotations

import importlib.util
import os
import shutil
import subprocess
import sys
import time
from dataclasses import dataclass
from typing import Iterable

from .methods import METHOD_RUNTIME_REQUIREMENTS


RSCRIPT_CHECK_COMMAND = (
    "Rscript",
    "--no-save",
    "--no-restore",
    "--no-site-file",
    "-e",
    "pkgs <- strsplit(Sys.getenv('MBATCHNET_R_PACKAGES'), ',')[[1]]; "
    "pkgs <- pkgs[nzchar(pkgs)]; "
    "for (pkg in pkgs) cat(pkg, requireNamespace(pkg, quietly = TRUE), '\\n')",
)

_R_CACHE: dict[str, bool] = {}
_R_CACHE_AT = 0.0
_R_CACHE_TTL_SECONDS = 60.0


@dataclass(frozen=True)
class RuntimeStatus:
    available: bool
    reason: str = ""


def _required_r_packages() -> tuple[str, ...]:
    packages: list[str] = []
    for requirement in METHOD_RUNTIME_REQUIREMENTS.values():
        packages.extend(requirement.get("r", ()))
    return tuple(sorted(set(packages)))


def _check_r_packages() -> dict[str, bool]:
    global _R_CACHE_AT
    now = time.time()
    if _R_CACHE and now - _R_CACHE_AT < _R_CACHE_TTL_SECONDS:
        return dict(_R_CACHE)

    packages = _required_r_packages()
    if not packages:
        return {}
    if shutil.which("Rscript") is None:
        return {package: False for package in packages}

    env = os.environ.copy()
    env["MBATCHNET_R_PACKAGES"] = ",".join(packages)
    try:
        proc = subprocess.run(
            RSCRIPT_CHECK_COMMAND,
            check=False,
            capture_output=True,
            text=True,
            encoding="utf-8",
            errors="replace",
            timeout=30,
            env=env,
        )
    except (OSError, subprocess.TimeoutExpired):
        return {package: False for package in packages}

    results = {package: False for package in packages}
    for line in proc.stdout.splitlines():
        parts = line.strip().split()
        if len(parts) >= 2 and parts[0] in results:
            results[parts[0]] = parts[1].upper() == "TRUE"

    _R_CACHE.clear()
    _R_CACHE.update(results)
    _R_CACHE_AT = now
    return dict(_R_CACHE)


def _python_module_available(module: str) -> bool:
    return importlib.util.find_spec(module) is not None


def method_runtime_status(method_code: str) -> RuntimeStatus:
    requirement = METHOD_RUNTIME_REQUIREMENTS.get(method_code, {})
    r_packages = tuple(requirement.get("r", ()))
    py_modules = tuple(requirement.get("python", ()))

    missing: list[str] = []
    if r_packages:
        r_results = _check_r_packages()
        missing.extend(f"R package {package}" for package in r_packages if not r_results.get(package, False))
    if py_modules:
        missing.extend(f"Python module {module}" for module in py_modules if not _python_module_available(module))

    if missing:
        return RuntimeStatus(False, "Missing " + ", ".join(missing) + f" in {sys.executable}.")
    return RuntimeStatus(True)
