from __future__ import annotations

import csv
from dataclasses import dataclass

from .paths import METHODS_REFERENCE_PATH


@dataclass(frozen=True)
class MethodInfo:
    code: str
    display: str
    citation: str = ""
    url: str = ""
    package: str = ""


DISPLAY_BY_CODE = {
    "BMC": "BMC",
    "limma": "Limma",
    "ConQuR": "ConQuR",
    "PLSDA": "PLSDA-batch",
    "ComBat": "ComBat",
    "FSQN": "FSQN",
    "MMUPHin": "MMUPHin",
    "RUV": "RUV-III-NB",
    "MetaDICT": "MetaDICT",
    "FAbatch": "FAbatch",
    "ComBatSeq": "ComBat-seq",
    "DEBIAS": "DEBIAS-M",
}


PARAMETER_CONFIG = {
    "ComBat": [{"name": "par.prior", "type": "boolean", "default": False}],
    "ConQuR": [
        {"name": "logistic_lasso", "type": "boolean", "default": False},
        {"name": "quantile_type", "type": "text", "default": "standard"},
        {"name": "lambda_quantile", "type": "text", "default": "2p/n"},
        {"name": "interplt", "type": "boolean", "default": False},
        {"name": "delta", "type": "number", "default": 0.4999},
        {"name": "taus", "type": "text", "default": "seq(0.05,0.95,0.05)"},
    ],
    "FAbatch": [
        {"name": "minerr", "type": "number", "default": 0.000001},
        {"name": "probcrossbatch", "type": "boolean", "default": False},
        {"name": "maxnbf", "type": "number", "default": 8},
    ],
    "MetaDICT": [
        {"name": "alpha", "type": "number", "default": 0.05},
        {"name": "beta", "type": "number", "default": 0.2},
        {"name": "normalization", "type": "text", "default": "uq"},
    ],
    "MMUPHin": [
        {"name": "zero_inflation", "type": "boolean", "default": False},
        {"name": "conv", "type": "number", "default": 0.0001},
    ],
    "PLSDA": [
        {"name": "ncomp.trt", "type": "number", "default": 1},
        {"name": "ncomp.bat", "type": "number", "default": 5},
        {"name": "keepX.trt", "type": "number", "default": 50},
        {"name": "near.zero.var", "type": "boolean", "default": False},
        {"name": "balance", "type": "boolean", "default": False},
    ],
    "RUV": [
        {"name": "k", "type": "number", "default": 2},
        {"name": "use.pseudosample", "type": "boolean", "default": False},
        {"name": "batch.disp", "type": "boolean", "default": False},
        {"name": "zeroinf", "type": "boolean", "default": False},
    ],
}


METHOD_RUNTIME_REQUIREMENTS = {
    "BMC": {"r": ("pamr",)},
    "limma": {"r": ("limma",)},
    "ConQuR": {"r": ("ConQuR", "doParallel")},
    "PLSDA": {"r": ("PLSDAbatch",)},
    "ComBat": {"r": ("sva",)},
    "FSQN": {"r": ()},
    "MMUPHin": {"r": ("MMUPHin",)},
    "RUV": {"r": ("ruvIIInb",)},
    "MetaDICT": {"r": ("MetaDICT", "vegan")},
    "FAbatch": {"r": ("bapred",)},
    "ComBatSeq": {"r": ("sva",)},
    "DEBIAS": {"r": ("reticulate",), "python": ("numpy", "debiasm")},
}


def load_methods() -> list[MethodInfo]:
    reference = {}
    if METHODS_REFERENCE_PATH.exists():
        with METHODS_REFERENCE_PATH.open(newline="", encoding="utf-8-sig") as handle:
            for row in csv.DictReader(handle):
                display = (row.get("Methods") or "").strip()
                if display:
                    reference[display] = row
    methods = []
    for code, display in DISPLAY_BY_CODE.items():
        row = reference.get(display, {})
        methods.append(
            MethodInfo(
                code=code,
                display=display,
                citation=(row.get("Citations") or "").strip(),
                url=(row.get("Url") or "").strip(),
                package=(row.get("Package") or "").strip(),
            )
        )
    return methods
