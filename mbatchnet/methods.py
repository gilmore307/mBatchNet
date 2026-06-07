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

