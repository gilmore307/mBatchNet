from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

from .methods import DISPLAY_BY_CODE, load_methods
from .dependencies import method_runtime_status
from .runtime import generate_previews, run_method, run_plot_script


@dataclass(frozen=True)
class AssessmentInfo:
    key: str
    title: str
    script: str
    stage: str
    description: str


PRE_ASSESSMENTS: tuple[AssessmentInfo, ...] = (
    AssessmentInfo("pca", "PCA", "pca.R", "pre", "Principal component views grouped by batch and target labels."),
    AssessmentInfo("pcoa", "PCoA", "pcoa.R", "pre", "Distance-based ordination in Aitchison and Bray-Curtis spaces."),
    AssessmentInfo("nmds", "NMDS", "NMDS.R", "pre", "Non-metric multidimensional scaling views for dissimilarity structure."),
    AssessmentInfo("dissimilarity", "Dissimilarity heatmaps", "Dissimilarity_Heatmaps.R", "pre", "Heatmap summaries of sample dissimilarity patterns."),
    AssessmentInfo("permanova", "PERMANOVA R2", "PERMANOVA.R", "pre", "Batch and target association effect-size summaries."),
    AssessmentInfo("r2", "Feature-wise ANOVA R2", "ANOVA.R", "pre", "Per-feature variance attribution to batch and target factors."),
    AssessmentInfo("prda", "pRDA", "pRDA.R", "pre", "Partial redundancy analysis of batch, target, and residual variance."),
    AssessmentInfo("pvca", "PVCA", "pvca.R", "pre", "Principal variance component analysis for study factors."),
)

POST_ONLY_ASSESSMENTS: tuple[AssessmentInfo, ...] = (
    AssessmentInfo("alignment", "Alignment score", "Alignment_Score.R", "post", "Local batch alignment after correction."),
    AssessmentInfo("ebm", "Entropy score", "Entropy_Score.R", "post", "Neighborhood-level entropy of batch mixing."),
    AssessmentInfo("silhouette", "Silhouette score", "Silhouette.R", "post", "UMAP silhouette summaries after correction."),
)

POST_ASSESSMENTS: tuple[AssessmentInfo, ...] = tuple(
    AssessmentInfo(item.key, item.title, item.script, "post", item.description) for item in PRE_ASSESSMENTS
) + POST_ONLY_ASSESSMENTS


def assessments_for(stage: str) -> tuple[AssessmentInfo, ...]:
    return POST_ASSESSMENTS if stage == "post" else PRE_ASSESSMENTS


def assessment_by_key(stage: str, key: str) -> AssessmentInfo | None:
    for item in assessments_for(stage):
        if item.key == key:
            return item
    return None


def run_assessment(session_dir: Path, stage: str, key: str, args: Iterable[str] = ()) -> bool:
    item = assessment_by_key(stage, key)
    if item is None:
        return False
    ok = run_plot_script(session_dir, item.script, tuple(args))
    if ok:
        generate_previews(session_dir)
    return ok


def run_all_assessments(session_dir: Path, stage: str) -> bool:
    ok = True
    for item in assessments_for(stage):
        ok = run_assessment(session_dir, stage, item.key) and ok
    return ok


def run_correction_method(session_dir: Path, method_code: str, params: dict[str, object] | None = None) -> bool:
    return run_method(session_dir, method_code, params=params)


def list_files(session_dir: Path) -> list[dict[str, object]]:
    if not session_dir.exists():
        return []
    files: list[dict[str, object]] = []
    for path in sorted(session_dir.rglob("*")):
        if not path.is_file():
            continue
        rel = path.relative_to(session_dir).as_posix()
        files.append(
            {
                "path": rel,
                "name": path.name,
                "size": path.stat().st_size,
                "is_image": path.suffix.lower() in {".png", ".jpg", ".jpeg"},
                "is_tiff": path.suffix.lower() in {".tif", ".tiff"},
                "is_csv": path.suffix.lower() == ".csv",
            }
        )
    return files


def stage_files(session_dir: Path, stage: str) -> list[dict[str, object]]:
    files = list_files(session_dir)
    if stage == "pre":
        return [
            item
            for item in files
            if "_pre" in str(item["name"])
            or str(item["name"]).startswith(("pca_", "pcoa_", "nmds_", "dissimilarity_", "permanova_", "anova_", "pRDA_", "PVCA", "mosaic_"))
            or str(item["name"]) in {"raw_clr.csv", "raw_tss.csv"}
        ]
    if stage == "post":
        return [
            item
            for item in files
            if "_post" in str(item["name"])
            or str(item["name"]).startswith(("alignment_", "ebm", "silhouette", "pca_", "pcoa_", "nmds_", "dissimilarity_", "permanova_", "anova_", "pRDA_", "PVCA"))
        ]
    return files


def method_status(session_dir: Path) -> list[dict[str, object]]:
    outputs = {item["name"] for item in list_files(session_dir)}
    rows = []
    for method in load_methods():
        normalized = method.code.lower().replace("-", "").replace("_", "")
        has_output = any(
            name.startswith("normalized_") and normalized in name.lower().replace("-", "").replace("_", "")
            for name in outputs
        )
        runtime_status = method_runtime_status(method.code)
        rows.append(
            {
                "method": method,
                "selected": has_output,
                "available": runtime_status.available,
                "unavailable_reason": runtime_status.reason,
            }
        )
    return rows


def display_name_for(code: str) -> str:
    return DISPLAY_BY_CODE.get(code, code)
