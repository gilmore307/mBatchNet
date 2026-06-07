from __future__ import annotations

import threading
import time
import uuid
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass, asdict
from typing import Callable, Optional


@dataclass
class JobRecord:
    id: str
    kind: str
    session_id: str
    status: str
    started_at: float
    finished_at: Optional[float] = None
    error: str = ""

    def to_dict(self) -> dict:
        payload = asdict(self)
        payload["elapsed_sec"] = round(((self.finished_at or time.time()) - self.started_at), 3)
        return payload


class JobRunner:
    def __init__(self, max_workers: int = 2) -> None:
        self._executor = ThreadPoolExecutor(max_workers=max_workers)
        self._lock = threading.Lock()
        self._jobs: dict[str, JobRecord] = {}

    def submit(self, kind: str, session_id: str, fn: Callable[[], bool]) -> JobRecord:
        job = JobRecord(
            id=str(uuid.uuid4()),
            kind=kind,
            session_id=session_id,
            status="running",
            started_at=time.time(),
        )
        with self._lock:
            self._jobs[job.id] = job

        def _run() -> None:
            try:
                ok = bool(fn())
                job.status = "success" if ok else "failed"
            except Exception as exc:
                job.status = "failed"
                job.error = str(exc)
            finally:
                job.finished_at = time.time()

        self._executor.submit(_run)
        return job

    def get(self, job_id: str) -> Optional[JobRecord]:
        with self._lock:
            return self._jobs.get(job_id)


runner = JobRunner()

