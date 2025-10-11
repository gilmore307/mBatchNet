# Environment setup

This folder collects everything required to bootstrap a fully working environment
for the Batch Effect Correction dashboard and the accompanying R-based
correction methods.

## Quick start

```bash
# From the repository root
./env/setup.sh  # uses python3 and Rscript from PATH
```

You can optionally provide explicit interpreters:

```bash
./env/setup.sh /opt/venv/bin/python /usr/local/bin/Rscript
```

The script will
1. upgrade `pip` for the selected interpreter,
2. install the Python packages from [`requirements.txt`](./requirements.txt),
3. call [`r-packages.R`](./r-packages.R) to install all required R packages from
   CRAN, Bioconductor, or GitHub.

After the script finishes export `RETICULATE_PYTHON` so that the R methods reuse
the same Python interpreter when calling DEBIAS through `reticulate`:

```bash
export RETICULATE_PYTHON="$(python3 -c 'import sys; print(sys.executable)')"
```

## Python dependencies

The Python side of the project is a Dash web application. The key libraries are:

- `dash`, `dash-bootstrap-components`, and `dash-ag-grid` for the UI.
- `gunicorn` for production hosting (skip on Windows if desired).
- `numpy` and `DEBIAS-M` to support the DEBIAS method invoked from R.
- `pandas` for lightweight data wrangling in helpers and notebooks.

See [`requirements.txt`](./requirements.txt) for version pins. Feel free to
install the list into a virtual environment or conda environment if you prefer
not to use the provided script.

## R dependencies

The R scripts live in `methods.R` and `preprocess.R` and implement every batch
correction algorithm. They rely on a mix of CRAN, Bioconductor, and GitHub
packages. [`r-packages.R`](./r-packages.R) installs the complete set, including:

- CRAN: `jsonlite`, `doParallel`, `reticulate`, `pamr`, `vegan`, `bapred`.
- Bioconductor: `preprocessCore`, `limma`, `sva`, `MMUPHin`.
- GitHub: `wdl2459/ConQuR`, `jenniferfranks/FSQN`, `limfuxing/ruvIIInb`,
  `EvaYiwenWang/PLSDAbatch`, `BoYuan07/MetaDICT`.

If any package fails to install, check that you have a C/C++/Fortran toolchain
available (see below) and retry running `./env/setup.sh`.

## System packages

Several R packages require external libraries. On Debian/Ubuntu-based systems
install:

```bash
sudo apt-get update && sudo apt-get install -y \
  build-essential gfortran libxml2-dev libcurl4-openssl-dev libssl-dev \
  libgit2-dev libgsl0-dev libfftw3-dev libopenblas-dev liblapack-dev \
  libharfbuzz-dev libfribidi-dev
```

On macOS install Xcode command line tools (`xcode-select --install`) and
consider using Homebrew to install `libgit2`, `gsl`, and `fftw`. Windows users
can rely on Rtools.

## Conda environment (optional)

If you prefer conda, an [`environment.yml`](./environment.yml) is provided as a
starting point. It installs Python, `pip`, and a minimal R stack. After
creating the environment run `./env/setup.sh` **inside** the conda environment
to install the remaining R GitHub packages that conda does not provide.

## 将改动同步到 GitHub

完成环境脚本或其他文件的修改后，可按照以下步骤把改动推送到 GitHub：

1. 确认当前工作目录位于仓库根目录，并查看修改内容：
   ```bash
   git status
   git diff
   ```
2. 将确认过的文件加入暂存区：
   ```bash
   git add path/to/file1 path/to/file2
   ```
3. 使用清晰的信息提交：
   ```bash
   git commit -m "Describe the change"
   ```
4. 把提交推送到远程分支（如 `main` 或功能分支）：
   ```bash
   git push origin <branch-name>
   ```
5. 如果使用功能分支，登录 GitHub，在对应仓库中创建 Pull Request 并合并。

如在执行 `git push` 时遇到权限问题，需先在本地配置 GitHub 凭据或 SSH key。
