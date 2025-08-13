from setuptools import setup, find_packages
import os

# Configure adamantine source
# You can override this via environment variable ADAMANTINE_GIT_URL, e.g.:
#   export ADAMANTINE_GIT_URL=https://github.com/adamantine-lang/adamantine.git@main
_default_adamantine = "https://github.com/exfenestram/adamantine.git"
_raw_url = os.environ.get("ADAMANTINE_GIT_URL", _default_adamantine)
# Ensure PEP 508 style direct URL (prefix with git+ if missing)
_adamantine_url = _raw_url if _raw_url.startswith("git+") else f"git+{_raw_url}"

install_requires = [
    "pyrsistent>=0.19",
    f"adamantine @ {_adamantine_url}",
]

# Read long description from MANUAL.md if available
long_description = ""
long_description_ct = "text/markdown"
manual_path = os.path.join(os.path.dirname(__file__), "MANUAL.md")
if os.path.exists(manual_path):
    with open(manual_path, "r", encoding="utf-8") as fh:
        long_description = fh.read()

setup(
    name="lysp",
    version="0.1.0",
    description="Lisp→Python AST compiler with hygienic macros and Python interop",
    long_description=long_description,
    long_description_content_type=long_description_ct,
    packages=find_packages(exclude=("tests", "tests.*")),
    python_requires=">=3.8",
    install_requires=install_requires,
    include_package_data=True,
    entry_points={
        "console_scripts": [
            "lysp=lysp.lrepl:run_repl",
        ]
    },
    scripts=["bin/lysp"],
    classifiers=[
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3 :: Only",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Topic :: Software Development :: Compilers",
    ],
)
